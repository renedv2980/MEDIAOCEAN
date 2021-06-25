*          DATA SET SPREPSS02  AT LEVEL 014 AS OF 01/20/11                      
*PHASE SPSS02A                                                                  
*INCLUDE SORTER                                                                 
*INCLUDE DLFLD                                                                  
         TITLE 'SPSS02  SUPERDESK AUTHORIZATION STATUS REPORT'                  
SPSS02   CSECT                                                                  
         NMOD1 0,SPSS02,R8                                                      
         L     RC,=A(SPSSWORK)                                                  
         USING SPSSWORK,RC                                                      
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         LA    R1,HEADHK                                                        
         ST    R1,HEADHOOK                                                      
         MVI   RCSUBPRG,1                                                       
*                                                                               
         CLI   MODE,REQFRST                                                     
         BNE   MAIN10                                                           
         L     RE,ADCONLST                                                      
         MVC   OFFICER,VOFFICER-SPADCONS(RE)                                    
         XC    OFCLIST,OFCLIST     CLEAR OFFICE LIST                            
         LA    R1,4                FOR BCT LOOP                                 
         LA    R2,CLTLIST          CLIENT LIST                                  
MAIN05   XC    0(250,R2),0(R2)     CLEAR 250 BYTES OF THE CLIENT LIST           
         AHI   R2,250              BUMP BY 250                                  
         BCT   R1,MAIN05           CLEAR THE REST OF THE LIST                   
         CLI   QOPT3,C'D'          DOWNLOAD                                     
         BNE   EXIT                                                             
         MVI   FORCEHED,C'Y'                                                    
         XC    HEADHOOK,HEADHOOK                                                
         MVI   RCSUBPRG,0                                                       
         BRAS  RE,DOWNLD                                                        
         GOTO1 REPORT                                                           
         B     EXIT                                                             
*                                                                               
MAIN10   CLI   MODE,CLTFRST                                                     
         BE    RDAUTHS                                                          
         B     EXIT                                                             
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
EXIT     XIT1                                                                   
RELO     DC    A(0)                                                             
*                                                                               
***********************************************************************         
*        READ AUTHS - FILTER - AND ADD SORT RECORDS                             
***********************************************************************         
RDAUTHS  DS    0H                  READ AUTHORIZATIONS AND FILTER               
*                                                                               
         MVI   ANYDATA,C'N'                                                     
         BAS   RE,INIT             ANY INITIALIZING                             
         GOTO1 DATCON,DMCB,(5,0),(3,TODAYB)                                     
         GOTO1 DATCON,DMCB,(0,QSTART),(3,BQSTART)                               
         GOTO1 DATCON,DMCB,(0,QEND),(3,BQEND)                                   
         BAS   RE,QUARTERS         BUILD TABLE OF QARTERS FOR SORT              
         BAS   RE,FILTERS          BUILD TABLES OF POSSIBLE FILTERS             
*                                                                               
         CLC   QMKT(3),=C'ALL'                                                  
         BE    RA10                                                             
         PACK  DUB,QMKT                                                         
         CVB   R1,DUB                                                           
         STCM  R1,3,BMKT                                                        
*                                                                               
RA10     GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD                                 
*                                                                               
         USING AUTRECD,R6                                                       
         LA    R6,XKEY             READ AUTHS                                   
         XC    XKEY,XKEY                                                        
         MVI   AUPKTYP,AUPKTYPQ    X'0D'                                        
         MVI   AUPKSUB,AUPKSUBQ    X'B9'  - DUE DATE PASSIVE                    
         CLI   QOPT1,C'S'          PROCESS AUTH BY START DATE?                  
         BNE   *+8                                                              
         MVI   AUSKSUB,AUSKSUBQ    X'C9'  - START DATE PASSIV                   
*                                                                               
         MVC   AUPKAM,BAGYMD                                                    
         MVC   AUPKDUE,BQSTART     SAME DISP FOR DUE AND START DATE             
*                                                                               
         CLC   QCLT,=C'ALL'        ALL CLIENT REQUEST                           
         BE    *+18                                                             
         CLI   QCLT,C'*'           OR OFFICE REQUEST                            
         BE    *+10                                                             
         MVC   AUPKCLT,BCLT        JUST GET THAT ONE THEN                       
*                                                                               
         MVC   XKEYSAVE,XKEY                                                    
RA15     GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'XSPDIR',XKEY,XKEY                     
         B     RA20                                                             
*                                                                               
RA20SEQ  GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'XSPDIR',XKEY,XKEY                     
RA20     CLC   XKEY(AUPKDUE-AUPKEY),XKEYSAVE      SAME THRU MED                 
         BNE   RAX                 DONE READING - NOW PROCESS                   
         LA    R6,XKEY                                                          
*                                                                               
         CLC   AUPKDUE,BQEND       PAST REQUESTED END DATE                      
         BH    RAX                 DONE READING - NOW PROCESS                   
*                                                                               
         CLC   QCLT,=C'ALL'        FILTER ON SINGLE CLIENT                      
         BE    *+22                                                             
         CLI   QCLT,C'*'           OR OFFICE REQUEST                            
         BE    *+14                                                             
         CLC   AUPKCLT,BCLT                                                     
         BNE   RA99X                                                            
         BRAS  RE,SKIPOFC          CLT ALREADY VALIDATED VIA OFFICER?           
         BE    RA20A               YES                                          
         BAS   RE,OFCFILT          PASSED OFFICE LIST SECURITY?                 
         BNE   RA96                NO - BUMP TO NEXT CLIENT                     
*                                                                               
         BRAS  RE,PUTOFC           PUT CLT IN OFFICE VALIDATED TABLE            
*                                                                               
RA20A    CLC   QPRD,=C'ALL'        FILTER ON SINGLE PRD                         
         BE    *+14                                                             
         CLC   AUPKPRD,BPRD                                                     
         BNE   RA99X                                                            
*                                                                               
         CLC   QEST,=C'ALL'        FILTER ON SINGLE EST                         
         BE    *+24                                                             
         CLC   QEST,=C'NO '                                                     
         BE    *+14                                                             
         CLC   AUPKEST,BEST                                                     
         BNE   RA99X                                                            
*                                                                               
         CLI   QCLT,C'*'           CLIENT MUST BE IN OFFICE                     
         BE    RA21                                                             
         CLI   Q2USER+6,C'S'       SUPV'S REQUEST                               
         BE    RA22                SKIP CLT CHECK FOR SUPV- DONE LATER          
RA21     MVC   CLTFLT,AUPKCLT      SET CLIENT AND                               
         MVI   CLTFLTYP,C'O'       CHECK OFFICE                                 
         BAS   RE,CLTFILT          CHECK IF FILTERED OUT                        
         BNE   RA99X                                                            
*                                                                               
RA22     GOTO1 DATAMGR,DMCB,=C'GETREC',=C'XSPFIL',AUTKDA,ADBUY,DMWORK           
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   QOPT1,C'S'          FILTER BY START DATE                         
         BNE   *+12                                                             
         BAS   RE,CHKLAST          ONLY PROCESS LAST REVISION                   
         BNE   RA99X                                                            
*                                                                               
         L     R6,ADBUY                                                         
         MVC   SVSTRTDT,AUDFLST   BUY START DATE - ONLY IN AUTH LEVEL           
*                                                                               
         CLI   QOPT1,C'S'          FILTER BY START DATE                         
         BNE   RA24                                                             
         CLC   SVSTRTDT,BQSTART                                                 
         BL    RA99X                                                            
         CLC   SVSTRTDT,BQEND                                                   
         BH    RA99X                                                            
*                                                                               
RA24     MVI   SVAPPREQ,C'N'       SUPV APPROVAL REQ'D                          
         LA    R6,AUDEL                                                         
         USING AINFELD,R6                                                       
         MVI   ELCDLO,AINFELQ      X'04'                                        
         MVI   ELCDHI,AINFELQ                                                   
         BRAS  RE,NEXTEL3                                                       
         BNE   RA26                NO STATUS - CHK FOR STATIONS                 
         MVC   SVAPPREQ,AINFSPRQ   SUPV APP REQ'D Y/N                           
*                                                                               
RA26     L     R6,ADBUY                                                         
         MVC   XMKEY(L'AUTKEY),0(R6)   USE ACTIVE KEY TO GET TO MKTS            
*                                                                               
         USING AUTRECD,R6                                                       
         LA    R6,XMKEY                                                         
         MVI   AUTKREV,X'FF'       PASS OVER AUTH LEVEL REVISIONS               
         MVC   XMKEYSV,XMKEY                                                    
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'XSPDIR',XMKEY,XMKEY                   
         B     RA30                                                             
*                                                                               
RA30SEQ  GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'XSPDIR',XMKEY,XMKEY                   
RA30     CLC   XMKEY(AUTKMKT-AUTKEY),XMKEYSV       SAME UP TO MKT               
         BNE   RA99X               DONE READING MKTS - GET NEXT AUTH            
*                                                                               
         CLC   QMKT,=C'ALL '        REQUESTED FOR 1 MKT                         
         BE    *+14                                                             
         CLC   AUTKMKT,BMKT                                                     
         BNE   RA95                NEXT MKT                                     
*                                                                               
         CLI   Q2USER+6,C'S'       SUPV'S REQUEST                               
         BNE   RA32                                                             
         MVC   CLTFLT,AUTKCLT      SET CLIENT                                   
         MVI   CLTFLTYP,C'S'       CHECK SUPV CLT                               
         BAS   RE,CLTFILT          IF VALID CLIENT SKIP MKT FILT                
         BE    RA34                   FOR SUPV'S CLIENTS                        
         CLI   NOBYRS,C'Y'         NO BUYERS = CLT MUST MATCH                   
         BE    RA95                                                             
RA32     MVC   MKTFLT,AUTKMKT      SET MARKET AND                               
         BAS   RE,MKTFILT          CHECK IF FILTERED OUT                        
         BNE   RA95                                                             
*                                                                               
RA34     LA    R6,XMKEY                                                         
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'XSPFIL',AUTKDA,ADBUY,DMWORK           
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,ADBUY            R6 FOR MKT AUTH RECORD                       
*                                                                               
         LA    R2,SORTREC          R2 FOR SORT RECORD                           
         USING SRTRECD,R2                                                       
         XC    SORTREC,SORTREC                                                  
*                                                                               
         MVC   SRTSTART,SVSTRTDT   BUY START DATE                               
         MVC   SRTCLT,AUTKCLT      CLIENT                                       
         MVC   SRTPRD,AUTKPRD      PRODUCT                                      
         MVC   SRTPRD2,AUTKPRD2    PIGGY                                        
         MVC   SRTEST,AUTKEST      ESTIMATE                                     
         MVC   SRTFLT,AUTKAUN      FLIGHT                                       
         MVC   SRTMKT,AUTKMKT      MARKET                                       
*                                                                               
         LA    R6,AUTEL            POINT TO 01 ELEM                             
         USING MINFEL,R6                                                        
         MVC   SRTDUE,MINFDUDT     DUE DATE                                     
         MVC   SRTREV,MINFRVNO     MKT REVISION NUMBER                          
*                                                                               
         CLI   QOPT1,C'S'          FILTER BY START DATE                         
         BE    RA35                                                             
         CLC   SRTDUE,BQSTART      THEN FILTER DUE DATE                         
         BL    RA95                NEXT MKT                                     
         CLC   SRTDUE,BQEND                                                     
         BH    RA95                NEXT MKT                                     
*                                                                               
RA35     MVC   SRTDATE,SRTDUE      SORT BY DUE DATE - UNLESS                    
         CLI   QOPT1,C'S'          REQUESTED BY START DATE                      
         BNE   *+10                                                             
         MVC   SRTDATE,SRTSTART                                                 
*                                                                               
         BAS   RE,SETQTR           SET QUARTER START DATE IN SORTREC            
*                                                                               
         L     R6,ADBUY            DETERMINE CURRENT STATUS                     
         USING AUTRECD,R6                                                       
         TM    AUTRSTAT,AUTRSCAN   IS MARKET CANCELLED                          
         BNO   *+12                                                             
         MVI   SRTSSTA,SRTCANQ     SET SUPV STATUS AS CANCELLED                 
         B     RA50                GO CHECK BUYER STATUS                        
*                                                                               
         TM    AUTRSTAT,AUTRSDL    IS MARKET DELETED                            
         BNO   *+12                                                             
         MVI   SRTSSTA,SRTDELQ     SET SUPV STATUS AS CANCELLED                 
         B     RA50                GO CHECK BUYER STATUS                        
*                                                                               
         LA    R6,AUTEL            FIND FIRST STATUS ELEM                       
         USING MSTAELD,R6                                                       
         MVI   ELCDLO,MSTAREJQ     LOWEST STATUS ELEMENT X'06'                  
         MVI   ELCDHI,MSTACMSQ     HIGHEST STATUS ELEMENT X'0A'                 
         BRAS  RE,NEXTEL3                                                       
         BNE   RA90                NO STATUS - CHK FOR STATIONS                 
*                                                                               
         CLI   MSTAEL,MSTAAPPQ     APPROVED?                                    
         BNE   RA42                                                             
         MVC   SRTSSTA,MSTADATE    SUPERVISOR APPROVED DATE                     
         B     RA50                GO CHECK BUYER STATUS                        
*                                                                               
RA42     CLI   MSTAEL,MSTACMPQ     COMPLETED BY BUYER?                          
         BE    RA43                OR                                           
         CLI   MSTAEL,MSTACMSQ     COMPLETED BY SUPV?                           
         BNE   RA44                                                             
RA43     MVI   SRTSSTA,SRTPNDQ     SET SUPV STATUS TO PENDING                   
         B     RA50                GO CHECK BUYER STATUS                        
*                                                                               
RA44     CLI   MSTAEL,MSTAREJQ     REJECTED?                                    
         BNE   RA50                                                             
         MVI   SRTSSTA,SRTREJQ     SET SUPV STATUS TO REJECTED                  
         MVI   SRTBSTA,SRTPNDQ     SET BUYR STATUS TO PENDING                   
         B     RA90                GO CHECK FOR STATIONS                        
*                                                                               
         USING AUTRECD,R6                                                       
RA50     L     R6,ADBUY            IF COMPLETED - SET BUYER DATE                
         LA    R6,AUTEL                                                         
         MVI   ELCODE,MSTACMPQ     COMPLETED BUYER ELEMENT?                     
         BRAS  RE,NEXTEL                                                        
         BE    RA52                YES - SET DATE                               
         L     R6,ADBUY                                                         
         LA    R6,AUTEL                                                         
         MVI   ELCODE,MSTACMSQ     COMPLETED SUPV ELEMENT?                      
         BRAS  RE,NEXTEL                                                        
         BNE   RA54                                                             
*                                                                               
         USING MSTAELD,R6                                                       
RA52     MVC   SRTBSTA,MSTADATE    SET COMPLETED DATE - BUYER                   
         B     RA90                                                             
*                                                                               
RA54     MVI   SRTBSTA,SRTPNDQ     OTHERWISE BUYER IS PENDING                   
*                                                                               
RA90     BAS   RE,CHKSTNS          CHECK FOR STATIONS ORDERED                   
         BAS   RE,SETSTAT          SET OVERALL STATUS FOR SORT                  
*                                                                               
         CLI   QOPT5,C'Y'          TRACE                                        
         BNE   *+8                                                              
         BAS   RE,PSTRACE                                                       
*                                                                               
         GOTO1 =V(SORTER),DMCB,=C'PUT',SORTREC                                  
*                                                                               
         USING AUTRECD,R6                                                       
RA95     LA    R6,XMKEY                                                         
         MVC   AUTKSTA(4),=X'FFFFFFFF'    FORCE TO NEXT MKT                     
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'XSPDIR',XMKEY,XMKEY                   
         B     RA30                                                             
*                                                                               
RA96     LA    R6,XKEY                                                          
         MVC   AUPKPRD(5),=X'FFFFFFFFFF'  FORCE NEXT CLIENT                     
         B     RA15                       GO READ HIGH                          
*                                                                               
RA99X    LA    R6,XKEY                                                          
         MVI   AUTKREV,X'FF'       GET TO NEXT AUTH                             
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'XSPDIR',XKEY,XKEY                     
         B     RA20                RESTORE SEQ AND GET NEXT PASSIVE KEY         
*                                                                               
RAX      DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
*        GET SORT RECORDS AND PRINT AUTH REPORT                                 
***********************************************************************         
*                                                                               
         XC    LASTSORT,LASTSORT                                                
         B     *+10                                                             
PA10GET  MVC   LASTSORT,SORTREC                                                 
PA10GET2 GOTO1 =V(SORTER),DMCB,=C'GET',SORTREC                                  
         ICM   R5,15,4(R1)                                                      
         BZ    PA100                                                            
*                                                                               
*        MOVE FROM R5 TO SORTREC                                                
         LA    R2,SORTREC                                                       
         USING SRTRECD,R2                                                       
         MVC   SORTREC,0(R5)                                                    
*                                                                               
         CLC   LASTSORT,SORTREC    CAN GET DUPS BY DUE DATE                     
         BE    PA10GET2                                                         
*                                                                               
         BAS   RE,GETPRDS          GET ALPHA PRDS                               
         BAS   RE,ESTFILT          CHECK FOR EST FILTERS                        
         BE    PA10                GO PROCESS                                   
         OC    LASTSORT,LASTSORT   IF SKIPPING FIRST RECORD                     
         BZ    PA10GET2            DON'T SAVE IT                                
         B     PA10GET                                                          
*                                                                               
PA10     OC    LASTSORT,LASTSORT   FIRST TIME                                   
         BZ    PA20                                                             
*                                                                               
         CLC   SORTREC(3),LASTSORT CHANGE OF QUARTER                            
         BNE   *+14                                                             
         CLC   SRTSTAT,LASTSORT+3  SAME STATUS                                  
         BE    *+8                                                              
         BAS   RE,STATTOTS         LAST STATUS SUB TOTAL                        
*                                                                               
         CLC   SORTREC(3),LASTSORT CHANGE OF QUARTER                            
         BE    *+16                                                             
         BAS   RE,QRTRTOTS         LAST QUARTER TOTALS                          
PA20     BAS   RE,QRTRHEAD         NEW QUARTER HEADLINES                        
         B     PA30                AND ALWAYS NEW STATUS                        
*                                                                               
         CLC   SRTSTAT,LASTSORT+3  SAME STATUS                                  
         BE    *+8                                                              
PA30     BAS   RE,STATHEAD         NEW STATUS HEADING                           
*                                                                               
*                                                                               
*-------------------  PRINT OUT LINE DETAILS -----------------------            
*                                                                               
         CLI   QOPT2,C'Y'          ONLY PRINT TOTALS                            
         BNE   PA50                                                             
         MVI   RCSUBPRG,2          TOTAL COLUMNS                                
         BAS   RE,ADDTOTS          ADD TO TOTALS                                
         MVI   ANYDATA,C'Y'                                                     
         B     PA10GET                                                          
*                                                                               
PA50     GOTO1 DATCON,DMCB,(3,SRTDATE),(5,PDATE1)                               
         MVC   WORK(3),SRTSTART                                                 
         CLI   QOPT1,C'S'          PROCESS AUTH BY START DATE?                  
         BNE   *+10                                                             
         MVC   WORK(3),SRTDUE      USE WHAT NOT REQUESTED FOR 2ND DATE          
         GOTO1 DATCON,DMCB,(3,WORK),(5,PDATE2)                                  
*                                                                               
         LA    R1,PDATE1                                                        
         CLI   QOPT1,C'S'          CHECK DUE DATE FOR TBD                       
         BNE   *+8                                                              
         LA    R1,PDATE2                                                        
*                                                                               
         CLC   =C'APR04/80',0(R1)                                               
         BNE   *+10                                                             
         MVC   0(8,R1),=C'TBD     '                                             
*                                                                               
         GOTO1 CLUNPK,DMCB,SRTCLT,PCLT                                          
*                                                                               
         MVC   PPRD,QPRD                                                        
         MVC   PPRD2,QPRD2                                                      
         CLC   QPRD2,SPACES                                                     
         BE    *+8                                                              
         MVI   PPRD+3,C'-'                                                      
*                                                                               
         EDIT  SRTEST,PEST,FILL=0                                               
*                                                                               
         ZAP   WORK(2),=P'0'       VERSION/FLIGHT NUMBER                        
         MVO   WORK(2),SRTFLT      CONVERT TO PACKED                            
         SP    WORK(2),=P'99'                                                   
         SRP   WORK(2),1,0         SHIFT LEFT 1 DIGIT                           
         CLI   WORK,0                                                           
         BE    PA52                                                             
*                                                                               
         ZIC   RE,WORK             CHANGE FLIGHT NUMBER TO LETTER               
         LA    R3,VERSTAB                                                       
PA51NXT  CLI   0(R3),C' '                                                       
         BNE   *+6                                                              
         DC    H'00'                                                            
         CHI   RE,0                                                             
         BNH   PA51                                                             
         BCTR  RE,0                                                             
         LA    R3,1(R3)                                                         
         B     PA51NXT                                                          
PA51     MVC   PFLT,0(R3)                                                       
         MVI   PHYPH,C'-'                                                       
         B     PA52                                                             
VERSTAB  DC    C'ABCDEFGHIJKLMNOPQRSTUVWXYZ '                                   
*                                                                               
PA52     EDIT  SRTMKT,PMKT,FILL=0                                               
*                                                                               
         BAS   RE,PRTMKTNM         PRINT MARKET NAME                            
*                                                                               
         EDIT  SRTREV,PMKTREV,FILL=0                                            
*                                                                               
         OC    SRTBSTA+1(2),SRTBSTA+1       COMPLETED DATE?                     
         BZ    PA56                                                             
         GOTO1 DATCON,DMCB,(3,SRTBSTA),(5,PBCOMP)                               
         B     PA60                                                             
PA56     MVC   PBCOMP(7),=C'PENDING'                                            
*                                                                               
PA60     OC    SRTSSTA+1(2),SRTSSTA+1       APPROVED DATE?                      
         BZ    PA62                                                             
         GOTO1 DATCON,DMCB,(3,SRTSSTA),(5,PSAPPR)                               
         B     PA70                                                             
PA62     MVC   PSAPPR(7),=C'PENDING'                                            
         CLI   SRTSSTA,SRTCANQ                                                  
         BNE   *+14                                                             
         MVC   PSAPPR(9),=C'CANCELLED'                                          
         B     PA70                                                             
         CLI   SRTSSTA,SRTDELQ                                                  
         BNE   *+14                                                             
         MVC   PSAPPR(7),=C'DELETED'                                            
         B     PA70                                                             
         CLI   SRTSSTA,SRTREJQ                                                  
         BNE   *+10                                                             
         MVC   PSAPPR(8),=C'REJECTED'                                           
*                                                                               
PA70     DS    0H                                                               
         CLI   SRTSTORD,STAPRTL    STATIONS ORDERED?                            
         BNE   PA72                                                             
         MVC   PSTAORD,=CL10'PARTIAL'                                           
         B     PA80                                                             
PA72     CLI   SRTSTORD,STAORDRD   ALL STATIONS ORDERED?                        
         BNE   PA74                                                             
         MVC   PSTAORD,=CL10'ORDERED'                                           
         B     PA80                                                             
PA74     CLI   SRTSTORD,STACNFRM   ALL STATIONS ORDERED?                        
         BNE   PA80                                                             
         MVC   PSTAORD,=CL10'CONFIRMED'                                         
*                                                                               
PA80     DS    0H                                                               
         BAS   RE,ADDTOTS          ADD TO TOTALS                                
         MVI   ANYDATA,C'Y'                                                     
*                                                                               
         CLI   QOPT3,C'D'          DOWNLOAD OPTION                              
         BNE   PA90                                                             
         XC    HEADHOOK,HEADHOOK   MUST CLEAR HEADHOOK                          
         BRAS  RE,DOWNLD                                                        
         B     PA10GET                                                          
*                                                                               
PA90     GOTO1 REPORT                                                           
         B     PA10GET                                                          
*                                                                               
PA100    GOTO1 =V(SORTER),DMCB,=C'END'                                          
         CLI   ANYDATA,C'N'                                                     
         BE    PA105                                                            
         BAS   RE,STATTOTS         LAST STATUS SUB TOTAL                        
         BAS   RE,QRTRTOTS         LAST QUARTER TOTALS                          
         B     PAX                                                              
*                                                                               
PA105    CLI   QOPT3,C'D'          DOWNLOAD                                     
         BE    PAX                                                              
         MVC   P(44),=C'REPORT RAN TO COMPLETION - NOTHING TO REPORT'           
         MVC   QTRHEAD,SPACES                                                   
         MVC   QTRHEAD(15),=C'= ALL REQUESTED'                                  
         GOTO1 REPORT                                                           
*                                                                               
PAX      MVI   MODE,REQLAST                                                     
         CLI   QOPT3,C'D'          DOWNLOAD OPTION                              
         BNE   PAXX                                                             
         XC    HEADHOOK,HEADHOOK   MUST CLEAR HEADHOOK                          
         BRAS  RE,DOWNLD                                                        
         MVI   MODE,CLTFRST                                                     
PAXX     GOTO1 AENDREQ                                                          
*                                                                               
***********************************************************************         
*        CHECK IF LAST REVISION OF AUTH                                         
***********************************************************************         
*                                                                               
CHKLAST  NTR1                                                                   
         USING AUTRECD,R6                                                       
         L     R6,ADBUY                                                         
*                                                                               
         MVC   WORK(1),AUTKREV     SAVE CURRENT REVISION                        
*                                                                               
         MVC   XMKEY(L'AUTKEY),0(R6)   USE ACTIVE KEY                           
         LA    R6,XMKEY                                                         
         MVI   AUTKREV,X'00'       AND GET LATEST REVISION                      
         MVC   XMKEYSV,XMKEY                                                    
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'XSPDIR',XMKEY,XMKEY                   
         CLC   XMKEY(AUTKMKT-AUTKEY),XMKEYSV       SAME UP TO MKT               
         BNE   CLXNO                                                            
         CLC   WORK(1),AUTKREV     DO WE HAVE THE LATEST REV                    
         BNE   CLXNO                                                            
*                                                                               
CLXYES   SR    RC,RC                                                            
CLXNO    LTR   RC,RC                                                            
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        BUILD TABLE OF QUARTERS                                                
***********************************************************************         
*                                                                               
QUARTERS NTR1                      BUILD TABLE OF QUARTER START DATES           
         XC    QUARTAB,QUARTAB     TABLE OF BRD QUARTERS                        
         LA    R2,QUARTAB                                                       
         MVC   WORK(6),=C'800404'              ALWAYS HAVE A                    
         GOTO1 DATCON,DMCB,WORK,(3,(R2))     TBD QUARTER                        
         MVC   3(3,R2),0(R2)                                                    
         LA    R2,6(R2)                                                         
*                                                                               
         MVC   WORK(2),QSTART      USE REQ START YEAR                           
         MVC   WORK+2(4),=C'0101'  AND JUST START AT JAN                        
QU10     GOTO1 GETBROAD,DMCB,WORK,WORK+6                                        
         GOTO1 DATCON,DMCB,WORK+6,(3,(R2))     QTR START                        
         LA    R2,3(R2)                                                         
         GOTO1 ADDAY,DMCB,(C'M',WORK),WORK,2                                    
         GOTO1 GETBROAD,DMCB,WORK,WORK+6                                        
         GOTO1 DATCON,DMCB,WORK+12,(3,(R2))    QTR END                          
         LA    R2,3(R2)                                                         
         CLC   WORK+12(6),QEND     ARE WE DONE?                                 
         BH    QUX                                                              
         GOTO1 ADDAY,DMCB,(C'M',WORK),WORK,1   NEXT QTR START                   
         B     QU10                                                             
*                                                                               
QUX      MVC   0(3,R2),=X'FFFFFF'  EOT                                          
         B     EXIT                                                             
*                                                                               
***********************************************************************         
*        SET QUARTER START DATE IN SRTREC FROM AUTH DUE OR START DATE           
***********************************************************************         
SETQTR   NTR1                      FIND QUATER FOR THIS START/DUE DATE          
         LA    R2,SORTREC          R2 FOR SORT RECORD                           
         USING SRTRECD,R2                                                       
         LA    R3,QUARTAB                                                       
*                                                                               
SQ05     CLC   SRTDATE,0(R3)       DATE TO QTR START                            
         BL    SQ10                                                             
         CLC   SRTDATE,3(R3)       DATE TO QTR END                              
         BNH   SQ20                                                             
SQ10     LA    R3,6(R3)                                                         
         CLC   0(3,R3),=X'FFFFFF'  EOT                                          
         BNE   SQ05                                                             
         DC    H'0'                                                             
SQ20     MVC   SRTQTRST,0(R3)      SET QTR START DATE                           
         B     EXIT                                                             
         DROP  R2                                                               
*                                                                               
***********************************************************************         
*        CHECK FOR STATIONS - SET IF ANY ORDERED OR CONFIRMED                   
***********************************************************************         
*                                                                               
CHKSTNS  NTR1                      CHECK FOR STATIONS ORDERED                   
         XC    SFLAG,SFLAG                                                      
*                                  LAST READ MKT LEVEL                          
STNSSEQ  GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'XSPDIR',XMKEY,XSKEY                   
         CLC   XSKEY(AUTKSTA-AUTKEY),XMKEY         SAME THRU MKT                
         BNE   STNS60              DONE READING STATIONS                        
*                                                                               
         OI    SFLAG,SFEXIST       STATIONS EXIST                               
         USING AUTRECD,R6                                                       
         LA    R6,XSKEY                                                         
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'XSPFIL',AUTKDA,ADBUY,DMWORK           
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,ADBUY            R6 FOR STA AUTH RECORD                       
         CLI   AUTEL,SDTLELQ             IS THERE A STATUS ELEMENT?             
         BNE   STNS40                    NO, STATION HASN'T BEEN CNFRMD         
*                                                                               
STNS30   OC    SDTLORSN,SDTLORSN         ORDERED DATE THERE?                    
         BZ    *+12                                                             
         OI    SFLAG,SFORDER             YES, STATION HAS BEEN ORDERED          
         B     *+8                                                              
         OI    SFLAG,SFNORDER            A STATION HAS NOT BEEN ORDERED         
*                                                                               
         OC    SDTLSCNF,SDTLSCNF         CONFIRMED DATE THERE?                  
         BZ    STNS50                    NO READ NEXT STATION                   
         OI    SFLAG,SFCNFRM                                                    
         B     STNSSEQ                   YES, READ NEXT STATION                 
*                                                                               
STNS40   OI    SFLAG,SFNORDER            A STATION HAS NOT BEEN ORDERED         
STNS50   OI    SFLAG,SFNOCNFM            A STATION HAS NOT BEEN CNFRMD          
         B     STNSSEQ                   READ NEXT STATION                      
*                                                                               
* SET STATUS FOR ALL STATIONS IN SRTSTORD                                       
* BLNK=NO STATIONS EXIST                                                        
* BLSTA=NO STATIONS ORDERED AND NO STATIONS CONFIRMED                           
* PRTL=EITHER A STATION HAS BEEN ORDERED OR CONFIRMED - NOT ALL THOUGH          
* ORDRD=ALL STATIONS ORDERED                                                    
* CNFRM=ALL STATIONS CONFIRMED, BUT NOT NECESARILY ORDERED                      
*                                                                               
STNS60   LA    R2,SORTREC          R2 FOR SORT RECORD                           
         USING SRTRECD,R2                                                       
*                                                                               
         MVI   SRTSTORD,STABLNK          NO STATIONS                            
         TM    SFLAG,SFEXIST             DO ANY STATIONS EXIST?                 
         BNO   STNSX                                                            
*                                                                               
         MVI   SRTSTORD,STABLSTA         BLANK, BUT HAS STATIONS                
         TM    SFLAG,SFORDER+SFCNFRM     ANY STATIONS ORDERD OR CNFRMD?         
         BZ    STNSX                     NO, THEN BLANK W/ STATIONS             
*                                                                               
         MVI   SRTSTORD,STACNFRM         ALL CONFIRMED                          
         TM    SFLAG,SFNOCNFM            WERE ANY STATIONS NOT CNFRMED?         
         BNO   STNSX                     NO, THEN CONFIRMED                     
*                                                                               
         MVI   SRTSTORD,STAORDRD         ALL ORDERED                            
         TM    SFLAG,SFNORDER            WERE ANY STATIONS NOT ORDERED?         
         BNO   STNSX                     NO, THEN ORDERED                       
         MVI   SRTSTORD,STAPRTL          YES, PARTIAL                           
STNSX    B     EXIT                                                             
         DROP  R2,R6                                                            
*                                                                               
***********************************************************************         
*        SET OVERALL STATUS OF AUTH FOR SORTING                                 
***********************************************************************         
*                                                                               
SETSTAT  NTR1                      SET OVERALL STATUS FOR SORT                  
         USING SRTRECD,R2                                                       
         LA    R2,SORTREC          R2 FOR SORT RECORD                           
*                                                                               
* ---------------- BUYER COMPLETED ----------------------------------*          
         OC    SRTBSTA+1(2),SRTBSTA+1    BUYER STAT=COMPLETED                   
         BZ    SS50                      NO                                     
*                                                                               
         OC    SRTSSTA+1(2),SRTSSTA+1    SUPV STAT=COMPLETED                    
         BZ    SS20                      NO                                     
         MVI   SRTSTAT,COMPAPPR     COMPLETED AND APPROVED                      
         CLI   SRTSTORD,STACNFRM         ALL CONFIRMED                          
         BNE   SSX                                                              
         MVI   SRTSTAT,DONE         DONE IF STATIONS ALL CONF                   
         B     SSX                                                              
*                                                                               
SS20     CLI   SRTSSTA,SRTCANQ     SUPV CANCELLED/BYR COMPLETE                  
         BNE   *+12                                                             
         MVI   SRTSTAT,DONECANX    DONE - CANX                                  
         B     SSX                                                              
*                                                                               
         CLI   SRTSSTA,SRTDELQ     SUPV DELETED/BYR COMPLETE                    
         BNE   *+12                                                             
         MVI   SRTSTAT,DONEDELD    DONE - DELETED                               
         B     SSX                                                              
*                                                                               
         CLI   SRTSSTA,SRTREJQ     SUPV REJECTED/BYR COMPLETE                   
         BNE   *+12                                                             
         MVI   SRTSTAT,COMPREJ     COMPLETED/REJECTED                           
         B     SSX                                                              
*                                                                               
         MVI   SRTSTAT,DONECONF    DONE ORDER CONF - NO SUPV APPR               
         CLI   SVAPPREQ,C'Y'       SUPV APP REQ'D                               
         BE    *+12                                                             
         CLI   SRTSTORD,STACNFRM   APPROVAL NOT REQ'D + ALL CONFIRMED           
         BE    SSX                                                              
*                                                                               
         CLI   SRTSSTA,SRTPNDQ     SUPV PENDING/BYR COMPLETE                    
         BNE   *+12                                                             
         MVI   SRTSTAT,COMPPEND    COMPLETED/PENDING                            
         B     SSX                                                              
         DC    H'0'                                                             
*                                                                               
* ---------------- BUYER NOT COMPLETED ------------------------------*          
*                                                                               
SS50     CLI   SRTSSTA,SRTCANQ     SUPV CANCELLED/BYR PENDING                   
         BNE   *+12                                                             
         MVI   SRTSTAT,PENDCANX    PENDING -CANX                                
         B     SSX                                                              
*                                                                               
         CLI   SRTSSTA,SRTDELQ     SUPV DELETED/BYR PENDING                     
         BNE   *+12                                                             
         MVI   SRTSTAT,PENDDELD    PENDING -DELETED                             
         B     SSX                                                              
*                                                                               
         MVI   SRTSTAT,WIPPAST     WIP - PAST DUE DATE                          
         CLC   SRTDUE,=X'500404'   TBD                                          
         BE    SS60                NOT PAST DUE                                 
         CLC   SRTDUE,TODAYB       PAST DUE DATE                                
         BNH   *+8                                                              
SS60     MVI   SRTSTAT,WIPNOT      WIP - NOT PAST DUE                           
*                                                                               
SSX      B     EXIT                                                             
         DROP  R2                                                               
*                                                                               
***********************************************************************         
*        ADD TO TYPE AND QUARTER TOTALS                                         
***********************************************************************         
*                                                                               
ADDTOTS  NTR1                                                                   
         USING SRTRECD,R2                                                       
         LA    R2,SORTREC          ADD TO TYPE COUNTER                          
         LA    R3,STATLIST                                                      
AT10     CLC   SRTSTAT,0(R3)       MATCH STATUS LETTER                          
         BE    AT20                                                             
         LA    R3,33(R3)                                                        
         CLI   0(R3),X'FF'                                                      
         BNE   AT10                                                             
         DC    H'0'                                                             
AT20     LA    R4,TOTALS                                                        
         SR    R1,R1                                                            
         ICM   R1,3,1(R3)          DISP TO TYPE TOTAL                           
         AR    R4,R1                                                            
         AP    0(6,R4),=P'1'       ONE FOR TYPE                                 
         AP    TOTALS,=P'1'        ONE FOR TOTALS                               
         B     EXIT                                                             
         DROP  R2                                                               
*                                                                               
***********************************************************************         
*        PRINT LAST STATUS TOTALS                                               
***********************************************************************         
*                                                                               
STATTOTS NTR1                                                                   
         CLI   QOPT3,C'D'          DOWNLOAD OPTION SKIP TOTALS                  
         BE    STX                                                              
         OC    LASTSORT,LASTSORT                                                
         BZ    STX                                                              
*                                                                               
         USING SRTRECD,R2                                                       
         LA    R2,LASTSORT         USE LAST RECORD FOR TYPE                     
         LA    R3,STATLIST                                                      
ST10     CLC   SRTSTAT,0(R3)       MATCH STATUS LETTER                          
         BE    ST20                                                             
         LA    R3,33(R3)                                                        
         CLI   0(R3),X'FF'                                                      
         BNE   ST10                                                             
         DC    H'0'                                                             
ST20     MVC   PSTATUS+6,3(R3)                                                  
         MVC   PSTATUS(6),=C'TOTAL '                                            
         LA    R4,TOTALS                                                        
         SR    R1,R1                                                            
         ICM   R1,3,1(R3)          DISP TO TYPE TOTAL                           
         AR    R4,R1                                                            
         EDIT  (P6,0(R4)),(6,PSTATUS+38)                                        
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
STX      B     EXIT                                                             
         DROP  R2                                                               
***********************************************************************         
*        PRINT NEXT STATUS HEADING                                              
***********************************************************************         
*                                                                               
STATHEAD NTR1                                                                   
         CLI   QOPT3,C'D'          DOWNLOAD OPTION SKIP TOTALS                  
         BE    SHX                                                              
         CLI   QOPT2,C'Y'          ONLY PRINT TOTALS                            
         BE    SHX                                                              
*                                                                               
         USING SRTRECD,R2                                                       
         LA    R2,SORTREC          R2 FOR SORT RECORD                           
         LA    R3,STATLIST                                                      
SH10     CLC   SRTSTAT,0(R3)       MATCH STATUS LETTER                          
         BE    SH20                                                             
         LA    R3,33(R3)                                                        
         CLI   0(R3),X'FF'                                                      
         BNE   SH10                                                             
         DC    H'0'                                                             
SH20     MVC   PSTATUS,3(R3)                                                    
         GOTO1 REPORT                                                           
SHX      B     EXIT                                                             
         DROP  R2                                                               
***********************************************************************         
*        SET QUARTER DATES IN HEADLINES                                         
***********************************************************************         
*                                                                               
QRTRHEAD NTR1                                                                   
         USING SRTRECD,R2                                                       
         LA    R2,SORTREC          R2 FOR SORT RECORD                           
         LA    R3,QUARTAB                                                       
*                                                                               
QH05     CLC   SRTQTRST,0(R3)      MATCH QRT START                              
         BE    QH10                                                             
         LA    R3,6(R3)                                                         
         CLC   0(3,R3),=X'FFFFFF'  EOT                                          
         BNE   QH05                                                             
         DC    H'0'                                                             
QH10     MVC   WORK(6),0(R3)      SET QTR START DATE                            
         GOTO1 DATCON,DMCB,(X'13',WORK),(X'85',WORK+10)                         
         MVC   QTRHEAD,WORK+17                                                  
         B     EXIT                                                             
         DROP  R2                                                               
*                                                                               
HEADHK   NTR1                                                                   
         MVC   H3+53(L'QTRHEAD),QTRHEAD                                         
*                                                                               
         CLC   Q2USER(7),SPACES                                                 
         BE    HH10                                                             
         MVC   H4+45(2),Q2USER                                                  
         MVI   H4+47,C'-'                                                       
         MVC   H4+48(4),Q2USER+2                                                
         MVC   H4+53(29),USRNAME                                                
*                                                                               
HH10     CLI   QOPT2,C'Y'          ONLY PRINT TOTALS                            
         BE    HHX                                                              
*                                                                               
         CLI   QOPT1,C'S'          START DATE FILTER                            
         BNE   HHX                                                              
         MVC   H7+10(8),=C'START DT'                                            
         MVC   H7+20(8),=C'DUE DATE'                                            
*                                                                               
HHX      B     EXIT                                                             
***********************************************************************         
*        PRINT QUARTER TOTALS                                                   
***********************************************************************         
*                                                                               
QRTRTOTS NTR1                                                                   
         CLI   QOPT3,C'D'          DOWNLOAD OPTION SKIP TOTALS                  
         BE    QTX                                                              
         MVC   PSTATUS(32),=C'TOTAL AUTHORIZATIONS FOR QUARTER'                 
         EDIT  (P6,TOTALS),(8,PSTATUS+36),ZERO=NOBLANK                          
         GOTO1 REPORT                                                           
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         ZAP   TOTALS,=P'0'                                                     
         ZAP   ATOTS,=P'0'                                                      
         ZAP   BTOTS,=P'0'                                                      
         ZAP   CTOTS,=P'0'                                                      
         ZAP   DTOTS,=P'0'                                                      
         ZAP   ETOTS,=P'0'                                                      
         ZAP   FTOTS,=P'0'                                                      
         ZAP   GTOTS,=P'0'                                                      
         ZAP   HTOTS,=P'0'                                                      
         ZAP   ITOTS,=P'0'                                                      
         ZAP   JTOTS,=P'0'                                                      
         ZAP   KTOTS,=P'0'                                                      
*                                                                               
QTX      B     EXIT                                                             
***********************************************************************         
*        PRINT MARKET NAME                                                      
***********************************************************************         
*                                                                               
PRTMKTNM NTR1                                                                   
         USING SRTRECD,R2                                                       
         LA    R2,SORTREC          R2 FOR SORT RECORD                           
*                                                                               
         GOTO1 BINSRCH,MKTPAR1,SRTMKT                                           
         CLI   0(R1),0             TEST  FOUND                                  
         BNE   PM50                                                             
         L     RE,0(R1)                                                         
         MVC   PMKTNAME,2(RE)                                                   
         B     PMX                                                              
*                                                                               
PM50     MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),QMED                                                    
         SR    R0,R0                                                            
         ICM   R0,3,SRTMKT                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  KEY+2(4),DUB                                                     
         MVC   KEY+6(2),QAGY                                                    
         GOTO1 READMKT                                                          
*                                                                               
         L     R6,ADMARKET                                                      
         USING MKTREC,R6                                                        
         OC    MKTNAME,SPACES      TRAILING NULLS TO SPACES                     
         MVC   WORK(2),SRTMKT                                                   
         MVC   WORK+2(24),MKTNAME                                               
         MVC   PMKTNAME,MKTNAME                                                 
*                                                                               
         GOTO1 BINSRCH,MKTPAR1,(1,WORK)  INSERT IF NOT FOUND                    
         OC    0(4,R1),0(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
PMX      B     EXIT                                                             
         DROP  R2,R6                                                            
         EJECT                                                                  
***********************************************************************         
*        GET ALPHA PRODUCTS                                                     
***********************************************************************         
GETPRDS  NTR1                                                                   
         USING SRTRECD,R2                                                       
         LA    R2,SORTREC          R2 FOR SORT RECORD                           
*                                                                               
         MVC   QPRD2,SPACES                                                     
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),SRTCLT                                                  
*                                                                               
         USING CLTRECD,R6                                                       
         L     R6,ADCLT                                                         
         CLC   KEY(13),0(R6)       ALREADY HAVE CLIENT RECORD                   
         BE    GP10                                                             
*                                                                               
         GOTO1 HIGH                                                             
         GOTO1 GETCLT                                                           
*                                                                               
GP10     LA    R6,CLIST                                                         
GP12     CLC   SRTPRD,3(R6)                                                     
         BE    GP14                                                             
         LA    R6,4(R6)                                                         
         CLI   0(R6),0                                                          
         BNE   GP12                                                             
         DC    H'0'                                                             
GP14     MVC   QPRD,0(R6)                                                       
*                                                                               
         OC    SRTPRD2,SRTPRD2                                                  
         BZ    GPX                                                              
         L     R6,ADCLT                                                         
         LA    R6,CLIST                                                         
GP22     CLC   SRTPRD2,3(R6)                                                    
         BE    GP24                                                             
         LA    R6,4(R6)                                                         
         CLI   0(R6),0                                                          
         BNE   GP22                                                             
         DC    H'0'                                                             
GP24     MVC   QPRD2,0(R6)                                                      
*                                                                               
GPX      B     EXIT                                                             
         DROP  R2,R6                                                            
*                                                                               
***********************************************************************         
*        INIT                                                                   
***********************************************************************         
INIT     NTR1                                                                   
         CLC   QPRD,SPACES                                                      
         BNE   *+10                                                             
         MVC   QPRD,=C'ALL'                                                     
         CLC   QEST,SPACES                                                      
         BNE   *+10                                                             
         MVC   QEST,=C'ALL'                                                     
         CLC   QMKT,SPACES                                                      
         BNE   *+10                                                             
         MVC   QMKT,=C'ALL'                                                     
         CLC   QSTA,SPACES                                                      
         BNE   *+10                                                             
         MVC   QSTA,=C'ALL'                                                     
*                                                                               
         CLC   =C'ALL',QEST        SET SPECIFIC EST FILTER                      
         BE    INIT10                                                           
         CLC   =C'NO ',QEST                                                     
         BE    INIT10                                                           
         PACK  DUB,QEST                                                         
         CVB   R0,DUB                                                           
         LTR   R0,R0                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         STC   R0,BEST                                                          
*                                                                               
INIT10   CLC   QPRD,=C'ALL'                                                     
         BE    INITX                                                            
         USING CLTRECD,R6                                                       
         L     R6,ADCLT                                                         
         LA    R6,CLIST                                                         
INIT14   CLC   QPRD,0(R6)                                                       
         BE    INIT16                                                           
         LA    R6,4(R6)                                                         
         CLI   0(R6),0                                                          
         BNE   INIT14                                                           
         DC    H'0'                                                             
INIT16   MVC   BPRD,3(R6)                                                       
         DROP  R6                                                               
*                                                                               
INITX    B     EXIT                                                             
***********************************************************************         
*        BUILD TABLES OF POSSIBLE FILTERS                                       
*         TABLE OF SUPV'S CLIENTS                                               
*         TABLE OF BUYER'S MARKETS                                              
*         TABLE OF CLIENT GROUP'S CLIENTS                                       
*         TABLE OF SINGLE OFFICE'S CLIENTS                                      
***********************************************************************         
*                                                                               
FILTERS  NTR1                                                                   
         XC    SVBYRFLT,SVBYRFLT                                                
         XC    USRNAME,USRNAME                                                  
         MVI   FILTMKTS,C'N'       NO FILTERING                                 
         MVI   FILTCLTS,C'N'                                                    
         MVI   NOBYRS,C'Y'                                                      
         L     R1,AFMKTTAB                                                      
         MVC   0(2,R1),=X'FFFF'    SET END OF TABLES                            
         L     R1,AFCLTTAB                                                      
         MVC   0(2,R1),=X'FFFF'                                                 
*                                                                               
         L     R2,AFMKTTAB         R2 FOR MARKET TABLE                          
         L     R3,AFCLTTAB         R3 FOR CLIENT TABLE                          
*                                                                               
         CLC   Q2USER(7),SPACES    Q2USER=GROUP/CODE/TYPE                       
         BE    FILT100                                                          
         CLI   Q2USER+6,C'B'       BUYER'S MARKETS                              
         BNE   FILT50                                                           
*------------------------------------------------------------                   
*  READ BUYER RECORD AND BUILD TABLE OF MARKETS                                 
*------------------------------------------------------------                   
*                                                                               
         USING BYRRECD,R6                                                       
         LA    R6,KEY                                                           
         XC    KEY,KEY                                                          
         MVC   BYRKTYP,=X'0D62'                                                 
         MVC   BYRKAGY,BAGYMD                                                   
         NI    BYRKAGY,X'F0'       AGY ONLY                                     
         MVC   BYRKOFC(L'BYRKOFC+L'BYRKBYR),Q2USER                              
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,ADBUY                                                         
         ST    R6,AREC                                                          
         GOTO1 GET                                                              
*                                                                               
         USING BYRNAMED,R6                                                      
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BNE   FILT100                                                          
         MVC   SVSUPV,BYRSPV                                                    
         MVC   SVBYRFLT,BYRFILT                                                 
*                                                                               
         MVC   USRNAME(L'BYRLNAME),BYRLNAME                                     
         LA    R1,USRNAME+L'BYRLNAME-1                                          
         LA    R5,L'BYRLNAME                                                    
FILT02   CLI   0(R1),C' '                                                       
         BH    FILT04                                                           
         BCTR  R1,0                                                             
         BCT   R5,FILT02                                                        
FILT04   MVI   1(R1),C','                                                       
         MVC   3(L'BYRFNAME,R1),BYRFNAME                                        
*                                                                               
         BAS   RE,BYRSMKTS         ADD BUYER'S MARKETS TO TABLE                 
*                                                                               
FILT40   MVC   0(2,R2),=X'FFFF'    EOT                                          
FILT49X  OC    SVBYRFLT,SVBYRFLT   ANY BUYER FILTER                             
         BNZ   FILT50B             THEN FILTER WITH SUPV CLIENTS                
         B     FILT100                                                          
         DROP  R6                                                               
*------------------------------------------------------------                   
*  READ SUPV RECORD AND BUILD TABLE OF CLIENTS                                  
*  ALSO READ FOR SUPV'S BUYERS AND BUILD TABLE OF MKTS                          
*       SUPV SEES EVERY MKT FOR THEIR CLIENTS AND EVERY CLIENT IN               
*       ANY OF THEIR BUYER'S MARKETS                                            
*------------------------------------------------------------                   
FILT50   CLI   Q2USER+6,C'S'       SUPV'S CLIENTS                               
         BNE   FILT100                                                          
         MVC   SVSUPV,Q2USER+2                                                  
         MVI   FILTCLTS,C'Y'                                                    
*                                                                               
         USING SPVRECD,R6                                                       
FILT50B  LA    R6,KEY                                                           
         XC    KEY,KEY                                                          
         MVC   SPVKTYP,=X'0D61'                                                 
         MVC   SPVKAGY,BAGYMD                                                   
         NI    SPVKAGY,X'F0'       AGY ONLY                                     
         MVC   SPVKOFC,Q2USER                                                   
         MVC   SPVKSPV,SVSUPV                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,ADBUY                                                         
         ST    R6,AREC                                                          
         GOTO1 GET                                                              
*                                                                               
         CLI   Q2USER+6,C'B'       BUYER'S SUPV'S CLIENTS                       
         BE    FILT56                                                           
         LA    R6,SPVEL                                                         
         USING SPVNAMED,R6                                                      
         MVC   USRNAME(14),SPVLNAME      GET LAST NAME,FIRST INITIAL            
         LA    R1,USRNAME+L'SPVLNAME-1                                          
         LA    R5,L'SPVLNAME                                                    
FILT52   CLI   0(R1),C' '                                                       
         BH    FILT54                                                           
         BCTR  R1,0                                                             
         BCT   R5,FILT52                                                        
FILT54   MVI   1(R1),C','                                                       
         MVC   3(L'SPVFNAME,R1),SPVFNAME                                        
*                                                                               
FILT56   MVI   ELCODE,SPVCLTT      ELEM FOR MEDIA T MARKETS                     
         CLI   QMED,C'T'                                                        
         BE    FILT60                                                           
         MVI   ELCODE,SPVCLTR      ELEM FOR MEDIA R MARKETS                     
         CLI   QMED,C'R'                                                        
         BE    FILT60                                                           
         MVI   ELCODE,SPVCLTX      ELEM FOR MEDIA X MARKETS                     
         CLI   QMED,C'X'                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING SPVCLTD,R6                                                       
FILT60   L     R6,AREC                                                          
         BAS   RE,GETEL                                                         
         BNE   FILT80              NO CLTS- CHECK FOR CLIENT GROUPS             
         B     *+8                                                              
FILT62   BAS   RE,NEXTEL                                                        
         BNE   FILT64                                                           
*                                                                               
         CLI   Q2USER+6,C'B'       BUYER'S SUPV'S CLIENTS                       
         BNE   FILT63                                                           
         OC    SVBYRFLT,SVBYRFLT   ANY BUYER FILTER (SHOULD BE THERE)           
         BZ    FILT63                                                           
         CLC   SVBYRFLT,SPVFILT                                                 
         BNE   FILT62                                                           
*                                                                               
FILT63   MVI   FILTCLTS,C'Y'                                                    
         GOTO1 CLPACK,DMCB,SPVCLT,WORK     PACK INTO TABLE                      
         MVC   0(2,R3),WORK                                                     
         OI    2(R3),X'20'         SPV/BYR CLIENT                               
         LA    R3,3(R3)                                                         
         C     R3,AFCLTTBX                                                      
         BL    FILT62                                                           
         DC    H'0'                TABLE'S FULL                                 
*                                                                               
FILT64   MVC   0(2,R3),=X'FFFF'    EOT                                          
         B     FILT90X                                                          
*                                                                               
         USING SPVCGRD,R6                                                       
FILT80   OI    ELCODE,X'10'        CLT GROUP ELEMS                              
         L     R6,AREC                                                          
         BAS   RE,GETEL                                                         
         B     *+8                                                              
FILT82   BAS   RE,NEXTEL                                                        
         BNE   FILT90                                                           
*                                                                               
         USING GRPRECD,R5                READ CLIENT GROUP RECORDS              
         LA    R5,KEY                                                           
         XC    KEY,KEY                                                          
         MVC   GRPKTYP(2),=X'0D04'                                              
         MVC   GRPKAGMD,BAGYMD                                                  
         MVC   GRPKID(3),SPVCGRID        GROUP ID & GROUP                       
         GOTO1 HIGH                                                             
         CLC   GRPKEY(GRPKMSQL),KEYSAVE                                         
         BNE   FILT90                                                           
         L     R5,ADCLTGRP                                                      
         ST    R5,AREC                                                          
         GOTO1 GET                                                              
         LA    R5,24(R5)                                                        
         USING GRPVALD,R5                                                       
FILT84   CLI   0(R5),0                                                          
         BE    FILT82              GO GET NEX SUPV ELEM                         
         CLI   0(R5),X'30'         CLIENTS                                      
         BE    FILT86                                                           
FILT85   ZIC   R1,1(R5)                                                         
         AR    R5,R1                                                            
         B     FILT84                                                           
FILT86   GOTO1 CLPACK,DMCB,GRPVALUE,WORK                                        
         MVC   0(2,R3),WORK             SAVE CLIENT IN TABLE                    
         OI    2(R3),X'20'         SPV/BYR CLIENT                               
         MVI   FILTCLTS,C'Y'                                                    
         LA    R3,3(R3)                                                         
         C     R3,AFCLTTBX                                                      
         BL    FILT85                                                           
         DC    H'0'                TABLE'S FULL                                 
         DROP  R5                                                               
*                                                                               
FILT90   MVC   0(2,R3),=X'FFFF'    EOT                                          
*                                                                               
*  FOR SUPV GET ALL THEIR BUYER'S MKTS                                          
*                                                                               
FILT90X  DS    0H                  READ SUPV'S BUYERS FOR MKTLIST               
         CLI   Q2USER+6,C'S'                                                    
         BNE   FILT100             ONLY FOR SUPVS                               
*                                                                               
         USING BYRRECD,R5                                                       
         LA    R5,KEY                                                           
         XC    KEY,KEY                                                          
         MVC   BYRPTYP2,=X'0DE3'         LOOK FOR PASSIVES TO GET BYRS          
         MVC   BYRPAGY2,BAGYMD                                                  
         NI    BYRPAGY2,X'F0'       AGY ONLY                                    
         MVC   BYRPOFC2,Q2USER                                                  
         MVC   BYRPSPV2,Q2USER+2                                                
         MVC   SVBYRKEY,KEY                                                     
         GOTO1 HIGH                                                             
         B     FILT91                                                           
FILT91SQ GOTO1 SEQ                                                              
*                                                                               
FILT91   LA    R5,KEY                                                           
         CLC   KEY(9),SVBYRKEY          SAME THRU SUPV                          
         BNE   FILT94                   NO THEN DONE                            
         MVC   SVBYRKEY,KEY                                                     
*                                                                               
         L     R6,ADBUY                                                         
         ST    R6,AREC                                                          
         GOTO1 GET                                                              
*                                                                               
         MVI   NOBYRS,C'N'         SUPV HAS BUYERS                              
         BAS   RE,BYRSMKTS         ADD BUYER'S MARKETS TO TABLE                 
         CLI   ALLMKTS,C'Y'        IF ANY BUYERS HAVE ALL MKTS                  
         BE    FILT94              THEN NO NEED TO LOOK ANYMORE                 
         B     FILT91SQ                                                         
*                                                                               
FILT94   MVC   0(2,R2),=X'FFFF'    EOT                                          
*                                                                               
FILT99X  B     FILT100                                                          
         DROP  R6                                                               
*------------------------------------------------------------                   
*  READ CLIENT GROUP AND BUILD TABLE OF CLIENTS                                 
*------------------------------------------------------------                   
FILT100  CLC   QCLGID,SPACES       CLIENT GROUP FILTER?                         
         BE    FILT200                                                          
*                                                                               
         USING GRPRECD,R5                READ CLIENT GROUP RECORDS              
         LA    R5,KEY                                                           
         XC    KEY,KEY                                                          
         MVC   GRPKTYP(2),=X'0D04'                                              
         MVC   GRPKAGMD,BAGYMD                                                  
         MVC   GRPKID,QCLGID             GROUP ID                               
*                                                                               
         MVC   FULL,QCLGRP                                                      
         LA    R1,FULL                                                          
         LA    R0,4                                                             
FILT102  CLI   0(R1),C' '                                                       
         BE    *+12                                                             
         CLI   0(R1),C'*'                                                       
         BNE   *+8                                                              
         MVI   0(R1),0                                                          
         LA    R1,1(R1)                                                         
         BCT   R0,FILT102                                                       
         PACK  DUB,FULL(5)         PACK 1 EXTRA CHARACTER                       
         MVC   GRPKCODE,DUB+5                                                   
*                                                                               
         GOTO1 HIGH                                                             
         B     FILT103A                                                         
*                                                                               
FILT103  GOTO1 SEQ                 CAN BE MORE THEN 1 CGROUP RECORD!            
*                                                                               
FILT103A CLC   KEY(GRPKMSQL),KEYSAVE                                            
         BNE   FILT200                                                          
         L     R5,ADCLTGRP                                                      
         ST    R5,AREC                                                          
         GOTO1 GET                                                              
         LA    R5,24(R5)                                                        
         USING GRPVALD,R5                                                       
FILT104  CLI   0(R5),0                                                          
         BE    FILT110                                                          
         CLI   0(R5),X'30'         CLIENTS                                      
         BE    FILT106                                                          
FILT105  ZIC   R1,1(R5)                                                         
         AR    R5,R1                                                            
         B     FILT104                                                          
FILT106  GOTO1 CLPACK,DMCB,GRPVALUE,WORK                                        
         MVC   0(2,R3),WORK             SAVE CLIENT IN TABLE                    
         OI    2(R3),X'40'         CLIENT IN CLIENT GROUP                       
         MVI   FILTCLTS,C'Y'                                                    
         LA    R3,3(R3)                                                         
         C     R3,AFCLTTBX                                                      
         BL    FILT105                                                          
         DC    H'0'                TABLE'S FULL                                 
         DROP  R5                                                               
*                                                                               
FILT110  MVC   0(2,R3),=X'FFFF'    EOT                                          
         B     FILT103             READ SEQ                                     
*                                                                               
*------------------------------------------------------------                   
*  READ OFFICE POINTERS AND BUILD TABLE OF CLIENTS                              
*------------------------------------------------------------                   
FILT200  CLI   QCLT,C'*'           SINGLE OFFICE FILTER?                        
         BNE   FILTX                                                            
*                                                                               
         USING CLTPHDR,R5          READ CLIENT OFFICE POINTERS                  
         LA    R5,KEY                                                           
         XC    KEY,KEY                                                          
         MVC   CPKEYTYP(2),=X'0D80'                                             
         MVC   CPKEYAM,BAGYMD                                                   
         MVC   CPKEYOFF,QCLT+1     OFFICE                                       
         GOTO1 HIGH                                                             
         B     FILT204                                                          
FILT203  GOTO1 SEQ                                                              
FILT204  CLC   CPKEY(CPKEYCLT-CPKEY),KEYSAVE                                    
         BNE   FILT230                                                          
         MVI   FILTCLTS,C'Y'                                                    
*                                                                               
         CLI   Q2USER+6,C'S'       SUPV'S CLIENTS                               
         BNE   FILT220                                                          
         L     R1,AFCLTTAB         CHECK IF ALREADY IN TABLE                    
FILT206  CLC   0(2,R1),=X'FFFF'                                                 
         BE    FILT220                                                          
         CLC   0(2,R1),=X'0000'                                                 
         BE    FILT220                                                          
         C     R1,AFCLTTBX                                                      
         BNL   FILT220             NOT IN TABLE YET                             
         CLC   CPKEYCLT,0(R1)                                                   
         BNE   FILT208                                                          
         OI    2(R1),X'80'         CLIENT IN OFFICE                             
         B     FILT203                                                          
FILT208  LA    R1,3(R1)                                                         
         B     FILT206                                                          
*                                                                               
FILT220  MVC   0(2,R3),CPKEYCLT    CLIENT                                       
         OI    2(R3),X'80'         CLIENT IN OFFICE                             
         LA    R3,3(R3)                                                         
         C     R3,AFCLTTBX                                                      
         BL    FILT203                                                          
         DC    H'0'                TABLE'S FULL                                 
         DROP  R5                                                               
*                                                                               
FILT230  MVC   0(2,R3),=X'FFFF'    EOT                                          
*                                                                               
FILTX    B     EXIT                                                             
*                                                                               
         EJECT                                                                  
**********************************************************************          
*  PUT BUYER'S MKTS IN TABLE (R2 - POINTS TO TABLE)/BUYER REC IN ADBUY          
**********************************************************************          
*                                                                               
BYRSMKTS NTR1                                                                   
         MVI   ALLMKTS,C'N'                                                     
*                                                                               
         MVI   ELCODE,BYRMKTT      ELEM FOR MEDIA T MARKETS                     
         CLI   QMED,C'T'                                                        
         BE    BM10                                                             
         MVI   ELCODE,BYRMKTR      ELEM FOR MEDIA R MARKETS                     
         CLI   QMED,C'R'                                                        
         BE    BM10                                                             
         MVI   ELCODE,BYRMKTX      ELEM FOR MEDIA X MARKETS                     
         CLI   QMED,C'X'                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING BYRMKTD,R6                                                       
BM10     L     R6,ADBUY                                                         
         BAS   RE,GETEL                                                         
         BNE   BM30                NO MKTS- CHECK FOR MARKET GROUPS             
         B     *+8                                                              
BM12     BAS   RE,NEXTEL                                                        
         BNE   BMX                                                              
         CLC   BYRMKT,=X'FFFF'     ALL MARKETS FOR BUYER                        
         BNE   *+16                                                             
         MVI   FILTMKTS,C'N'       THEN SET NO FILTER                           
         MVI   ALLMKTS,C'Y'                                                     
         B     BMX                                                              
*                                                                               
         MVI   FILTMKTS,C'Y'                                                    
         MVC   0(2,R2),BYRMKT      SAVE MKT IN TABLE                            
         LA    R2,2(R2)                                                         
         C     R2,AFMKTTBX                                                      
         BL    BM12                                                             
         DC    H'0'                TABLE'S FULL                                 
*                                                                               
         USING BYRMKGD,R6                                                       
BM30     OI    ELCODE,X'10'        MKT GROUP ELEMS                              
         L     R6,AREC                                                          
         BAS   RE,GETEL                                                         
         BNE   BMX                                                              
         B     *+8                                                              
BM32     BAS   RE,NEXTEL                                                        
         BNE   BM50                                                             
*                                                                               
         USING MKGRECD,R5                READ MARKET GROUP RECORDS              
         LA    R5,KEY                                                           
         XC    KEY,KEY                                                          
         MVC   MKGPTYP,=X'0D82'                                                 
         MVC   MKGPAGMD,BAGYMD                                                  
         MVC   MKGPMID(3),BYRMKGID       GROUP ID & GROUP                       
         GOTO1 HIGH                                                             
         B     BM34                                                             
BM33     GOTO1 SEQ                                                              
BM34     CLC   MKGKEY(MKGPMKT-MKGPTYP),KEYSAVE                                  
         BNE   BM32                                                             
         MVI   FILTMKTS,C'Y'                                                    
         MVC   0(2,R2),MKGPMKT              SEND MAR                            
         LA    R2,2(R2)                                                         
         C     R2,AFMKTTBX                                                      
         BL    BM33                                                             
         DC    H'0'                TABLE'S FULL                                 
         DROP  R5                                                               
*                                                                               
BM50     MVC   KEY,SVBYRKEY                                                     
         GOTO1 HIGH                RESTORE BUYER PASSIVE SEQUENCE               
*                                                                               
BMX      XIT1  REGS=(R2)                                                        
         DROP  R6                                                               
***********************************************************************         
*        CHECK CLIENT FILTERS - BINARY CLIENT IN CLTFLT                         
***********************************************************************         
*                                                                               
CLTFILT  NTR1                                                                   
         CLI   FILTCLTS,C'Y'                                                    
         BNE   CFXYES                                                           
         L     R3,AFCLTTAB         R3 FOR CLIENT TABLE                          
CF10     CLC   0(2,R3),=X'FFFF'                                                 
         BE    CFXNO                                                            
         CLC   CLTFLT,0(R3)                                                     
         BNE   CF20                                                             
*                                                                               
         CLI   CLTFLTYP,C'S'       CHECK SUPV CLT                               
         BNE   CF12                                                             
         CLI   Q2USER+6,C'S'       SUPV'S REQUEST                               
         BE    *+12                                                             
         CLI   Q2USER+6,C'B'       BUYER'S REQUEST                              
         BNE   *+12                                                             
         TM    2(R3),X'20'         CLIENT FROM SPV/BYR                          
         BNO   CF20                                                             
         B     CFXYES                                                           
*                                                                               
CF12     CLI   QCLT,C'*'           OFFICE REQUEST                               
         BNE   *+12                                                             
         TM    2(R3),X'80'         MAKE SURE CLIENT IN OFFICE                   
         BNO   CF20                                                             
         B     CFXYES                                                           
*                                                                               
CF20     LA    R3,3(R3)                                                         
         B     CF10                                                             
*                                                                               
CFXYES   SR    RC,RC                                                            
CFXNO    LTR   RC,RC                                                            
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        CHECK MARKET FILTERS - BINARY MARKET IN MKTFLT                         
***********************************************************************         
*                                                                               
MKTFILT  NTR1                                                                   
         CLI   FILTMKTS,C'Y'                                                    
         BNE   MFXYES                                                           
         L     R3,AFMKTTAB         R3 FOR MARKET TABLE                          
MF10     CLC   0(2,R3),=X'FFFF'                                                 
         BE    MFXNO                                                            
         CLC   MKTFLT,0(R3)                                                     
         BE    MFXYES                                                           
         LA    R3,2(R3)                                                         
         B     MF10                                                             
*                                                                               
MFXYES   SR    RC,RC                                                            
MFXNO    LTR   RC,RC                                                            
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        CHECK ESTIMATE FILTERS                                                 
***********************************************************************         
*                                                                               
ESTFILT  NTR1                                                                   
         CLC   QESTEND,SPACES      NO FILTERS                                   
         BE    EFXYES                                                           
*                                                                               
         USING SRTRECD,R2                                                       
         LA    R2,SORTREC          R2 FOR SORT RECORD                           
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(2),SRTCLT                                                   
         MVC   WORK+2(3),QPRD                                                   
         MVC   WORK+5(1),SRTEST                                                 
*                                                                               
         GOTO1 BINSRCH,ESFPAR1,WORK                                             
         CLI   0(R1),0             TEST  FOUND                                  
         BNE   EF50                                                             
         L     RE,0(R1)                                                         
         MVC   SVESTFIL,6(RE)                                                   
         B     EF100               GO CHECK FILTER AGAINST REQUEST              
*                                                                               
EF50     XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),SRTCLT                                                  
         MVC   KEY+4(3),QPRD                                                    
         MVC   KEY+7(1),SRTEST                                                  
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 GETEST                                                           
         L     R6,ADEST                                                         
         USING ESTRECD,R6                                                       
         XC    WORK,WORK                                                        
         MVC   WORK(2),SRTCLT                                                   
         MVC   WORK+2(3),QPRD                                                   
         MVC   WORK+5(1),SRTEST                                                 
         MVC   WORK+6(3),EPROF                                                  
         MVC   SVESTFIL,EPROF                                                   
         DROP  R6                                                               
*                                                                               
         GOTO1 BINSRCH,ESFPAR1,(1,WORK)  INSERT IF NOT FOUND                    
         OC    0(4,R1),0(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
EF100    LA    R0,3                APPLY ESTIMATE FILTER                        
         LA    RE,QESTEND                                                       
         LA    RF,SVESTFIL                                                      
*                                                                               
EF102    CLI   0(RE),C'*'                                                       
         BE    EF106                                                            
         CLI   0(RE),C' '                                                       
         BE    EF106                                                            
         TM    0(RE),X'40'         TEST NEGATIVE FILTER                         
         BZ    EF104               YES                                          
*                                                                               
         CLC   0(1,RE),0(RF)       POSITIVE FILTER MUST MATCH                   
         BNE   EFXNO                                                            
         B     EF106                                                            
*                                                                               
EF104    MVC   BYTE,0(RE)                                                       
         OI    BYTE,C' '           MAKE CHAR UPPER CASE FOR COMPARE             
         CLC   BYTE,0(RF)                                                       
         BE    EFXNO                                                            
*                                                                               
EF106    LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,EF102                                                         
*                                                                               
EFXYES   SR    RC,RC                                                            
EFXNO    LTR   RC,RC                                                            
         B     EXIT                                                             
         DROP  R2                                                               
***********************************************************************         
*        TRACE SORT RECS                                                        
***********************************************************************         
PSTRACE  NTR1                                                                   
         USING SRTRECD,R2                                                       
         LA    R2,SORTREC          R2 FOR SORT RECORD                           
         MVC   P(7),=C'PUTSORT'                                                 
         GOTO1 CLUNPK,DMCB,SRTCLT,P+10                                          
         EDIT  SRTEST,(3,P+15),FILL=0                                           
         EDIT  SRTMKT,(4,P+20),FILL=0                                           
         EDIT  SRTREV,(2,P+25),FILL=0                                           
         GOTO1 HEXOUT,DMCB,SORTREC,P+30,L'SORTREC,=C'TOG'                       
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         DROP  R2                                                               
***********************************************************************         
*        GET AND NEXTELS AND LTORG                                              
***********************************************************************         
*                                                                               
         GETEL R6,24,ELCODE                                                     
*                                                                               
NEXTEL3  CLI   0(R6),0                                                          
         JE    NEXTEL3X                                                         
         ZIC   R0,1(R6)                                                         
         LTR   R0,R0                                                            
         JNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         JE    NEXTEL3X                                                         
         CLC   ELCDLO,0(R6)                                                     
         JH    NEXTEL3                                                          
         CLC   ELCDHI,0(R6)                                                     
         JL    NEXTEL3                                                          
         CR    RB,RB                                                            
         J     *+6                                                              
NEXTEL3X LTR   RB,RB                                                            
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        DOWNLOAD CODE                                                          
***********************************************************************         
*                                                                               
DOWNLD   NTR1  BASE=*,LABEL=*                                                   
         USING SRTRECD,R6                                                       
         LA    R6,SORTREC                                                       
*                                                                               
         USING DLCBD,R2                                                         
         LA    R2,DLCB                                                          
         OI    DLCBFLG1,DLCBFXTN                                                
         MVC   DLCXTND(7),MAXLINE                                               
         MVC   DLCBAPR,=A(DNPRINT)                                              
         LA    R0,P                                                             
         ST    R0,DLCBAPL                                                       
*                                                                               
         CLI   MODE,REQLAST       SEE IF END OF REPORT                          
         BE    DNP10                                                            
*                                                                               
         CLI   MODE,REQFRST       SEE IF I NEED TO INTIALIZE                    
         BE    DNP20                                                            
*                                                                               
         MVC   DNLINE,P           SAVE CONTENTS OF PRINTLINE                    
         MVC   P,SPACES                                                         
         MVC   DNLINE2,P2         SAVE CONTENTS OF PRINTLINE2                   
         MVC   P2,SPACES                                                        
         MVC   DNLINE3,P3         SAVE CONTENTS OF PRINTLINE2                   
         MVC   P3,SPACES                                                        
*                                                                               
         OC    LASTSORT,LASTSORT   FIRST TIME HERE                              
         BNZ   DNP04                                                            
*                                                                               
         CLI   QOPT1,C'S'          START DATE FILTER                            
         BNE   DNP01                                                            
         MVC   HEADDUE+1(8),=C'START DT'                                        
         MVC   HEADST+1(8),=C'DUE DATE'                                         
*                                                                               
DNP01    LA    R3,HEADTAB          SEND COLUMN HEADINGS                         
DNP02    ZIC   R4,0(R3)                                                         
         AHI   R4,-1                                                            
         BM    DNP04                                                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   DLCBFLD(0),1(R3)    COLUMN HEADING                               
         MVI   DLCBTYP,C'T'        TEXT FIELD                                   
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 =V(DLFLD),DLCB                                                   
         AHI   R4,2                                                             
         AR    R3,R4                                                            
         CLI   0(R3),X'FF'                                                      
         BNE   DNP02                                                            
*                                                                               
         MVI   DLCBACT,DLCBEOL           SEND END OF LINE                       
         GOTO1 =V(DLFLD),DLCB                                                   
         GOTO1 REPORT                                                           
*                                                                               
DNP04    LA    R3,DNLINE                                                        
         USING PSTAT,R3                                                         
*                                                                               
         LA    R4,STATLIST                                                      
DNP06    CLC   SRTSTAT,0(R4)       MATCH STATUS LETTER                          
         BE    DNP08                                                            
         LA    R4,33(R4)                                                        
         CLI   0(R4),X'FF'                                                      
         BNE   DNP06                                                            
         DC    H'0'                                                             
DNP08    MVC   DLCBFLD(L'PSTATUS),3(R4)                                         
         MVI   DLCBTYP,C'T'              TEXT FIELD                             
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 =V(DLFLD),DLCB                                                   
*                                                                               
         MVC   DLCBFLD(L'PDATE1),PDATE1  DUE OR START DATE                      
         MVI   DLCBTYP,C'T'              TEXT FIELD                             
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 =V(DLFLD),DLCB                                                   
*                                                                               
         MVC   DLCBFLD(L'PDATE2),PDATE2  DUE OR START DATE                      
         MVI   DLCBTYP,C'T'              TEXT FIELD                             
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 =V(DLFLD),DLCB                                                   
*                                                                               
         MVC   DLCBFLD(L'PCLT),PCLT      CLIENT CODE                            
         MVI   DLCBTYP,C'T'              TEXT FIELD                             
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 =V(DLFLD),DLCB                                                   
*                                                                               
         MVC   DLCBFLD(7),PPRD           PRD-PRD                                
         MVI   DLCBTYP,C'T'              TEXT FIELD                             
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 =V(DLFLD),DLCB                                                   
*                                                                               
         MVC   DLCBFLD(L'PEST+2),PEST      ESTIMATE-FLIGHT                      
         MVI   DLCBTYP,C'T'              TEXT FIELD                             
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 =V(DLFLD),DLCB                                                   
*                                                                               
         MVC   DLCBFLD(L'PMKT),PMKT      MARKET NUMBER                          
         MVI   DLCBTYP,C'T'              TEXT FIELD                             
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 =V(DLFLD),DLCB                                                   
*                                                                               
         MVC   DLCBFLD(L'PMKTNAME),PMKTNAME   MARKET NAME                       
         MVI   DLCBTYP,C'T'              TEXT FIELD                             
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 =V(DLFLD),DLCB                                                   
*                                                                               
         MVC   DLCBFLD(L'PMKTREV),PMKTREV   MARKET REVISION                     
         MVI   DLCBTYP,C'T'              TEXT                                   
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 =V(DLFLD),DLCB                                                   
*                                                                               
         MVC   DLCBFLD(L'PBCOMP),PBCOMP  BUYER COMPLETED                        
         MVI   DLCBTYP,C'T'              TEXT FIELD                             
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 =V(DLFLD),DLCB                                                   
*                                                                               
         MVC   DLCBFLD(9),PSAPPR         SUPV APPR                              
         MVI   DLCBTYP,C'T'              TEXT FIELD                             
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 =V(DLFLD),DLCB                                                   
*                                                                               
         MVC   DLCBFLD(9),PSTAORD        STATION ORDERED                        
         MVI   DLCBTYP,C'T'              TEXT FIELD                             
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 =V(DLFLD),DLCB                                                   
*                                                                               
         MVI   DLCBACT,DLCBEOL           SEND END OF LINE                       
         GOTO1 =V(DLFLD),DLCB                                                   
         B     DNPX                                                             
*                                                                               
DNP10    DS    0H                                                               
         MVC   P,SPACES                  JUST IN CASE                           
         MVI   DLCBACT,C'R'              SET END OF REPORT                      
         GOTO1 =V(DLFLD),DLCB                                                   
         B     DNPX                                                             
*                                                                               
DNP20    DS    0H                                                               
         MVC   P,SPACES                  JUST IN CASE                           
         MVI   DLCBACT,C'I'              START AND INTIALIZE REPORT             
         GOTO1 =V(DLFLD),DLCB                                                   
DNPX     DS    0H                                                               
         J     EXIT                                                             
         DROP  R2,R3,R6                                                         
*                                                                               
DNPRINT  NTR1                                                                   
         MVI   LINE,0                                                           
         MVI   FORCEHED,C'N'                                                    
         GOTO1 REPORT                                                           
         MVI   FORCEHED,C'N'                                                    
         J     EXIT                                                             
*                                                                               
OFCFILT  NTR1                                                                   
*                                                                               
         USING AUTRECD,R6          SUPERDESK AUTH RECORD DSECT                  
         XC    KEY,KEY             CLEAR THE KEY                                
         MVC   KEY+1(1),BAGYMD     A/M                                          
         MVC   KEY+2(2),AUPKCLT    CLIENT                                       
         GOTO1 HIGH                READ HIGH                                    
         GOTO1 GETCLT              GET THE CLIENT RECORD                        
         L     R4,ADCLT            A(CLIENT RECORD)                             
         USING CLTRECD,R4          A(CLIENT RECORD DSECT)                       
         LA    R2,WORK             BUILD OFFICER BLOCK IN WORK                  
         XC    WORK,WORK           CLEAR WORK                                   
         USING OFFICED,R2          OFFICER DSECT                                
         L     R3,VMASTC           A(DDMASTD)                                   
         USING MASTD,R3            MASTD DSECT                                  
         MVI   OFCSYS,C'S'         SYSTEM = SPOT                                
         MVC   OFCAUTH,MCS2ACCS    TWA+6(2)                                     
         MVC   OFCLMT,MCS2ACCS     LIMIT ACCESS VALUE TWA+6(4)                  
         MVC   OFCAGY,AGENCY       AGENCY                                       
         MVC   OFCSAGMD,BAGYMD     A/M                                          
         MVC   OFCCLT2,AUPKCLT     BINARY CLIENT                                
         OI    OFCINDS,OFCI2CSC    PASSING 2-BYTE CLIENT CODE                   
         MVC   OFCACCSC,CACCESS    CLIENT LIMIT ACCESS                          
         MVC   OFCOFC,COFFICE      CLIENT OFFICE                                
         MVI   OFCACCSM,X'FF'      IGNORE MKT LIMIT ACCESS FOR CLT              
*                                                                               
         GOTO1 OFFICER,DMCB,(C'2',WORK),(X'80',ACOMFACS),OFCLIST                
         CLI   0(R1),0             ANY ERRORS?                                  
         JE    YES                 NO - THIS CLIENT HAS ACCESS                  
         J     NO                  YES- BUMP TO NEXT CLIENT                     
         DROP  R2,R3,R4,R6         DROP USINGS                                  
*                                                                               
SKIPOFC  NTR1  BASE=*,LABEL=*                                                   
         USING AUTRECD,R6          SUPERDESK AUTH RECORD DSECT                  
         LA    R1,CLTLIST          CLIENT PASSED OFFICE FILTER LIST             
*                                                                               
SKIPO10  OC    0(2,R1),0(R1)       END OF CLIENT LIST?                          
         JZ    NO                  YES - EXIT CC NEQ                            
         CLC   0(2,R1),=X'FFFF'    END OF CLIENT LIST?                          
         JE    NO                  YES - EXIT CC NEQ                            
         CLC   0(2,R1),AUPKCLT     CLIENT ALREADY IN LIST?                      
         JE    YES                 YES - EXIT CC EQU                            
         LA    R1,2(R1)            NO - BUMP CLIENT LIST POINTER                
         B     SKIPO10             TRY NEXT ENTRY                               
         DROP  R6                  DROP R6                                      
*                                                                               
PUTOFC   NTR1  BASE=*,LABEL=*                                                   
         USING AUTRECD,R6          SUPERDESK AUTH RECORD DSECT                  
         LA    R1,CLTLIST          CLIENT PASSED OFFICE FILTER LIST             
*                                                                               
PUTO10   OC    0(2,R1),0(R1)       EMPTY SLOT?                                  
         JZ    PUTO20              YES - PUT CLIENT HERE                        
         CLC   0(2,R1),=X'FFFF'    CLIENT LIST FULL?                            
         JE    EXIT                YES - EXIT                                   
         CLC   0(2,R1),AUPKCLT     CLIENT ALREADY IN LIST?                      
         JE    EXIT                YES - EXIT CC EQU                            
         LA    R1,2(R1)            NO - BUMP CLIENT LIST POINTER                
         B     PUTO10              TRY NEXT ENTRY                               
*                                                                               
PUTO20   MVC   0(2,R1),AUPKCLT     MOVE IN CLIENT CODE                          
         J     EXIT                                                             
         DROP  R6                                                               
*                                                                               
         LTORG                                                                  
DLCB     DS    XL256                                                            
DNLINE   DS    CL132                                                            
DNLINE2  DS    CL132                                                            
DNLINE3  DS    CL132                                                            
         DS    0H                                                               
MAXLINE  DC    H'132'                                                           
DELIM    DC    C' '        FIELD DELIMITER                                      
EOTCHR   DC    C'"'        END OF TEXT FIELD DELIMITER                          
EOTALT   DC    C''''       END OF TEXT CHR ALTERNATE                            
EOLCHR   DC    X'5E'       END OF LINE CHAR - SEMI-COLON                        
EORCHR   DC    C':'        END OF REPORT CHR                                    
*                                                                               
         EJECT                                                                  
***********************************************************************         
*                 LITERALS, TABLES, STORAGE AND STUFF                           
***********************************************************************         
         LTORG                                                                  
*                                                                               
SPSSWORK DS    0D                                                               
         DC    CL8'SPSSWORK'                                                    
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,15,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=29'                                    
*                                                                               
HEADTAB  DC    XL1'06',C'STATUS'                                                
HEADDUE  DC    XL1'08',C'DUE DATE'                                              
HEADST   DC    XL1'08',C'START DT'                                              
         DC    XL1'03',C'CLT'                                                   
         DC    XL1'07',C'PRODUCT'                                               
         DC    XL1'03',C'EST'                                                   
         DC    XL1'04',C'MKT#'                                                  
         DC    XL1'0B',C'MARKET NAME'                                           
         DC    XL1'04',C'REV#'                                                  
         DC    XL1'0A',C'BUYER COMP'                                            
         DC    XL1'09',C'SUPV APPR'                                             
         DC    XL1'0B',C'STATION ORD'                                           
         DC    X'FF'                                                            
*                                                                               
STATLIST DC    C'A',AL2(ATOTS-TOTALS),CL30'COMPLETED AND APPROVED'              
         DC    C'B',AL2(BTOTS-TOTALS),CL30'PENDING, CANCELLED'                  
         DC    C'C',AL2(CTOTS-TOTALS),CL30'PENDING, DELETED'                    
         DC    C'D',AL2(DTOTS-TOTALS),CL30'COMPLETED, PENDING APPROVAL'         
         DC    C'E',AL2(ETOTS-TOTALS),CL30'COMPLETED, REJECTED'                 
         DC    C'F',AL2(FTOTS-TOTALS),CL30'IN PROGRESS, PAST DUE DATE'          
         DC    C'G',AL2(GTOTS-TOTALS),CL30'IN PROGRESS, NOT YET DUE'            
         DC    C'H',AL2(HTOTS-TOTALS),CL30'DONE, CANCELLED'                     
         DC    C'I',AL2(ITOTS-TOTALS),CL30'DONE, DELETED'                       
         DC    C'J',AL2(JTOTS-TOTALS),CL30'DONE'                                
         DC    C'K',AL2(KTOTS-TOTALS),CL30'DONE, ORDER CONFIRMED'               
         DC    X'FF'                                                            
*                                                                               
ESFPAR1  DC    A(0)                                                             
ESFPAR2  DC    A(ESTFTAB)          A(TABLE)                                     
ESFPAR3  DC    F'0'                RECORD COUNT                                 
ESFPAR4  DC    A(9)                RECORD LENGTH                                
ESFPAR5  DC    A(6)                KEYDSPL/KEYLEN                               
ESFPAR6  DC    A((ESTFTBX-ESTFTAB)/L'ESTFTAB)                                   
*                                                                               
*                                                                               
MKTPAR1  DC    A(0)                                                             
MKTPAR2  DC    A(MKTTAB)           A(TABLE)                                     
MKTPAR3  DC    F'0'                RECORD COUNT                                 
MKTPAR4  DC    A(26)               RECORD LENGTH                                
MKTPAR5  DC    A(2)                KEYDSPL/KEYLEN                               
MKTPAR6  DC    A((MKTTBX-MKTTAB)/L'MKTTAB)                                      
*                                                                               
TOTALS   DC    PL6'0'                                                           
ATOTS    DC    PL6'0'                                                           
BTOTS    DC    PL6'0'                                                           
CTOTS    DC    PL6'0'                                                           
DTOTS    DC    PL6'0'                                                           
ETOTS    DC    PL6'0'                                                           
FTOTS    DC    PL6'0'                                                           
GTOTS    DC    PL6'0'                                                           
HTOTS    DC    PL6'0'                                                           
ITOTS    DC    PL6'0'                                                           
JTOTS    DC    PL6'0'                                                           
KTOTS    DC    PL6'0'                                                           
*                                                                               
AFMKTTAB DC    A(FMKTTAB)          A(TABLE)                                     
AFMKTTBX DC    A(FMKTTBX)                                                       
AFCLTTAB DC    A(FCLTTAB)          A(TABLE)                                     
AFCLTTBX DC    A(FCLTTBX)                                                       
*                                                                               
         DC    C'XKEY'                                                          
XKEY     DS    XL64                                                             
XKEYSAVE DS    XL64                                                             
XMKEY    DS    XL64                                                             
XMKEYSV  DS    XL64                                                             
XSKEY    DS    XL64                                                             
ELCODE   DS    X                                                                
ELCDLO   DS    X                                                                
ELCDHI   DS    X                                                                
SVBYRKEY DS    CL24                                                             
*                                                                               
ANYDATA  DS    CL1                                                              
ALLMKTS  DS    CL1                                                              
CLTFLT   DS    XL2                                                              
CLTFLTYP DS    XL1                                                              
MKTFLT   DS    XL2                                                              
NOBYRS   DS    CL1                 SUPV HAS BUYERS                              
SVSUPV   DS    CL4                 BUYER'S SUPV                                 
SVBYRFLT DS    CL1                 BUYER'S FILTER                               
SVAPPREQ DS    CL1                 SUPV APPROVAL REQD                           
SVSTRTDT DS    XL3                                                              
SVESTFIL DS    CL3                 SAVED EST FILTERS                            
FILTMKTS DS    CL1                                                              
FILTCLTS DS    CL1                                                              
QTRHEAD  DS    CL19                                                             
USRNAME  DS    CL29                                                             
*                                                                               
SFLAG    DS    XL1                 STATION FLAG                                 
SFEXIST  EQU   X'80'               STATIONS EXIST                               
SFORDER  EQU   X'40'               STATION HAS BEEN ORDERED                     
SFNORDER EQU   X'20'               STATION NOT ORDERED                          
SFCNFRM  EQU   X'10'               STATION CONFIRMED                            
SFNOCNFM EQU   X'08'               STATION NOT CONFIRMED                        
*                                                                               
OFFICER  DS    A                   A(OFFICER)                                   
OFCLIST  DS    XL32                OFFICE LIST FROM ID RECORD                   
*                                                                               
CLTLIST  DS    XL1000              CLIENT PASSED OFFICER LIST                   
         DC    X'FFFF'             END OF CLTLIST TABLE                         
*                                                                               
         DC    C'*SORTREC'                                                      
SORTREC  DS    CL(SRRECLEN)        SORT RECORD                                  
LASTSORT DS    CL(SRRECLEN)        SORT RECORD                                  
QUARTAB  DS    CL255               QUARTER TABLE                                
*                                                                               
         DS    0D                                                               
         DC    CL8'FMKTTAB'                                                     
FMKTTAB  DS    2000XL2                                                          
FMKTTBX  EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'FCLTTAB'                                                     
FCLTTAB  DS    2000XL3                                                          
FCLTTBX  EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'ESTFTAB'                                                     
ESTFTAB  DS    2000XL9                                                          
ESTFTBX  EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'MKTTAB'                                                      
MKTTAB   DS    4000XL26                                                         
MKTTBX   EQU   *                                                                
*                                                                               
         EJECT                                                                  
***********************************************************************         
*                  DSECTS                                                       
***********************************************************************         
SRTRECD  DSECT                                                                  
SRTQTRST DS    XL3                 QUARTER START DATE                           
*                                                                               
SRTSTAT  DS    CL1                 STATUS                                       
COMPAPPR EQU   C'A'                COMPLETED AND APPROVED                       
PENDCANX EQU   C'B'                PENDING COMPLETION, CANX                     
PENDDELD EQU   C'C'                PENDING COMPLETION, DELETED                  
COMPPEND EQU   C'D'                COMPLETED, PENDING APPROVAL                  
COMPREJ  EQU   C'E'                COMPLETED, REJECTED                          
WIPPAST  EQU   C'F'                IN PROGRESS, PAST DUE                        
WIPNOT   EQU   C'G'                IN PROGRESS, NOT YET DUE                     
DONECANX EQU   C'H'                DONE - CANX                                  
DONEDELD EQU   C'I'                DONE - DELETED                               
DONE     EQU   C'J'                DONE                                         
DONECONF EQU   C'K'                DONE - ORDER CONFIRMED                       
*                                                                               
SRTDATE  DS    XL3                 COULD BE START OR DUE DATE                   
*                                  END OF SORT KEY                              
SRTCLT   DS    XL2                 CLIENT                                       
SRTPRD   DS    XL1                 PRODUCT                                      
SRTPRD2  DS    XL1                 PRODUCT2                                     
SRTEST   DS    XL1                 ESTIMATE                                     
SRTFLT   DS    XL1                 FLIGHT                                       
SRTMKT   DS    XL2                 MARKET                                       
*                                                                               
SRTDUE   DS    XL3                 DUE DATE                                     
SRTSTART DS    XL3                 START DATE                                   
*                                                                               
SRTREV   DS    XL1                 REVISION NUMBER                              
SRTSSTA  DS    XL3                 SUPV STATUS BYTE OR DATE                     
SRTCANQ  EQU   1                   CANCELLED                                    
SRTPNDQ  EQU   2                   PENDING                                      
SRTREJQ  EQU   3                   REJECTED                                     
SRTDELQ  EQU   4                   DELETED                                      
SRTBSTA  DS    XL3                 BUYER STATUS BYTE OR DATE -SEE ABOVE         
*                                                                               
SRTSTORD DS    XL1                 STATIONS ORDERED                             
STABLNK  EQU   0                   NO STATIONS ORDERED YET                      
STAPRTL  EQU   1                   SOME STATIONS ORDERED                        
STABLSTA EQU   2                   BLANK, BUT HAS STATIONS                      
STAORDRD EQU   3                   ALL STATIONS ORDERED                         
STACNFRM EQU   10                  ALL STATIONS CONFIRMED                       
*                                                                               
SRRECLEN EQU   *-SRTRECD                                                        
*                                                                               
*  PRINT LINE DSECT                                                             
         PRINT OFF                                                              
       ++INCLUDE SPGENAUTH                                                      
CLTRECD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
ESTRECD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
       ++INCLUDE SPGENBYR                                                       
       ++INCLUDE SPGENSPV                                                       
       ++INCLUDE SPGENMKG                                                       
       ++INCLUDE SPGENGRP                                                       
       ++INCLUDE SPGENCLTO                                                      
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDDLCB                                                         
       ++INCLUDE DDOFFICED                                                      
       ++INCLUDE DDMASTD                                                        
*                                                                               
       ++INCLUDE SPREPWORKD                                                     
         ORG   P                                                                
PSTATUS  DS    CL30                                                             
         DS    CL1                                                              
         ORG   P                                                                
PSTAT    DS    CL8                                                              
         DS    CL2                                                              
PDATE1   DS    CL8                                                              
         DS    CL2                                                              
PDATE2   DS    CL8                                                              
         DS    CL2                                                              
PCLT     DS    CL3                                                              
         DS    CL2                                                              
PPRD     DS    CL3                                                              
         DS    CL1                                                              
PPRD2    DS    CL3                                                              
         DS    CL3                                                              
PEST     DS    CL3                                                              
PHYPH    DS    CL1                                                              
PFLT     DS    CL1                                                              
         DS    CL1                                                              
PMKT     DS    CL4                                                              
         DS    CL1                                                              
PMKTNAME DS    CL24                                                             
         DS    CL2                                                              
PMKTREV  DS    CL3                                                              
         DS    CL2                                                              
PBCOMP   DS    CL8                                                              
         DS    CL3                                                              
PSAPPR   DS    CL8                                                              
         DS    CL2                                                              
PSTAORD  DS    CL10                                                             
         DS    CL1                                                              
         ORG                                                                    
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'014SPREPSS02 01/20/11'                                      
         END                                                                    
