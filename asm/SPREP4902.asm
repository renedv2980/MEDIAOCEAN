*          DATA SET SPREP4902  AT LEVEL 071 AS OF 02/08/17                      
*PHASE SP4902A                                                                  
*INCLUDE OFFOUT                                                                 
         SPACE 1                                                                
*********************************************************************           
* CHANGE LOG                                                        *           
*                                                                               
* AKAT   12/13     CROSSFILE AND 2-CHARCATER SYSTEM NAME SUPPORT                
*                                                                               
* BPLA   03/07     DETECT MAX MONTHS FOR REQUESTED REPORT                       
*                  AND DISPLAY ERROR MESSAGE                                    
*                                                                               
* BPLA   09/05     CHANGES FOR 2 CHARACTER OFFICES                              
*                                                                               
* BPLA   05/04     CLOSED-OUT RECORD  - CONTROL BITS                            
*                                                                               
* BPLA   12/03     USE CORE-RES OFFICER AND ALLOW FOR ONE CLIENT                
*                                                                               
* DEIS   8/02      USE PL6 RETURNED VALUES FROM SPBVAL                          
*                                                                               
* AKAT   6/02      FILTER OUT ETYPE=B/S IN ESTIMATE REC                         
*                                                                               
* BPLA   1/00      FIX Y2K BUG                                                  
*                                                                               
* BPLA   5/99      USE PACKED ACCUMLATORS FOR ORDERED CURRENT                   
*                  AND PAID CURRENT (TODAY)                                     
*                                                                               
* BPLA   1/98      ADD OPTION IN QOPT1-1 (COL61) TO SUPPRESS                    
*                  CLIENT DETAILS                                               
*                                                                               
* BPLA   1/96      PASS CPROF+6 TO CLUNPK                                       
*                                                                               
* BPLA   12/1/92   FOR OFFICE LIST REQUESTS - LIST CODE IN HEADLINES            
*                                                                               
* BPLA   8/7/92    CHANGES TO ALLOW FOR REPORT BY CLIENT GROUP                  
*                                                                               
* BPLA   8/22/91   DISPLAY OFFICE AND ACC OFFICE WITH CLIENT NAME               
*                  IN MIDLINES                                                  
* BPLA   10/5/90   CLEAR SVOFCLST FOR EACH REQUEST                              
*                  ALLOW FOR $* - ALL OFFICE REQUESTS                           
*                                                                               
* 19FEB87  RE-TAG ROUTINE TO BUILD CLIENTS AND INCLUDE SUPPORT FOR  *           
*          OFFICER                                                  *           
*                                                                   *           
*********************************************************************           
         TITLE 'SP049-02  SPOTPAK AGENCY SUMMARY'                               
*                                                                               
* QOPT1-1 (61) C=SUPPRESS CLIENT DETAILS                                        
*                                                                               
* QOPT 1       P=PRIOR                                                          
*              S=SUBS                                                           
*              B=BOTH                                                           
*                                                                               
* QOPT2        C=CLOSE-OUT RUN                                                  
*                                                                               
* QOPT3        1=PRINT ALL-MEDIA TOTALS AFTER                                   
*              2=PRINT ONLY ALL-MEDIA TOTALS                                    
*                                                                               
* QOPT4        N=SUPPRESS OFFICE ORDER- ALLOWS SELECTION OF                     
*                OFFICE WITHOUT GETTING OFFICE ORDER                            
*                                                                               
* QOPT 5       S=PRINT SPECIAL TOTALS                                           
*                                                                               
* QOPT 6       C=CURRENT MONTH BILLED, (DEFAULT IS AUTHORIZED)                  
*                ALSO YTD TOTALS, NOT TODAY   (**MARGA'S BILLING**)             
*                                                                               
* QOPT7        P=PRODUCTION RUN - USE SP49WF                                    
*                                                                               
         EJECT                                                                  
SP4902   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SP4902                                                         
         SPACE 2                                                                
         L     RA,0(R1)                                                         
         LA    R9,1(RA)                                                         
         LA    R9,4095(R9)                                                      
         USING SPWORKD,RA,R9                                                    
*                                                                               
         LA    R8,SP4902+4095                                                   
         LA    R8,1(R8)                                                         
         USING SP4902+4096,R8      ** NOTE USE OF R8 AS SECOND BASE **          
*                                                                               
         LA    RC,SPACEND                                                       
         USING SP49WRKD,RC                                                      
*                                                                               
         CLI   MODE,RUNLAST                                                     
         BE    LAST                                                             
         CLI   MODE,REQFRST                                                     
         BNE   EXIT                                                             
         BAS   RE,PROCAGYM                                                      
         GOTO1 AENDREQ                                                          
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                  INITIALIZATION                               
INIT     NTR1                                                                   
         SPACE 1                                                                
         CLI   FRSTIM,0                                                         
         BNE   INIT2                                                            
         MVI   FRSTIM,1                                                         
*                                  RELOCATE ACONS                               
         L     RF,ADCONLST                                                      
         USING SPADCONS,RF                                                      
         MVC   AOFFICER,VOFFICER    SAVE OFFICER'S ADDRESS                      
         DROP  RF                                                               
*                                                                               
         RELOC (R4)                                                             
         LA    R0,(ACONX-ACONS)/4                                               
         LA    R1,ACONS                                                         
         LA    R2,ACLTTOTS                                                      
INIT0    DS    0H                                                               
         L     R3,0(R1)                                                         
         AR    R3,R4                                                            
         ST    R3,0(R2)                                                         
         LA    R1,4(R1)                                                         
         LA    R2,4(R2)                                                         
         BCT   R0,INIT0                                                         
*                                                                               
         XC    WORK(16),WORK       READ 00A PROFILE                             
         MVC   WORK(4),=C'S00A'                                                 
         NI    WORK,X'BF'          MAKE 'S' LOWERCASE                           
         MVC   WORK+4(2),AGY                                                    
         GOTO1 GETPROF,DMCB,WORK,SV00APRF                                       
*                                                                               
         XC    WFID,WFID                                                        
*                                                                               
         MVC   TODAY+0(2),RCDATE+6      YR                                      
         MVC   TODAY+2(2),RCDATE+0      MO                                      
         MVC   TODAY+4(2),RCDATE+3      DA                                      
*                                                                               
         GOTO1 DATCON,DMCB,TODAY,(3,BTODAY)                                     
         GOTO1 DATCON,DMCB,(3,BTODAY),(0,TODAY)                                 
         GOTO1 GETDAY,DMCB,TODAY,WORK                                           
         MVC   DAY,DMCB            SAVER DAY OF WEEK                            
*                                                                               
         XC    OLDKEY,OLDKEY                                                    
         L     R2,ADDSTOTS         CLEAR DDSTOTS                                
         BAS   RE,CLEAR                                                         
         SPACE 1                                                                
         MVC   MLISTX(10),=C'AMTGTNMGMN'                                        
         SPACE 1                                                                
INIT2    DS    0H                  **DONE FOR EACH REQUEST**                    
         OI    DMINBTS,X'08'       SET TO PASS DELETES                          
         MVI   DMOUTBTS,X'FD'                                                   
         L     R2,ACLTTOTS         CLEAR CLTTOTS                                
         BAS   RE,CLEAR                                                         
         L     R2,AAGYTOTS         CLEAR AGYTOTS                                
         BAS   RE,CLEAR                                                         
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVC   P,SPACES                                                         
*                                  BUILD MLIST FROM RUN DATE                    
         MVC   BQEND,=3X'FF'       SET ENDING DATE                              
         CLI   QEND,C' '                                                        
         BNH   INIT3                                                            
         GOTO1 DATCON,DMCB,QEND,(3,BQEND)                                       
         MVI   QAREA+54,C' '       MONTHS AHEAD TO BE CALCULATED                
*                                                                               
INIT3    DS    0H                                                               
         MVC   WORK(6),QSTART      USE QSTART IF THERE                          
         CLI   QSTART,C' '                                                      
         BNE   INIT4                                                            
         MVC   WORK(2),RCDATE+6   YR                                            
         MVC   WORK+2(2),RCDATE   MO                                            
         MVC   WORK+4(2),RCDATE+3 DA                                            
INIT4    DS    0H                                                               
         GOTO1 DATCON,DMCB,WORK,(3,BSTART)                                      
         SPACE 1                                                                
         MVC   MBKOPT,=H'36'                                                    
         MVC   MUPOPT,=H'23'                                                    
         CLI   QSTART,C' '                                                      
         BE    INIT4B                                                           
         MVC   MBKOPT,=H'1'                                                     
         MVC   MUPOPT,=H'58'                                                    
*                                                                               
INIT4B   DS    0H                                                               
         XC    MLIST,MLIST                                                      
         LH    R3,MBKOPT           NUMBER OF MONTHS BACK OPTION                 
         SLL   R3,1                X 2                                          
         LA    R2,MLIST(R3)        R2 = A(THIS MONTH)                           
         LR    RF,R2               SAVE R2                                      
         MVC   0(2,R2),BSTART                                                   
         LH    R3,MBKOPT                                                        
         BAS   RE,MNTHBACK         BACK UP 1 MONTH                              
         BCT   R3,*-4                                                           
         LH    R3,MUPOPT           NUMBER OF MONTHS UP OPTION                   
         LR    R2,RF               RESTORE R2                                   
         SR    R4,R4                                                            
*                                                                               
INIT6    DS    0H                                                               
         CLC   0(2,R2),BQEND       TEST HAVE REACHED REQ'D END                  
         BL    INIT7                                                            
         CLI   QAREA+54,C' '       SET MONTHS AHEAD                             
         BNE   INIT7                                                            
         CVD   R4,DUB                                                           
         CP    DUB,=P'58'          CHECK VS. MAX                                
         BH    MAXERR                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  QAREA+54(2),DUB                                                  
*                                                                               
INIT7    DS    0H                                                               
         LA    R4,1(R4)                                                         
         CH    R4,=H'58'           CHECK VS. MAX                                
         BH    MAXERR                                                           
         BAS   RE,MNTHUP           BUMP 1 MONTH                                 
*                                                                               
         BCT   R3,INIT6                                                         
*                                                                               
*        BELOW CODE TO HANDLE REQUESTED 49 FOR EXACTLY 59 MONTHS                
*        SINCE JUST REQUESTS START BACK ONE MONTH AND GO FORWARD 58             
*        FOR THE MAXIMUM OF 60 MONTHS ALTOGETHER                                
*                                                                               
         CLI   QAREA+54,C' '       MAY NEED TO SET MONTHS FORWARD               
         BNE   INITX                                                            
         CLC   0(2,R2),BQEND       TEST HAVE REACHED REQ'D END                  
         BNE   MAXERR              EXACTLY 58 MTHS FORWARD IN THIS CASE         
         CVD   R4,DUB              IN QAREA+54                                  
         OI    DUB+7,X'0F'                                                      
         UNPK  QAREA+54(2),DUB                                                  
*                                                                               
         SPACE 2                                                                
INITX    DS    0H                                                               
         B     EXIT                                                             
*                                                                               
MAXERR   DS    0H                                                               
         MVI   SKIPSPEC,C'Y'                                                    
         MVC   P(80),QRECORD                                                    
         GOTO1 REPORT                                                           
         MVC   P(39),=C'*** ERROR - MAXIMUM MONTHS EXCEEDED ***'                
         GOTO1 REPORT                                                           
         MVC   P(23),=C'*** REQUEST SKIPPED ***'                                
         GOTO1 REPORT                                                           
         MVI   MODE,REQLAST                                                     
         B     EXIT                                                             
*                                                                               
         SPACE 3                                                                
LAST     DS    0H                                                               
         OC    WFID,WFID                                                        
         BZ    EXIT                NO WORKER FILES                              
         CLI   CROSSFIL,C'Y'       CROSS FILE?                                  
         BE    EXIT                                                             
         GOTO1 AWFMOD,DMCB,=C'CLOSE'                                            
         B     EXIT                                                             
         EJECT                                                                  
*                                  PROCESS AN AGY/MED                           
PROCAGYM NTR1                                                                   
         BAS   RE,INIT                                                          
         CLI   MODE,REQLAST      MODE SET TO THIS IF INIT FINDS                 
         BE    EXIT              MAX MONTH ERROR - JUST EXIT                    
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,10                                                      
         MVC   PAGE,=H'1'                                                       
         MVI   PSTART,0                                                         
*                               ADJUST OLD REQ OPTIONS                          
         CLC   QOPT1(2),=C'NN'     NO PRIOR,NO SUBS                             
         BNE   *+14                                                             
         MVC   QOPT1(2),=C'  '                                                  
         B     PRA1A                                                            
         CLC   QOPT1(2),=C'NY'     NO PRIOR,SUBS                                
         BNE   *+14                                                             
         MVC   QOPT1(2),=C'S '                                                  
         B     PRA1A                                                            
         CLC   QOPT1(2),=C'YN'     PRIOR,NO SUBS                                
         BNE   *+14                                                             
         MVC   QOPT1(2),=C'P '                                                  
         B     PRA1A                                                            
         CLC   QOPT1(2),=C'YY'     PRIOR + SUBS                                 
         BNE   *+10                                                             
         MVC   QOPT1(2),=C'B '                                                  
*                                                                               
PRA1A    DS    0H                                                               
         BAS   RE,SETSW                                                         
         CLI   CROSSFIL,C'Y'       CROSS FILE?                                  
         BNE   PRA1B                                                            
         BAS   RE,DDSSUM                                                        
         B     PRAGX                                                            
         SPACE                                                                  
PRA1B    DS    0H                                                               
         CLI   QOPT3,C'1'                                                       
         BH    PRAG30                                                           
         CLI   QSTART,C' '         TEST REQ'D 49                                
         BNE   PRA1B2              YES - ALWAYS PROCESS                         
         CLC   QAGY(6),OLDKEY          A/M/CLT                                  
         BE    PRAG23              SKIP PROCESSING IF SAME                      
*                                                                               
PRA1B2   DS    0H                                                               
         CLC   QAGY(2),OLDKEY      ELSE CLEAR ON AGY BRK                        
         BE    PRAG1                                                            
*                                                                               
         L     R2,ADDSTOTS                                                      
         BAS   RE,CLEAR                                                         
PRAG1    DS    0H                                                               
         L     R2,AAGYTOTS                                                      
         BAS   RE,CLEAR                                                         
         BAS   RE,BLDCLT           BUILD LIST OF CLIENTS                        
*                                                                               
         L     R2,AOFFTOTS                                                      
         BAS   RE,CLEAR                                                         
         MVC   ACLIENT,ACLTTAB                                                  
         L     R4,ACLIENT                                                       
         OC    0(20,R4),0(R4)      TEST ANY ACTIVITY                            
         BZ    PRAG20              NO                                           
         MVC   LASTBRK(5),0(R4)                                                 
         SPACE                                                                  
PRAG5    DS    0H             GET NEW CLIENT HDR                                
         L     R5,ADCLT                                                         
         USING CKEY,R5                                                          
         MVI   CLTACT,C'N'                                                      
         MVC   KEY+14(4),7(R4)     DISK ADDR                                    
         GOTO1 GETCLT                                                           
*                                                                               
*        CLEAR ANY CLIENT SPECIFIC MEDIA NAME                                   
*        FOR THESE REQUESTS AS IT APPEARS IN THE HEADLINES                      
*                                                                               
         L     RF,ADCLT                                                         
         MVC   CMEDNAME-CLTHDR(L'CMEDNAME,RF),SPACES                            
*                                                                               
*        GET 2 CHARACTER CODE                                                   
*                                                                               
         MVC   SAVCOFF,SPACES                                                   
         MVI   C2ACTIVE,C'N'       DEFAULT TO NO OLA AND/OR AGY NOT             
*                                  USING 2 CHARACTER OFFICE CODES               
         XC    WORK,WORK                                                        
OFFD     USING OFFICED,WORK                                                     
         MVI   OFFD.OFCSYS,C'S'                                                 
*                                                                               
         MVC   OFFD.OFCAGY,SVAGY                                                
         MVC   OFFD.OFCOFC,COFFICE-CLTHDR(RF)                                   
         GOTO1 AOFFICER,DMCB,(C'2',WORK),(0,ACOMFACS)                           
         CLI   0(R1),0                                                          
         BNE   PRAG50                                                           
         TM    OFFD.OFCINDS,OFCINOLA NO OLA RECS AND/OR AGY NOT USING           
         BNZ   *+8                 2 CHARACTER OFFICES                          
         MVI   C2ACTIVE,C'Y'       SET ACTIVE                                   
*                                                                               
         MVC   SAVCOFF,OFFD.OFCOFC2                                             
*                                                                               
         DROP  OFFD                                                             
*                                                                               
PRAG50   MVI   HAVPRI,0                                                         
         MVI   HAVSUB,0                                                         
         L     R2,ACLTTOTS         CLEAR CLTTOTS                                
         BAS   RE,CLEAR                                                         
         SPACE                                                                  
         MVC   KEY(13),CKEY        00/AM/CLT(4)+9X'00'                          
         GOTO1 HIGH                                                             
         B     PRAG5B                                                           
PRAG5A   GOTO1 SEQ                                                              
PRAG5B   CLC   KEY(4),KEYSAVE      TEST A/M/C                                   
         BNE   PRAG6A6                                                          
         CLC   KEY+7(6),=6X'00'    TEST FOR EST HDR/ BILL                       
         BE    PRAG5A              NEITHER EST NOR BILL                         
         CLC   KEY+8(5),=5X'00'                                                 
         BE    PRAG6               EST HDR                                      
         B     PRAG10              BILL                                         
         SPACE                                                                  
PRAG6    EQU   *                                                                
         CLC   KEY(4),KEYSAVE                                                   
         BNE   PRAG6A6                                                          
         CLI   QOPT2,C'C'          TEST CLOSE-OUT RUN                           
         BNE   PRAG6A              NO                                           
*                                                                               
         TM    KEY+13,X'81'        YES- MUST BE CLOSED OUT                      
         BO    PRAG6A4             (WAS X'C0')                                  
         B     PRAG5A                                                           
*                                                                               
PRAG6A   DS    0H                                                               
         TM    KEY+13,X'80'                                                     
         BNZ   PRAG5A              SKIP ALL DELETES                             
*                                                                               
PRAG6A4  DS    0H                                                               
         B     PRAG16                                                           
         SPACE                                                                  
*                                  FINISH OLD CLIENT                            
PRAG6A6  DS    0H                                                               
         CLI   CLTACT,C'Y'                                                      
         BNE   PRAG6B                                                           
         MVC   MID1,SPACES                                                      
         L     R5,ADCLT                                                         
         USING CKEY,R5                                                          
******** GOTO1 CLUNPK,DMCB,CKEYCLT,MID1                                         
         GOTO1 CLUNPK,DMCB,(CPROF+6,CKEYCLT),MID1                               
         MVC   MID1+4(20),CNAME                                                 
         LA    R2,MID2                                                          
         CLI   QCLT,C'*'           SEE IF OFFICE                                
         BE    PRAG6A8                                                          
         CLI   QCLT,C'$'           OR OFFICE LIST REQUEST                       
         BE    PRAG6A8             IF SO ONLY SHOWW ACC OFFICE                  
         CLC   SAVCOFF,SPACES                                                   
         BE    PRAG6A8                                                          
         LA    R2,MID2+10                                                       
         MVC   MID2(6),=C'OFFICE'                                               
         MVC   MID2+7(2),SAVCOFF                                                
*                                                                               
         CLI   C2ACTIVE,C'Y'       2 CHARACTERS ACTIVE?                         
         BE    PRAG6A8                                                          
*                                  IF NOT FALL THRU OLD CODE                    
*                                                                               
         CLI   SV00APRF+2,C'Y'     TEST ALWAYS PRINT AS CHAR                    
         BE    PRAG6A8                                                          
         GOTO1 =V(OFFOUT),DMCB,COFFICE,HEXOUT,MID2+7                            
*                                                                               
PRAG6A8  CLI   CACCOFC,C' '                                                     
         BNH   PRAG6A9                                                          
         MVC   0(10,R2),=C'ACC OFFICE'                                          
         MVC   11(2,R2),CACCOFC                                                 
*                                                                               
         DROP  R5                                                               
*                                                                               
PRAG6A9  DS    0H                                                               
         MVI   FORCEMID,C'Y'                                                    
         L     R2,ACLTTOTS                                                      
         L     R3,AOFFTOTS                                                      
         BAS   RE,ROLLUP                                                        
*                                                                               
         CLI   QOPT1-1,C'C'       SEE IF SUPPRESSING CLIENT DETAILS             
         BE    PRAG6B                                                           
*                                                                               
         GOTO1 APRNT,DMCB,ACLTTOTS                                              
*                                                                               
PRAG6B   DS    0H                                                               
         L     R4,ACLIENT                                                       
         LA    R4,20(R4)                                                        
         ST    R4,ACLIENT                                                       
         CLC   LASTBRK(5),0(R4)                                                 
         BE    PRAG7                                                            
         OC    LASTBRK,LASTBRK                                                  
         BZ    PRAG6D                                                           
*                                  END OF OFFICE OR GROUP                       
         MVC   MID1,SPACES                                                      
         CLI   QCLGID,C' '        SEE IF CLIENT GROUP REQUEST                   
         BNH   PRAG6C                                                           
         MVC   MID1(12),=C'GROUP TOTALS'                                        
         MVC   MID2(12),=C'------------'                                        
         B     PRAG6C5                                                          
*                                                                               
PRAG6C   MVC   MID1(13),=C'OFFICE TOTALS'                                       
         MVC   MID2(13),=C'-------------'                                       
PRAG6C5  MVI   FORCEMID,C'Y'                                                    
         GOTO1 APRNT,DMCB,AOFFTOTS                                              
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
PRAG6D   DS    0H                                                               
         L     R2,AOFFTOTS                                                      
         L     R3,AAGYTOTS                                                      
         BAS   RE,ROLLUP                                                        
         L     R2,AOFFTOTS                                                      
         BAS   RE,CLEAR                                                         
         CLI   0(R4),X'FF'                                                      
         BE    PRAG20                                                           
         MVC   LASTBRK(5),0(R4)                                                 
PRAG7    B     PRAG5                                                            
         EJECT                                                                  
* READ BILL RECORDS *                                                           
PRAG8    DS    0H                                                               
PRAG10   CLI   QOPT2,C'C'          TEST CLOSE OUT RUN                           
         BNE   PRAG11              NO                                           
*                                                                               
         TM    KEY+13,X'81'        YES- MUST BE CLOSED OUT                      
         BO    PRAG12              (WAS X'C0')                                  
         B     PRAG5A                                                           
*                                                                               
PRAG11   DS    0H                                                               
         TM    KEY+13,X'80'        SKIP DELETES                                 
         BNZ   PRAG5A                                                           
*                                                                               
PRAG12   DS    0H                                                               
         L     RF,ADEST                                                         
         TM    EPRDCD-ESTHDR(RF),X'80'        TEST NEW NETPAK                   
         BZ    PRAG13              NO-OK                                        
         CLI   QOPT6,C'C'                                                       
         BNE   PRAG5A              SKIP UNLESS CURRENT MONTH BILLING            
*                                                                               
PRAG13   DS    0H                                                               
         MVI   CLTACT,C'Y'                                                      
         L     R5,ADBILL                                                        
         USING BILLREC,R5                                                       
         GOTO1 GETBILL                                                          
         CLI   BRETAIL-BILLREC(R5),X'41'   SKIP RETAIL CORP SUMMARIES           
         BE    PRAG5A                                                           
         BAS   RE,ADDUP                                                         
         B     PRAG5A                                                           
         SPACE                                                                  
* READ ESTHDR BUCKETS                                                           
PRAG16   DS    0H                                                               
         MVI   CLTACT,C'Y'                                                      
         L     R5,ADEST                                                         
         USING EKEY,R5                                                          
         GOTO1 GETEST                                                           
**                                                                              
* ONLY FILTER OUT ETYPE=BAR/STW IF USER REQUESTS IT                             
***                                                                             
         CLI   Q2USER,C'Y'             FILTER OUT ETYPE=STW/BAR?                
         BE    PRAG19                  NO                                       
*                                                                               
         CLI   ETYPE-ESTHDR(R5),C'B'   ETYPE = BARTER?                          
         BE    PRAG5A                  YES...SKIP                               
         CLI   ETYPE-ESTHDR(R5),C'S'   ETYPE = STEWARDSHIP?                     
         BE    PRAG5A                  YES...SKIP                               
PRAG19   BAS   RE,ADDUP                                                         
         B     PRAG5A                                                           
*                                                                               
PRAG20   EQU   *                                                                
         XC    LASTBRK,LASTBRK                                                  
         MVC   MID1,SPACES                                                      
         MVC   MID1(13),=C'AGENCY TOTALS'                                       
         CLI   QCLGID,C' '              SEE IF CLIENT GROUP REQUEST             
         BNH   PRAG21                                                           
         MVC   MID1(15),=C'SCHEME   TOTALS'                                     
         MVC   MID1+7(1),QCLGID                                                 
*                                                                               
PRAG21   MVI   FORCEMID,C'Y'                                                    
         CLI   QOPT7,C'P'                                                       
         BNE   PRAG22                                                           
*                                  SAVE AGYNAME & TOTS                          
*                                            WRITE FROM AGYTOTS                 
         GOTO1 AWFMOD,DMCB,=C'WA',AAGYTOTS                                      
*                                                                               
PRAG22   EQU   *                                                                
         CLI   QSTART,C' '         NO CROSS MEDIA TOTALS                        
         BNE   PRAG22B             FOR NON-AUTO REPORTS                         
         L     R2,AAGYTOTS                                                      
         L     R3,ADDSTOTS                                                      
         BAS   RE,ROLLUP           ADD TO CROSS MEDIA TOTALS                    
*                                                                               
PRAG22B  DS    0H                                                               
         CLI   QCLGID,C' '         SEE IF OFFICE GROUP REQUEST                  
         BNH   PRAG22F                                                          
         CLC   QCLGRP(3),=C'ALL'   SEE IF DOING ONE GROUP                       
         BNE   PRAG24              YES THEN SKIP AGY TOTALS                     
         B     PRAG23                                                           
*                                                                               
PRAG22F  CLI   QCLT,C'*'                                                        
         BNE   PRAG23                                                           
         CLI   QOPT4,C'N'          SUPPRESS OFFICE                              
         BE    PRAG23                                                           
         CLI   QCLT+2,C' '            BYPASS AGY TOTS IF ONLY 1 OFFICE          
         BNE   PRAG23                                                           
         CLI   QCLT+1,C' '                                                      
         BNE   PRAG24                                                           
*                                                                               
PRAG23   DS    0H                                                               
         GOTO1 APRNT,DMCB,AAGYTOTS                                              
*                                                                               
PRAG24   DS    0H                                                               
         CLI   QOPT3,C'1'                                                       
         BE    PRAG30                                                           
         B     PRAGX                                                            
*                                                                               
*                                                                               
PRAG30   DS    0H                                                               
         MVC   MID1(13),=C'AGENCY TOTALS'                                       
         CLI   QCLT,C'*'                                                        
         BNE   PRAG31                                                           
         CLI   QOPT4,C'N'          SUPPRESS OFFICE                              
         BE    PRAG31                                                           
         CLI   QCLT+2,C' '         TEST                                         
         BNE   PRAG31                  ONLY                                     
         CLI   QCLT+1,C' '                 ONE OFFICE                           
         BE    PRAG31              NO                                           
         MVC   MID1(6),=C'OFFICE'                                               
PRAG31   DS    0H                                                               
         MVC   SVMEDNM,MEDNM        SAVE REAL MEDIA NAME                        
         MVC   SVMEDCD,MED          AND CODE                                    
         MVC   MEDNM,=C'ALL MEDIA '                                             
         MVI   MED,C' '                                                         
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 APRNT,DMCB,ADDSTOTS                                              
         CLI   QOPT3,C'2'                                                       
         BE    PRAG32                                                           
*                                                                               
         CLI   QOPT7,C'P'                                                       
         BNE   PRAG32                                                           
         MVI   MED,C'Z'                                                         
         GOTO1 AWFMOD,DMCB,=C'WA',ADDSTOTS                                      
*                                                                               
PRAG32   DS    0H                                                               
         CLI   CROSSFIL,C'Y'       CROSS FILE?                                  
         BE    *+16                                                             
         MVC   MEDNM,SVMEDNM         RESTORE REAL MEDIA NAME                    
         MVC   MED,SVMEDCD           AND CODE                                   
*                                                                               
         L     R2,ADDSTOTS         CLEAR DDS TOTS AFTER PRINT                   
         BAS   RE,CLEAR                                                         
*                                                                               
PRAGX    EQU   *                                                                
         MVC   OLDKEY,QAGY                                                      
         MVI   FORCEHED,C'Y'                                                    
         XIT1                                                                   
         EJECT                                                                  
********************************************************************            
* SUBROUTINE TO BUILD LIST OF ALL CLIENTS AND SORT IN OFFICE ORDER *            
********************************************************************            
         SPACE                                                                  
BLDCLT   NTR1                                                                   
         L     R5,ADCLT                                                         
         USING CKEY,R5                                                          
         XC    KEY,KEY                                                          
         MVI   KEY,X'00'           REC TYPE                                     
         MVC   KEY+1(1),BAGYMD                                                  
         XC    MYOFCLST,MYOFCLST                                                
         L     RE,ACLTTAB          CLEAR CLIENT TABLE                           
         L     RF,=A(MAXCLTS*20)                                                
         XCEFL                                                                  
         XC    CLTCNT,CLTCNT       CLEAR COUNT                                  
         L     R4,ACLTTAB                                                       
*                                                                               
         CLI   QCLT,C'A'                                                        
         BL    BLDCL2                                                           
         CLC   =C'ALL',QCLT                                                     
         BE    BLDCL2                                                           
         MVI   CLTSW,C'1'       MUST BE ONE CLIENT                              
         GOTO1 CLPACK,DMCB,QCLT,KEY+2                                           
*                                                                               
BLDCL2   GOTO1 HIGH                                                             
         B     BLDCL6                                                           
BLDCL4   DS    0H                                                               
         GOTO1 SEQ                                                              
BLDCL6   DS    0H                                                               
         CLC   KEY(2),KEYSAVE                                                   
         BNE   BLDCL18                                                          
         CLI   QCLT,C'$'           TEST LIST OF OFFICES                         
         BE    BLDCL8                                                           
         CLI   QCLT,C'*'           PROC BY OFFICE                               
         BNE   BLDCL14             NO                                           
         GOTO1 GETCLT                                                           
*                                                                               
*        GET 2 CHARACTER CODE                                                   
*                                                                               
         MVC   SAVCOFF,SPACES                                                   
         L     RF,ADCLT                                                         
         XC    WORK,WORK                                                        
OFFD     USING OFFICED,WORK                                                     
         MVI   OFFD.OFCSYS,C'S'                                                 
*                                                                               
         MVC   OFFD.OFCAGY,SVAGY                                                
         MVC   OFFD.OFCOFC,COFFICE-CLTHDR(RF)                                   
         GOTO1 AOFFICER,DMCB,(C'2',WORK),(0,ACOMFACS)                           
         CLI   0(R1),0                                                          
         BNE   BLDCL6X                                                          
         MVC   SAVCOFF,OFFD.OFCOFC2                                             
*                                                                               
         DROP  OFFD                                                             
*                                                                               
         SPACE                                                                  
BLDCL6X  CLI   QCLT+1,C' '         ALL OFFICES                                  
         BE    BLDCL12             YES                                          
         CLI   QCLT+1,C'-'         ALL BUT                                      
         BE    BLDCL10                                                          
         SPACE                                                                  
         CLC   COFFICE(1),QCLT+1                                                
         BL    BLDCL16                                                          
         BE    BLDCL12             OK                                           
         SPACE                                                                  
         CLI   QCLT+2,C' '         TEST RANGE                                   
         BE    BLDCL16             NO                                           
         CLC   COFFICE(1),QCLT+2      TEST WITHIN RANGE                         
         BH    BLDCL16                                                          
         B     BLDCL12                                                          
         SPACE                                                                  
* PROCESS LIST OF OFFICES *                                                     
         SPACE                                                                  
BLDCL8   DS    0H                                                               
         GOTO1 GETCLT                                                           
*                                                                               
*        GET 2 CHARACTER CODE                                                   
*                                                                               
         MVC   SAVCOFF,SPACES                                                   
         XC    WORK,WORK                                                        
         L     RF,ADCLT                                                         
OFFD     USING OFFICED,WORK                                                     
         MVI   OFFD.OFCSYS,C'S'                                                 
*                                                                               
         MVC   OFFD.OFCAGY,SVAGY                                                
         MVC   OFFD.OFCOFC,COFFICE-CLTHDR(RF)                                   
         GOTO1 AOFFICER,DMCB,(C'2',WORK),(0,ACOMFACS)                           
         CLI   0(R1),0                                                          
         BNE   BLDCL8X                                                          
         MVC   SAVCOFF,OFFD.OFCOFC2                                             
*                                                                               
         DROP  OFFD                                                             
*                                                                               
*                                                                               
BLDCL8X  CLC   QCLT(2),=C'$*'      ALL OFFICES BY OFFICE                        
         BE    BLDCL12                                                          
*                                                                               
         XC    WORK,WORK                                                        
         LA    R1,WORK                                                          
         USING OFFICED,R1                                                       
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAUTH,QCLT                                                     
         MVC   OFCLMT(3),QCLT                                                   
         MVI   OFCLMT+3,C' '                                                    
         MVC   OFCAGY,SVAGY                                                     
         MVC   OFCOFC,COFFICE                                                   
         DROP  R1                                                               
         GOTO1 AOFFICER,DMCB,WORK,(X'D0',ACOMFACS),MYOFCLST                     
         CLI   0(R1),0             TEST AUTHORIZED                              
         BE    BLDCL12             YES - PROCESS                                
         B     BLDCL16                                                          
         SPACE                                                                  
BLDCL10  DS    0H                                                               
         CLC   COFFICE(1),QCLT+2       ALL BUT                                  
         BE    BLDCL16             BYPASS IF =                                  
BLDCL12  DS    0H                                                               
         XC    0(5,R4),0(R4)                                                    
         MVC   0(2,R4),SAVCOFF                                                  
         CLC   0(2,R4),SPACES      NO OFFICE?                                   
         BH    *+10                                                             
         MVC   0(2,R4),=X'FEFF'    USE FOR NO OFFICE PRESENT                    
*                                                                               
         CLI   QOPT4,C'N'          TEST SUPPRESS OFFICE ORDER                   
         BNE   BLDCL15A                                                         
         B     BLDCL15                                                          
*                                                                               
BLDCL14  DS    0H                                                               
         CLI   QCLGID,C' '         SEE IF DOING CLIENT GROUP(S)                 
         BE    BLDCL15                                                          
BLDCL14B GOTO1 =A(NEXTCGR)                                                      
         BNE   BLDCL18                                                          
         MVC   0(5,R4),CGR2       SAVE GROUP ID/CODE                            
         MVC   5(2,R4),SVCLT                                                    
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),SVCLT                                                   
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(4),KEY                                                   
         BE    *+6                                                              
         DC    H'0'                SOMETHING VERY WRONG                         
         MVC   7(4,R4),KEY+14      SAVE DISK ADDRESS                            
         LA    R4,20(R4)                                                        
         L     RE,CLTCNT                                                        
         A     RE,=F'1'                                                         
         ST    RE,CLTCNT                                                        
         MVI   0(R4),X'FF'                                                      
         L     RE,ACLTTAB                                                       
         A     RE,=A(CLTTABX-CLTTAB)                                            
         CR    R4,RE                    COMPARE TO END OF TABLE                 
         BL    *+6                                                              
         DC    H'0'                     TOO MANY CLIENTS                        
         B     BLDCL14B                                                         
*                                                                               
*                                                                               
BLDCL15  DS    0H                                                               
         XC    0(5,R4),0(R4)       ZERO BREAK                                   
BLDCL15A MVC   5(2,R4),KEY+2       CLIENT CODE                                  
         MVC   7(4,R4),KEY+14      DISK ADDRS                                   
         LA    R4,20(R4)                                                        
         MVI   0(R4),X'FF'                                                      
         L     RE,CLTCNT                                                        
         A     RE,=F'1'                                                         
         ST    RE,CLTCNT                                                        
*                                                                               
         L     RE,ACLTTAB                                                       
         A     RE,=A(CLTTABX-CLTTAB)                                            
         CR    R4,RE                    COMPARE TO END OF TABLE                 
         BL    *+6                                                              
         DC    H'0'                     TOO MANY CLIENTS                        
*                                                                               
         CLI   CLTSW,C'1'          WAS I DOING JUST ONE CLIENT?                 
         BE    EXIT                IF SO, DONE                                  
*                                                                               
         SPACE                                                                  
BLDCL16  MVI   KEY+4,X'FF'         TO SKIP TO NXT CLT HDR                       
         B     BLDCL2                                                           
         SPACE                                                                  
BLDCL18  DS    0H                                                               
         C     R4,ACLTTAB                                                       
         BE    EXIT                 NO CLIENTS                                  
         L     R4,CLTCNT                                                        
         GOTO1 XSORT,DMCB,ACLTTAB,(R4),20,7,0                                   
         B     EXIT                                                             
         EJECT                                                                  
DDSSUM   NTR1                                                                   
         SPACE 2                                                                
         XC    LASTBRK,LASTBRK                                                  
*                                                                               
         SPACE 1                                                                
DDSSUM1  DS    0H                                                               
         MVC   QUESTOR(5),=C'SPOT '                                             
         MVC   QUESTOR+5(2),FILENUM2                                            
         MVI   CURMED,C'R'                                                      
         MVI   MED,C'R'                                                         
DDSSUM2  DS    0H                                                               
         XC    WFID,WFID                                                        
DDSSUM2B DS    0H                                                               
         GOTO1 AWFMOD,DMCB,=C'RA'                                               
*                                                                               
         CLI   HLDAGYN,X'FF'       END OF MEDIA                                 
         BNE   DDSSUM8                                                          
         MVC   MID1,SPACES                                                      
         MVC   MID1(10),=C'DDS TOTALS'                                          
         MVI   FORCEMID,C'Y'                                                    
         GOTO1 APRNT,DMCB,ADDSTOTS                                              
*                                                                               
         CLI   CURMED,C'Z'                                                      
         BE    EXIT                                                             
         CLI   CURMED,C'N'                                                      
         BNE   *+12                                                             
         MVI   CURMED,C'Z'                                                      
         B     DDSS4                                                            
         CLI   CURMED,C'X'                                                      
         BNE   *+12                                                             
         MVI   CURMED,C'N'                                                      
         B     DDSS4                                                            
         CLI   CURMED,C'T'                                                      
         BNE   *+12                                                             
         MVI   CURMED,C'X'                                                      
         B     DDSS4                                                            
         CLI   CURMED,C'R'                                                      
         BNE   *+12                                                             
         MVI   CURMED,C'T'                                                      
         B     DDSS4                                                            
         DC    H'0'                                                             
         SPACE                                                                  
DDSS4    DS    0H                                                               
         MVC   MED,CURMED                                                       
         CLI   MED,C'Z'                                                         
         BNE   *+8                                                              
         MVI   MED,C' '                                                         
         MVI   FORCEHED,C'Y'                                                    
         L     R2,ADDSTOTS         CLEAR DDSTOTS                                
         BAS   RE,CLEAR                                                         
         B     DDSSUM2                                                          
         SPACE                                                                  
DDSSUM8  DS    0H                                                               
         L     R2,ACLTTOTS                                                      
         L     R3,ADDSTOTS                                                      
         BAS   RE,ROLLUP                                                        
         MVC   MID1,SPACES                                                      
         MVC   MID1(2),HLDAGYC       AGY                                        
         MVC   MEDNM,HLDMEDN                                                    
         MVC   MID1+4(33),HLDAGYN AGENCY NAME                                   
         MVI   FORCEMID,C'Y'                                                    
         MVI   RCSUBPRG,30                                                      
DDSSUM8B DS    0H                                                               
         GOTO1 APRNT,DMCB,ACLTTOTS                                              
*                                                                               
         CLI   MED,C' '                                                         
         BNE   *+8                                                              
         MVI   MED,C'Z'                                                         
         B     DDSSUM2B                                                         
         SPACE 2                                                                
DDSSUM10 EQU   *                                                                
         B     EXIT                                                             
         EJECT                                                                  
*                                  ACCUMULATING FOR BILLS & BUYS                
ADDUP    NTR1                                                                   
         SPACE 1                                                                
*                                                                               
         CLI   KEY+8,X'00'         IS IT EST HDR                                
         BE    RBK                 YES                                          
         SPACE                                                                  
* BILL RECORDS *                                                                
         SPACE                                                                  
         L     R5,ADBILL                                                        
         USING BILLREC,R5                                                       
         LA    R1,BKEYYSRV         MONTH OF SERVICE                             
         BAS   RE,FINDMO                                                        
*                                                                               
         GOTO1 SPBVAL,DMCB,(C'B',BILLREC),SPBVALD                               
         ZAP   WGRS,SPBVGRSP        EFFECTIVE GROSS                             
         ZAP   WNET,SPBVNETP        AND NET                                     
*                                   R2 HAS A(ROW)                               
         AP    28(7,R2),WGRS+1(7)   GROSS                                       
         SPACE                                                                  
ADDUP2   DS    0H                                                               
         CLC   BDATE(4),TODAY        TEST THIS MONTH                            
         BNE   ADDUP4                                                           
         SPACE                                                                  
         CLI   QOPT6,C'C'          IS IT A CURRENT BILLED REQUEST               
         BNE   ADDUP3              NO.                                          
         AP    0(7,R2),WGRS+1(7)   YES. ADD TO CURRENT BILLED ROW               
         SPACE                                                                  
ADDUP3   L     R7,ACLTTOTS                                                      
         SPACE                                                                  
         LA    R7,MGROW(R7)                                                     
         AP    28(7,R7),WGRS+1(7)  BILLED                                       
         LA    R7,ROWL(R7)                                                      
         AP    28(7,R7),WNET+1(7)   NET                                         
*                                                                               
ADDUP4   DS    0H                                                               
         CLC   BDATE(2),TODAY                                                   
         BNE   ADDUPX                                                           
         CLI   QOPT6,C'C'          YEAR TO DATE OPT                             
         BE    *+14                                                             
         CLC   BDATE,TODAY         ELSE TEST TODAY                              
         BNE   ADDUPX                                                           
         L     R7,ACLTTOTS                                                      
         LA    R7,TGROW(R7)                                                     
         AP    28(7,R7),WGRS+1(7)   BILLED                                      
         LA    R7,ROWL(R7)                                                      
         AP    28(7,R7),WNET+1(7)   NET                                         
*                                                                               
         B     ADDUPX                                                           
         EJECT                                                                  
* BUCKET RECORDS *                                                              
         SPACE                                                                  
* EDIT ESTART TO GIVE BROADCAST MONTH,                                          
* THEN CONVERT TO YMD FORMAT.                                                   
* USING CONVERTED ESTART, LOOP THROUGH BKDTBL TO FIND CORRESPONDING             
* TBL MONTH.  INSERT YEAR. MONTHS ABOVE ESTART MONTH ARE YEAR+1.                
         SPACE 2                                                                
RBK      DS    0H                                                               
         L     R5,ADEST                                                         
         USING ESTHDR,R5                                                        
         MVC   DTWRK1,ESTART                                                    
         L     RF,VMASTC                                                        
         CLI   MCNETPAK-MCBLOCK(RF),C'Y' FOR NETPAK USE CALENDAR MONTHS         
         BE    RBKA                                                             
         GOTO1 GETDAY,DMCB,ESTART,DTWRK1                                        
         ZIC   R1,DMCB                                                          
         SH    R1,=H'7'                                                         
         LCR   R2,R1                                                            
         GOTO1 ADDAY,DMCB,ESTART,DTWRK1,(R2)            GIVES BRD MO            
*                                                                               
RBKA     DS    0H                                                               
         GOTO1 DATCON,DMCB,(0,DTWRK1),(3,DTWRK2)        DTWRK2 = YMD            
         SPACE                                                                  
         LA    RF,12               CLEAR BKDTBL YEAR-BYTES                      
         LA    RE,BKDTBL                                                        
RBK00    MVI   0(RE),X'00'                                                      
         LA    RE,2(RE)                                                         
         BCT   RF,RBK00                                                         
         SPACE                                                                  
         LA    RF,12               MATCH ESTART MONTH IN TBL                    
         LA    RE,BKDTBL                                                        
RBK1     CLC   1(1,RE),DTWRK2+1                                                 
         BE    RBK2                MONTH FOUND                                  
         LA    RE,2(RE)                                                         
         BCT   RF,RBK1                                                          
         DC    H'0'                NO DATE MATCH/BLOW IT UP                     
         SPACE                                                                  
RBK2     DS    0H                  RE POINTS TO MONTH IN TBL                    
RBK2A    MVC   0(1,RE),DTWRK2      MOVE YEAR TO TBL                             
         LA    RE,2(RE)                                                         
         CLI   0(RE),X'FF'                                                      
         BE    RBK3                END OF TBL                                   
         B     RBK2A                                                            
         SPACE                                                                  
* GOTO TOP OF TBL / ADD YEAR IF YEAR-BYTE IS EMPTY                              
RBK3     DS    0H                                                               
         LA    RE,BKDTBL                                                        
         IC    RF,DTWRK2           ADD 1 TO YEAR                                
         LA    RF,1(RF)                                                         
         STC   RF,DTWRK2                                                        
         SPACE                                                                  
RBK3A    CLI   0(RE),X'00'         DOES TBL HAVE DATA                           
         BNE   RBK4                YES. GOTO ADD ROUTINES                       
         MVC   0(1,RE),DTWRK2                                                   
         LA    RE,2(RE)                                                         
         B     RBK3A                                                            
         EJECT                                                                  
* PROCESS EORD/EPAID/EAUTH IN EST HDR. ADD TO TOTALS                            
         SPACE                                                                  
RBK4     DS    0H                                                               
         LA    R0,12                                                            
         LA    RF,EORD             ORDERED                                      
         LA    R1,BKDTBL                                                        
*                                                                               
         ZAP   ORDCUR,=P'0'        CLEAR ORDERED CURRENT                        
         SPACE                                                                  
RBK4A    BAS   RE,FINDMO                                                        
         AP    7(7,R2),0(6,RF)                                                  
**NOP    AP    7(7,R2),78(6,RF)    ADD CURRENT TO YTD                           
**NOP    AP    ORDCUR,78(6,RF)     TODAY'S ORDERED                              
         LA    R1,2(R1)                                                         
         LA    RF,6(RF)                                                         
         BCT   R0,RBK4A                                                         
         SPACE                                                                  
         CLI   QOPT6,C'C'          SKIP TODAY FIGURES FOR BILLING RUN           
         BE    RBK4A2                                                           
*                                                                               
**NOP    L     R7,ACLTTOTS                                                      
**NOP    LA    R7,TGROW(R7)                                                     
**NOP    AP    7(7,R7),ORDCUR      PUT TODAY'S ORDERED IN TABLE                 
         SPACE                                                                  
         L     R7,ACLTTOTS                                                      
         LA    R7,TNROW(R7)                                                     
         AP    21(7,R7),ECURPDN                                                 
         SPACE                                                                  
RBK4A2   DS    0H                                                               
         ZAP   PDCUR,=P'0'         CLEAR PAID TODAY                             
*                                                                               
         LA    R0,12                                                            
         LA    RF,EPAID            PAID                                         
         LA    R1,BKDTBL                                                        
RBK4AA   BAS   RE,FINDMO                                                        
         AP    21(7,R2),0(6,RF)                                                 
**NOP    AP    21(7,R2),78(6,RF)   ADD CURRENT TO YTD                           
**NOP    AP    PDCUR,78(6,RF)                                                   
         LA    R1,2(R1)                                                         
         LA    RF,6(RF)                                                         
         BCT   R0,RBK4AA                                                        
         SPACE                                                                  
         CLI   QOPT6,C'C'          IS IT A 'CURRENT BILLED' REQUEST             
         BE    ADDUPX              YES- SKIP AUTHORIZED PROC AND TODAY          
         SPACE                                                                  
         L     R7,ACLTTOTS                                                      
         LA    R7,TGROW(R7)                                                     
         AP    21(7,R7),PDCUR      PUT TODAY'S PAID IN TABLE                    
         SPACE                                                                  
         LA    R0,12                                                            
         LA    RF,EAUTH            AUTHORIZED                                   
         LA    R1,BKDTBL                                                        
RBK4B    BAS   RE,FINDMO                                                        
         AP    0(7,R2),0(6,RF)                                                  
         LA    R1,2(R1)                                                         
         LA    RF,6(RF)                                                         
         BCT   R0,RBK4B                                                         
         SPACE                                                                  
ADDUPX   EQU   *                                                                
         XIT1                                                                   
         DROP  R5                                                               
*                                                                               
         EJECT                                                                  
BKDTBL   DS    0C                  DATE TBL TO MATCH BUCKETS OF                 
JAN      DS    CL1                 ORDERED AND CURRENT IN EST HDR               
         DC    X'1'                                                             
FEB      DS    CL1                                                              
         DC    X'2'                                                             
MAR      DS    CL1                                                              
         DC    X'3'                                                             
APRIL    DS    CL1                                                              
         DC    X'4'                                                             
MAY      DS    CL1                                                              
         DC    X'5'                                                             
JUNE     DS    CL1                                                              
         DC    X'6'                                                             
JULY     DS    CL1                                                              
         DC    X'7'                                                             
AUG      DS    CL1                                                              
         DC    X'8'                                                             
SEP      DS    CL1                                                              
         DC    X'9'                                                             
OCT      DS    CL1                                                              
         DC    X'A'                                                             
NOV      DS    CL1                                                              
         DC    X'B'                                                             
DEC      DS    CL1                                                              
         DC    X'C'                                                             
TBLTAB   DC    X'FF'                                                            
         EJECT                                                                  
*                                  SET R2 = A(ROW)                              
*                                  R1 = A(MONTH)                                
*                                  NOTE - 13TH MONTH BILLS IGNORED              
*                                         (GO INTO PRIOR)                       
FINDMO   NTR1                                                                   
         LA    R2,MLIST                                                         
         L     R3,ACLTTOTS                                                      
         MVC   HALF,0(R1)          MOVE YYMM TO HALF                            
         CLI   HALF+1,13           CHANGE 13TH PERIOD                           
         BNE   FM2                                                              
         MVI   HALF+1,12           TO DECEMBER                                  
*                                                                               
FM2      CLC   HALF,0(R2)                                                       
         BL    FM4                                                              
         BE    FM10                                                             
         LA    R2,2(R2)                                                         
         B     FM2                                                              
FM4      EQU   *                                                                
         CLI   0(R2),C'A'          EOL                                          
         BE    FM8                                                              
         LR    R2,R3               R2 = A(PRIOR ROW)                            
         B     FINDMX                                                           
FM8      EQU   *                                                                
         LA    R2,NMOS*ROWL+ROWL(R3)    R2 = A(SUBSQ ROW)                       
         B     FINDMX                                                           
FM10     EQU   *                                                                
         LA    R0,MLIST                                                         
         SR    R2,R0                                                            
         SRL   R2,1                                                             
         MH    R2,=Y(ROWL)                                                      
         L     R3,ACLTTOTS                                                      
         LA    R2,ROWL(R2,R3)      R2 = A(ROW)                                  
         B     FINDMX                                                           
FINDMX   EQU   *                                                                
         XIT1  REGS=(R2)                                                        
         SPACE 3                                                                
CRDERR   EQU   *                                                                
         MVC   P(80),QRECORD                                                    
         MVC   P+85(15),=C'INVALID REQUEST'                                     
         MVI   SKIPSPEC,C'Y'                                                    
         GOTO1 REPORT                                                           
         SPACE 1                                                                
         DC    H'0'                                                             
         EJECT                                                                  
*                                  BUMP NM AT 0(R2)                             
*                                  PUT NEW YM AT 2(R2)                          
         SPACE 2                                                                
MNTHUP   EQU   *                                                                
         LH    R0,0(R2)                                                         
         LA    R1,1                                                             
         CLI   1(R2),12            DEC                                          
         BNE   *+8                                                              
         LA    R1,245                                                           
         AR    R0,R1                                                            
         STH   R0,2(R2)                                                         
         LA    R2,2(R2)                                                         
         BR    RE                                                               
         SPACE 2                                                                
*                                  BACK UP YM AT 0(R2)                          
*                                  PUT NEW YM AT -2(R2)                         
         SPACE 2                                                                
MNTHBACK EQU   *                                                                
         BCTR  R2,R0                                                            
         BCTR  R2,R0                                                            
         LH    R0,2(R2)                                                         
         LA    R1,1                                                             
         CLI   3(R2),1             JAN                                          
         BNE   *+8                                                              
         LA    R1,245                                                           
         SR    R0,R1                                                            
         STH   R0,0(R2)                                                         
         BR    RE                                                               
         SPACE 2                                                                
*                                  ROLL ACCUM AT R2                             
*                                  TO ACCUM AT R3                               
ROLLUP   EQU   *                                                                
         LA    R0,ACCN*2                                                        
ROLLUP2  AP    0(7,R3),0(7,R2)                                                  
         LA    R2,7(R2)                                                         
         LA    R3,7(R3)                                                         
         BCT   R0,ROLLUP2                                                       
         BR    RE                                                               
         SPACE 3                                                                
*                                  SET PRIOR & SUBSEQUENT                       
SETSW    EQU   *                                                                
         MVI   PRIOR,0                                                          
         MVI   SUBSQ,0                                                          
         CLI   QOPT1,C' '                                                       
         BCR   8,RE                                                             
         CLI   QOPT1,C'P'                                                       
         BE    SETSW2                                                           
         MVI   SUBSQ,1                                                          
         CLI   QOPT1,C'S'                                                       
         BCR   8,RE                                                             
SETSW2   MVI   PRIOR,1                                                          
         BR    RE                                                               
         SPACE 3                                                                
*                                  ZAP  AREA AT R2                              
         SPACE 2                                                                
CLEAR    EQU   *                                                                
         LA    R3,ACCN*2           NUMBER                                       
         ZAP   0(7,R2),=P'0'                                                    
         LA    R2,7(R2)                                                         
         BCT   R3,*-10                                                          
         BR    RE                                                               
         SPACE 3                                                                
*                                                                               
*                                                                               
ACONS    DS    0F                                                               
         DC    A(CLTTOTS)                                                       
         DC    A(AGYTOTS)                                                       
         DC    A(DDSTOTS)                                                       
         DC    A(WRKTOTS)                                                       
         DC    A(OFFTOTS)                                                       
         DC    A(AGYNAMS)                                                       
         DC    A(CLTTAB)                                                        
         DC    A(SP49WFM)                                                       
         DC    A(PRNT)                                                          
         DC    A(WRKRBUFF)                                                      
ACONX    EQU   *                                                                
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*                             PRINT ROUTINE                                     
PRNT     NMOD1 0,PRNT                                                           
         SPACE 2                                                                
         LA    RC,SPACEND                                                       
         SPACE 2                                                                
*                                 R2 = A(ACCUM)                                 
         L     R2,0(R1)                                                         
         SR    RF,RF                                                            
         SR    RE,RE                                                            
         IC    RF,LINE                                                          
         IC    RE,MAXLINES                                                      
         SR    RE,RF                                                            
         CHI   RE,6                                                             
         BH    *+8                                                              
         MVI   FORCEHED,C'Y'       NEW PAGE NEEDED                              
         SPACE 2                                                                
         ST    R2,SAVR2                                                         
         BAS   RE,ROLLDOWN         ADD TO TOTAL ROW                             
*                                  AND MOVE TO WORK AREA                        
         L     R2,AWRKTOTS                                                      
         LA    R2,ROWL(R2)         POINT TO FIRST MO ROW                        
         LA    R3,MLIST                                                         
         MVI   TOTSW,0                                                          
PRNT2    EQU   *                                                                
         MVC   HLDDAT,SPACES                                                    
         MVC   HLDPCT,SPACES                                                    
PRNT6    EQU   *                                                                
         CLC   0(2,R3),=C'AM'                                                   
         BNE   PRNT7                                                            
         MVC   HLDDAT(5),=C'TOTAL'                                              
         MVI   TOTSW,1                                                          
         LA    R2,ROWL(R2)         BUMP TO TOTAL ROW                            
         B     PRNT10                                                           
PRNT7    EQU   *                                                                
         CLC   0(2,R3),=C'MG'                                                   
         BL    PRNT7A                                                           
         CLC   0(2,R3),=C'TG'                                                   
         BNE   *+8                                                              
         BAS   RE,REPRT            SKIP A LINE                                  
         CLI   QOPT5,C'S'                                                       
         BNE   PRNT21                                                           
         MVI   TOTSW,0                                                          
         B     PRNT10                                                           
*                                                                               
PRNT7A   DS    0H                                                               
         GOTO1 DATCON,DMCB,(3,0(R3)),(6,HLDDAT)                                 
         CLC   0(2,R3),BSTART                                                   
         BNE   PRNT8                                                            
*                                  CHECK PRIOR                                  
         CLI   PRIOR,0                                                          
         BE    PRNT10                                                           
         LA    R0,5                                                             
         L     R5,AWRKTOTS         POINT TO PRIOR ROW                           
         LR    RF,R2                                                            
         MVI   BYTE,0                                                           
PRNT7B   EQU   *                                                                
         CP    0(7,R5),=P'0'                                                    
         BE    PRNT7D                                                           
         AP    0(7,RF),0(7,R5)                                                  
         MVI   BYTE,1                                                           
PRNT7D   EQU   *                                                                
         LA    R5,7(R5)                                                         
         LA    RF,7(RF)                                                         
         BCT   R0,PRNT7B                                                        
         CLI   BYTE,0                                                           
         BE    *+16                                                             
         MVC   HLDDAT(6),PSTART                                                 
         MVC   HLDDAT+6(2),=C'+P'                                               
PRNT8    EQU   *                                                                
         CLC   0(2,R3),BEND                                                     
         BNE   PRNT10                                                           
*                                  CHECK SUBSQ                                  
         CLI   SUBSQ,0                                                          
         BE    PRNT10                                                           
         LA    R0,5                                                             
         LR    R5,R2                                                            
         L     R6,AWRKTOTS                                                      
         LA    R6,(NMOS+1)*ROWL(R6)     SUBSQ ROW                               
         MVI   BYTE,0                                                           
PRNT8B   EQU   *                                                                
         CP    0(7,R6),=P'0'                                                    
         BE    PRNT8D                                                           
         AP    0(7,R5),0(7,R6)                                                  
         MVI   BYTE,1                                                           
PRNT8D   EQU   *                                                                
         LA    R5,7(R5)                                                         
         LA    R6,7(R6)                                                         
         BCT   R0,PRNT8B                                                        
         CLI   BYTE,0                                                           
         BE    *+16                                                             
         MVC   HLDDAT(6),PEND                                                   
         MVC   HLDDAT+6(2),=C'+S'                                               
PRNT10   EQU   *                   DO PCT PAID                                  
*                                  CHECK ANY TO PRINT                           
         CLC   0(2,R3),=C'AM'                                                   
         BE    PRNT10D                                                          
*                                                                               
         LR    R5,R2                                                            
         LA    R0,5                                                             
PRNT10B  EQU   *                                                                
         CP    0(7,R5),=P'0'                                                    
         BNE   PRNT10D                                                          
         LA    R5,7(R5)                                                         
         BCT   R0,PRNT10B                                                       
         B     PRNT21              NONE                                         
PRNT10D  EQU   *                                                                
*                                  PCT PAID                                     
         ZAP   DUBDUB,21(7,R2)    PAID                                          
         BZ    PRNT12              NOTHING PAID                                 
         MP    DUBDUB,=P'10000'                                                 
         ZAP   WORK(7),7(7,R2)     BILLING MO GROSS                             
         CP    WORK(7),=P'0'                                                    
         BZ    PRNT12                                                           
         DP    DUBDUB,WORK(7)                                                   
         AP    DUBDUB(9),=P'5'                                                  
         MVO   DUBDUB(9),DUBDUB(8)                                              
         CP    DUBDUB(9),=P'0'                                                  
         BE    PRNT12                                                           
         LA    R1,DUBDUB+6                                                      
         EDIT  (P3,0(R1)),(5,HLDPCT),1                                          
         SPACE 1                                                                
PRNT12   EQU   *                                                                
         LA    R4,P                                                             
PM       EQU   *                                                                
         USING MLINED,R4                                                        
         SPACE 1                                                                
         MVC   MDATE(8),HLDDAT     MOVE DATE + P OR S                           
         MVC   MPCT,HLDPCT                                                      
         SPACE 1                                                                
         LA    R5,0(R2)                                                         
         LA    R6,MAUTH            AUTHORIZED                                   
         BAS   RE,EDIT                                                          
         SPACE                                                                  
         LA    R5,7(R2)                                                         
         LA    R6,MBLORD           BILL MO GROSS                                
         CP    7(7,R2),=P'99999999999' COMPARE ORD TO 100M                      
         BNH   *+8                                                              
         LA    R6,132(R6)          AND POINT TO NEXT LINE                       
         BAS   RE,EDIT                                                          
         SPACE 1                                                                
PM4      EQU   *                                                                
         SPACE 1                                                                
         LA    R5,21(R2)                                                        
         LA    R6,MPD              PAID                                         
         BAS   RE,EDIT                                                          
         SPACE 1                                                                
         CLC   0(2,R3),=C'MG'                                                   
         BNL   PM6                                                              
         MVC   WORK(7),7(R2)                                                    
         SP    WORK(7),21(7,R2)                                                 
         LA    R5,WORK                                                          
         LA    R6,MUNPD            UNPAID                                       
         CP    7(7,R2),=P'99999999999' COMPARE ORD TO 100M                      
         BNH   *+8                                                              
         LA    R6,132(R6)          AND POINT TO NEXT LINE                       
         BAS   RE,EDIT                                                          
         SPACE 1                                                                
PM6      DS    0H                                                               
         LA    R5,28(R2)                                                        
         LA    R6,MBLD             BILLED                                       
         BAS   RE,EDIT                                                          
         SPACE 1                                                                
         CLC   0(2,R3),=C'MG'                                                   
         BNL   PM8                                                              
         MVC   WORK(7),7(R2)                                                    
         SP    WORK(7),28(7,R2)                                                 
         LA    R5,WORK                                                          
         LA    R6,MUNBLD           UNBILLED                                     
         CP    7(7,R2),=P'99999999999' COMPARE ORD TO 100M                      
         BNH   *+8                                                              
         LA    R6,132(R6)          AND POINT TO NEXT LINE                       
         BAS   RE,EDIT                                                          
         SPACE 1                                                                
PM8      DS    0H                                                               
         B     PRNT20                                                           
         SPACE 3                                                                
PRNT20   EQU   *                                                                
         CLC   0(2,R3),=C'MG'      SPECIAL TOTAL                                
         BL    PRNT20B                                                          
         MVC   P(13),=C'TODAY        '                                          
         CLC   0(2,R3),=C'TG'                                                   
         BNE   PRNT20A                                                          
         CLI   QOPT6,C'C'                                                       
         BNE   *+10                                                             
         MVC   P(5),=C' YEAR'                                                   
         MVC   MPCT,SPACES                                                      
         B     PRNT20B                                                          
PRNT20A  MVC   P(13),=C'TODAY (NET)  '                                          
         CLC   0(2,R3),=C'TN'                                                   
         BNE   PRNT20A2                                                         
         CLI   QOPT6,C'C'                                                       
         BNE   *+10                                                             
         MVC   P(5),=C' YEAR'                                                   
         B     PRNT20B                                                          
*                                                                               
PRNT20A2 DS    0H                                                               
         MVC   P(13),=C'MONTH        '                                          
         CLC   0(2,R3),=C'MG'                                                   
         BE    PRNT20B                                                          
         MVC   P(13),=C'MONTH (NET)  '                                          
*                                                                               
PRNT20B  DS    0H                                                               
         BAS   RE,REPRT                                                         
PRNT21   EQU   *                                                                
         CLC   0(2,R3),=C'MN'      END                                          
         BE    PRNT24                                                           
PRNT22   EQU   *                                                                
         LA    R3,2(R3)            BUMP MONTH                                   
         LA    R2,ROWL(R2)                                                      
         CLI   0(R3),0                                                          
         BE    PRNT22                                                           
         B     PRNT2                                                            
PRNT24   DS    0H                                                               
         BAS   RE,REPRT            SKIP A LINE                                  
PRNT26   EQU   *                                                                
PRNTX    EQU   *                                                                
         XIT1                                                                   
         SPACE 3                                                                
*                                  ADD TO TOTAL ROW                             
*                                  AND MOVE TO WORK AREA                        
ROLLDOWN NTR1                                                                   
         SPACE 2                                                                
         OC    QAREA+52(4),=4C'0'                                               
         PACK  DUB,QAREA+52(2)     MONTHS BACK                                  
         CVB   R5,DUB                                                           
         LA    R0,36                                                            
         CLI   QSTART,C' '                                                      
         BNH   *+8                                                              
         LA    R0,1                ONE IF REQ'S DATES                           
         SR    R5,R0                                                            
         BNP   *+6                                                              
         SR    R5,R5                                                            
         LCR   R5,R5                                                            
         SLL   R5,1                                                             
         LA    R3,MLIST(R5)                                                     
         GOTO1 DATCON,DMCB,(3,0(R3)),(6,PSTART)                                 
*                                                                               
         MVC   BSTART,0(R3)                                                     
         SRL   R5,1                                                             
         MH    R5,=Y(ROWL)                                                      
         LA    R5,ROWL(R2,R5)                                                   
         ST    R5,WORK             A(FIRST MONTH THAT COUNTS)                   
         PACK  DUB,QAREA+54(2)     MONTHS UP                                    
         CVB   R5,DUB                                                           
         LA    R0,23                                                            
         CLI   QSTART,C' '                                                      
         BNH   *+8                                                              
         LA    R0,58               58 IF REQ'D DATES                            
         CR    R5,R0                                                            
         BNH   *+6                                                              
         LR    R5,R0                                                            
         SLL   R5,1                                                             
         LA    R3,MLIST+36*2(R5)   36 MONTHS INTO LIST                          
         CLI   QSTART,C' '                                                      
         BNH   *+8                                                              
         LA    R3,MLIST+1*2(R5)    OR 1 MONTH IF REQ'D DATES                    
         GOTO1 DATCON,DMCB,(3,0(R3)),(6,PEND)                                   
*                                                                               
         MVC   BEND,0(R3)                                                       
         SRL   R5,1                                                             
         MH    R5,=Y(ROWL)                                                      
         LA    R3,36*ROWL+ROWL(R2,R5)   36 MONTHS                               
         CLI   QSTART,C' '                                                      
         BNH   *+8                                                              
         LA    R3,1*ROWL+ROWL(R2,R5)    OR ONE MOTNH IF REQ'D DATES             
         ST    R3,WORK+4                A(LAST MONTH THAT COUNTS)               
*                                                                               
*                                                                               
RD3      DS    0H                                                               
*                                  CLEAR WRKTOTS                                
         LA    R3,ACCN                                                          
         L     R2,AWRKTOTS                                                      
         MVC   0(7,R2),=PL7'0'                                                  
         LA    R2,7(R2)                                                         
         BCT   R3,*-10                                                          
*                                                                               
         LA    R5,NMOS+2                                                        
         L     R3,AWRKTOTS                                                      
         L     R2,SAVR2                                                         
RD4      DS    0H                                                               
         C     R2,WORK                                                          
         BL    RD8                                                              
         C     R2,WORK+4                                                        
         BH    RD9                                                              
         ZAP   00(7,R3),00(7,R2)                                                
         ZAP   07(7,R3),07(7,R2)                                                
         ZAP   14(7,R3),14(7,R2)                                                
         ZAP   21(7,R3),21(7,R2)                                                
         ZAP   28(7,R3),28(7,R2)                                                
*                                                                               
RD5      DS    0H                                                               
         L     RF,AWRKTOTS                                                      
         LA    RF,(NMOS+2)*ROWL(RF)     TOTAL ROW                               
         AP    00(7,RF),00(7,R2)                                                
         AP    07(7,RF),07(7,R2)                                                
         AP    14(7,RF),14(7,R2)                                                
         AP    21(7,RF),21(7,R2)                                                
         AP    28(7,RF),28(7,R2)                                                
*                                                                               
RD6      DS    0H                                                               
         LA    R2,ROWL(R2)                                                      
         LA    R3,ROWL(R3)                                                      
         BCT   R5,RD4                                                           
         B     RD10                                                             
*                                                                               
RD8      DS    0H                                                               
         CLI   PRIOR,0                                                          
         BE    RD6                                                              
         L     RF,AWRKTOTS         PRIOR                                        
         AP    00(7,RF),00(7,R2)                                                
         AP    07(7,RF),07(7,R2)                                                
         AP    14(7,RF),14(7,R2)                                                
         AP    21(7,RF),21(7,R2)                                                
         AP    28(7,RF),28(7,R2)                                                
         B     RD5                                                              
*                                                                               
RD9      DS    0H                                                               
         CLI   SUBSQ,0                                                          
         BE    RD10                                                             
         L     RF,AWRKTOTS                                                      
         LA    RF,(NMOS+1)*ROWL(RF)     SUBSQ ROW                               
         AP    00(7,RF),00(7,R2)                                                
         AP    07(7,RF),07(7,R2)                                                
         AP    14(7,RF),14(7,R2)                                                
         AP    21(7,RF),21(7,R2)                                                
         AP    28(7,RF),28(7,R2)                                                
         B     RD5                                                              
*                                                                               
RD10     DS    0H                                                               
*                                  SPECIAL TOTALS                               
         L     R3,AWRKTOTS                                                      
         LA    R3,TGROW(R3)        FIRST SPECIAL ROW                            
         L     R2,SAVR2                                                         
         LA    R2,TGROW(R2)                                                     
         LA    R4,5*ROWL/7         5 ROWS                                       
RD11     DS    0H                                                               
         AP    0(7,R3),0(7,R2)                                                  
         LA    R2,7(R2)                                                         
         LA    R3,7(R3)                                                         
         BCT   R4,RD11                                                          
*                                                                               
         XIT1                                                                   
         SPACE 3                                                                
EDIT     EQU   *                                                                
         CP    0(7,R5),=P'0'                                                    
         BCR   8,RE                                                             
*                                  EDIT MACRO NOT USED BECAUSE OF               
*                                  SPECIAL COMMAS                               
EDITEDIT DS    0H                                                               
         ZAP   DUB,0(7,R5)                                                      
         MVC   WORK(17),=X'4020202020206B2020206B2020214B2020'                  
         ED    WORK(17),DUB+1                                                   
*                                                                               
         MVI   WORK+17,C' '        START WITH A SPACE                           
         CLI   TOTSW,1                                                          
         BNE   *+8                                                              
         MVI   WORK+17,C'*'        SET TOTAL FLAG                               
*                                                                               
         CP    DUB,=P'0'                                                        
         BNL   *+8                                                              
         MVI   WORK+17,C'-'                                                     
*                                                                               
         MVC   0(17,R6),WORK+1                                                  
         BR    RE                                                               
         SPACE 3                                                                
REPRT    NTR1                                                                   
         SPACE 2                                                                
         CLI   QOPT6,C'C'                                                       
         BE    REP1                                                             
         SPACE                                                                  
         MVC   HEAD8+31(10),=C'AUTHORIZED'                                      
         MVC   HEAD9+31(10),=C'----------'                                      
         B     REP1A                                                            
         SPACE                                                                  
REP1     MVC   HEAD8+32(6),=C'BILLED'                                           
         MVC   HEAD9+28(13),=C'-------------'                                   
         MVC   HEAD7+28(13),=C'CURRENT MONTH'                                   
REP1A    CLC   QUESTOR,SPACES                                                   
         BE    REP2                                                             
         MVC   HEAD2(09),=C'REQUESTOR'                                          
         MVC   HEAD2+13(12),QUESTOR                                             
REP2     DS    0H                                                               
         MVC   HEAD3+64(6),PSTART                                               
         MVC   HEAD3+74(6),PEND                                                 
         LA    RF,HEAD3                                                         
         CLI   QCLT,C'$'           SEE IF OFFICE LIST REQUEST                   
         BNE   REP3                                                             
         CLI   QCLT+1,C'*'                                                      
         BE    REP3                                                             
         MVC   HEAD3(11),=C'OFFICE LIST'                                        
         MVC   HEAD3+12(2),QCLT+1                                               
         LA    RF,HEAD4                                                         
         B     REP3                                                             
*                                                                               
REP3     OC    LASTBRK,LASTBRK                                                  
         BZ    REP6                                                             
         CLI   QCLGID,C' '          SEE IF CLIENT GROUP REQUEST                 
         BNH   REP5                                                             
         MVC   HEAD3(12),=C'CLIENT GROUP'                                       
         MVC   HEAD3+13(5),LASTBRK                                              
         B     REP6                                                             
*                                                                               
REP5     MVC   0(6,RF),=C'OFFICE'                                               
         MVC   9(2,RF),LASTBRK                                                  
         CLC   LASTBRK(2),=X'FEFF'                                              
         BNE   *+10                                                             
         MVC   9(10,RF),=C'UNASSIGNED'                                          
*                                                                               
REP6     DS    0H                                                               
         CLI   QOPT1,C'C'                                                       
         BNE   REP7                                                             
         MVC   HEAD6(20),=C'**CLOSED-OUT ITEMS**'                               
*                                                                               
REP7     DS    0H                                                               
         GOTO1 REPORT                                                           
*                                                                               
         B     PRNTX                                                            
GLCDWDS  DC    CL57'-----------------GROSS LESS CASH DISCOUNT----------X        
               -----'                                                           
*                                                                               
SUMMSG   DC    C'**ERROR - DETAIL BILLS PLUS REVERSALS NOT EQUAL TO SUMX        
                OF DETAIL BILLED ITEMS ON FILE **'                              
*                                                                               
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                  WORK FILE MODULE                             
         SPACE 2                                                                
SP49WFM  NMOD1 0,WFMOD                                                          
         SPACE 2                                                                
         LA    RC,SPACEND                                                       
         USING SP49WRKD,RC                                                      
         SPACE 2                                                                
         LM    R2,R3,0(R1)                                                      
         CLC   =C'CLOSE',0(R2)                                                  
         BE    WF30                                                             
         CLI   CROSSFIL,C'Y'       CROSS FILE?                                  
         BE    WF8                                                              
*                                                                               
WF4      DS    0H                                                               
         OC    WFID,WFID           TEST FIRST TIME                              
         BNZ   WF4A                                                             
*                                  SET INDEX                                    
         LA    R6,WFID                                                          
         USING UKRECD,R6                                                        
*                                                                               
         MVC   UKUSRID,RCORIGID                                                 
         MVC   UKSYSPRG,=C'S49'                                                 
         MVC   UKSUBPRG,FILENUM2   SPOT FILE SYSTEM ID                          
         CLI   FILENUM2+1,C' '     SPACE?                                       
         BNH   *+10                                                             
         MVC   UKEXTRA,FILENUM2+1  2ND CHAR FILE SYSTEM ID                      
         MVI   UKDAY,1                                                          
         MVI   UKCLASS,C'R'                                                     
         DROP  R6                                                               
*                                                                               
         GOTO1 WORKER,DMCB,=C'INDEX',SP49WFN,WFID                               
*                                                                               
WF4A     DS    0H                                                               
         LA    RE,WFREC                                                         
         LH    RF,=Y(WFRECL)                                                    
         XCEF                                                                   
*                                                                               
         XC    WFREC-4(4),WFREC-4                                               
         MVC   WFREC-4(2),=Y(WFRECL+4)                                          
         L     R5,ADAGY                                                         
         USING AGYKEY,R5                                                        
         MVC   WFAGY(2),AGYKAGY                                                 
         MVC   WFMED,MED                                                        
         MVC   WFAGYNAM,AGYNAME                                                 
         MVC   WFAGYADR,AGYADDR                                                 
         MVC   WFMEDNAM,MEDNM                                                   
         MVC   WFDATE,BTODAY       SET TODAY                                    
         MVI   WFTYP,C'A'                                                       
*                                                                               
         GOTO1 WORKER,DMCB,=C'ADD',SP49WFN,WFID,WFREC-4                         
*                                                                               
         LA    R4,8                SPLIT INTO 8 RECS                            
         LR    R5,R3                                                            
         LA    R6,1                                                             
*                                                                               
WF4B     DS    0H                                                               
         STC   R6,WFTYP                                                         
         LA    R1,L'WFTOTS                                                      
         LR    RE,R5                                                            
         MOVE  (WFTOTS,(R1)),(RE)                                               
         GOTO1 WORKER,DMCB,=C'ADD',SP49WFN,WFID,WFREC-4                         
*                                                                               
         LA    R5,L'WFTOTS(R5)                                                  
         LA    R6,1(R6)                                                         
         BCT   R4,WF4B                                                          
         B     WFEXIT                                                           
         EJECT                                                                  
*                                                                               
* ONLY GETS HERE IF CROSS FILE                                                  
*                                                                               
WF8      DS    0H                                                               
         GOTO1 WORKER,DMCB,=C'INDEX',SP49WFN,WFID                               
         TM    DMCB+8,X'80'                                                     
         BZ    WF9                                                              
         MVI   HLDAGYN,X'FF'       END OF MEDIA                                 
         B     WFEXIT                                                           
WF9      DS    0H                                                               
         CLC   WFID+UKSYSPRG-UKRECD(L'UKSYSPRG),=C'S49'                         
         BNE   WF8                                                              
         CLC   WFID+UKSUBPRG-UKRECD(1),FILENUM2                                 
         BNE   WF8                                                              
         CLI   WFID+UKEXTRA-UKRECD,0                                            
         BNE   WF9A                                                             
         CLI   FILENUM2+1,C' '                                                  
         BE    WF9B                                                             
WF9A     CLC   WFID+UKEXTRA-UKRECD(1),FILENUM2+1                                
         BNE   WF8                                                              
WF9B     DS    0H                                                               
         GOTO1 WORKER,DMCB,=C'READ',,,WFREC-4                                   
*                                                                               
         TM    DMCB+8,X'80'                                                     
         BNZ   WF8                 NEXT AGY                                     
         CLC   WFREC-4(2),=H'110'                                               
         BNH   WF9                 BYPASS Q RECS                                
         CLC   WFMED,CURMED                                                     
         BNE   WF9                                                              
         CLC   WFDATE,BTODAY       TEST TODAY'S FILE                            
         BNE   WF9                                                              
*                                                                               
         CLI   WFTYP,C'A'                                                       
         BNE   WF10                                                             
         MVC   HLDAGYN,WFAGYNAM                                                 
         MVC   HLDMEDN,WFMEDNAM                                                 
         MVC   HLDAGYC,WFAGY                                                    
*                                                                               
         B     WF9B                                                             
*                                                                               
WF10     DS    0H                                                               
*                                  WFTYP IS INDEX (1 - 8)                       
         ZIC   RF,WFTYP                                                         
         BCTR  RF,R0                                                            
         MH    RF,=Y(L'WFTOTS)                                                  
         A     RF,ACLTTOTS         MOVE TO ADDR                                 
         LA    R1,L'WFTOTS                                                      
         MOVE  ((RF),(R1)),WFTOTS                                               
*                                                                               
         CLI   WFTYP,8                                                          
         BNE   WF9B                                                             
         B     WFEXIT                                                           
         SPACE 3                                                                
WF30     DS    0H                                                               
         GOTO1 WORKER,DMCB,=C'CLOSE',SP49WFN                                    
*                                                                               
WFEXIT   DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
         SPACE 2                                                                
EOF      DC    H'0'                                                             
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
         DS    F                                                                
*                                                                               
WFREC    DS    0C                                                               
WFAGY    DS    CL2                                                              
WFMED    DS    CL1                                                              
WFDATE   DS    XL3                                                              
WFTYP    DS    XL1                                                              
WFTOTS   DS    0XL595              ACCL*2/4 = 1/8 OF ACCUMS PER REC             
WFAGYNAM DS    CL33                                                             
WFAGYADR DS    CL33                                                             
WFAGYABR DS    CL7                                                              
WFMEDNAM DS    CL11                                                             
         DS    XL511                                                            
WFRECL   EQU   602                                                              
         EJECT                                                                  
*                                                                               
*=======================================================*                       
* SUBROUTINE TO FIND NEXT CLIENT GROUP (SPOT)           *                       
*=======================================================*                       
         SPACE 1                                                                
         DS    0H                                                               
NEXTCGR  NMOD1 0,**NCGR**                                                       
         CLI   QMED,C'N'           TEST NETWORK                                 
         BNE   NCGR2                                                            
         L     RF,ADAGY                                                         
         LA    RF,AGYPROF-AGYHDR(RF)                                            
         CLI   7(RF),C'C'          TEST CANADIAN                                
         BNE   NCGN                NO - NETPAK                                  
*                                                                               
NCGR2    OC    SVCGRKEY,SVCGRKEY   TEST FIRST TIME                              
         BNZ   NCGR40              NO                                           
* READ AND PROCESS GROUP DEFINITION RECORD                                      
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING GRPRECD,R6                                                       
*                                                                               
         MVI   GRPKTYP,GRPKTYPQ                                                 
         MVI   GRPKSTYP,GRPKCTYQ                                                
         MVC   GRPKAGMD,SVAGYMD                                                 
         MVC   GRPKID,QCLGID                                                    
         GOTO1 HIGH                                                             
         DROP  R6                                                               
*                                                                               
         CLC   KEY(4),KEYSAVE      SAME TYPE/A-M/ID                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,ADCLTGRP                                                      
         ST    R6,AREC                                                          
         GOTO1 GET                                                              
* EXTRACT LENGTHS/TITLES                                                        
         LA    R6,24(R6)                                                        
         MVI   BYTE,X'10'                                                       
         BAS   RE,NCGNXT2                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING GRPBRKD,R6                                                       
NCGR4    XC    CGR1(42),CGR1                                                    
         XC    CGR2(42),CGR2                                                    
         MVC   CGR1BK,GRPBK1                                                    
         MVC   CGR2BK,GRPBK2                                                    
         ZIC   RE,GRPBK1LN                                                      
         STC   RE,CGR1LEN                                                       
         ZIC   R0,GRPBK2LN                                                      
         AR    RE,R0                                                            
         STC   RE,CGR2LEN                                                       
         DROP  R6                                                               
         EJECT                                                                  
         LA    R6,KEY                                                           
         USING GRPRECD,R6                                                       
*                                                                               
         CLC   =C'ALL',QCLGRP                                                   
         BE    NCGR8                                                            
         MVC   FULL,QCLGRP                                                      
         LA    R1,FULL                                                          
         LA    R0,4                                                             
*                                                                               
NCGR6    CLI   0(R1),C' '                                                       
         BE    *+12                                                             
         CLI   0(R1),C'*'                                                       
         BNE   *+8                                                              
         MVI   0(R1),0                                                          
         LA    R1,1(R1)                                                         
         BCT   R0,NCGR6                                                         
*                                                                               
         PACK  DUB,FULL(5)         PACK 1 EXTRA CHARACTER                       
         MVC   GRPKCODE,DUB+5                                                   
*                                                                               
NCGR8    OC    GRPKCODE,GRPKCODE                                                
         BNZ   *+12                                                             
         LA    R0,1                                                             
         STCM  R0,3,GRPKCODE                                                    
*                                                                               
NCGR10   DS    0H                                                               
         GOTO1 HIGH                                                             
*                                                                               
NCGR20   CLC   KEY(4),KEYSAVE      TEST SAME SCHEME                             
         BNE   NCGRNEQ                                                          
         EJECT                                                                  
NCGR22   CLC   =C'ALL',QCLGRP                                                   
         BE    NCGR30                                                           
* CALCULATE NUMBER OF DIGITS FOR COMPARE                                        
         LA    RE,QCLGRP+3                                                      
         LA    RF,3                                                             
NCGR24   CLI   0(RE),C' '                                                       
         BE    *+12                                                             
         CLI   0(RE),C'*'                                                       
         BNE   NCGR26                                                           
         BCTR  RE,0                                                             
         BCT   RF,NCGR24                                                        
* RF IS SET FOR EX ON GROUPS                                                    
NCGR26   UNPK  DUB,GRPKCODE(3)     DUB HAS X'000000F1F2F3F4XX'                  
         EX    RF,*+8                         0 1 2 3 4 5 6 7                   
         B     *+10                                                             
         CLC   QCLGRP(0),DUB+3 *EXECUTED*                                       
         BNE   NCGRNEQ                                                          
         DROP  R6                                                               
         EJECT                                                                  
* PROCESS THIS GROUP *                                                          
         SPACE 1                                                                
NCGR30   XC    SVCGRKEY,SVCGRKEY                                                
         MVC   SVCGRKEY(6),KEY     SAVE TYPE/A-M/ID/CODE                        
         L     R6,ADCLTGRP                                                      
         ST    R6,AREC                                                          
         GOTO1 GET                                                              
*                                                                               
         LA    R6,24(R6)                                                        
         MVI   BYTE,X'20'                                                       
         BAS   RE,NCGNXT2                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
* MOVE GROUP CODES AND NAMES FOR USER                                           
         USING GRPGRPD,R6                                                       
         MVC   CGR1(1),QCLGID                                                   
         MVC   CGR1NM,GRPGNAM1                                                  
         MVC   CGR2(1),QCLGID                                                   
         MVC   CGR2NM,GRPGNAM2                                                  
*                                                                               
         L     RE,ADCLTGRP         POINT TO START OF RECORD                     
         UNPK  DUB,GRPKCODE-GRPKEY(3,RE)                                        
         ZIC   RE,CGR1LEN          GET BREAK 1 LEN                              
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   CGR1+1(0),DUB+3                                                  
         ZIC   RE,CGR2LEN          GET BREAK 2 LEN                              
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   CGR2+1(0),DUB+3                                                  
         B     NCGR42                                                           
         DROP  R6                                                               
         EJECT                                                                  
*===========================================================*                   
* NOT FIRST TIME - ADVANCE TO NEXT CLIENT OR NEXT GROUP     *                   
* SVCGRKEY HAS 0D 04 AM ID CD CD CL CL CL                   *                   
*               0  1  2  3  4  5  6  7  8                   *                   
*===========================================================*                   
         SPACE 1                                                                
NCGR40   L     R6,ADCLTGRP         ADVANCE TO PREVIOUS ELEMENT                  
         LA    R6,24(R6)                                                        
         MVI   BYTE,X'30'                                                       
         BAS   RE,NCGNXT2                                                       
         B     NCGR44                                                           
*                                                                               
NCGR42   MVI   BYTE,X'30'                                                       
         BAS   RE,NCGNXTEL                                                      
NCGR44   BNE   NCGR50                                                           
*                                                                               
         CLC   SVCGRKEY+6(3),2(R6)  SAVED CODE TO ELEMENT                       
         BNL   NCGR42               CONTINUE IF SAVED LOW OR EQ                 
         MVC   SVCGRKEY+6(3),2(R6)  SAVE CURRENT ELEMENT VALUE                  
         GOTO1 CLPACK,DMCB,2(R6),SVCLT                                          
*                                                                               
         CR    RB,RB               SET CC EQ                                    
         B     NCGRX                                                            
*                                                                               
NCGRNEQ  LTR   RB,RB                                                            
*                                                                               
NCGRX    XIT1                                                                   
*                                                                               
NCGR50   LA    R6,KEY              POINT FOR LATER DSECT USAGE                  
         XC    KEY,KEY             READ FOR NEXT GROUP RECORD                   
         L     RE,ADCLTGRP                                                      
         MVC   KEY(13),0(RE)                                                    
         GOTO1 HIGH                                                             
         GOTO1 SEQ                                                              
         B     NCGR20                                                           
         EJECT                                                                  
*=======================================================*                       
* SUBROUTINE TO FIND NEXT CLIENT GROUP (NET)            *                       
*=======================================================*                       
         SPACE 1                                                                
NCGN     DS    0H                                                               
         OC    SVCGRKEY,SVCGRKEY   TEST FIRST TIME                              
         BNZ   NCGN10              NO                                           
* READ AND PROCESS GROUP DEFINITION RECORD                                      
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING CLGRECD,R6                                                       
*                                                                               
         MVC   CLGKTYP,=X'0D06'                                                 
         MVC   CLGKAGMD,SVAGYMD                                                 
         MVC   CLGKID,QCLGID                                                    
         GOTO1 HIGH                                                             
         DROP  R6                                                               
*                                                                               
         CLC   KEY(4),KEYSAVE      SAME TYPE/A-M/ID                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,ADCLTGRP                                                      
         ST    R6,AREC                                                          
         GOTO1 GET                                                              
* EXTRACT LENGTHS/TITLES                                                        
         LA    R6,24(R6)                                                        
         MVI   BYTE,X'10'                                                       
         BAS   RE,NCGNXT2                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING GRPBRKD,R6                                                       
NCGN4    XC    CGR1(42),CGR1                                                    
         XC    CGR2(42),CGR2                                                    
         MVC   CGR1BK,GRPBK1                                                    
         MVC   CGR2BK,GRPBK2                                                    
         ZIC   RE,GRPBK1LN                                                      
         STC   RE,CGR1LEN                                                       
         ZIC   R0,GRPBK2LN                                                      
         AR    RE,R0                                                            
         STC   RE,CGR2LEN                                                       
         DROP  R6                                                               
         EJECT                                                                  
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING CLGRECD,R6                                                       
         MVC   CLGCTYP,=X'0D86'                                                 
         MVC   CLGCAGMD,SVAGYMD                                                 
         MVC   CLGCID,QCLGID                                                    
*                                                                               
         CLC   =C'ALL',QCLGRP                                                   
         BE    NCGN8                                                            
         MVC   FULL,QCLGRP                                                      
         LA    R1,FULL                                                          
         LA    R0,4                                                             
*                                                                               
NCGN6    CLI   0(R1),C' '                                                       
         BE    *+12                                                             
         CLI   0(R1),C'*'                                                       
         BNE   *+8                                                              
         MVI   0(R1),0                                                          
         LA    R1,1(R1)                                                         
         BCT   R0,NCGN6                                                         
*                                                                               
         PACK  DUB,FULL(5)         PACK 1 EXTRA CHARACTER                       
         MVC   CLGCGRP,DUB+5                                                    
*                                                                               
NCGN8    OC    CLGCGRP,CLGCGRP                                                  
         BNZ   *+12                                                             
         LA    R0,1                                                             
         STCM  R0,3,CLGCGRP                                                     
         B     NCGN12                                                           
*                                                                               
NCGN10   XC    KEY,KEY             READ NEXT CLIENT IN GROUP                    
         LA    R6,KEY                                                           
         USING CLGRECD,R6                                                       
         MVC   KEY,SVCGRKEY                                                     
         MVI   KEY+12,X'FF'        FORCE SEQ                                    
*                                                                               
NCGN12   DS    0H                                                               
         GOTO1 HIGH                                                             
         MVC   SVCGRKEY,KEY        SAVE NEW KEY                                 
         MVC   SVCLT,CLGCCLT       MOVE CLIENT CODE                             
*                                                                               
         CLC   KEY(8),KEYSAVE      SAME TY/A-M/ID/GR                            
         BE    NCGNEQX                                                          
         EJECT                                                                  
NCGN20   CLC   KEY(6),KEYSAVE      TEST SAME SCHEME                             
         BNE   NCGNNEQ             NO - FINISHED                                
         EJECT                                                                  
NCGN22   CLC   =C'ALL',QCLGRP                                                   
         BE    NCGN30                                                           
* CALCULATE NUMBER OF DIGITS FOR COMPARE                                        
         LA    RE,QCLGRP+3                                                      
         LA    RF,3                                                             
NCGN24   CLI   0(RE),C' '                                                       
         BE    *+12                                                             
         CLI   0(RE),C'*'                                                       
         BNE   NCGN26                                                           
         BCTR  RE,0                                                             
         BCT   RF,NCGN24                                                        
* RF IS SET FOR EX ON GROUPS                                                    
NCGN26   UNPK  DUB,CLGCGRP(3)      DUB HAS X'000000F1F2F3F4XX'                  
         EX    RF,*+8                         0 1 2 3 4 5 6 7                   
         B     *+10                                                             
         CLC   QCLGRP(0),DUB+3 *EXECUTED*                                       
         BNE   NCGNNEQ                                                          
         DROP  R6                                                               
         EJECT                                                                  
* PROCESS THIS GROUP *                                                          
         SPACE 1                                                                
NCGN30   XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING CLGRECD,R6                                                       
         MVC   CLGKTYP,=X'0D06'    SET TO READ GROUP RECORD                     
         MVC   CLGKAGMD,SVAGYMD                                                 
         MVC   CLGKID(3),SVCGRKEY+(CLGCID-CLGKEY)                               
         GOTO1 HIGH                                                             
         CLC   KEY(6),KEYSAVE                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,ADCLTGRP                                                      
         ST    R6,AREC                                                          
         GOTO1 GET                                                              
*                                                                               
         LA    R6,24(R6)                                                        
         MVI   BYTE,X'20'                                                       
         BAS   RE,NCGNXT2                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
* MOVE GROUP CODES AND NAMES FOR USER                                           
         USING GRPGRPD,R6                                                       
         MVC   CGR1(1),QCLGID                                                   
         MVC   CGR1NM,GRPGNAM1                                                  
         MVC   CGR2(1),QCLGID                                                   
         MVC   CGR2NM,GRPGNAM2                                                  
*                                                                               
         L     RE,ADCLTGRP         POINT TO START OF RECORD                     
         UNPK  DUB,GRPKCODE-GRPKEY(3,RE)                                        
         ZIC   RE,CGR1LEN          GET BREAK 1 LEN                              
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   CGR1+1(0),DUB+3                                                  
         ZIC   RE,CGR2LEN          GET BREAK 2 LEN                              
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   CGR2+1(0),DUB+3                                                  
         DROP  R6                                                               
*                                                                               
NCGNEQX  CR    RB,RB               SET CC EQ                                    
         B     NCGNX                                                            
*                                                                               
NCGNNEQ  LTR   RB,RB                                                            
*                                                                               
NCGNX    XIT1                                                                   
         SPACE 2                                                                
NCGNXTEL CLI   0(R6),0                                                          
         BE    NCGNXTX                                                          
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         LTR   R0,R0                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
NCGNXT2  CLC   BYTE(1),0(R6)                                                    
         BER   RE                                                               
         B     NCGNXTEL                                                         
NCGNXTX  LTR   RE,RE                                                            
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                  WORK AREA DSECT                              
SP49WRKD DSECT                                                                  
*                                                                               
SAVR2    DS    F                                                                
MLIST    DS    CL120               60 MONTH LIST                                
MLISTX   DS    CL10                                                             
BSTART   DS    XL3                                                              
BEND     DS    XL3                                                              
PSTART   DS    CL6                                                              
PEND     DS    CL6                                                              
MBKOPT   DS    H                                                                
MUPOPT   DS    H                                                                
PRIOR    DS    X                                                                
SUBSQ    DS    X                                                                
HAVPRI   DS    X                                                                
HAVSUB   DS    X                                                                
HLDDAT   DS    CL8                                                              
HLDPCT   DS    CL5                                                              
TOTSW    DS    X                                                                
FRSTIM   DS    X                                                                
DAY      DS    CL1                 BINARY DAY OF WEEK                           
LASTBRK  DS    XL5                                                              
BTODAY   DS    XL3                                                              
ELCODE   DS    XL1                                                              
SAVCOFF  DS    CL2                                                              
C2ACTIVE DS    CL1                                                              
CURMED   DS    C                                                                
CLTACT   DS    X                                                                
DAYADJ   DS    H                                                                
CLTCNT   DS    F                  USED TO COUNT CLIENTS                         
         DS    0D                                                               
SV00APRF DS    CL16                                                             
DUBDUB   DS    PL16                                                             
*                                  1 = NO CD                                    
OLDKEY   DS    CL32                                                             
WFID     DS    CL18                                                             
ACLIENT  DS    A                                                                
AOFFICER DS    A                   SAVED CORE-RES ADDRESS OF OFFICER            
MYOFCLST DS    CL128                                                            
         DS    F                                                                
REQCRDH  DS    CL26                                                             
REQCRD   DS    CL80                                                             
HLDAGYC  DS    CL2                                                              
HLDAGYN  DS    CL33                                                             
HLDMEDN  DS    CL10                                                             
ACLTTOTS DS    A                                                                
AAGYTOTS DS    A                                                                
ADDSTOTS DS    A                                                                
AWRKTOTS DS    A                                                                
AOFFTOTS DS    A                                                                
AAGYNAMS DS    A                                                                
ACLTTAB  DS    A                                                                
AWFMOD   DS    A                                                                
APRNT    DS    A                                                                
SP49WFN  DS    A                                                                
DDSSW    DS    CL1                                                              
DTWRK1   DS    CL6                                                              
DTWRK2   DS    CL3                                                              
SVMEDNM  DS    CL10                                                             
SVMEDCD  DS    CL1                                                              
*                                                                               
WGRS     DS    D                                                                
WNET     DS    D                                                                
*                                                                               
ORDCUR   DS    PL7            ORDERED TODAY                                     
PDCUR    DS    PL7            PAID TODAY                                        
*                                                                               
       ++INCLUDE SPBVALD                                                        
*                                                                               
         EJECT                                                                  
*                             MAGAZINE LINE DSECT                               
         SPACE 1                                                                
MLINED   DSECT                                                                  
MLINE    DS    0CL132                                                           
         DS    CL10                                                             
MDATE    DS    CL6                                                              
MPS      DS    CL2                                                              
         DS    CL1                                                              
MPCT     DS    CL5                                                              
         DS    CL1                                                              
MAUTH    DS    CL15                                                             
         DS    CL2                                                              
MBLORD   DS    CL15                                                             
         DS    CL2                                                              
MPD      DS    CL15                                                             
         DS    CL2                                                              
MUNPD    DS    CL15                                                             
         DS    CL2                                                              
MBLD     DS    CL15                                                             
         DS    CL2                                                              
MUNBLD   DS    CL15                                                             
         DS    CL7                                                              
         SPACE 2                                                                
*                                                                               
BUCKEL   DSECT                                                                  
       ++INCLUDE DDBKELEM                                                       
*                                                                               
       ++INCLUDE DMWRKRK                                                        
         EJECT                                                                  
SP4902   CSECT                                                                  
*                   ACCUMULATORS ARE STRUCTURED AS FOLLOWS                      
*                   AUTHORIZED / CURRENT                                        
*                   ORDERED                                                     
*                   CD  (ALWAYS ZERO)                                           
*                   PAID                                                        
*                   BILLED                                                      
*                                                                               
*                   (PL7 X 5 ) X (PRI + 60M + SUBS + TOT )                      
*                                 TODAY G + TODAY N + MONTH G +                 
*                                 MONTH N + BILLROW)= 35 X 68=2380              
*                                                                               
*                                                                               
CLTTOTS  DS    0D                                                               
         DS    340PL7                                                           
         DS    340PL7              FOR ALT TOTALS (NO CD)                       
*                                                                               
AGYTOTS  DS    0D                                                               
         DS    340PL7                                                           
         DS    340PL7                                                           
*                                                                               
DDSTOTS  DS    0D                                                               
         DS    340PL7                                                           
         DS    340PL7                                                           
*                                                                               
WRKTOTS  DS    0D                                                               
         DS    340PL7                                                           
*                                                                               
OFFTOTS  DS    0D                                                               
         DS    340PL7                                                           
         DS    340PL7                                                           
*                                                                               
*                                                                               
ACCL     EQU   2380                                                             
ACCN     EQU   340                                                              
NMOS     EQU   60                                                               
ROWL     EQU   35                                                               
TGROW    EQU   (NMOS+3)*ROWL                                                    
TNROW    EQU   (NMOS+4)*ROWL                                                    
MGROW    EQU   (NMOS+5)*ROWL                                                    
MNROW    EQU   (NMOS+6)*ROWL                                                    
TOTROW   EQU   (NMOS+2)*ROWL                                                    
         SPACE 3                                                                
AGYNAMS  DS    0D                                                               
         DC    A(*+4)              A(NEXT)                                      
         DS    100CL11             ROOM FOR 100 AGYMEDS                         
*                                  BYTES 0-2    MED/AGY                         
*                                        3-7    WF DISK ADDR                    
*                                        8-9    WF REC NO                       
*                                                                               
*                                                                               
CLTTAB   DS    0D                                                               
         DS    (MAXCLTS)CL20            CLIENT LIST                             
CLTTABX  DS    X                                                                
*                                                                               
MAXCLTS  EQU   10000                                                            
         SPACE 2                                                                
WRKRBUFF DS    0D                                                               
         DS    4096X                                                            
         SPACE 2                                                                
         PRINT OFF                                                              
       ++INCLUDE DDOFFICED                                                      
       ++INCLUDE DDMASTC                                                        
         DS    0D                                                               
       ++INCLUDE SPGENCLT                                                       
         DS    0D                                                               
       ++INCLUDE SPGENEST                                                       
         DS    0D                                                               
       ++INCLUDE SPGENBILL                                                      
         DS    0D                                                               
       ++INCLUDE SPGENAGY                                                       
         PRINT ON                                                               
         DS    0D                                                               
       ++INCLUDE SPGENCLG                                                       
         DS    0D                                                               
       ++INCLUDE SPGENGRP                                                       
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
         PRINT ON                                                               
         ORG   QGRP                                                             
QOPT6    DS    CL1                                                              
QOPT7    DS    CL1                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'071SPREP4902 02/08/17'                                      
         END                                                                    
