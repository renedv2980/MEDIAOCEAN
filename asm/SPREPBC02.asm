*          DATA SET SPREPBC02  AT LEVEL 034 AS OF 07/09/14                      
*$PANAPT01S$ *******  FIRST LINE TO DELETE  *****************                   
*                                                           *                   
*  ATTN: THE LEVEL STAMPS ARE *LEGITIMATELY* OUT-OF-SYNC!   *                   
*        DELETE THIS COMMENT BLOCK ON THE NEXT PROMOTION!   *                   
*                                                           *                   
*  THIS SOURCE MODULE IS AT A HIGHER LEVEL NUMBER THAN ITS  *                   
*  CORRESPONDING LOAD OR OBJECT MODULE, BECAUSE THE MEMBER  *                   
*  WAS PROMOTED USING THE SRCE LIBCODE VIA MR# 044156.      *                   
*                                                           *                   
*  THIS COMMENT BLOCK WAS INSERTED *PROGRAMMATICALLY* BY    *                   
*  PANAPT, TO EXPLAIN WHY THE LEVEL STAMPS ARE OUT-OF-SYNC. *                   
*  BEFORE THIS MEMBER CAN BE PROMOTED AGAIN, THIS ENTIRE    *                   
*  COMMENT BLOCK *MUST* BE MANUALLY DELETED.                *                   
*                                                           *                   
*$PANAPT01X$ *******  LAST LINE TO DELETE  ******************                   
*PHASE SPBC02A                                                                  
*INCLUDE OFFOUT                                                                 
         SPACE 1                                                                
*********************************************************************           
* CHANGE LOG                                                        *           
*                                                                               
* BPLA  3/13   REPORT NEW SUBMEDIA V UNDER CABLE (SUBMEDIA C)                   
*                                                                               
* BPLA  2/10   ABILITY TO REPORT BY OFFICE                                      
*                                                                               
* BPLA  8/04   IGNORE BILLS FOR AGENCY UB AND CLIENT AFX                        
*              IF BILLED BEFORE AUG01/04 - THEY ARE ADSERVE                     
*              FIXES                                                            
*                                                                   *           
*********************************************************************           
         TITLE 'SP0BC-02  SPOTPAK ACROSS AGENCY BILLING REPORT'                 
*                                                                               
* QOPT1        D=BREAKOUT SUBMEDIA D                                            
*              O=BREAKOUT SUBMEDIA O                                            
*              B=BREAKOUT SUBMEDIA D + O                                        
*                                                                               
* QOPT2        C=CLOSE-OUT RUN                                                  
*              OPTION 5 SHOULD BE SET TO Y TO PROCESS ALL BILLS                 
*                                                                               
* QOPT3        T=INCLUDE TEST AGENCIES                                          
*                                                                               
* QOPT4        C=CANADIAN AGENCIES ONLY                                         
*              T=TEST AGENCIES ONLY                                             
*              B=TEST OR CANADIAN AGENCIES ONLY                                 
*                (NORMALLY USED WITH QOPT3 =T)                                  
*              X=EXCULDE CANADIAN AGENCIES                                      
*                                                                               
* QOPT5        Y=DO WHOLE FILE INSTEAD OF YEAR TO DATE                          
*                                                                               
         EJECT                                                                  
SPBC02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPBC02                                                         
         SPACE 2                                                                
         L     RA,0(R1)                                                         
         LA    R9,1(RA)                                                         
         LA    R9,4095(R9)                                                      
         USING SPWORKD,RA,R9                                                    
         LA    RC,SPACEND                                                       
         USING SPBCWRKD,RC                                                      
         SPACE 2                                                                
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
*                                                                               
*                                  INITIALIZATION                               
INIT     NTR1                                                                   
*                                                                               
         L     RE,ADCONLST                                                      
         USING SPADCONS,RE                                                      
         MVC   AOFFICER,VOFFICER                                                
         DROP  RE                                                               
         SPACE 1                                                                
******   CLI   FRSTIM,0            NOT NEEDED FOR SPOT                          
******   BNE   INIT2            SO I'LL CLEAR BUFFALO FOR EACH REQ              
******   MVI   FRSTIM,1                                                         
*                                  RELOCATE ACONS                               
         RELOC (R4)                                                             
         LA    R0,(ACONX-ACONS)/4                                               
         LA    R1,ACONS                                                         
         LA    R2,VBTOTS                                                        
INIT0    DS    0H                                                               
         L     R3,0(R1)                                                         
         AR    R3,R4                                                            
         ST    R3,0(R2)                                                         
         LA    R1,4(R1)                                                         
         LA    R2,4(R2)                                                         
         BCT   R0,INIT0                                                         
         MVC   TODAY+0(2),RCDATE+6      YR                                      
         MVC   TODAY+2(2),RCDATE+0      MO                                      
         MVC   TODAY+4(2),RCDATE+3      DA                                      
*                                                                               
         GOTO1 DATCON,DMCB,TODAY,(3,BTODAY)                                     
         GOTO1 DATCON,DMCB,(3,BTODAY),(0,TODAY)                                 
         GOTO1 DATCON,DMCB,(0,TODAY),(9,CURMTH)                                 
*                                                                               
         MVI   NETOPT,C'N'         SET FOR NEW NETPAK                           
         L     R1,VMASTC                                                        
         USING MASTD,R1                                                         
         CLI   MCNETPAK,C'Y'       SEE IF NETPAK                                
         BE    *+8                                                              
*                                                                               
         DROP  R1                                                               
*                                                                               
         MVI   NETOPT,0                                                         
*                                                                               
         MVC   QUESTOR(5),=C'SPOT '                                             
         MVC   QUESTOR+5(2),FILENUM2                                            
         CLI   NETOPT,C'N'                                                      
         BNE   *+10                                                             
         MVC   QUESTOR+7(4),=C'-NET'                                            
*                                                                               
         XC    OLDKEY,OLDKEY                                                    
*                                                                               
         LA    R0,BUFREC                                                        
         ST    R0,BUFFIO                                                        
         RELOC (R4)                                                             
         L     R0,=A(BUFFALOC)                                                  
         AR    R0,R4                                                            
         ST    R0,BUFFBUFF                                                      
*                                                                               
         GOTO1 BUFFALO,DMCB,=C'SET',BUFFBUFF                                    
         SPACE 1                                                                
INIT2    DS    0H                  **DONE FOR EACH REQUEST**                    
         OI    DMINBTS,X'08'       SET TO PASS DELETES                          
         MVI   DMOUTBTS,X'FD'                                                   
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVC   P,SPACES                                                         
INITX    DS    0H                                                               
         B     EXIT                                                             
         SPACE 3                                                                
LAST     DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                  PROCESS AN AGY/MED                           
PROCAGYM NTR1                                                                   
         BAS   RE,INIT                                                          
*                                                                               
         MVI   SUBMEDTD+1,C'N'    BE SURE IT'S N TO START                       
         CLI   QOPT1,C'D'    SEE IF REPORTING SUBMEDIA D SEPARATELY             
         BNE   *+8                                                              
         MVI   SUBMEDTD+1,C'D'    ALTER REPORTING MEDIA TO D                    
         CLI   QOPT1,C'B'    OR BOTH D +O                                       
         BNE   *+8                                                              
         MVI   SUBMEDTD+1,C'D'    ALTER REPORTING MEDIA TO D                    
*                                                                               
         MVI   SUBMEDTO+1,C'N'    BE SURE IT'S N TO START                       
         CLI   QOPT1,C'O'    SEE IF REPORTING SUBMEDIA D SEPARATELY             
         BNE   *+8                                                              
         MVI   SUBMEDTO+1,C'O'    ALTER REPORTING MEDIA TO O                    
         CLI   QOPT1,C'B'    OR BOTH D +O                                       
         BNE   *+8                                                              
         MVI   SUBMEDTO+1,C'O'    ALTER REPORTING MEDIA TO O                    
*                                                                               
         MVC   SVQOPT5,QOPT5      SAVE FOR REPORT                               
         MVC   SVQOPT4,QOPT4      SAVE FOR REPORT                               
         MVC   SVQOPT2,QOPT2      SAVE FOR REPORT                               
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         BAS   RE,BLDAGY           BUILD LIST OF AGY/MEDIAS                     
         CLC   QCLT,=C'ALL'                                                     
         BE    PRAG2                                                            
         BAS   RE,BLDOFF           BUILD LIST OF OFFICES                        
PRAG2    MVI   PSTART,0                                                         
         XC    KEY,KEY                                                          
         MVI   KEY+1,X'11'         FIRST AGY/MED                                
PRAG3    GOTO1 HIGH                                                             
         B     PRAG5B                                                           
PRAG5A   GOTO1 SEQ                                                              
PRAG5B   CLI   KEY,0               TEST HEADER                                  
         BE    PRAG5C                                                           
*                                                                               
         MVI   TOTTYP,X'01'                                                     
         GOTO1 VBTOTS,DMCB,(RA)                                                 
         MVI   FORCEHED,C'Y'                                                    
         MVI   TOTTYP,X'02'         FILE TOTALS                                 
         GOTO1 VBTOTS,DMCB,(RA)                                                 
         MVI   MODE,REQLAST                                                     
         B     EXIT                                                             
                                                                                
PRAG5C   CLC   KEY+7(6),=6X'00'    TEST FOR EST HDR/ BILL                       
         BE    PRAG5A              NEITHER EST NOR BILL                         
         CLC   KEY+8(5),=5X'00'                                                 
         BE    PRAG5A              EST HDR - SKIP                               
         B     PRAG7               BILL                                         
         SPACE                                                                  
PRAG7    DS    0H                                                               
         CLC   KEY(2),OLDKEY       SEE IF SAME AGY/MED                          
         BE    PRAG10                                                           
         BAS   RE,CKAGY            SETS SORTAM FOR BUFAM                        
         CLI   SKIPSW,0            SEE IF SKIPPING                              
         BE    PRAG10                                                           
         MVC   KEY+2(3),=3X'FF'                                                 
         XC    OLDKEY(4),OLDKEY    CLEAR OLD KEY FOR NEW A/M                    
         B     PRAG3               SKIP TO NEXT A/M                             
*                                                                               
PRAG10   DS    0H                                                               
         CLC   OLDKEY(4),KEY       SEE IF SAME AGY/MED/CLT                      
         BE    PRAG10C                                                          
*                                                                               
         CLC   QCLT,=C'ALL'         ALL CLIENT REQ?                             
         BE    PRAG10B                                                          
*                                                                               
         MVC   MYSVKEY,KEY        KEY                                           
         XC    KEY,KEY             READ CLIENT HERE FOR OTHER REQS              
         MVC   KEY(4),MYSVKEY     AGY/MED/CLT                                   
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE    13-SO IT MUST BE A CLIENT HEADER              
         BE    *+6                                                              
         DC    H'0'             MISSING CLIENT                                  
         GOTO1 GETCLT                                                           
         MVC   KEY,MYSVKEY                                                      
         GOTO1 HIGH             RESTORE FOR SEQ READ                            
*                                                                               
         CLC   QCLT,=C'$* '         ALL OFFICE REQ?                             
         BE    PRAG10B                                                          
         BAS   RE,CKCLT            FILTER CLIENTS                               
         CLI   CSKIPSW,0           SEE IF SKIPPING                              
         BE    PRAG10B                                                          
         MVC   KEY+4(4),=4X'FF'    SKIP TO NEXT CLIENT                          
         B     PRAG3                                                            
*                                                                               
PRAG10B  DS    0H                     HERE IF NEW CLIENT                        
CFIRST   DS    0H                                                               
         MVC   SAVQCLT,QCLT           SAVE REQUESTED QCLT                       
         CLC   QCLT(2),=C'$*'         ALL OFFICES                               
         BNE   CFIRSTX                                                          
*                                                                               
         MVC   SAVCOFF,SPACES                                                   
         L     RF,ADCLT                                                         
         MVC   SAVCOFF(1),COFFICE-CLTHDR(RF)                                    
*                                                                               
         XC    WORK,WORK                                                        
OFFD     USING OFFICED,WORK                                                     
         MVI   OFFD.OFCSYS,C'P'                                                 
*                                                                               
         MVC   OFFD.OFCAGY,QAGY                                                 
         MVC   OFFD.OFCPMED,QMED                                                
         MVC   OFFD.OFCOFC,COFFICE-CLTHDR(RF)                                   
*                                                                               
         GOTO1 AOFFICER,DMCB,(C'2',WORK),(0,ACOMFACS)                           
         CLI   0(R1),0                                                          
         BNE   CFIRSTX                                                          
         MVC   SAVCOFF,OFFD.OFCOFC2                                             
*                                                                               
         DROP  OFFD                                                             
*                                                                               
CFIRSTX  DS    0H                                                               
         EJECT                                                                  
*                                                                               
PRAG10C  MVC   OLDKEY(4),KEY       SAVE LATEST AGY/MED/CLT                      
         CLI   QOPT2,C'C'          TEST CLOSE OUT RUN                           
         BNE   PRAG11              NO                                           
*                                                                               
         TM    KEY+13,X'C0'        YES- MUST BE CLOSED OUT                      
         BO    PRAG12                                                           
         B     PRAG5A                                                           
*                                                                               
PRAG11   DS    0H                                                               
         TM    KEY+13,X'80'        SKIP DELETES                                 
         BNZ   PRAG5A                                                           
*                                                                               
PRAG12   DS    0H                                                               
******** L     RF,ADEST                                                         
****     TM    EPRDCD-ESTHDR(RF),X'80'        TEST NEW NETPAK                   
****     BZ    PRAG13              NO-OK                                        
****     CLI   QOPT6,C'C'                                                       
****     BNE   PRAG5A              SKIP UNLESS CURRENT MONTH BILLING            
*                                                                               
PRAG13   DS    0H                                                               
         MVI   CLTACT,C'Y'                                                      
         L     R5,ADBILL                                                        
         USING BILLREC,R5                                                       
         XC    BILLREC(250),BILLREC    CLEAR BEFORE READ                        
         XC    BILLREC+250(250),BILLREC+250                                     
         GOTO1 GETBILL                                                          
         CLI   BRETAIL-BILLREC(R5),X'41'   SKIP RETAIL CORP SUMMARIES           
         BE    PRAG5A                                                           
         TM    BILSTAT,BSTTAORQ      IGNORE AOR BILLS                           
         BO    PRAG5A                                                           
*                                                                               
*        MUST IGNORE CARAT BILLS FOR CLIENT AFX RUN                             
*        BEFORE AUG01/04                                                        
*        THEY ARE SPECIAL FILE FIX BILLS FOR ADSERVE DATA                       
*                                                                               
         CLC   BILLREC+20(2),=C'UB'    CARAT                                    
         BNE   PRAG15                                                           
         CLC   BKEYCLT,=X'80B7'     CLIENT AFX                                  
         BNE   PRAG15                                                           
         CLC   BDATE,=X'FAF4F0F8F0F1'   SEE IF BEFORE AUG01/04                  
         BL    PRAG5A             SKIP IF BEFORE                                
*                                                                               
***MAN** CLI   NETOPT,C'N'       SEE IF NETPAK                                  
***MAN** BNE   PRAG15                                                           
***MAN** TM    BILSTAT,BSTMANQ   SEE IF MANUAL BILL                             
***MAN** BO    PRAG5A            SKIP                                           
*                                                                               
PRAG15   BAS   RE,ADDUP                                                         
         B     PRAG5A                                                           
         SPACE                                                                  
         XIT1                                                                   
         EJECT                                                                  
ADDUP    NTR1                                                                   
         SPACE 1                                                                
*                                                                               
* BILL RECORDS *                                                                
         SPACE                                                                  
         L     R5,ADBILL                                                        
         USING BILLREC,R5                                                       
*                                                                               
         CLC   BDATE(6),TODAY      SEE IF AFTER TODAY                           
         BH    ADDUPX              SKIP                                         
*                                                                               
         CLI   QOPT5,C'Y'          SKIPPING YEAR CHECK?                         
         BE    ADDUP1                                                           
         CLC   BDATE(2),TODAY      SEE IF IN THIS YESR                          
         BL    ADDUPX              SKIP                                         
*                                                                               
ADDUP1   DS    0H                                                               
         CLI   NETOPT,C'N'         NETPAK?                                      
         BE    ADDNP1              GO TO SPECIAL ROUTINE                        
*                                                                               
ADDUP1X  GOTO1 SPBVAL,DMCB,(C'B',BILLREC),(0,SPBVALD)                           
*                                                                               
ADDUP2   DS    0H                   COULD GET HERE FROM ADDNP1                  
         ZAP   WGRS,SPBVGRSP        EFFECTIVE GROSS                             
         ZAP   WNET,SPBVNETP        AND NET                                     
*                                   R2 HAS A(ROW)                               
         SPACE                                                                  
         XC    BUFREC,BUFREC                                                    
         MVI   BUFTYP,X'01'                                                     
         MVC   BUFAM,SORTAM        AGY/MED/TEST/CAN/                            
         CLI   NETOPT,C'N'         NETPAK?                                      
         BNE   ADDUP2C                                                          
*******  CLC   BDATE,=X'FAF3F0F4F0F1'   BEFORE APR01/2003                       
*******  BL    ADDUP2B                                                          
         MVC   BUFAM+2(1),1(R3)    SUBMEDIA                                     
         CLI   0(R3),X'FF'         UNLESS END OF TABLE                          
         BNE   ADDUP2C                                                          
ADDUP2B  MVI   BUFAM+2,C'N'        SET TO NETWORK                               
*                                                                               
ADDUP2C  DS    0H                                                               
         CLC   QCLT(2),=C'$*'      ALL OFFICE REQUEST?                          
         BNE   *+10                                                             
         MVC   BUFOFF,SAVCOFF                                                   
*                                                                               
         GOTO1 CLUNPK,DMCB,KEY+2,BUFCLT                                         
         ZAP   BUFYTD,WGRS         YTD GROSS                                    
         ZAP   BUFCM,=P'0'         CLEAR CURRENT MONTH GROSS                    
         ZAP   BUFYTDM,=P'0'       CLEAR MANUAL YTD                             
         ZAP   BUFCMM,=P'0'        CLEAR MANUAL CURRENT MONTH                   
         ZAP   BUFCMR,=P'0'        CLEAR MANUAL REVERSALS                       
         CLC   BDATE(4),TODAY      SEE IF CURRENT MONTH                         
         BNE   *+10                                                             
         ZAP   BUFCM,WGRS                                                       
*                                                                               
         TM    BILSTAT,BSTMANQ      SEE IF MANUAL BILL                          
         BNO   ADDUP3                                                           
*                                                                               
         ZAP   BUFYTDM,WGRS         YTD MANUAL                                  
         CLC   BDATE(4),TODAY      SEE IF CURRENT MONTH                         
         BNE   *+10                                                             
         ZAP   BUFCMM,WGRS                                                      
*                                                                               
         CLI   NETOPT,C'N'          NETPAK?                                     
         BNE   ADDUP3            IF NOT LEAVE $ IN YTD AND CURRENT              
*                                                                               
         ZAP   BUFYTD,=P'0'      CLEAR YTD AND CURRENT MONTH                    
         ZAP   BUFCM,=P'0'       FOR MANUAL BILLS                               
*                                                                               
ADDUP3   TM    BILSTAT2,BSTRVSLQ    SEE IF REVERSAL                             
         BNO   ADDUP5                                                           
         CLC   BDATE(4),TODAY      SEE IF CURRENT MONTH                         
         BNE   *+10                                                             
         ZAP   BUFCMR,WGRS         CURRENT MONTH REVERSALS                      
*                                                                               
ADDUP5   DS    0H                                                               
         CLI   QOPT6,C'Y'    TRACING BUFFALO PUTS?                              
         BNE   ADDUP5A                                                          
         MVC   P(3),BKEYPRD                                                     
         ZIC   R0,BKEYEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+4(3),DUB+6(2)                                                  
         MVC   WORK(2),BKEYYSRV   YEAR AND MONTH OF SERVICE                     
         MVI   WORK+2,X'01'                                                     
         GOTO1 DATCON,DMCB,(3,WORK),(6,P+8)                                     
         MVC   HALF,BKEYINV                                                     
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+16(4),DUB+5(3)                                                 
         GOTO1 HEXOUT,DMCB,BUFREC,P+25,49,0                                     
         MVI   SPACING,2                                                        
         GOTO1 VPRINTIT,DMCB,(RA)                                               
*                                                                               
ADDUP5A  DS    0H                                                               
         GOTO1 BUFFALO,DMCB,=C'PUT',BUFFBUFF,BUFREC                             
         MVC   BUFCLT,=X'FFFFFF'    FOR AGY/MED/OFF TOTALS                      
         GOTO1 BUFFALO,DMCB,=C'PUT',BUFFBUFF,BUFREC                             
         CLC   QCLT(2),=C'$*'       SEE IF ALL OFFICE REQUEST                   
         BNE   ADDUP10                                                          
         MVC   BUFOFF,=X'FFFF'      FOR AGY/MED TOTALS                          
         GOTO1 BUFFALO,DMCB,=C'PUT',BUFFBUFF,BUFREC                             
ADDUP10  MVI   BUFAM+2,C'Z'         FOR AGY TOTALS                              
         XC    BUFOFF,BUFOFF        CLEAR OFFICE                                
         MVI   BUFAM+2,C'Z'                                                     
         MVC   BUFAM+3(2),SPACES    CLEAR TEST/CAN                              
******** OI    BUFAM,X'0F'          FOR AGY TOTALS                              
         GOTO1 BUFFALO,DMCB,=C'PUT',BUFFBUFF,BUFREC                             
*                                                                               
         MVI   BUFTYP,X'02'         FILE TOTALS                                 
         MVC   BUFAM,SORTAM        AGY/MED                                      
         XC    BUFOFF,BUFOFF       CLEAR OFFICE                                 
         CLI   NETOPT,C'N'         NETPAK?                                      
         BNE   ADDUP5C                                                          
*******  CLC   BDATE,=X'FAF3F0F4F0F1'   BEFORE APR01/2003                       
*******  BL    ADDUP5B                                                          
         MVC   BUFAM+2(1),1(R3)    SUBMEDIA                                     
         CLI   0(R3),X'FF'         UNLESS END OF TABLE                          
         BNE   ADDUP5C                                                          
ADDUP5B  MVI   BUFAM+2,C'N'        SET TO NETWORK                               
*                                                                               
ADDUP5C  DS    0H                                                               
         MVC   BUFAM+3(2),SPACES    CLEAR TEST/CAN                              
         MVC   BUFAM(2),=C'ZZ'      FILE TOTALS                                 
******** OI    BUFAM,X'F0'          FILE TOTALS - FOR MEDIA                     
         GOTO1 BUFFALO,DMCB,=C'PUT',BUFFBUFF,BUFREC                             
         MVI   BUFAM+2,C'Z'         FILE TOTAL - ALL MEDIA                      
*******  OI    BUFAM,X'0F'          FILE TOTALS - ALL MEDIA                     
         GOTO1 BUFFALO,DMCB,=C'PUT',BUFFBUFF,BUFREC                             
*                                                                               
         CLI   NETOPT,C'N'        NETPAK?                                       
         BNE   ADDUPX             NO - THEN DONE                                
         CLC   BDATE,=X'FAF3F0F4F0F1'    ONLY THERE AFTER APR01/2003            
         BL    ADDUPX             IF BEFORE THEN DONE                           
         CLI   0(R3),X'FF'       END OF SUB-MEDIAS                              
         BE    ADDUPX            (DIDN'T FIND ANY - THEN DONE)                  
         B     ADDNP3X           DO THE NEXT SUB-MEDIA                          
*                                                                               
*   NETPAK BILLS - GET GROSS AND NET BY SUBMEDIA                                
*                                                                               
ADDNP1   DS    0H                                                               
         MVI   SUBMACT,0                                                        
*                                                                               
         TM    BILSTAT,BSTMANQ     MANUAL BILL?                                 
         BNO   ADDNP2                                                           
*                                                                               
*        SET SPECIAL WORKING SUBMEDIA TABLE                                     
*        I ALSO GET HERE WHEN PROCESS BILLS BEFORE APR01/03                     
*        SO I CAN REPORT BY BLMED (IF PRESENT)                                  
*                                                                               
ADDNP1S  MVC   WKSUBMED(3),=C'NN '  SET DEFAULT (NETWORK)                       
         MVC   WKSUBMED+3(3),=X'FFD540'                                         
         OI    SUBMACT,1          SET SUBMEDIA "FOUND"                          
*                                (TO FOOL ADDNP5)                               
         LA    R3,WKSUBMED                                                      
         CLI   BLMED,C' '         MEDIA GIVEN?                                  
         BNH   ADDUP1X             PROCESS NORMALLY                             
         MVC   WKSUBMED(1),BLMED    SET SUBMEDIA                                
         MVC   WKSUBMED+1(1),BLMED                                              
         CLI   WKSUBMED,C'N'                                                    
         BE    ADDUP1X                                                          
         CLI   WKSUBMED,C'C'     CABLE                                          
         BE    ADDUP1X                                                          
         CLI   WKSUBMED,C'S'     SYNDICATION                                    
         BE    ADDUP1X                                                          
*                                                                               
         CLI   QOPT1,C'B'        BREAKING OUT BOTH D + O                        
         BE    ADDUP1X           LEAVE WKSUBMED ALONE                           
*                                                                               
         CLI   QOPT1,C'O'        BREAKING OUT SUBMEDIA O?                       
         BNE   ADDUP1U                                                          
*                                                                               
         CLI   WKSUBMED,C'O'     OTHER                                          
         BE    ADDUP1X                                                          
         MVC   WKSUBMED(2),=C'NN' REPORT OTHERS AS NETWORK                      
         B     ADDUP1X             PROCESS NORMALLY                             
*                                                                               
ADDUP1U  CLI   QOPT1,C'D'        BREAKING OUT SUBMEDIA D?                       
         BNE   ADDNP1V                                                          
*                                                                               
         CLI   WKSUBMED,C'D'     RADIO NETWORK                                  
         BE    ADDUP1X                                                          
ADDNP1V  MVC   WKSUBMED(2),=C'NN' REPORT OTHERS AS NETWORK                      
         B     ADDUP1X             PROCESS NORMALLY                             
*                                                                               
ADDNP2   LA    R3,SUBMEDTX             POINT TO END OF TABLE                    
*                                      SO I'LL PROCESS AS NORNAL                
         CLC   BDATE,=X'FAF3F0F4F0F1'  IF BEFORE APR01/2003                     
         BL    ADDNP1S                MIGHT PROCESS BY BLMED                    
*                                                                               
*                        SPBVAL WILL DUMP IF ASKED FOR SUB-MEDIA                
*                        VALUES FOR BILLS DATED BEFORE THIS                     
*                                                                               
         LA    R3,SUBMEDTB         SUB-MEDIA TABLE                              
*                                                                               
ADDNP3   CLI   0(R3),X'FF'          END OF TABLE                                
         BE    ADDNP5                                                           
*                                                                               
         ZIC   R0,0(R3)                                                         
         GOTO1 SPBVAL,DMCB,(C'B',BILLREC),((R0),SPBVALD),0                      
         CP    SPBVGRSP,=P'0'       MIGHT NOT FIND ANY                          
         BNE   ADDNP4               FOUND MONEY                                 
         CP    SPBVNETP,=P'0'       MIGHT NOT FIND ANY                          
         BNE   ADDNP4               FOUND MONEY                                 
ADDNP3X  LA    R3,3(R3)                                                         
         B     ADDNP3                                                           
*                                                                               
ADDNP4   OI    SUBMACT,1            SUB MEDIA ELEMENTS FOUND                    
         B     ADDUP2                                                           
*                                                                               
***NP5   CLI   SUBMACT,0       (WAS ADDNP5)                                     
***      BE    ADDUP1X          IF NO $ FOR ANY SUB-MEDIA                       
*                                  PROCESS AS NORMAL                            
*                                  OTHERWISE DONE                               
ADDNP5   B     ADDUPX      JUST EXIT IF NO SUBMEDIA DATA                        
*                                                                               
******** CODE BELOW FOR DEBUGGING                                               
******** GOTO1 SPBVAL,DMCB,(C'B',BILLREC),(0,SPBVALD)                           
******** CP    SPBVGRSP,=P'0'                                                   
******** BNE   DIE                                                              
******** CP    SPBVNETP,=P'0'    OR NET                                         
******** BNE   DIE                                                              
ADDUPX   EQU   *                                                                
         XIT1                                                                   
*                                                                               
DIE      DC    H'0'                                                             
*                                                                               
         DROP  R5                                                               
         SPACE 2                                                                
SUBMEDTB DS    0C                  NETPAK SUB-MEDIA TABLE                       
         DC    C'NN '         NETWORK                                           
         DC    C'CC '         CABLE                                             
         DC    C'SS '         SYNDICATION                                       
SUBMEDTO DC    C'ON '         OTHER (ALLOCATED TO NETWORK)                      
SUBMEDTD DC    C'DN '         NETWORK RADIO (ALLOCATED TO NETWORK)              
*                                                                               
*        NOTE - SUBMEDTO ENTRY WILL BE ALTERED TO C'OO'                         
*               TO REPORT SUBMEDIA O SEPARATELY (QOPT1=O OR B)                  
*        NOTE - SUBMEDTD ENTRY WILL BE ALTERED TO C'DD'                         
*               TO REPORT SUBMEDIA D SEPARATELY (QOPT1=D OR B)                  
*          DATA SET SPREPBC02X AT LEVEL 033 AS OF 12/11/13                      
         DC    C'VC '          REPORT SUBMEDIA V UNDER C                        
*                                                                               
SUBMEDTX DC    X'FF',C'N'          EOT                                          
********************************************************************            
*  SUBROUTINE TO BUILD LIST OFF OFFICES                                         
********************************************************************            
BLDOFF   NTR1                                                                   
         XC   OFFTAB,OFFTAB                                                     
         CLI  QCLT,C'*'    ONE OFFICE?                                          
         BNE  BLDOF5                                                            
         MVC  OFFTAB(1),QCLT+1                                                  
         MVC  OFFTAB+1(2),=2X'FF'     END OF TABLE                              
         B    BLDOFX              DONE                                          
*                                                                               
BLDOF5   DS   0H                                                                
         CLI  QCLT,C'$'     OFFICE LIST?                                        
         BE   *+6                                                               
         DC   H'0'        BAD CLIENT IN REQUEST                                 
*                         MUST BE ALL, ONE OFFICE, OR OFFICE LIST               
         XC    WORK,WORK       READ '$X' PROFILE                                
         LA    RF,WORK                                                          
         USING PROFKD,RF                                                        
         MVI   PROFKSYS,C'S'                                                    
         MVC   PROFKPGM(2),=C'0$'                                               
         MVC   PROFKPGM+2(1),QCLT+1               OFFICE LIST CODE              
         MVC   PROFKAGN,QAGY                                                    
         MVC   PROFKMED,MED                                                     
         DROP  RF                                                               
         GOTO1 GETPROF,DMCB,(X'A0',WORK),OFFTAB,DATAMGR,,WORK+16                
*                           SEE IF THERE'S AN EXTENSION TO THAT PROF            
         DS    0H                                                               
         XC    WORK,WORK       READ '$X' PROFILE EXTENSION                      
         LA    RF,WORK                                                          
         USING PROFKD,RF                                                        
         MVI   PROFKSYS,C'S'-X'40' LOWER-CASE 'S'                               
         MVC   PROFKPGM(3),=C'$ A'                                              
         MVC   PROFKPGM+1(1),QCLT+1    OFFICE LIST CODE                         
         MVC   PROFKAGN,AGY                                                     
         MVC   PROFKMED,MED                                                     
         DROP  RF                                                               
         GOTO1 GETPROF,DMCB,(X'A0',WORK),OFFTAB+16,DATAMGR,,WORK+16             
         MVC   OFFTAB+32(2),=X'FFFF'   SET END OF TABLE                         
BLDOFX   XIT1                                                                   
         EJECT                                                                  
********************************************************************            
* SUBROUTINE TO BUILD LIST OF AGENCIES                                          
********************************************************************            
         SPACE                                                                  
BLDAGY   NTR1                                                                   
         L     R5,ADAGY                                                         
         USING AGYKEY,R5                                                        
         XC    KEY,KEY                                                          
         MVI   KEY,X'06'           REC TYPE                                     
*                                                                               
         CLI   CROSSFIL,C'Y'       CROSS FILE?                                  
         BE    *+10                                                             
         MVC   KEY+1(2),QAGY       ONE AGENCY                                   
*                                                                               
         LA    RE,AGYTAB           CLEAR AGENCY TABLE                           
         L     RF,=A(MAXAGYS*39)                                                
         XCEFL                                                                  
         LA    R4,AGYTAB                                                        
*                                                                               
BLDAG2   GOTO1 HIGH                                                             
         B     BLDAG6                                                           
BLDAG4   DS    0H                                                               
         GOTO1 SEQ                                                              
BLDAG6   DS    0H                                                               
         CLC   KEY(1),KEYSAVE      JUST CHECK REC TYPE                          
         BNE   BLDAG18                                                          
*                                                                               
         CLI   CROSSFIL,C'Y'       CROSS FILE?                                  
         BE    BLDAG15                                                          
         CLC   KEY(3),KEYSAVE      MUST FIND                                    
         BE    BLDAG15                                                          
         DC    H'0'              BAD REQUEST                                    
*                                                                               
BLDAG15  DS    0H                                                               
         GOTO1 GETAGY                                                           
         XC    0(39,R4),0(R4)                                                   
         LA    R2,AGYEL                                                         
         MVI   ELCODE,X'02'                                                     
BLDAG15A BAS   RE,NEXTEL                                                        
         BNE   BLDAG17                                                          
         USING AGYMEDEL,R2                                                      
         MVC   0(1,R4),AGYMEDBT    AGY/MED                                      
         MVC   1(2,R4),KEY+1       AGY CODE                                     
         MVI   3(R4),C'L'          DEFAULT TO LIVE                              
         TM    AGYFLAG1,AGYTESTQ   SEE IF TEST AGENCY                           
         BNO   *+8                                                              
         MVI   3(R4),C'T'                                                       
*                                                                               
         MVI   4(R4),C'U'          DEFAULT TO USA                               
         CLI   AGYPCNDA,C'C'       SEE IF CANADIAN                              
         BNE   *+8                                                              
         MVI   4(R4),C'C'                                                       
         MVC   5(1,R4),AGYMEDCD                                                 
         MVC   6(33,R4),AGYNAME                                                 
*                                                                               
         CLI   NETOPT,C'N'        NETPAK?                                       
         BNE   BLDAG15X                                                         
         MVC   39(39,R4),0(R4)    ADD ENTRY FOR CABLE (SUB-MEDIA)               
         MVI   39+5(R4),C'C'                                                    
         LA    R4,39(R4)                                                        
         MVC   39(39,R4),0(R4)    ADD ENTRY FOR SYNDICATION (SUB-MED)           
         MVI   39+5(R4),C'S'                                                    
         LA    R4,39(R4)                                                        
         MVC   39(39,R4),0(R4)    ADD ENTRY FOR RADIO NETWRK (SUB-MED)          
         MVI   39+5(R4),C'D'                                                    
         LA    R4,39(R4)                                                        
         MVC   39(39,R4),0(R4)    ADD ENTRY FOR OTHER NETWRK (SUB-MED)          
         MVI   39+5(R4),C'O'                                                    
         LA    R4,39(R4)                                                        
*                                                                               
BLDAG15X LA    R4,39(R4)                                                        
         DROP  R2                                                               
         LA    RE,AGYTAB                                                        
         A     RE,=A(AGYTABX-AGYTAB)                                            
         CR    R4,RE                    COMPARE TO END OF TABLE                 
         BL    *+6                                                              
         DC    H'0'                  TOO MANY ENTRIES                           
         B     BLDAG15A                                                         
*                                                                               
BLDAG17  DS    0H                                                               
         CLI   CROSSFIL,C'Y'       CROSS FILE?                                  
         BE    BLDAG4              NEXT AGENCY                                  
         B     BLDAG18             ONE AGENCY - DONE                            
*                                                                               
BLDAG18  DS    0H                                                               
         MVC   1(2,R4),=C'ZZ'           FILE TOTALS                             
         MVI   5(R4),C'R'               RADIO                                   
         MVC   6(33,R4),SPACES                                                  
         MVC   6(12,R4),QUESTOR                                                 
         LA    R4,39(R4)                                                        
         MVC   1(2,R4),=C'ZZ'           FILE TOTALS                             
         MVI   5(R4),C'T'               TELEVISION                              
         MVC   6(33,R4),SPACES                                                  
         MVC   6(12,R4),QUESTOR                                                 
         LA    R4,39(R4)                                                        
         MVC   1(2,R4),=C'ZZ'           FILE TOTALS                             
         MVI   5(R4),C'N'               NETWORK                                 
         MVC   6(33,R4),SPACES                                                  
         MVC   6(12,R4),QUESTOR                                                 
         LA    R4,39(R4)                                                        
         MVC   1(2,R4),=C'ZZ'           FILE TOTALS                             
         MVI   5(R4),C'C'               NETWORK-CABLE                           
         MVC   6(33,R4),SPACES                                                  
         MVC   6(12,R4),QUESTOR                                                 
         LA    R4,39(R4)                                                        
         MVC   1(2,R4),=C'ZZ'           FILE TOTALS                             
         MVI   5(R4),C'S'               NETWORK-SYNDICATION                     
         MVC   6(33,R4),SPACES                                                  
         MVC   6(12,R4),QUESTOR                                                 
         LA    R4,39(R4)                                                        
         MVC   1(2,R4),=C'ZZ'           FILE TOTALS                             
         MVI   5(R4),C'X'               RADIO-NETWORK                           
         MVC   6(33,R4),SPACES                                                  
         MVC   6(12,R4),QUESTOR                                                 
         LA    R4,39(R4)                                                        
*                                                                               
         CLI   QOPT1,C'B'               BREAK OUT BOTH D + O                    
         BE    BLDAG30                                                          
*                                                                               
         CLI   QOPT1,C'D'                BREAKING OUT SUBMEIA D?                
         BNE   BLDAG40                                                          
*                                                                               
BLDAG30  MVC   1(2,R4),=C'ZZ'           FILE TOTALS                             
         MVI   5(R4),C'D'               RADIO-NETWORK (NETPAK)                  
         MVC   6(33,R4),SPACES                                                  
         MVC   6(12,R4),QUESTOR                                                 
         LA    R4,39(R4)                                                        
         CLI   QOPT1,C'D'       ONLY D                                          
         BE    BLDAGX                                                           
*                                                                               
BLDAG40  DS    0H                                                               
         CLI   QOPT1,C'B'         BOTH?                                         
         BE    BLDAG45                                                          
         CLI   QOPT1,C'O'         OTHER ONLY?                                   
         BNE   BLDAGX                                                           
BLDAG45  MVC   1(2,R4),=C'ZZ'           FILE TOTALS                             
         MVI   5(R4),C'O'               OTHER (NETPAK)                          
         MVC   6(33,R4),SPACES                                                  
         MVC   6(12,R4),QUESTOR                                                 
         LA    R4,39(R4)                                                        
*                                                                               
BLDAGX   MVC   0(5,R4),=5X'FF'         SET END OF TABLE                         
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
CRDERR   EQU   *                                                                
         MVC   P(80),QRECORD                                                    
         MVC   P+85(15),=C'INVALID REQUEST'                                     
         MVI   SKIPSPEC,C'Y'                                                    
         GOTO1 REPORT                                                           
         SPACE 1                                                                
         DC    H'0'                                                             
         EJECT                                                                  
         SPACE 2                                                                
NEXTEL   DS    0H                                                               
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    NEXTEL2                                                          
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         B     NEXTEL                                                           
*                                                                               
NEXTEL2  DS    0H                                                               
         LTR   R2,R2                                                            
         BR    RE                  SET CC NOT EQ                                
*                                                                               
*                                                                               
CKAGY    NTR1                                                                   
         MVI   SKIPSW,0                                                         
         LA    R1,AGYTAB                                                        
CKAG2    CLC   0(5,R1),=5X'FF'   END OF TABLE                                   
         BNE   CKAG3                                                            
         CLI   FILENUM2,C'1'   SPOT AGENCY C0 IS SJR                            
         BNE   CKAG2ERR                                                         
         CLI   FILENUM2,C' '                                                    
         BH    CKAG2ERR                                                         
         CLI   KEY+1,X'C3'     BAD NETWORK CANADIAN MEDIA BILL                  
         BE    CKAG50          JUST SKIP                                        
*                                                                               
CKAG2ERR CLI   CROSSFIL,C'Y'       CROSS FILE?                                  
         BNE   CKAG50          NO - JUST SKIP IF NOT IN MY TABLE                
*                                                                               
         DC    H'0'        SOMETHING SCREWY-BILL FOR INVALID MEDIA              
*                                                                               
CKAG3    CLC   KEY+1(1),0(R1)                                                   
         BE    CKAG5                                                            
         LA    R1,39(R1)     NEXT ENTRY                                         
         B     CKAG2                                                            
*                                                                               
CKAG5    CLI   QOPT3,C'T'      SEE IF INCLUDING TEST AGYS                       
         BE    CKAG10                                                           
         CLI   3(R1),C'T'        TEST AGY?                                      
         BE    CKAG50            SET SKIPSW AND EXIT                            
*                                                                               
CKAG10   CLI   QOPT4,C' '        CANADIAN FILTER PRESENT                        
         BE    CKAGX                                                            
         CLI   QOPT4,C'X'        EXCLUDING CANADIAN AGYS?                       
         BNE   CKAG20                                                           
         CLI   4(R1),C'C'                                                       
         BE    CKAG50            SKIP                                           
         B     CKAGX                                                            
*                                                                               
CKAG20   CLI   QOPT4,C'C'        CANADIAN AGYS ONLY?                            
         BNE   CKAG30                                                           
         CLI   4(R1),C'C'        MUST BE CANADIAN                               
         BNE   CKAG50            ELSE SKIP                                      
         B     CKAGX                                                            
*                                                                               
CKAG30   CLI   QOPT4,C'T'         TEST AGY ONLY?                                
         BNE   CKAG35                                                           
         CLI   3(R1),C'T'                                                       
         BNE   CKAG50              SKIP NON-TEST                                
         B     CKAGX                                                            
*                                                                               
CKAG35   CLI   QOPT4,C'B'         REPORT TEST OR CANADIAN                       
         BNE   CKAGX                                                            
         CLI   3(R1),C'T'         TEST?                                         
         BE    CKAGX                                                            
         CLI   4(R1),C'C'         OR CANADIAN                                   
         BE    CKAGX                                                            
         B     CKAG50             ELSE SKIP                                     
*                                                                               
CKAG50   MVI   SKIPSW,1                                                         
CKAGX    MVC   SORTAM(2),1(R1)     AGYALPHA                                     
         MVC   SORTAM+2(1),5(R1)   MEDIA CHARACTER                              
         MVC   SORTAM+3(2),3(R1)   TEST AND CANADIAN FLAGS                      
         XIT1                                                                   
         EJECT                                                                  
*                      MUST BE OFFICE OR OFFICE LIST REQ                        
*                      SEE IF CLIENT IS IN PROPER OFFICE                        
*                                                                               
CKCLT    NTR1                                                                   
         MVI   CSKIPSW,0                                                        
*                                                                               
         LA    R1,OFFTAB                                                        
         L     R5,ADCLT                                                         
CKCL1    CLC   0(2,R1),=2X'FF'   END OF TABLE                                   
         BE    CKCL50          SKIP THIS CLIENT                                 
         CLI   0(R1),0         SKIP EMPTY BYTES                                 
         BE    CKCL2                                                            
         CLC   COFFICE-CLTHDR(,R5),0(R1)                                        
         BE    CKCLX                                                            
CKCL2    LA    R1,1(R1)                                                         
         B     CKCL1                                                            
*                                                                               
*                                                                               
CKCL50   MVI   CSKIPSW,1        OFFICE NOT IN MY LIST                           
CKCLX    DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
ACONS    DS    0F                                                               
         DC    A(BTOTALS)                                                       
         DC    A(PRINTIT)                                                       
ACONX    EQU   *                                                                
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
BTOTALS  CSECT                                                                  
         NMOD1 0,BTOTALS                                                        
         L     RA,0(R1)                                                         
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
         USING SPWORKD,RA,R9                                                    
         LA    R8,BTOTALS+4095                                                  
         LA    R8,1(R8)                                                         
         USING BTOTALS+4096,R8                                                  
*                                                                               
         LA    RC,SPACEND                                                       
         USING SPBCWRKD,RC                                                      
         XC    BUFREC,BUFREC                                                    
         MVC   BUFTYP,TOTTYP                                                    
*                                                                               
BTOT2    DS    0H                                                               
         XC    LASTAM,LASTAM                                                    
         MVI   AGYPSW,0                                                         
         MVI   MEDPSW,0                                                         
         GOTO1 BUFFALO,DMCB,=C'HIGH',BUFFBUFF,BUFREC,0                          
         B     BTOT10                                                           
BTOT5    GOTO1 BUFFALO,DMCB,=C'SEQ',(TOTTYP,BUFFBUFF),BUFREC,0                  
*                                                                               
BTOT10   CLI   DMCB+8,X'80'        END                                          
         BE    BTOTX                                                            
BTOT20   DS    0H                                                               
         CLC   LASTAM,BUFAM        SEE IF FIRST/NEW AGY/MED                     
         BE    BTOT20C                                                          
*                                                                               
         CLI   TOTTYP,X'02'      FILE TOTAL?                                    
         BNE   BTOT20A                                                          
         CLI   AGYPSW,1          SEE IF NAME PRINTED                            
         BE    BTOT20A4                                                         
*                                                                               
BTOT20A  MVI   FORCEHED,C'Y'                                                    
         CLI   BUFAM+2,C'Z'       AGENCY TOTAL?                                 
         BNE   BTOT20A2                                                         
*                                 CAN'T LOOK FOR AGENCY NAME                    
         MVI   FORCEHED,C'N'      NO NEW PAGE                                   
         B     BTOT20A7                                                         
*                                                                               
BTOT20A2 BAS   RE,BTAGY           FIND AGENCY NAME                              
*                                                                               
         MVC   P(36),WORK         RETURNED IN WORK                              
         MVC   P2(15),WORK+45     TEST/CANADIAN IN WORK+45                      
         MVC   SVNAGY,P           SAVE AGENCY NAME FOR NEW PAGE                 
         MVC   SVTCAGY,P2         SAVE AGENCY TEST/CANADIAN                     
         GOTO1 VPRINTIT,DMCB,(RA)                                               
         MVI   AGYPSW,1           SET AGY NAME PRINTED                          
*                                                                               
BTOT20A4 CLI   BUFAM+2,C'Z'       SEE IF AGENCY TOTAL                           
         BE    BTOT20A7                                                         
         BAS   RE,BTAGY           FIND MEDIA CODE                               
         MVC   P(09),=C'R-RADIO  '                                              
         CLI   WORK+40,C'R'                                                     
         BE    BTOT20A5                                                         
         MVC   P(09),=C'N-NETWORK'                                              
         CLI   WORK+40,C'N'                                                     
         BE    BTOT20A5                                                         
         MVC   P(12),=C'T-TELEVISION'                                           
         CLI   WORK+40,C'T'                                                     
         BE    BTOT20A5                                                         
         MVC   P(15),=C'X-RADIO NETWORK'                                        
         CLI   WORK+40,C'X'                                                     
         BE    BTOT20A5                                                         
         MVC   P(15),=C'C-NETWORK-CABLE'                                        
         CLI   WORK+40,C'C'                                                     
         BE    BTOT20A5                                                         
         MVC   P(21),=C'S-NETWORK-SYNDICATION'                                  
         CLI   WORK+40,C'S'                                                     
         BE    BTOT20A5                                                         
         MVC   P(23),=C'D-NETWORK-RADIO NETWORK'                                
         CLI   WORK+40,C'D'                                                     
         BE    BTOT20A5                                                         
         MVC   P(23),=C'O-NETWORK-OTHER        '                                
         CLI   WORK+40,C'O'                                                     
         BE    BTOT20A5                                                         
         DC    H'0'          UNKNOWN MEDIA                                      
BTOT20A5 DS    0H                                                               
         MVC   SVNMED,P              SAVE MEDIA FOR NEW PAGE                    
         GOTO1 VPRINTIT,DMCB,(RA)                                               
         CLI   BUFTYP,X'02'       FILE TOTAL?                                   
         BE    BTOT20A7                                                         
         GOTO1 VPRINTIT,DMCB,(RA)   NO - THE SKIP A LINE                        
*                                                                               
BTOT20A7 MVC   LASTAM,BUFAM                                                     
         MVI   MEDPSW,1                                                         
         B     BTOT20F                                                          
*                                                                               
BTOT20B  MVC   LASTAM,BUFAM                                                     
*                                                                               
BTOT20C  DS    0H                                                               
         ZIC   R1,LINE                                                          
         LA    R1,1(R1)                                                         
         STC   R1,X                                                             
         CLC   X(1),MAXLINES                                                    
         BNH   BTOT20F                                                          
         MVC   SAVEP,P                                                          
         MVC   P,SPACES                                                         
         MVC   P2,SPACES                                                        
         MVC   P(L'SVNAGY),SVNAGY                                               
         MVC   P2(L'SVTCAGY),SVTCAGY                                            
         GOTO1 VPRINTIT,DMCB,(RA)  PRINT THE AGENCY STUFF                       
*                                                                               
         MVC   P(L'SVNMED),SVNMED                                               
         MVI   SPACING,2                                                        
         GOTO1 VPRINTIT,DMCB,(RA)  PRINT THE MEDIA DESCRIPTION                  
         MVC   P,SAVEP             RESTORE THE PRINTLINE                        
*                                                                               
BTOT20F  DS    0H                                                               
         CLC   SAVQCLT(2),=C'$*'    ALL OFFICE REQUEST                          
         BNE   BTOT20G                                                          
         CLC   BUFOFF,=X'FFFF'                                                  
         BNE   BTOT20F3                                                         
         MVC   P(3),=C'ALL'                                                     
         B     BTOT20F5                                                         
*                                                                               
BTOT20F3 MVC   P(2),BUFOFF                                                      
*                                                                               
BTOT20F5 JIF   BUFCLT,NE,=3X'FF',BTOT20F8,JUMP=N                                
         MVC   P+3(3),=C'ALL'                                                   
         B     BTOT20FX                                                         
*                                                                               
BTOT20F8 MVC   P+3(3),BUFCLT                                                    
*                                                                               
BTOT20FX DS    0H                                                               
         B     BTOT20I                                                          
*                                                                               
BTOT20G  JIF   BUFCLT,NE,=3X'FF',BTOT20H,JUMP=N                                 
         JIF   BUFCLT,NE,=3X'FF',BTOT20H,JUMP=N                                 
         MVC   P(3),=C'ALL'                                                     
         B     BTOT20I                                                          
*                                                                               
BTOT20H  MVC   P(3),BUFCLT                                                      
*                                                                               
BTOT20I  DS    0H                                                               
         ZAP   DUB,BUFYTD                                                       
*   CAN'T USE EDIT MARCO - $ MAY BE TOO BIG                                     
         MVC   WORK(18),=X'4020206B2020206B2020206B2020214B2020'                
         ED    WORK(18),DUB+1                                                   
*                                                                               
         MVI   WORK+18,C' '        START WITH A SPACE                           
*                                                                               
         CP    DUB,=P'0'                                                        
         BNL   *+8                                                              
         MVI   WORK+18,C'-'                                                     
*                                                                               
         MVC   P+20(18),WORK+1                                                  
         ZAP   DUB,BUFCM                                                        
*   CAN'T USE EDIT MARCO - $ MAY BE TOO BIG                                     
*   EDIT CAN'T PRINT AN AMOUNT OVER 999,999,999.99                              
*                                                                               
         MVC   WORK(18),=X'4020206B2020206B2020206B2020214B2020'                
         ED    WORK(18),DUB+1                                                   
*                                                                               
         MVI   WORK+18,C' '        START WITH A SPACE                           
*                                                                               
         CP    DUB,=P'0'                                                        
         BNL   *+8                                                              
         MVI   WORK+18,C'-'                                                     
*                                                                               
         MVC   P+40(18),WORK+1                                                  
*                                                                               
         ZAP   DUB,BUFYTDM                                                      
*   CAN'T USE EDIT MARCO - $ MAY BE TOO BIG                                     
         MVC   WORK(18),=X'4020206B2020206B2020206B2020214B2020'                
         ED    WORK(18),DUB+1                                                   
*                                                                               
         MVI   WORK+18,C' '        START WITH A SPACE                           
*                                                                               
         CP    DUB,=P'0'                                                        
         BNL   *+8                                                              
         MVI   WORK+18,C'-'                                                     
*                                                                               
         MVC   P+59(18),WORK+1                                                  
         ZAP   DUB,BUFCMM                                                       
*   CAN'T USE EDIT MARCO - $ MAY BE TOO BIG                                     
*   EDIT CAN'T PRINT AN AMOUNT OVER 999,999,999.99                              
*                                                                               
         MVC   WORK(18),=X'4020206B2020206B2020206B2020214B2020'                
         ED    WORK(18),DUB+1                                                   
*                                                                               
         MVI   WORK+18,C' '        START WITH A SPACE                           
*                                                                               
         CP    DUB,=P'0'                                                        
         BNL   *+8                                                              
         MVI   WORK+18,C'-'                                                     
*                                                                               
         MVC   P+78(18),WORK+1                                                  
*                                                                               
         ZAP   DUB,BUFCMR                                                       
*   CAN'T USE EDIT MARCO - $ MAY BE TOO BIG                                     
*   EDIT CAN'T PRINT AN AMOUNT OVER 999,999,999.99                              
*                                                                               
         MVC   WORK(18),=X'4020206B2020206B2020206B2020214B2020'                
         ED    WORK(18),DUB+1                                                   
*                                                                               
         MVI   WORK+18,C' '        START WITH A SPACE                           
*                                                                               
         CP    DUB,=P'0'                                                        
         BNL   *+8                                                              
         MVI   WORK+18,C'-'                                                     
*                                                                               
         MVC   P+97(18),WORK+1                                                  
*                                                                               
         MVI   TOTSW,0                                                          
         CLC   BUFCLT,=3X'FF'     AGY/MED TOTAL                                 
         BNE   BTOT24                                                           
         CLI   BUFTYP,X'02'                                                     
         BE    *+8                                                              
         MVI   AGYPSW,0         SO AGENCY NAME WILL REPRINT                     
*                               UNLESS FILE TOTAL                               
         MVI   TOTSW,1                                                          
         CLC   SAVQCLT(2),=C'$*'     ALL OFFICE REQUEST?                        
         BNE   BTOT23                                                           
         CLC   BUFOFF,=X'FFFF'                                                  
         BE    BTOT23                                                           
         OC    BUFOFF,BUFOFF                                                    
         BZ    BTOT23                                                           
         MVC   P(12),=C'OFFICE TOTAL'                                           
         B     BTOT23B                                                          
                                                                                
BTOT23   MVC   P(11),=C'MEDIA TOTAL'                                            
****ED   MVI   P+37,C'*'                                                        
****ED   MVI   P+57,C'*'                                                        
BTOT23B  CLI   BUFAM+2,C'Z'       AGENCY TOTAL                                  
         BNE   BTOT24                                                           
         MVC   P(12),=C'AGENCY TOTAL'                                           
****ED   MVI   P+38,C'*'                                                        
****ED   MVI   P+58,C'*'                                                        
         CLI   BUFTYP,X'01'  IF BUFTYP IS X'01' MUST AGY TOTAL                  
         BE    BTOT24                                                           
*                             OTHERWISE MUST BE FILE TOTAL                      
         MVC   P(12),=C'FILE TOTAL  '                                           
****ED   MVI   P+39,C'*'                                                        
****ED   MVI   P+59,C'*'                                                        
*                                                                               
BTOT24   GOTO1 VPRINTIT,DMCB,(RA)     SKIP A LINE                               
         CLI   TOTSW,1                                                          
         BNE   BTOT5                                                            
         MVC   SVMAXL,MAXLINES                                                  
         MVI   MAXLINES,99                                                      
         GOTO1 VPRINTIT,DMCB,(RA)     SKIP A LINE                               
         MVC   MAXLINES,SVMAXL                                                  
         B     BTOT5                                                            
*                                                                               
BTOTX    XIT1                                                                   
*                                                                               
BTAGY    NTR1                                                                   
*                                                                               
         MVC   WORK(60),SPACES                                                  
         LA    R1,AGYTAB                                                        
BTAG2    CLC   0(5,R1),=5X'FF'   END OF TABLE                                   
         BNE   *+6                                                              
         DC    H'0'            SOMETHING SCREWY                                 
*                                                                               
         CLC   BUFAM(2),1(R1)   MATCH AGY CODE?                                 
         BNE   BTAG3                                                            
         CLC   BUFAM+2(1),5(R1)  MATCH MEDIA CHARACTER?                         
         BNE   BTAG3                                                            
         CLI   BUFTYP,X'02'       LOOKING FOR FILE TOTAL?                       
         BNE   BTAG5                                                            
         CLI   0(R1),0            AGYMED BYTE MUST BE ZERO                      
         BE    BTAG5              ELSE KEEP LOOKING                             
*                            JUST IN CASE A ZZ AGYHEADER ON FILE                
BTAG3    LA    R1,39(R1)     NEXT ENTRY                                         
         B     BTAG2                                                            
*                                                                               
BTAG5    MVC   WORK(2),1(R1)   AGY CODE                                         
         MVC   WORK+3(33),6(R1)  NAME                                           
         MVC   WORK+40(1),5(R1)  RETURN MEDIA CODE                              
         LA    R3,WORK+45                                                       
         MVC   WORK+45(4),=C'TEST'                                              
         CLI   3(R1),C'T'        TEST AGENCY?                                   
         BNE   BTAG6                                                            
         LA    R3,6(R3)                                                         
*                                                                               
BTAG6    MVC   0(4,R3),SPACES    NEED TO CLEAR IF NOT TEST                      
         CLI   4(R1),C'C'        CANADIAN?                                      
         BNE   BTAG7                                                            
         MVC   0(08,R3),=C'CANADIAN'                                            
*                                                                               
BTAG7    DS    0H                                                               
         CLC   WORK(2),=C'ZZ'    FILE TOTALS?                                   
         BNE   BTAG8                                                            
         MVC   WORK(39),SPACES                                                  
         MVC   WORK(33),6(R1)    NAME ONLY                                      
BTAG8    XIT1                                                                   
         EJECT                                                                  
BMTH13   DS    0H                  PRINTS BILLING PERIODS NOT MTHS              
         ZIC   R0,1(R2)            R2 POINTS TO BINARY YM                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(2,R3),DUB         R3 POINTS TO PRINT LOCATION                  
*                                                                               
         MVI   2(R3),C'/'                                                       
         ZIC   R0,0(R2)                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  3(2,R3),DUB                                                      
         BR    RE                                                               
*                                                                               
         SPACE 2                                                                
         LTORG                                                                  
*                             PRINT ROUTINE                                     
PRINTIT  CSECT                                                                  
         NMOD1 0,PRINTIT                                                        
         L     RA,0(R1)                                                         
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
         USING SPWORKD,RA,R9                                                    
         LA    RC,SPACEND                                                       
         USING SPBCWRKD,RC                                                      
*                                                                               
         CLI   SVQOPT2,C'C'  SEE IF CLOSE-OUT RUN                               
         BNE   PRNT1                                                            
         MVC   H4+56(17),=C'CLOSED-OUT $ ONLY'                                  
*                                                                               
PRNT1    MVC   H3+54(15),=C'CURRENT MONTH -'                                    
         MVC   H3+70(6),CURMTH                                                  
*                                                                               
         CLI   SVQOPT4,C'C' CANADIAN AGYS ONLY?                                 
         BNE   PRNT2                                                            
         MVC   H4+1(28),=C'** CANADIAN AGENCIES ONLY **'                        
         B     PRNT5                                                            
*                                                                               
PRNT2    CLI   SVQOPT4,C'T' TEST AGENCIES ONLY?                                 
         BNE   PRNT3                                                            
         MVC   H4+1(24),=C'** TEST AGENCIES ONLY **'                            
         B     PRNT5                                                            
*                                                                               
PRNT3    CLI   SVQOPT4,C'B' CANADIAN OR TEST ONLY                               
         BNE   PRNT5                                                            
         MVC   H4+1(37),=C'** TEST AND CANADAIN AGENCIES ONLY **'               
         B     PRNT5                                                            
*                                                                               
PRNT5    DS    0H                                                               
*                                                                               
         MVI   RCSUBPRG,10     YTD                                              
*                                                                               
         CLI   SVQOPT5,C'Y'                                                     
         BNE   *+8                                                              
         MVI   RCSUBPRG,11     WHOLE FILE                                       
*                                                                               
         CLI   BUFTYP,X'01'                                                     
         BE    PRNTX                                                            
         MVI   RCSUBPRG,30     YTD FILE TOTALS                                  
*                                                                               
         CLI   SVQOPT5,C'Y'                                                     
         BNE   *+8                                                              
         MVI   RCSUBPRG,31     WHOLE FILE                                       
*                                                                               
         CLI   BUFTYP,X'02'                                                     
         BE    PRNTX                                                            
*                                                                               
PRNTX    GOTO1 REPORT                                                           
*                                                                               
PRNTITX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                  WORK AREA DSECT                              
SPBCWRKD DSECT                                                                  
*                                                                               
SKIPSW   DS    CL1                                                              
CSKIPSW  DS    CL1                                                              
SVQOPT2  DS    CL1                                                              
SVQOPT4  DS    CL1                                                              
SVQOPT5  DS    CL1                                                              
SUBMACT  DS    XL1                                                              
CURMTH   DS    CL6           CURRENT MONTH (MMM/YY)                             
WKSUBMED DS    CL6           DUMMY SUBMED TABLE                                 
*                            FOR MANUAL BILL PROCESSING                         
MYSVKEY  DS    CL32                                                             
X        DS    F                                                                
TOTTYP   DS    XL1                                                              
LASTAM   DS    CL3                                                              
BSTART   DS    XL3                                                              
BEND     DS    XL3                                                              
PSTART   DS    CL6                                                              
PEND     DS    CL6                                                              
MBKOPT   DS    H                                                                
MUPOPT   DS    H                                                                
HAVPRI   DS    X                                                                
HAVSUB   DS    X                                                                
HLDDAT   DS    CL8                                                              
HLDPCT   DS    CL5                                                              
TOTSW    DS    X                                                                
SVMAXL   DS    X                                                                
NETOPT   DS    C                                                                
FRSTIM   DS    X                                                                
*                                  1 = NO CD                                    
CLTACT   DS    X                                                                
SAVCOFF  DS    CL2                                                              
SAVQCLT  DS    CL3                                                              
SORTAM   DS    CL5                                                              
OLDKEY   DS    CL32                                                             
CURMED   DS    C                                                                
WFID     DS    CL18                                                             
ACLIENT  DS    A                                                                
AOFFICER DS    A                                                                
LASTBRK  DS    XL5                                                              
DAY      DS    CL1                 BINARY DAY OF WEEK                           
DAYADJ   DS    H                                                                
BTODAY   DS    XL3                                                              
ELCODE   DS    XL1                                                              
         DS    F                                                                
VBTOTS   DS    A              ACONS                                             
VPRINTIT DS    A                                                                
         DS    3A             SPARE                                             
REQCRDH  DS    CL26                                                             
REQCRD   DS    CL80                                                             
HLDAGYC  DS    CL2                                                              
HLDAGYN  DS    CL33                                                             
HLDMEDN  DS    CL10                                                             
AAGYNAMS DS    A                                                                
SP49WFN  DS    A                                                                
OFFTAB   DS    CL50    OFFICE TABLE FOR OFFICE AND OFFICE LIST REQS             
DDSSW    DS    CL1                                                              
DTWRK1   DS    CL6                                                              
DTWRK2   DS    CL3                                                              
*                                                                               
SAVEP    DS    CL132                                                            
SVNMED   DS    CL21                                                             
SVNAGY   DS    CL36       AGENCY NAME                                           
SVTCAGY  DS    CL15       TEST/CANADIAN LINE                                    
AGYPSW   DS    CL1                                                              
MEDPSW   DS    CL1                                                              
*                                                                               
WGRS     DS    D                                                                
WNET     DS    D                                                                
*                                  BUFFALO RECORD                               
         DS    0D                                                               
BUFREC   DS    0CL51                                                            
BUFKEY   DS    0CL11                                                            
BUFTYP   DS    CL1                 TYPE X'01' OR X'02' (FILE TOTALS)            
BUFAM    DS    CL5                 AGY/MED/TEST/CAN                             
BUFOFF   DS    CL2                 OFFICE (ONLY IF QCLT= $*)                    
BUFCLT   DS    CL3                 CLIENT                                       
BUFYTD   DS    PL8                 YTD GROSS                                    
BUFCM    DS    PL8                 CURRENT MTH GROSS                            
BUFYTDM  DS    PL8                 YTD MANUAL BILLING                           
BUFCMM   DS    PL8                 CURRENT MTH MANUAL BILLING                   
BUFCMR   DS    PL8                 CURRENT MTH REVERSALS                        
*                                                                               
       ++INCLUDE SPBVALD                                                        
AGYTAB   DS    0D                                                               
         DS    150CL39           ROOM FOR 150 AGY/MEDS                          
AGYTABX  EQU   *                                                                
*                                  BYTE  1      AGY/MED                         
*                                  BYTES 2-3    AGY CODE                        
*                                  BYTE  4      T=TEST AGY                      
*                                  BYTE  5      C=CANADIAN AGY                  
*                                  BYTE  6      MEDIA                           
*                                  BYTES 7-39   AGY NAME                        
*                                                                               
*                                                                               
MAXAGYS  EQU   150                                                              
*                                                                               
         EJECT                                                                  
SPBC02   CSECT                                                                  
*                                                                               
*                                                                               
         BUFF  LINES=9000,ROWS=1,COLUMNS=5,FLAVOR=PACKED,KEYLIST=(11,A)         
*                                                                               
         SPACE 2                                                                
         PRINT OFF                                                              
       ++INCLUDE DDOFFICED                                                      
         DS    0D                                                               
       ++INCLUDE SPGENCLT                                                       
         DS    0D                                                               
       ++INCLUDE SPGENEST                                                       
         DS    0D                                                               
       ++INCLUDE SPGENBILL                                                      
         DS    0D                                                               
       ++INCLUDE SPGENAGY                                                       
         DS    0D                                                               
         PRINT OFF                                                              
       ++INCLUDE DDBUFFALOD                                                     
       ++INCLUDE DDGETPROFD                                                     
       ++INCLUDE DDREPMASTD                                                     
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
         PRINT ON                                                               
         ORG   QGRP                                                             
QOPT6    DS    CL1                                                              
QOPT7    DS    CL1                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'034SPREPBC02 07/09/14'                                      
         END                                                                    
