*          DATA SET NEMEDCE    AT LEVEL 012 AS OF 06/11/04                      
*          DATA SET NEMEDCE    AT LEVEL 002 AS OF 06/12/91                      
*PHASE T31ECEA,*                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'T31ECE-NETWORK UN-CLOSEOUT REPORT'                              
T31ECE   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**UNCL**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
*********L     RA,ATWA                                                          
*********USING T31EFFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS4          R7-ANETWS4/WORKING STORAGE                   
         USING WORKD,R7                                                         
         ST    R2,RELO                                                          
         L     R1,ANETWS3             ANETWS3=CLIST                             
         ST    R1,ACLISTSV                                                      
         LA    R6,1(RB)                                                         
         LA    R6,4095(R6)                                                      
         LA    RA,4095(R6)                                                      
         LA    RA,1(RA)                                                         
         USING T31ECE,RB,R6,RA     R6,RA EXTRA BASE REGISTER                    
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         LA    R1,HDRTN                                                         
         ST    R1,HEADHOOK                                                      
         EJECT                                                                  
*HIPO******************************************************************         
*  TITLE: NEMEDCE                                                               
*                                                                     *         
*  COMMENTS: NETWORK UNCLOSEOUT REPORT                                *         
*                                                                     *         
*  CALLS TO: NETIO                                                              
*                                                                     *         
*  GLOBAL: R7-MYWORKD (ANETWS2+500)                                   *         
*                                                                     *         
***********************                                               *         
*  LOGIC:                                                                       
*                                                                     *         
*ENDHIPO***************************************************************         
         SPACE 3                                                                
         CLI   MODE,VALKEY                                                      
         BE    VK                                                               
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BNE   EXIT                                                             
*                                                                               
         BAS   RE,CLEARTOT                                                      
         BAS   RE,CLRUNTOT                                                      
         BAS   RE,MAINLINE                                                      
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*************************************                                           
* VALIDATE REQUEST SCREEN DATA                                                  
*                                                                               
*                                                                               
VK       DS    0H                                                               
         L     R3,ATWA                                                          
         USING T31EFFD,R3                                                       
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BE    VK01                                                             
         CLI   1(RA),C'*'          IS IT DDS TERMINAL                           
         BE    VK01                                                             
         MVI   ERROR,INVRCACT                                                   
         B     TRAPERR                                                          
*                                                                               
VK01     DS    0H                                                               
         MVI   FTERMFLG,0          SET REQUIRED FLAG                            
         OI    NBDMGOPT,X'08'      TO PASS DELETED RECS                         
*                                                                               
         LA    R2,SPLCLIH              CLIENT                                   
         NETGO NVCLIALL,DMCB,SPLCLIN                                            
         OI    SPLCLINH+6,X'80'                                                 
         L     R4,NBAIO                                                         
         USING CLTHDR,R4                                                        
         L     RF,ACLISTSV                                                      
         MOVE  ((RF),880),CLIST                                                 
         MVC   CURRCLI,2(R4)            SET CLIENT                              
         DROP  R4                                                               
*                                                                               
         MVI   FTERMFLG,1                                                       
         LA    R2,SPLPROH               PRODUCT                                 
         NETGO NVGETFLD,DMCB         *** MUST BE POL FOR NOW ***                
         BZ    VK8                                                              
         CLC   8(3,R2),=C'POL'                                                  
         BE    VK10                                                             
         MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
         NETGO NVPRD,DMCB,SPLPRON                                               
         OI    SPLPRONH+6,X'80'                                                 
         B     VK10                                                             
VK8      MVC   8(3,R2),=C'POL'                                                  
         OI    SPLPROH+6,X'80'                                                  
*                                                                               
VK10     LA    R2,SPLESTH                ESTIMATE                               
         MVI   FTERMFLG,0                                                       
         NETGO NVESTRNG,DMCB,SPLESTN                                            
         OI    SPLESTNH+6,X'80'                                                 
         L     R2,NBAIO                                                         
         MVC   ESTKEYSV,0(R2)         SAVE EST KEY                              
*                                                                               
         LA    R2,SPLTSTH                                                       
         MVI   UNDELSW,0                                                        
         CLI   8(R2),C'Y'                                                       
         BE    VKX                                                              
         MVI   UNDELSW,C'Y'                                                     
         CLI   8(R2),C'N'                                                       
         BNE   INVX                                                             
VKX      DS    0H                                                               
         CLI   OFFLINE,C'Y'         IF OFFLINE                                  
         BNE   VKXX                                                             
         L     R4,AIO                                                           
         USING AGYHDR,R4                                                        
                                                                                
         XC    KEY,KEY                                                          
         MVI   KEY,X'06'                                                        
         MVC   KEY+1(2),NBSELAGY                                                
         BAS   R5,DOSETSPT                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FILENAME,=C'SPTFIL  '                                            
         BAS   RE,IOGET                                                         
         MVC   CNTRYSV,AGYPROF+7                                                
         XC    FILENAME,FILENAME                                                
         NETGO NVSETUNT                                                         
VKXX     B     EXIT                                                             
                                                                                
CNTRYSV  DS    CL1                                                              
*                                                                               
INVX     MVI   ERROR,INVALID                                                    
TRAPERR  GOTO1 ERREX                                                            
         DROP  R3,R4                                                            
         EJECT                                                                  
*****************************************                                       
*                                                                               
* MAINLINE FOR DELETE MODULE                                                    
*                                                                               
MAINLINE NTR1                                                                   
         MVI   NBDATA,C'U'               SET UP FOR UNIT RECS                   
         MVI   NBSEQ,C'P'                                                       
         MVI   NBSELPST,C'B'             LOCKED AND UNLOCKED                    
         MVI   NBUSER+13,C'N'            OVERRIDE PROF. GET PRE-EMPTS           
         OI    NBDMGOPT,X'08'            PASS DELETED RECS                      
*                                                                               
*                                                                               
MAN10    NETGO NSNETIO,DMCB,NETBLOCK     GET UNIT RECS                          
         CLI   NBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   NBMODE,NBVALCLI                                                  
         BNE   *+8                                                              
         MVI   NBUSER+13,C'N'      OVERRIDE PROFILE/GET PREEMPTS                
*                                                                               
         CLI   NBMODE,NBPROCUN                                                  
         BE    MAN12                                                            
*                                                                               
         CLI   NBMODE,NBREQLST                                                  
         BNE   MAN10                                                            
         BAS   RE,ALLSUM                                                        
         B     MANXX                                                            
*                                                                               
MAN12    DS    0H                                                               
         TM    NBKEY+20,X'C0'      IS IT CLOSED OUT                             
         BNO   MAN10                                                            
         CLC   CURRCLI,NBACTCLI                                                 
         BE    MAN17                                                            
         MVC   P(12),=C'CLIENT BREAK'                                           
         LA    R3,UNTORD                                                        
         EDIT  (P8,0(R3)),(15,P+15),2,MINUS=YES                                 
         LA    R3,UNTPAID                                                       
         EDIT  (P8,0(R3)),(15,P+32),2,MINUS=YES                                 
         BAS   RE,SPOOLIT                                                       
         ZAP   UNTORD,=P'0'                                                     
         ZAP   UNTPAID,=P'0'                                                    
MAN14    BAS   RE,GETCLTRC         YES/GET CLIENT REC                           
         B     MAN20                                                            
MAN17    CLC   CURREST,NBACTEST    IS IT A NEW EST                              
         BNE   MAN20                                                            
         BAS   RE,DELUNIT          NO/PROCESS UNIT                              
         B     MAN10                                                            
*                                                                               
MAN20    DS    0H                  YES/PROCESS OTHER RECS, THEN UNIT            
         BAS   RE,DELEST                                                        
         BAS   RE,DELPACK                                                       
         BAS   RE,DELBILL                                                       
         BAS   RE,DELMANUL                                                      
         BAS   RE,DELGOAL                                                       
         BAS   RE,DELUNIT                                                       
         MVC   CURREST,NBACTEST                                                 
         MVC   CURRCLI,NBACTCLI                                                 
         B     MAN10                                                            
*                                                                               
MANXX    DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*****************************************                                       
* ESTIMATE KEY IS IN ESTKEYSV                                                   
*                                                                               
DELEST   NTR1                                                                   
         MVI   ESTADD,0                                                         
         MVI   SWIT,C'E'                                                        
         MVI   FORCEHED,C'Y'                                                    
         BAS   R5,DOSETSPT                                                      
****     MVC   AIO,ANETWS1        SET I/O AREA FOR SPOT RECORDS                 
****     NETGO NVSETSPT,DMCB                                                    
****     MVC   FILENAME,=C'SPTDIR  '                                            
         LA    R3,KEY                                                           
         USING ESTHDR,R3                                                        
         XC    KEY,KEY                                                          
         MVC   EKEYAM,NBACTAM                                                   
         MVC   EKEYCLT,NBACTCLI                                                 
         MVC   EKEYEST,NBACTEST                                                 
         MVC   ESTKEYSV,KEY              SET ESTKEYSV SINCE OTHER               
*                                         DELETE ROUTINES EXPECT IT             
         L     R3,AIO                                                           
         MVC   KEYSAVE,KEY                                                      
         OI    DMINBTS,X'08'       PASS DELETED RECS                            
         GOTO1 HIGH                                                             
         B     EST5                                                             
ESTSEQ   MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 SEQ                                                              
*                                                                               
EST5     DS    0H                                                               
         CLC   KEY(4),KEYSAVE       (ID/AM/CLT)                                 
         BNE   ESTX                                                             
         CLC   KEY+8(5),=5X'00'       IS IT EST HEADER                          
         BNE   ESTSEQ                                                           
         CLC   KEY+7(1),KEYSAVE+7     IS IT CURRENT EST                         
         BNE   ESTSEQ                                                           
         MVC   FILENAME,=C'SPTFIL  '                                            
         BAS   RE,IOGET                                                         
         TM    KEY+13,X'C0'           IS IT A DELETED RECORD                    
         BNO   ESTWRT              (POSSIBLE SINCE CLOSEOUT DOES NOT            
*                                   CLOSEOUT SPOT EST HEADERS)                  
         CLI   UNDELSW,C'Y'                                                     
         BNE   ESTWRT                                                           
         L     R1,AIO                                                           
         XI    15(R1),X'C0'           UNDELETE IT                               
         BAS   RE,IOPUT                                                         
         XI    KEY+13,X'C0'           UNDELETE IT                               
         MVC   FILENAME,=C'SPTDIR  '                                            
         BAS   RE,WRTDIR           WRITE TO DIRECTORY                           
ESTWRT   DS    0H                                                               
         LA    R2,P                                                             
         USING BPLINED,R2                                                       
****     GOTO1 =V(CLUNPK),DMCB,EKEYCLT,BPCLT                                    
         GOTO1 NBCLUNPK,DMCB,EKEYCLT,BPCLT                                      
         MVC   BPPRD,EKEYPRD                                                    
         EDIT  (B1,EKEYEST),(3,BPEST)                                           
         MVC   BPYRSRV(6),ESTART                                                
         MVI   BPYRSRV+6,C'-'                                                   
         MVC   BPYRSRV+7(6),EEND                                                
         DS    0H                                                               
         BAS   RE,SPOOLIT                                                       
         L     R1,ESTCNTR                                                       
         LA    R1,1(R1)                                                         
         ST    R1,ESTCNTR                                                       
         AP    TOTEST,=P'1'                                                     
         B     ESTSEQ                                                           
ESTX     DS    0H                                                               
         B     EXIT                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
*******************************************                                     
* READ BILL RECS AND DELETE DIRECTORY AND FILE                                  
*                                                                               
DELBILL  NTR1                                                                   
         MVI   SWIT,C'B'                                                        
         MVI   ESTADD,0         UES ESTADD AS SWITCH FOR HEAD PRINT             
         BAS   RE,SPOOLIT                                                       
         OI    DMINBTS,X'08'                                                    
         BAS   RE,BILLHI                                                        
         L     R2,AIO                                                           
         USING BILLREC,R2                                                       
         BE    BILL7                                                            
*                                                                               
         XC    P,P                                                              
         MVC   P(18),=C'NO BILLING RECORDS'                                     
         BAS   RE,SPOOLIT                                                       
         B     BILLX                                                            
BILL5    BAS   RE,BILLSEQ                                                       
         BNE   BILLX                                                            
BILL7    MVC   FILENAME,=C'SPTFIL  '                                            
         TM    KEY+13,X'C0'          ACCEPT ONLY CLOSED OUT REC                 
         BNO   BILL5                                                            
         BAS   RE,IOGET                                                         
         CLI   UNDELSW,C'Y'                                                     
         BNE   BILLWRIT                                                         
         XI    BCNTRL,X'C0'                                                     
         BAS   RE,IOPUT                                                         
         XI    KEY+13,X'C0'                                                     
         MVC   FILENAME,=C'SPTDIR  '                                            
         BAS   RE,WRTDIR              WRITES TO DIRECTORY                       
BILLWRIT LA    R3,P                                                             
         USING BPLINED,R3                                                       
*****    GOTO1 =V(CLUNPK),DMCB,BKEYCLT,BPCLT      CLIENT                        
         GOTO1 NBCLUNPK,DMCB,BKEYCLT,BPCLT                                      
         MVC   BPPRD,BKEYPRD                  PRODUCT                           
         EDIT  (B1,BKEYEST),(3,BPEST)          ESTIMATE                         
         EDIT  (B1,BKEYYSRV),(2,BPYRSRV)       YEAR SERVICE                     
         MVI   BPMNSRV+2,C'/'                                                   
         EDIT  (B1,BKEYMSRV),(2,BPMNSRV)        MONTH SERVICE                   
         MVC   BYTE,BKEYMBIL                                                    
         NI    BYTE,X'0F'                                                       
         EDIT  (B1,BYTE),(1,BPMNYR)                                             
         MVC   BPMNYR+1(2),=C'/8'                                               
         ZIC   R1,BKEYMBIL                                                      
         SRA   R1,4                                                             
         LA    R4,BPMNYR+3                                                      
         EDIT  (R1),(1,(R4))                                                    
         EDIT  (B2,BKEYINV),(4,BPNUM)          BILL NUMBER                      
         MVC   BPINVO,BINVNO                   BILL INVOICE                     
         MVC   BPDATE,BDATE                    BILL DATE                        
         EDIT  (C8,BAMT),(10,BPAMT)           BILL AMOUNT                       
         CLI   ESTADD,1            HAS HEADING BEEN PRINTED                     
         BE    BILL12                                                           
         MVI   ESTADD,1                                                         
         BAS   RE,BILLHEAD                                                      
BILL12   BAS   RE,SPOOLIT                                                       
         PACK  WORK(10),BAMT             ADD TO BLRECAM2                        
         AP    BLRECAM2,WORK(10)                                                
         L     R1,BILLCNT2               ADD TO BILL COUNTER                    
         LA    R1,1(R1)                                                         
         ST    R1,BILLCNT2                                                      
         AP    TOTBILL,=P'1'                                                    
         B     BILL5                                                            
*                                                                               
BILLX    MVI   SWIT,0                                                           
         B     EXIT                                                             
         EJECT                                                                  
***************************************                                         
*  UNCLOSE STAB RECORDS (MANUAL BILLING)                                        
*                                                                               
DELMANUL NTR1                                                                   
         BAS   RE,SPOOLIT                                                       
         MVI   SWIT,C'S'                                                        
         MVI   ESTADD,0            USE FOR HEADING PRINT SWITCH                 
         OI    DMINBTS,X'08'                                                    
         BAS   RE,STABHI                                                        
         L     R2,AIO                                                           
         USING STABUCK,R2                                                       
         BNE   STABX                                                            
         B     STAB7                                                            
STAB5    BAS   RE,STABSEQ                                                       
         BNE   STABX                                                            
STAB7    MVC   FILENAME,=C'SPTFIL  '                                            
         TM    KEY+13,X'C0'        ACCEPT ONLY CLOSED OUT RECS                  
         BNO   STAB5                                                            
         BAS   RE,IOGET                                                         
         CLI   UNDELSW,C'Y'                                                     
         BNE   STABWRIT                                                         
         XI    STABCNTL,X'C0'                                                   
         BAS   RE,IOPUT                                                         
         XI    KEY+13,X'C0'                                                     
         MVC   FILENAME,=C'SPTDIR  '                                            
         BAS   RE,WRTDIR               WRITE TO DIRECTORY                       
*                                                                               
STABWRIT LA    R3,P                                                             
         USING BPLINED,R3                                                       
         GOTO1 NBCLUNPK,DMCB,STABKCLT,BPCLT                                     
         MVC   WORK(1),STABKPRD                                                 
         BAS   RE,GETPRD                                                        
         MVC   BPPRD,WORK+10                                                    
         EDIT  (B1,STABKEST),(3,BPEST)                                          
***      GOTO1 MSUNPK,DMCB,STABKMKT,BPYRSRV,BPMNYR                              
         B     STAB07A             HOW ABOUT THIS?                              
*                                                                               
*          DATA SET SPREQ03    AT LEVEL 088 AS OF 07/13/99                      
***********************************************                                 
* USE NEW STAPACK                                                               
                                                                                
         LA    R5,WORK                                                          
         XC    0(40,R5),0(R5)                                                   
         USING STAPACKD,R5                                                      
         MVI   STAPACT,C'U'        UNPACK                                       
         MVC   STAPACOM,NBACOM                                                  
         MVC   STAPCTRY,CNTRYSV                                                 
         MVC   STAPMKST,STABKMKT                                                
         XC    DMCB(16),DMCB                                                    
         L     RF,ASTAPAK             DO I HAVE ADDR OF STAPACK?                
         LTR   RF,RF                                                            
         BNZ   STAB07                 YES                                       
STAB05   MVC   DMCB+4(4),=X'D9000A7A'  NO - GET STAPACK                         
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         ST    RF,ASTAPAK                                                       
STAB07   LR    R1,R5                                                            
         GOTO1 (RF),(R1)                                                        
         CLI   STAPERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   BPYRSRV(4),STAPQMKT                                              
         MVC   BPMNYR(5),STAPQSTA                                               
         DROP  R5                                                               
***********************************************                                 
*                                                                               
STAB07A  EDIT  (B2,STABINV),(6,BPINVO)                                          
         GOTO1 DATCON,DMCB,(2,STABBDT),(0,BPDATE)                               
         DROP  R2                                                               
         L     RF,AIO                                                           
         USING STABELEM,RF                                                      
         LA    RF,24(RF)                                                        
         SR    R1,R1                                                            
STAB9    CLI   0(RF),0                                                          
         BE    STAB10                                                           
         CLI   0(RF),X'0E'                                                      
         BNE   STAB9B                                                           
         L     RE,STABGRS                                                       
         AR    R1,RE                                                            
STAB9B   CLI   1(RF),0                                                          
         BE    STABX                                                            
         ZIC   RE,1(RF)                                                         
         AR    RF,RE                                                            
         B     STAB9                                                            
STAB10   SR    R0,R0                                                            
         M     R0,=F'1'                                                         
         D     R0,=F'100'                                                       
         EDIT  (R1),(10,BPAMT)                                                  
STAB11   CLI   ESTADD,1            HAS HEADING BEEN PRITNED                     
         BE    STAB12                                                           
         MVI   ESTADD,1                                                         
         BAS   RE,STABHEAD                                                      
STAB12   BAS   RE,SPOOLIT                                                       
         L     R1,STABCNT2                  ADD TO MANUAL BILL CNTR             
         LA    R1,1(R1)                                                         
         ST    R1,STABCNT2                                                      
         AP    TOTSTAB,=P'1'                                                    
         B     STAB5                                                            
STABX    DS    0H                                                               
         B     EXIT                                                             
         DROP  R2,R3                                                            
ASTAPAK  DS    F                                                                
         DROP  RF                                                               
         EJECT                                                                  
*******************************************                                     
* DELETE AND PRINT PACKAGE/UNIT RECORDS                                         
*                                                                               
DELPACK  NTR1                                                                   
         BAS   RE,SPOOLIT                                                       
         MVC   P(15),=C'PACKAGE RECORDS'                                        
         BAS   RE,SPOOLIT                                                       
*                                                                               
         NETGO NVSETUNT,DMCB                                                    
         MVC   FILENAME,=C'UNTDIR  '                                            
         MVC   AIO,ANETWS1                                                      
         LA    R2,KEY                                                           
         USING NPKEY,R2                                                         
         XC    KEY,KEY                                                          
         MVI   KEY,X'02'           SET PKG TYPE                                 
         MVC   NPKAM,ESTKEYSV+1    A/M                                          
         MVC   NPKCLT,ESTKEYSV+2   CLIENT                                       
         MVC   KEYSAVE,KEY                                                      
         OI    NBDMGOPT,X'08'                                                   
         GOTO1 HIGH                                                             
         B     DOP5                                                             
PKGSEQ   MVC   FILENAME,=C'UNTDIR  '                                            
         GOTO1 SEQ                                                              
DOP5     CLC   KEY(14),KEYSAVE     TYPE/AM/CLT                                  
         BNE   DOUNT                   NOT EQUAL/DO UNITS                       
         CLC   NPKEST,ESTKEYSV+7   EST                                          
         BNE   PKGSEQ                                                           
         MVC   FILENAME,=C'UNTFIL  '                                            
         TM    KEY+20,X'C0'           ACCEPT ONLY CLOSED OUT REC                
         BNO   PKGSEQ                                                           
         GOTO1 DATAMGR,DMCB,(X'08',=C'GETREC'),FILENAME,KEY+21,AIO,    X        
               (0,DMWORK)                                                       
         CLI   UNDELSW,C'Y'                                                     
         BNE   DOP10                                                            
         L     R1,AIO                                                           
         XI    22(R1),X'C0'                                                     
         GOTO1 DATAMGR,DMCB,=C'PUTREC',FILENAME,KEY+21,AIO,(0,DMWORK)           
         XI    KEY+20,X'C0'                                                     
         MVC   FILENAME,=C'UNTDIR  '                                            
         BAS   RE,WRTDIR                                                        
         DROP  R2                                                               
DOP10    DS    0H                                                               
         L     R3,AIO                                                           
         USING NPRECD,R3                                                        
         XC    DMCB,DMCB                                                        
****     GOTO1 =V(CLUNPK),DMCB,NPKCLT,P+1                                       
         GOTO1 NBCLUNPK,DMCB,NPKCLT,P+1                                         
         MVC   P+5(4),NPKNET                                                    
         EDIT  (B1,NPKEST),(4,P+10)                                             
         EDIT  (B1,NPKPACK),(4,P+15)                                            
         MVC   P+20(16),NPAKNAME                                                
         BAS   RE,SPOOLIT                                                       
         AF    PKGCNTR,=F'1'                                                    
         AP    TOTPKG,=P'1'                                                     
         B     PKGSEQ                                                           
         DROP  R3                                                               
         EJECT                                                                  
         SPACE                                                                  
DOUNT    B     EXIT                                                             
         EJECT                                                                  
DELUNIT  NTR1                                                                   
         MVI   SWIT,C'U'                                                        
         CLC   NBACTEST,CURREST                                                 
         BE    *+8                                                              
         MVI   ESTADD,0           (USE AS HEADING PRINT SWITCH)                 
         MVC   KEY,NBKEY                                                        
         MVC   FILENAME,=C'UNTFIL  '                                            
         GOTO1 DATAMGR,DMCB,(X'08',=C'GETREC'),FILENAME,KEY+21,        X        
               NBAIO,(0,DMWORK)                                                 
         L     R2,NBAIO                                                         
         USING NURECD,R2                                                        
         TM    NURSTAT,X'C0'        AN EXTRA CHECK ON RECORD AS WELL            
         BO    *+6                                                              
         DC    H'0'                 DIE IF NOT ALSO CLOSED OUT                  
*                                                                               
         BAS   RE,UNTHOOK          WRITE OUT UNIT DATA                          
* THE UNIT WILL NOT HAVE BILLING DATA SINCE THE NEW UNIT BILLING                
* RECORDS ARE STILL CLOSED OUT AT THIS POINT - WILL HAVE TO RUN WRITER          
* AFTR CLOSEOUT TO MAKE SURE WE HAVE ALL -                                      
         CLI   UNDELSW,C'Y'        UNCLOSE UNIT                                 
         BNE   UNTX                NO                                           
         B     UNT05               YES                                          
*                                                                               
UNT05    DS    0H               *** UNCLOSE UNIT                                
         XI    NURSTAT,X'C0'                                                    
         MVC   FILENAME,=C'UNTFIL  '                                            
         GOTO1 DATAMGR,DMCB,=C'PUTREC',FILENAME,KEY+21,NBAIO,(0,DMWORK)         
*                                                                               
         MVC   KEY(20),0(R2)      MOVE 04 KEY TO KEY(LEAVING DISK ADDR)         
         MVC   SAVEKEY,KEY         SAVCE 04 KEY                                 
*                                                                               
         LA    R2,KEY              ACTIVE KEY                                   
         USING NURECD,R2                                                        
         XI    NUKSTAT,X'C0'                                                    
         MVC   FILENAME,=C'UNTDIR  '                                            
         BAS   RE,WRTDIR                                                        
*                                                                               
         LA    R2,KEY              PASSIVE KEY                                  
         USING NUKPKEY,R2                                                       
         XC    KEY+4(16),KEY+4                                                  
         MVI   KEY,X'84'                                                        
         MVC   NUKPNET,NBACTNET                                                 
         MVC   NUKPPROG,NBACTPRG                                                
         MVC   NUKPDATE,NBACTDAT                                                
         MVC   NUKPEST,NBACTEST                                                 
         MVC   NUKPSUB,NBACTSUB                                                 
         MVC   NUKPDP,NBACTDP                                                   
         MVC   FILENAME,=C'UNTDIR  '                                            
         BAS   RE,WRTDIR                                                        
         MVC   KEYSAVE(20),KEY         SAVE FOR HISTORY RECORD                  
*                                                                               
         MVC   KEY,NBKEY             SECOND PASSIVE KEY                         
         XI    NUKSTAT,X'C0'                                                    
         MVC   FILENAME,=C'UNTDIR  '                                            
         BAS   RE,WRTDIR                                                        
**********************************    HANDLE HISTORY RECORDS                    
         TM    NBPACKST,X'02'         AUDIT TRAIL ON?                           
         BNO   UNT10                                                            
         MVI   KEYSAVE,X'40'           MARK AS HISTORY REC                      
         MVC   FILENAME,=C'UNTDIR  '                                            
         GOTO1 DATAMGR,DMCB,(X'08',=C'DMRDHI'),FILENAME,KEYSAVE,KEY             
         CLC   KEY(20),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    KEY+20,X'C0'        MUST BE ON                                   
         BO    *+6                                                              
         DC    H'0'                                                             
         NI    KEY+20,X'FF'-X'C0'                                               
         BAS   RE,WRTDIR                                                        
*                                                                               
         MVC   FILENAME,=C'UNTFIL  '                                            
         GOTO1 DATAMGR,DMCB,(X'08',=C'GETREC'),FILENAME,               X        
               KEY+21,NBAIO,(0,DMWORK)                                          
         L     R1,NBAIO                                                         
         CLC   0(20,R1),KEY                                                     
         BE    *+6                                                              
         DC    H'0'                    BAD!                                     
         NI    22(R1),X'FF'-X'C0'                                               
         GOTO1 DATAMGR,DMCB,=C'PUTREC',FILENAME,KEY+21,NBAIO,(0,DMWORK)         
*                                                                               
UNT10    DS    0H            ***** UNCLOSE NEW BILLING RECS ******              
         LA    R2,KEY                                                           
         USING NUBRECD,R2                                                       
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0E0A'                                                  
         MVC   KEY+2(19),SAVEKEY+1     SAVEKEY HAS UNIT 04 KEY                  
         MVC   FILENAME,=C'XSPDIR  '                                            
         OI    DMINBTS,X'08'                                                    
         MVC   KEYSAVE,KEY                                                      
         GOTO1 HIGH                                                             
         B     UNT15                                                            
UNT13    MVC   FILENAME,=C'XSPDIR  '                                            
         OI    DMINBTS,X'08'                                                    
         GOTO1 SEQ                                                              
         CLC   KEY(21),KEYSAVE          TESTINGNNN                              
UNT15    CLC   KEY(21),KEYSAVE                                                  
         BNE   UNTX                                                             
         TM    NUBKSTAT,X'C0'                                                   
         BNO   UNT13                                                            
*                                                                               
         MVC   FILENAME,=C'XSPFIL  '   GET BILLING RECORD                       
         GOTO1 DATAMGR,DMCB,(X'08',=C'GETREC'),FILENAME,               X        
               NUBDA,NBAIO,(0,DMWORK)                                           
*                                                                               
         L     R2,NBAIO                                                         
         NI    NUBSTAT,X'FF'-X'C0'                                              
         GOTO1 DATAMGR,DMCB,=C'PUTREC',FILENAME,NUBDA,NBAIO,(0,DMWORK)          
         LA    R2,KEY                                                           
         NI    NUBKSTAT,X'FF'-X'C0'   DEAL WITH 0E0A KEY                        
         MVC   FILENAME,=C'XSPDIR  '                                            
         BAS   RE,WRTDIR                                                        
         MVC   KEYSAVE,KEY                                                      
         MVC   SAVEKEY,KEY         SAVE TO RESTORE SEW                          
******** BUILD 0E06 KEY                                                         
         LA    R2,KEYSAVE                                                       
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0E06'                                                  
         MVC   KEY+2(3),NUBK0AM    AGY/CLT                                      
         MVC   KEY+5(10),NUBK0NET  NETWORK/PROGRAM                              
         MVC   KEY+15(2),NUBK0DAT  DATE                                         
         MVC   KEY+17(3),NUBK0EST    EST/DAYPART/SUB                            
         MVC   KEY+20(2),NUBK0BDT    DATE OF BILLING RUN                        
         MVC   KEY+32(8),NUBKSTAT    KEY STATUS/DISK ADDRESS                    
         MVC   KEYSAVE,KEY                                                      
         MVC   FILENAME,=C'XSPDIR  '                                            
         OI    DMINBTS,X'08'                                                    
         GOTO1  HIGH                                                            
         CLC   KEY(22),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                MUST BE HERE                                 
         LA    R2,KEY                                                           
***      GOTO1 =V(PRNTBL),DMCB,=C'BRC',0(R2),C'DUMP',40,=C'1D'                  
         TM    NUBKSTAT,X'C0'      AND MUST BE CLOSED OUT                       
         BO    *+6                                                              
         DC    H'0'                                                             
         NI    NUBKSTAT,X'FF'-X'C0' UNCLOSE                                     
         BAS   RE,WRTDIR                                                        
         L     R1,UNTBILR                                                       
         LA    R1,1(R1)                                                         
         ST    R1,UNTBILR                                                       
*                                      RESTORE SEQ                              
         XC    KEY,KEY                                                          
         MVC   KEY,SAVEKEY                                                      
         MVC   KEYSAVE,KEY                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(21),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                MUST BE HERE                                 
         B     UNT13               MORE BILL RECS FOR THIS UNIT?                
         DROP  R2                                                               
*                                                                               
*                                                                               
UNTX     MVC   KEY,NBKEY                                                        
         MVI   NBFUNCT,NBFRDHI                                                  
         XC    FILENAME,FILENAME                                                
         B     EXIT                                                             
         SPACE                                                                  
*                                                                               
         EJECT                                                                  
UNTHOOK  NTR1                  NETIO HOOK FOR UNIT RECORDS                      
         LA    R3,P                                                             
         USING UPLINED,R3                                                       
         GOTO1 NBCLUNPK,DMCB,NBACTCLI,UPCLT  CLIENT                             
         CLC   NBPRD,BPRDSV                                                     
         BE    DNR5                                                             
         MVC   WORK(1),NBPRD                                                    
         GOTO1 GETPRD                                                           
         MVC   BPRDSV,NBPRD        SET NEW BPRD                                 
         MVC   PRDSV,WORK+10       SET NEW 3CL PRD                              
DNR5     MVC   UPPRD,PRDSV                   PRODUCT                            
         EDIT  (B1,NBACTEST),(3,UPEST)       ESTIMATE                           
         MVC   UPNET,NBACTNET                NETWORK                            
         MVC   UPPROG,NBACTPRG               PROGRAM                            
         GOTO1 DATCON,DMCB,(2,NBACTDAT),(8,UPDATE)   DATE                       
         MVI   UPDATE+8,C'-'                                                    
         EDIT  (B1,NBACTSUB),(2,UPSUBLN)              SUB-LINE                  
         L     R1,NBACTUAL                                                      
         BAS   RE,SPECIALS                                                      
         AR    R1,R5               ADD SPECIALS                                 
         TM    NBUNITST,X'42'      TEST-MISSED/PRE-EMPT                         
         BNZ   DNR5A                                                            
         BAS   RE,XCENT                                                         
         AP    UNTORD2,DUB                                                      
         AP    UNTORD,DUB      WHAT IF WE DO THIS HERE???                       
         AP    RUNORD,DUB                                                       
         EDIT  (R1),(10,UPACTUAL),MINUS=YES                                     
         L     R1,NBINTEG                                                       
         BAS   RE,XCENT                                                         
         AP    UNTORD,DUB      WHAT IF WE DO THIS HERE???                       
         AP    UNTORD2,DUB                                                      
         AP    RUNORD,DUB                                                       
         EDIT  (R1),(10,UPINTEG)                                                
*                                                                               
DNR5A    L     R1,NBBILTGR         BILLED TIME                                  
         BAS   RE,XCENT                                                         
         AP    UNTBILL2,DUB                                                     
         AP    RUNBILL,DUB                                                      
         EDIT  (R1),(9,UPBILT),MINUS=YES                                        
         L     R1,NBBILIGR              BILLED INTEG                            
         BAS   RE,XCENT                                                         
         AP    UNTBILL2,DUB          BILLED TIME + INTEG =BILLED UNIT           
         AP    RUNBILL,DUB                                                      
         EDIT  (R1),(9,UPBILI)                                                  
*                                                                               
         L     R1,NBPAYTGR              PAID TIME                               
         BAS   RE,XCENT                                                         
         AP    UNTPAID2,DUB                                                     
         AP    RUNPAID,DUB                                                      
         AP    UNTPAID,DUB                                                      
         EDIT  (R1),(9,UPPAIDT)                                                 
         L     R1,NBPAYIGR              PAID INTEG                              
         BAS   RE,XCENT                                                         
         AP    UNTPAID2,DUB          PAID TIME + INTEG =PAID UNIT               
         AP    RUNPAID,DUB                                                      
         AP    UNTPAID,DUB                                                      
         EDIT  (R1),(9,UPPAIDI)                                                 
*                                                                               
         L     R1,NBASSIGN             ASSIGNED                                 
         BAS   RE,XCENT                                                         
         AP    UNTASSG2,DUB                                                     
         AP    RUNASS,DUB                                                       
         EDIT  (R1),(9,UPASSGN)                                                 
*                                                                               
         CLI   ESTADD,1             HAS HEADING BEEN PRINTED                    
         BE    DNR7                                                             
         MVI   ESTADD,1                                                         
         BAS   RE,UNTHEAD                                                       
DNR7     BAS   RE,SPOOLIT                                                       
         L     R1,UNTCNTR2                                                      
         LA    R1,1(R1)                                                         
         ST    R1,UNTCNTR2                                                      
         AP    TOTUNT,=P'1'                                                     
DNRUNTX  B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
SPECIALS NTR1                                                                   
         SR    R5,R5                                                            
         L     R2,NBAIO                                                         
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BNE   SPEC10                                                           
         USING NUSPRD,R2                                                        
SPEC5    MVC   FULL,NUSPRAMT                                                    
         A     R5,FULL                                                          
         BAS   RE,NEXTEL                                                        
         BE    SPEC5                                                            
*                                                                               
SPEC10   L     R2,NBAIO                                                         
         USING NUBILD,R2                                                        
         MVI   ELCODE,X'10'          BILLING                                    
         BAS   RE,GETEL                                                         
         B     *+8                                                              
SPC10    BAS   RE,NEXTEL                                                        
         BNE   SPC20                                                            
         CLI   NUBILTYP,C'T'          SKIP TIME(NETVALUE GETS IT)               
         BE    SPC10                                                            
         CLI   NUBILTYP,C'I'          SKIP INTEGRATION                          
         BE    SPC10                                                            
         ICM   R1,15,NUBILGRS                                                   
         ICM   R3,15,NBBILTGR                                                   
         AR    R3,R1                                                            
         STCM  R3,15,NBBILTGR                                                   
         B     SPC10                                                            
*                                                                               
SPC20    L     R2,NBAIO                                                         
         MVI   ELCODE,X'12'              PAYING                                 
         USING NUPAYD,R2                                                        
         BAS   RE,GETEL                                                         
         B     *+8                                                              
SPC30    BAS   RE,NEXTEL                                                        
         BNE   SPCX                                                             
         CLI   NUPAYTYP,C'T'       SKIP TIME                                    
         BE    SPC30                                                            
         CLI   NUPAYTYP,C'I'       AND INTEGRATION                              
         BE    SPC30                                                            
         ICM   R1,15,NUPAYGRS                                                   
         ICM   R3,15,NBPAYTGR                                                   
         AR    R3,R1                                                            
         STCM  R3,15,NBPAYTGR                                                   
         B     SPC30                                                            
SPCX     DS    0H                                                               
SPECX    XIT1  REGS=(R5)                                                        
         DROP  R2                                                               
         EJECT                                                                  
*******************************************                                     
* BILL REC READING                                                              
* EXCPECTS KEY INFO IN ESTYKEYSV                                                
*                                                                               
BILLHI   NTR1                                                                   
***      MVC   AIO,ANETWS1                                                      
***      NETGO NVSETSPT,DMCB                                                    
***      MVC   FILENAME,=C'SPTDIR  '                                            
         L     R4,ATWA                                                          
         USING T31EFFD,R4                                                       
         BAS   R5,DOSETSPT                                                      
         XC    KEY,KEY                                                          
         MVC   KEY,ESTKEYSV                                                     
         CLC   =C'POL',SPLPRO      IS IT POL                                    
         BNE   *+10                                                             
         XC    KEY+4(3),KEY+4      YES/CLEAR PROD FIELD                         
         MVI   KEY+8,1         SET TO 1 TO DISTINGUISH FROM EST KEY             
         MVC   KEYSAVE,KEY                                                      
         GOTO1 HIGH                                                             
         B     BH7                                                              
BILLSEQ  NTR1                                                                   
         MVC   FILENAME,=C'SPTDIR  '                                            
BH6      GOTO1 SEQ                                                              
BH7      DS    0H                                                               
         L     R4,ATWA                                                          
         CLC   =C'POL',SPLPRO        IS IT POL                                  
         BE    *+14                                                             
         CLC   KEY(8),KEYSAVE       NO/TEST ENTIRE KEY                          
         B     BHX                                                              
         CLC   KEY(4),KEYSAVE        YES/ ID/A-M/CLT                            
         BNE   BHX                                                              
         CLI   KEY+8,0             TEST FOR EST HEADER                          
         BE    BH6                 IF YES GET FOLLOWING BILL REC                
         CLC   KEY+7(1),KEYSAVE+7         ESTIMATE                              
         BNE   BH6                                                              
BHX      B     EXIT                   COND CODE IS SET                          
         DROP  R4                                                               
         EJECT                                                                  
*****************************************                                       
* READS STATION BUCKET RECORDS (STAB)                                           
* EXPECTS KEY IN ESTKEYSV                                                       
*                                                                               
STABHI   NTR1                                                                   
**       MVC   AIO,ANETWS1                                                      
**       NETGO NVSETSPT,DMCB                                                    
**       MVC   FILENAME,=C'SPTDIR  '                                            
         L     R4,ATWA                                                          
         USING T31EFFD,R4                                                       
         BAS   R5,DOSETSPT                                                      
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0E01'                                                  
         MVC   KEY+2(3),ESTKEYSV+1        AM/CLT                                
         MVC   KEY+6(1),ESTKEYSV+7        ESTIMATE                              
         CLC   =C'POL',SPLPRO                                                   
         BE    ST3                                                              
         MVC   WORK(3),ESTKEYSV+4       PASS 3 CHAR PRD                         
         BAS   RE,GETBPRD               RETURNS 1 BYTE PRD                      
         MVC   KEY+5(1),WORK            IN WORK                                 
ST3      MVC   KEYSAVE,KEY                                                      
         GOTO1 HIGH                                                             
         B     ST7                                                              
STABSEQ  NTR1                                                                   
         MVC   FILENAME,=C'SPTDIR  '                                            
ST4      GOTO1 SEQ                                                              
ST7      CLC   KEY(5),KEYSAVE             ID/AM/CLT                             
         BNE   STX                                                              
         CLC   KEY+6(1),KEYSAVE+6           EST                                 
         BNE   ST4                                                              
STX      B     EXIT                SET COND CODE                                
         EJECT                                                                  
         DROP  R4                                                               
**************************************                                          
*     ADD TO UNIT TOTALS                                                        
*     ORDERED/PAID/BILLED                                                       
*                                                                               
ADDTOTS  NTR1                                                                   
         TM    NBUNITST,X'42'      TEST=MISSED/PRE-EMPT                         
         BNZ   ADT5                                                             
         L     R1,NBACTUAL              ORDERED                                 
         BAS   RE,XCENT                                                         
         AP    UNTORD,DUB                                                       
         L     R1,NBINTEG               ADD INTEG TO ORDERED                    
         BAS   RE,XCENT                                                         
         AP    UNTORD,DUB                                                       
*                                                                               
ADT5     L     R1,NBASSIGN              ASSIGNED                                
         BAS   RE,XCENT                                                         
         AP    UNTASSGN,DUB                                                     
*                                                                               
         L     R1,NBBILTGR              BILLED TIME                             
         BAS   RE,XCENT                                                         
         AP    UNTBILL,DUB                                                      
         L     R1,NBBILIGR              BILLED INTEG                            
         BAS   RE,XCENT                                                         
         AP    UNTBILL,DUB          BILLED TIME + INTEG =BILLED UNIT            
*                                                                               
         L     R1,NBPAYTGR              PAID TIME                               
         BAS   RE,XCENT                                                         
         AP    UNTPAID,DUB                                                      
         L     R1,NBPAYIGR              PAID INTEG                              
         BAS   RE,XCENT                                                         
         AP    UNTPAID,DUB          PAID TIME + INTEG =PAID UNIT                
*                                                                               
         B     EXIT                                                             
         SPACE                                                                  
***************************************                                         
* ROUTINE CLEARS AND ZAPS TOTAL FIELDS                                          
*                                                                               
CLEARTOT NTR1                                                                   
         LA    R2,12               12=NUM OF TOT FIELDS                         
         LA    R3,UNTORD                                                        
CLR5     ZAP   0(8,R3),=P'0'                                                    
         LA    R3,8(R3)                                                         
         BCT   R2,CLR5                                                          
         LA    R2,7                7=NUM  OF CNTR FIELDS                        
         LA    R3,ESTCNTR                                                       
CLR7     XC    0(4,R3),0(R3)                                                    
         LA    R3,4(R3)                                                         
         BCT   R2,CLR7                                                          
         XC    PKGCNTR,PKGCNTR     ALSO CLEAR PKGCNTR                           
         B     EXIT                                                             
         SPACE                                                                  
*********************************                                               
* CLEAR RUN TOTAL FIELDS                                                        
CLRUNTOT NTR1                                                                   
         LA    R2,RUNTOTS                                                       
         LA    R3,11                                                            
TOT5     ZAP   0(8,R2),=P'0'                                                    
         LA    R2,8(R2)                                                         
         BCT   R3,TOT5                                                          
         B     EXIT                                                             
         SPACE                                                                  
********************************                                                
* EXPECTS VALUE IN R1                                                           
* RETURNS PACKED VALUE DIVIDED BY 100 IN DUB                                    
*                                                                               
         DS    F                                                                
XCENT    ST    RE,XCENT-4                                                       
*        M     R0,=F'1'                                                         
*        D     R0,=F'100'          GET RID OF CENTS                             
         CVD   R1,DUB                                                           
         L     RE,XCENT-4                                                       
         BR    RE                                                               
         SPACE                                                                  
********************************************                                    
*  TO GET 3 CHAR PRD CODE FROM C LIST                                           
*  INPUT   WORK HAS PRDNO                                                       
*  OUTPUT  PRDCODE IN WORK+10                                                   
*                                                                               
GETPRD   NTR1                                                                   
         L     R2,ACLISTSV                                                      
GP10     CLI   3(R2),0             IF E-O-F CLIST                               
         BNE   GP12                                                             
         MVC   WORK+10(3),=C'UNA'   .SET TO UNDEFINED                           
         B     GPX                                                              
GP12     CLC   3(1,R2),WORK                                                     
         BE    GP14                                                             
         LA    R2,4(R2)            INCREMENT CLIST                              
         B     GP10                RETURN TO LOOP                               
GP14     MVC   WORK+10(3),0(R2)      SET 3 CHAR PRINTABLE PRD CODE              
GPX      B     EXIT                                                             
         SPACE                                                                  
********************************************                                    
*  TO GET 1 CHAR PRD CODE FROM C LIST                                           
*  INPUT   WORK HAS 3 CHAR PRD CODE                                             
*  OUTPUT  BPRD IN WORK                                                         
*                                                                               
GETBPRD  NTR1                                                                   
         L     R2,ACLISTSV                                                      
GPB10    CLI   3(R2),0             IF E-O-F CLIST                               
         BNE   GPB12                                                            
         MVI   WORK,0               .SET TO UNDEFINED                           
         B     GPBX                                                             
GPB12    CLC   0(3,R2),WORK                                                     
         BE    GPB14                                                            
         LA    R2,4(R2)            INCREMENT CLIST                              
         B     GPB10               RETURN TO LOOP                               
GPB14    MVC   WORK(1),3(R2)      SET 1 CHAR PRD CODE                           
GPBX     B     EXIT                                                             
         SPACE                                                                  
         EJECT                                                                  
********************************************                                    
* SUMMARY ON LAST PAGE OF REPORT                                                
*                                                                               
SUMMARY  NTR1                                                                   
         L     R3,ASUMRECD                                                      
         L     R4,SUMCNTR                                                       
         LTR   R4,R4                                                            
         BZ    SUMX                                                             
         MVI   FORCEHED,C'Y'                                                    
         MVI   SPACING,2                                                        
         XC    P,P              NEED TO CLEAR P LINES SINCE IN                  
         XC    P2,P2            -SUMMARY ONLY OPTION- P LINES ARE               
         XC    P3,P3            FILLED BUT NOT PRINTED(SO NOT CLEARED)          
         XC    P4,P4                                                            
         MVC   P(25),=C'RECAP OF CLOSED ESTIMATES'                              
         MVI   P+132,C'-'                                                       
         MVC   P+133(24),P+132                                                  
         MVC   P4(17),=C'ESTIMATES CLOSED='                                     
         EDIT  (B4,SUMCNTR),(6,P4+18),ALIGN=LEFT                                
         L     R2,SUMCNTR                                                       
         CVD   R2,DUB                                                           
         AP    TOTEST,DUB                                                       
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   P+30(7),=C'ORDERED'                                              
         MVC   P+45(8),=C'ASSIGNED'                                             
         MVC   P+57(4),=C'PAID'                                                 
         MVC   P+67(6),=C'BILLED'                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
SUM10    MVC   P(80),0(R3)                                                      
         CLI   P+29,1                                                           
         BNE   *+10                                                             
         MVC   P+80(24),=C'*** WARNING - ERRORS ***'                            
         XC    P+4(4),P+4          DELETE PRD CODE                              
         GOTO1 SPOOL,DMCB,(R8)                                                  
SUM12    LA    R3,80(R3)                                                        
         BCT   R4,SUM10                                                         
         XC    SUMCNTR,SUMCNTR                                                  
SUMX     B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* DATAMGR WRITE TO DIRECTORY                                                    
WRTDIR   NTR1                                                                   
         GOTO1 DATAMGR,DMCB,=C'DMWRT',FILENAME,KEY,KEY                          
         B     EXIT                                                             
*                                                                               
* DATAMGR GETREC                                                                
IOGET    NTR1                                                                   
         GOTO1 DATAMGR,DMCB,(X'08',=C'GETREC'),FILENAME,               X        
               KEY+14,AIO,(0,DMWORK)                                            
         B     EXIT                                                             
*                                                                               
IOPUT    NTR1                                                                   
         GOTO1 DATAMGR,DMCB,=C'PUTREC',FILENAME,KEY+14,AIO,(0,DMWORK)           
         B     EXIT                                                             
*                                                                               
SPOOLIT  NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                                                             
         SPACE                                                                  
*                                                                               
         GETEL R2,DATADISP,ELCODE                                               
         EJECT                                                                  
*                                                                               
HDRTN    NTR1                                                                   
         L     R4,ATWA                                                          
         USING T31EFFD,R4                                                       
         MVC   H1(12),=C'NETWORK T.V.'                                          
         MVC   H3(6),=C'CLIENT'                                                 
         MVC   H3+10(3),SPLCLI                                                  
         MVC   H3+15(20),SPLCLIN                                                
         MVC   H4(7),=C'PRODUCT'                                                
         MVC   H4+10(3),SPLPRO                                                  
         MVC   H4+15(20),SPLPRON                                                
         MVC   H5(8),=C'ESTIMATE'                                               
         MVC   H5+10(3),SPLEST                                                  
         MVC   H5+15(20),SPLESTN                                                
*                                                                               
         CLI   SWIT,0                                                           
         BE    HDHK1A                                                           
         CLI   SWIT,C'E'                                                        
         BNE   *+12                                                             
         BAS   RE,ESTHEAD                                                       
         B     HDHK1A                                                           
         CLI   SWIT,C'B'                                                        
         BNE   *+12                                                             
         BAS   RE,BILLHEAD                                                      
         B     HDHK1A                                                           
         CLI   SWIT,C'S'                                                        
         BNE   *+12                                                             
         BAS   RE,STABHEAD                                                      
         B     HDHK1A                                                           
         CLI   SWIT,C'U'                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,UNTHEAD                                                       
*                                                                               
HDHK1A   DS    0H                                                               
         CLI   BOXSET,C'Y'          SET PARAMS FOR BOXES                        
         BE    HDX                                                              
         MVI   BOXSET,C'Y'                                                      
         L     R1,ABOX                                                          
         USING BOXD,R1                                                          
         LTR   R1,R1               IS ABOX ZEROS                                
         BZ    HDX                 YES/ ON-LINE SKIP BOXES                      
         MVC   BOXCOLS,SPACES                                                   
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0           (ONLY FOR SPOOF)                              
         SPACE                                                                  
         LA    R5,BOXCOLS                                                       
         USING BPLINED,R5                                                       
         SPACE                                                                  
         DROP  R4,R5                                                            
HDX      XIT1                                                                   
         EJECT                                                                  
*                                                                               
         SPACE 2                                                                
HEADING  SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,52,C' NETWORK UN-CLOSEOUT REPORT'                             
         SSPEC H2,52,C' --------------------------'                             
         SSPEC H1,93,AGYNAME                                                    
         SSPEC H2,93,AGYADD                                                     
         SSPEC H3,93,REPORT                                                     
         SSPEC H4,93,RUN                                                        
         SSPEC H5,103,PAGE                                                      
         DC    X'00'                                                            
         SPACE 2                                                                
*************************************                                           
* MANUAL(STAB) HEAD                                                             
*                                                                               
STABHEAD NTR1                                                                   
         MVC   P3,P                                                             
         XC    P,P                                                              
         MVC   P(22),=C'MANUAL BILLING RECORDS'                                 
         LA    R1,P2                                                            
         USING BPLINED,R1                                                       
         MVC   BPCLT(3),=C'CLT'                                                 
         MVC   BPPRD(3),=C'PRD'                                                 
         MVC   BPEST(3),=C'EST'                                                 
         MVC   BPINVO(4),=C'INVO'                                               
         MVC   BPDATE(4),=C'DATE'                                               
         MVC   BPAMT(3),=C'AMT'                                                 
         DROP  R1                                                               
         B     EXIT                                                             
         SPACE                                                                  
****************************************                                        
* HEADLINE FOR BILL RECORDS                                                     
*                                                                               
BILLHEAD NTR1                                                                   
         MVC   P4,P                                                             
         XC    P,P                                                              
         MVC   P(15),=C'BILLING RECORDS'                                        
         LA    R2,P2                                                            
         USING BPLINED,R2                                                       
         MVC   BPCLT+132,=C'CLT'                                                
         MVC   BPPRD+132,=C'PRD'                                                
         MVC   BPEST+132,=C'EST'                                                
         MVC   BPMNSRV(5),=C'MN/YR'                                             
         MVC   BPMNSRV+132(5),=C'SERVC'                                         
         MVC   BPMNYR(5),=C'MN/YR'                                              
         MVC   BPMNYR+132(4),=C'BILL'                                           
         MVC   BPNUM(4),=C'BILL'                                                
         MVC   BPNUM+132(3),=C'NUM'                                             
         MVC   BPINVO+132(5),=C'INVNO'                                          
         MVC   BPDATE(4),=C'BILL'                                               
         MVC   BPDATE+132(4),=C'DATE'                                           
         MVC   BPAMT+3(4),=C'BILL'                                              
         MVC   BPAMT+135(3),=C'AMT'                                             
         B     EXIT                                                             
         DROP  R2                                                               
         SPACE 2                                                                
****************************************                                        
* HEADLINE FOR UNIT PRINTOUT                                                    
*                                                                               
UNTHEAD  NTR1                                                                   
         MVC   P3,P                                                             
         XC    P,P                                                              
         MVC   P(12),=C'UNIT RECORDS'                                           
         LA    R2,P2                                                            
         USING UPLINED,R2                                                       
         MVC   UPCLT,=C'CLT'                                                    
         MVC   UPPRD,=C'PRD'                                                    
         MVC   UPEST,=C'EST'                                                    
         MVC   UPNET,=C'NTWK'                                                   
         MVC   UPPROG(4),=C'PROG'                                               
         MVC   UPDATE(4),=C'DATE'                                               
         MVC   UPACTUAL(7),=C'ORDERED'                                          
         MVC   UPINTEG(5),=C'INTEG'                                             
         MVC   UPASSGN(8),=C'ASSIGNED'                                          
         MVC   UPBILT(9),=C'BILL TIME'                                          
         MVC   UPBILI(9),=C'BILL INTG'                                          
         MVC   UPPAIDT(9),=C'PAID TIME'                                         
         MVC   UPPAIDI(9),=C'PAID INTG'                                         
         B     EXIT                                                             
         SPACE 2                                                                
*************************************                                           
* HEADLINE FOR ESTIMATE PRINTING                                                
*                                                                               
ESTHEAD  NTR1                                                                   
         MVC   P3,P                                                             
         XC    P,P                                                              
         MVC   P(15),=C'ESTIMATE HEADER'                                        
         LA    R2,P2                                                            
         USING BPLINED,R2                                                       
         MVC   BPCLT,=C'CLT'                                                    
         MVC   BPPRD,=C'PRD'                                                    
         MVC   BPEST,=C'EST'                                                    
         MVC   BPEST+7(4),=C'DATE'                                              
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
         SPACE 2                                                                
*                                                                               
*********************************************                                   
* PRINTS OUT AND CLEARS VARIOUS TOTAL FIELDS                                    
* ALSO CHKS IF 1ST READ TOTS=2ND READ TOTS                                      
*                                                                               
BREAKTOT NTR1                                                                   
*                                                                               
         MVC   P(11),=C'ESTIMATES ='                                            
         L     R2,ESTCNTR                                                       
         EDIT  (R2),(3,P+13),ALIGN=LEFT                                         
         BAS   RE,SPOOLIT                                                       
*                                                                               
         MVC   P(10),=C'PACKAGES ='                                             
         LA    R2,P+13                                                          
         L     R3,PKGCNTR                                                       
         EDIT  (R3),(6,0(R2)),ALIGN=LEFT                                        
         BAS   RE,SPOOLIT                                                       
         MVC   P(7),=C'UNITS ='                                                 
         LA    R2,P+13                                                          
         LA    R3,UNTCNTR2                                                      
         EDIT  (B4,0(R3)),(6,0(R2)),ALIGN=LEFT                                  
         BAS   RE,SPOOLIT                                                       
         MVC   P(12),=C'UNIT ORDERED'                                           
         LA    R2,P                                                             
         LA    R3,UNTORD2                                                       
         EDIT  (P8,0(R3)),(15,15(R2)),2,MINUS=YES                               
         BAS   RE,SPOOLIT                                                       
         MVC   P(12),=C'UNIT ASSIGND'                                           
         LA    R2,P                                                             
         LA    R3,UNTASSG2                                                      
         EDIT  (P8,0(R3)),(15,15(R2)),2                                         
         BAS   RE,SPOOLIT                                                       
         MVC   P(11),=C'UNIT BILLED'                                            
         LA    R2,P                                                             
         LA    R3,UNTBILL2                                                      
         EDIT  (P8,0(R3)),(15,15(R2)),2,MINUS=YES                               
         BAS   RE,SPOOLIT                                                       
         MVC   P(9),=C'UNIT PAID'                                               
         LA    R2,P                                                             
         LA    R3,UNTPAID2                                                      
         EDIT  (P8,0(R3)),(15,15(R2)),2                                         
         BAS   RE,SPOOLIT                                                       
*                                                                               
         MVC   P(11),=C'BILLED RECS'                                            
         LA    R2,P                                                             
         LA    R3,BLRECAM2                                                      
         EDIT  (P8,0(R3)),(15,15(R2)),2,MINUS=YES                               
         BAS   RE,SPOOLIT                                                       
         MVC   P(7),=C'BILLS ='                                                 
         L     R2,BILLCNT2                                                      
         EDIT  (R2),(4,P+13),ALIGN=LEFT                                         
         BAS   RE,SPOOLIT                                                       
         MVC   P(13),=C'MANUAL BILLED'                                          
         LA    R2,P                                                             
         LA    R3,STABAMT2                                                      
         EDIT  (P8,0(R3)),(15,15(R2)),2,MINUS=YES                               
         BAS   RE,SPOOLIT                                                       
         MVC   P(7),=C'MANUS ='                                                 
         L     R2,STABCNT2                                                      
         EDIT  (R2),(4,P+13),ALIGN=LEFT                                         
         BAS   RE,SPOOLIT                                                       
*                                                                               
BRKX     XIT1                                                                   
         EJECT                                                                  
***************************************                                         
* READS/WRITES/CLOSES OUT GOAL RECORDS                                          
*                                                                               
DELGOAL  NTR1                                                                   
         MVI   ESTADD,0                                                         
         MVI   SWIT,0                                                           
**       MVC   AIO,ANETWS1        SET I/O AREA FOR SPOT RECORDS                 
**       NETGO NVSETSPT,DMCB                                                    
**       MVC   FILENAME,=C'SPTDIR  '                                            
         BAS   R5,DOSETSPT                                                      
         L     R3,AIO                                                           
         USING GOALREC,R3                                                       
         XC    KEY,KEY                                                          
         MVI   KEY,X'02'                                                        
         MVC   KEY+1(3),ESTKEYSV+1        A/M/CLT                               
         MVC   KEY+7(1),ESTKEYSV+7        ESTIMATE                              
         MVC   KEYSAVE,KEY                                                      
         OI    DMINBTS,X'08'                                                    
         GOTO1 HIGH                                                             
         B     GL7                                                              
GOLSEQ   MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 SEQ                                                              
GL7      CLC   KEY(4),KEYSAVE             ID/A/M/CLT                            
         BNE   GOLX                                                             
         CLC   KEY+7(1),KEYSAVE+7          ESTIMATE                             
         BNE   GOLSEQ                                                           
         CLI   KEY+11,0                   MUST BE ACTIVE POINTER                
         BNE   GOLSEQ                                                           
         MVC   FILENAME,=C'SPTFIL  '                                            
         TM    KEY+13,X'C0'        ACCEPT ONLY CLOSED OUT REC                   
         BNO   GOLSEQ                                                           
         BAS   RE,IOGET                                                         
         CLI   UNDELSW,C'Y'                                                     
         BNE   GOLWRT                                                           
         XI    GCNTRLS,X'C0'                                                    
         BAS   RE,IOPUT                                                         
         XI    KEY+13,X'C0'                                                     
         MVC   FILENAME,=C'SPTDIR  '                                            
         BAS   RE,WRTDIR                                                        
GOLWRT   DS    0H                                                               
         LA    R2,P                                                             
         USING BPLINED,R2                                                       
         CLI   ESTADD,1                                                         
         BE    GL12                                                             
         MVI   ESTADD,1                                                         
         MVC   P(12),=C'GOAL RECORDS'                                           
         BAS   RE,SPOOLIT                                                       
GL12     XC    P,P                                                              
****     GOTO1 =V(CLUNPK),DMCB,KEY+2,BPCLT                                      
         GOTO1 NBCLUNPK,DMCB,KEY+2,BPCLT                                        
         MVC   WORK(1),KEY+4                                                    
         BAS   RE,GETPRD                                                        
         MVC   BPPRD,WORK+10                                                    
         EDIT  (B1,KEY+7),(3,BPEST)                                             
         MVC   BPMNSRV(1),KEY+8               DAYPART                           
         EDIT  (B1,KEY+9),(3,BPYRSRV)       SPOT LENGTH                         
         BAS   RE,SPOOLIT                                                       
         AP    TOTGOAL,=P'1'                                                    
         B     GOLSEQ                                                           
*                                                                               
GOLX     DS    0H                                                               
         NETGO NVSETUNT,DMCB                                                    
         XIT1                                                                   
         DROP  R2,R3                                                            
         EJECT                                                                  
********************************                                                
* RUN TOTALS ON LAST PAAGE                                                      
*                                                                               
ALLSUM   NTR1                                                                   
         MVI   SWIT,0              TURN OF OTHER HEADLINE PRINTING              
         XC    P,P                                                              
         XC    P2,P2                                                            
         MVI   FORCEHED,C'Y'                                                    
         MVC   P(10),=C'RUN TOTALS'                                             
         MVC   P+33(7),=C'ORDERED'                                              
         MVC   P+52(8),=C'ASSIGNED'                                             
         MVC   P+74(6),=C'BILLED'                                               
         MVC   P+96(4),=C'PAID'                                                 
         LA    R3,P2                                                            
**       EDIT  RUNORD,(15,25(R3)),2,MINUS=YES      UNTORD2                      
         MVC   WORK(18),=X'40202020202020202020202020214B202060'                
         ED    WORK(18),RUNORD                                                  
         MVC   25(15,R3),WORK+3                                                 
*                                                                               
***      EDIT  RUNPAID,(15,85(R3)),2,MINUS=YES     UNTPAID2                     
         MVC   WORK(18),=X'40202020202020202020202020214B202060'                
         ED    WORK(18),RUNPAID                                                 
         MVC   85(15,R3),WORK+3                                                 
         BAS   RE,SPOOLIT                                                       
*                                                                               
         MVC   P(10),=C'ESTIMATES='                                             
         EDIT  (P8,TOTEST),(10,P+11),ALIGN=LEFT                                 
         MVC   P2(9),=C'PACKAGES='                                              
         EDIT  (P8,TOTPKG),(10,P2+10),ALIGN=LEFT                                
         MVC   P3(6),=C'UNITS='                                                 
         EDIT  (P8,TOTUNT),(10,P3+10),ALIGN=LEFT                                
         MVC   P4(6),=C'BILLS='                                                 
         EDIT  (P8,TOTBILL),(10,P4+10),ALIGN=LEFT                               
         BAS   RE,SPOOLIT                                                       
         MVC   P(8),=C'MANUALS='                                                
         EDIT  (P8,TOTSTAB),(10,P+10),ALIGN=LEFT                                
         BAS   RE,SPOOLIT                                                       
         MVC   P(6),=C'GOALS='                                                  
         EDIT  (P8,TOTGOAL),(10,P+10),ALIGN=LEFT                                
         BAS   RE,SPOOLIT                                                       
         MVC   P(14),=C'UNIT BILL RECS'                                         
         L     R2,UNTBILR                                                       
         EDIT  (R2),(10,P+16),ALIGN=LEFT                                        
         XC    UNTBILR,UNTBILR                                                  
         BAS   RE,SPOOLIT                                                       
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
GETCLTRC NTR1                                                                   
***      MVC   AIO,ANETWS1        SET I/O AREA FOR SPOT RECORDS                 
***      NETGO NVSETSPT,DMCB                                                    
***      MVC   FILENAME,=C'SPTDIR  '                                            
         BAS   R5,DOSETSPT                                                      
         LA    R3,KEY                                                           
         USING CLTHDR,R3                                                        
         XC    KEY,KEY                                                          
         MVC   CKEYAM,NBACTAM                                                   
         MVC   CKEYCLT,NBACTCLI                                                 
         L     R3,AIO                                                           
         MVC   KEYSAVE,KEY                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FILENAME,=C'SPTFIL  '                                            
         BAS   RE,IOGET                                                         
         L     RF,ACLISTSV                                                      
         MOVE  ((RF),880),CLIST                                                 
         XIT1                                                                   
         DROP  R3                                                               
*                                                                               
DOSETSPT DS    0H                                                               
         MVC   AIO,ANETWS1                                                      
         NETGO NVSETSPT,DMCB                                                    
         MVC   FILENAME,=C'SPTDIR  '                                            
         BR    R5                                                               
*                                                                               
       EJECT                                                                    
       LTORG                                                                    
       EJECT                                                                    
*                                                                               
WORKD    DSECT                     MYWORK AREA  ANETWS4                         
*                            * THESE ARE RECIEVED FROM EDIT MOFULE              
ESTKEYSV DS    CL13          *                                                  
ESTBSV   DS    CL1           *                                                  
UNDELSW  DS    CL1           *                                                  
RELO     DS    F             *                                                  
ACLISTSV DS    F             *                                                  
*                                                                               
ASUMRECD DS    F                                                                
ADRSUMRC DS    F                                                                
ESTCNTR  DS    F                   COUNTER FOR ESTIMATE RECORDS                 
UNTCNTR  DS    F                   COUNTER FOR UNIT RECORDS                     
UNTCNTR2 DS    F                   COUNTER FOR UNIT RECORDS                     
BILLCNTR DS    F                   COUNTER FOR BILLING RECORDS                  
BILLCNT2 DS    F                   COUNTER FOR BILLING RECORDS                  
STABCNTR DS    F                                                                
STABCNT2 DS    F                   COUNTER FOR SPGENSTAB RECORDS                
SUMCNTR  DS    F                   COUNTER FOR SUMMARY RECAP RECS               
AP       DS    F                                                                
ABRKTOT  DS    F                                                                
AERRORS  DS    F                                                                
APKGDNR  DS    F                                                                
AGOALDNR DS    F                                                                
AALLSUM  DS    F                                                                
PKGCNTR  DS    F                   COUNTER FOR PACKAGE RECORDS                  
UNTBILR  DS    F                   NEW UNIT BILLING RECORDS                     
NEWESTK  DS    CL13                                                             
NEWESTB  DS    CL1                                                              
*                                                                               
ERRORSW  DS    CL1                                                              
FRST     DS    CL1                                                              
SWIT     DS    CL1                                                              
BOXSET   DS    CL1                                                              
PAIDSW   DS    CL1                                                              
BILLSW   DS    CL1                                                              
BPRDSV   DS    CL1                                                              
PRDSV    DS    CL3                                                              
ESTBYRS  DS    CL1                 ESTIMATE START YEAR                          
ESTBMNS  DS    CL1                 ESTIMATE START MONTH                         
ESTBYRE  DS    CL1                 ESTIMATE END YEAR                            
ESTBMNE  DS    CL1                 ESTIMATE END MONTH                           
BYEAR    DS    CL1                 REQUEST END YEAR                             
BMNTH    DS    CL1                 REQUEST END MONTH                            
ASSIGNB  DS    CL1                 BILLING ON ASIGNED                           
ESTWARN  DS    CL4                                                              
NXTOFFCD DS    CL1                                                              
NEWCLI   DS    CL1                                                              
ESTHDERR DS    CL1                                                              
ESTADD   DS    CL1                                                              
SAVEKEY  DS    CL32                                                             
SAVESTK  DS    CL20                                                             
CURREST  DS    CL1                                                              
CURRCLI  DS    CL2                                                              
*                                                                               
*                   TOTALS TAKEN FIRST TIME RECS ARE READ                       
*                     UNITS                                                     
UNTORD   DS    PL8       ORDERED UNIT TOTAL                                     
UNTORD2  DS    PL8       ORDERED UNIT TOTAL AT DELETE TIME                      
UNTBILL  DS    PL8       BILLED UNIT TOTAL                                      
UNTBILL2 DS    PL8       BILLED UNIT TOTAL AT DELETE TIME                       
UNTPAID  DS    PL8       PAID UNIT TOTAL                                        
UNTPAID2 DS    PL8       PAID UNIT TOTAL AT DELETE TIME                         
UNTASSGN DS    PL8                                                              
UNTASSG2 DS    PL8                                                              
*                     BILL RECS                                                 
BLRECAMT DS    PL8       BILL REC AMOUNT TOTAL                                  
BLRECAM2 DS    PL8       BILL REC AMOUNT TOTAL AT DELETE TIME                   
*                     SPGENSTAB                                                 
STABAMT  DS    PL8           SPGENSTAB TOTAL                                    
STABAMT2 DS    PL8           SPGENSTAB TOTAL AT DELETE TIME                     
*                                                                               
RUNTOTS  DS    PL8   *****  TOTALS FOR ENTIRE RUN  ****                         
RUNORD   DS    PL8                 ORDERED                                      
RUNASS   DS    PL8                 ASSIGNED                                     
RUNBILL  DS    PL8                 BILLED                                       
RUNPAID  DS    PL8                 PAID                                         
TOTEST   DS    PL8                                                              
TOTPKG   DS    PL8                                                              
TOTUNT   DS    PL8                                                              
TOTBILL  DS    PL8                                                              
TOTSTAB  DS    PL8                                                              
TOTGOAL  DS    PL8                                                              
*                                                                               
         EJECT                                                                  
*                                                                               
BPLINED  DSECT               BILL REC DSECT FOR PRINTING                        
         DS    CL1                                                              
BPCLT    DS    CL3                                                              
         DS    CL1                                                              
BPPRD    DS    CL3                                                              
         DS    CL1                                                              
BPEST    DS    CL3                                                              
         DS    CL1                                                              
BPMNSRV  DS    CL2                                                              
         DS    CL1                                                              
BPYRSRV  DS    CL2                                                              
         DS    CL4                                                              
BPMNYR   DS    CL4                                                              
         DS    CL4                                                              
BPNUM    DS    CL6                                                              
         DS    CL4                                                              
BPINVO   DS    CL6                                                              
         DS    CL4                                                              
BPDATE   DS    CL6                                                              
         DS    CL4                                                              
BPAMT    DS    CL11                                                             
         EJECT                                                                  
*                                                                               
UPLINED  DSECT               UNIT REC DSECT FOR PRINTING                        
         DS    CL1                                                              
UPCLT    DS    CL3                                                              
         DS    CL1                                                              
UPEST    DS    CL3                                                              
         DS    CL1                                                              
UPNET    DS    CL4                                                              
         DS    CL1                                                              
UPPRD    DS    CL3                                                              
         DS    CL1                                                              
UPPAK    DS    CL3                                                              
         DS    CL1                                                              
UPPROG   DS    CL6                                                              
         DS    CL1                                                              
UPDATE   DS    CL8                                                              
         DS    CL1                                                              
UPSUBLN  DS    CL6                                                              
         DS    CL1                                                              
UPACTUAL DS    CL10                ORDERED                                      
         DS    CL1                                                              
UPINTEG  DS    CL10                INTEGRATION                                  
         DS    CL1                                                              
UPASSGN  DS    CL10                ASSIGNED                                     
         DS    CL1                                                              
UPBILT   DS    CL10                BILLED TIME                                  
         DS    CL1                                                              
UPBILI   DS    CL10                BILLED INTEGRATION                           
         DS    CL1                                                              
UPPAIDT  DS    CL10                PAID TIME                                    
         DS    CL1                                                              
UPPAIDI  DS    CL10                PAID INTEGRATION                             
         EJECT                                                                  
ESVRECD  DSECT       *** NOT USED*** DESECT FOR SAVED ESTIMATE INFO             
ESVCLT   DS    CL3                                                              
ESVPRD   DS    CL3                                                              
ESVEST   DS    CL3                                                              
ESVDATE  DS    CL13                                                             
ESVLNE   EQU   *-ESVCLT                                                         
         EJECT                                                                  
         SPACE                                                                  
       ++INCLUDE NETINCLS                                                       
       ++INCLUDE NENETGOALD                                                     
         EJECT                                                                  
* INCLUDE NEMEDFFD                                                              
* INCLUDE NEMEDDDD                                                              
* INCLUDE DDBIGBOX                                                              
         PRINT OFF                                                              
       ++INCLUDE NEMEDFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDDDD                                                       
       ++INCLUDE DDBIGBOX                                                       
         EJECT                                                                  
PRDHD    DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
         EJECT                                                                  
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
       ++INCLUDE SPGENGOAL                                                      
         EJECT                                                                  
BILLRECD DSECT                                                                  
       ++INCLUDE SPGENBILL                                                      
       ++INCLUDE SPGENSTAB                                                      
         EJECT                                                                  
       ++INCLUDE NEGENUNIT                                                      
         EJECT                                                                  
       ++INCLUDE NEGENPACK                                                      
       ++INCLUDE SPSTAPACKD                                                     
       ++INCLUDE SPGENAGY                                                       
       ++INCLUDE NEGENUBILL                                                     
       PRINT ON                                                                 
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012NEMEDCE   06/11/04'                                      
         END                                                                    
