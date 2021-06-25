*          DATA SET NESFM11    AT LEVEL 117 AS OF 10/31/05                      
*PHASE T31C11A,*                                                                
         TITLE 'T31C11  CHANGE LOG'                                 L01         
*                                                                   L01         
* BPLA  6/97   IF ACCOUNTS NOT INPU (AND NOT REQUIRED)                          
*              SKIP CHECK OF ACCOUNT FILE                                       
*                                                                               
* BPLA 5/1/91  CODE ADDED FOR KILL DATE AND GST CODE                            
*              R7 USED AS 2ND BASE REGISTER                                     
*                                                                               
* BPLA 8/29/90 DISPLAY NEGATIVE AOR PCTS CORRECTLY                              
*              FLOAT=- ADDED TO AORPCT EDITS                                    
*                                                                               
* BPLA 6/11/90 DISALLOW DELETE IF BILLING EXISTS FOR AOR PRD                    
*                                                                               
* ROSA 5/18/89 PROVIDE ABILITY TO PRINT REPORT                      L01         
*                                                                   L01         
         TITLE 'T31C11  AOR INFORMATION RECORDS'                                
T31C11   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T31C11                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         LA    R7,1(RB)                                                         
         LA    R7,4095(R7)                                                      
         USING T31C11+4096,R7                                                   
*                                                                               
         BAS   RE,SETSPOT          SET FOR SPTFILE                              
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,PRINTREP       PRINT REPORT                   L01           
         BE    PR                                                 L01           
         CLI   MODE,RECDEL                                                      
         BNE   EXIT                                                             
* READ BILLING RECORD                                                           
*                                                                               
*  NOTE.. KEY CONTAINS AOR KEY TO BE DELETED                                    
*                                                                               
*                                                                               
**       =====           **                                                     
CKBIL    DS    0H                                                               
         LA    R6,KEY                                                           
         USING BILLREC,R6                                                       
         MVC   WORK(L'KEY),KEY   FIRST SAVE KEY                                 
**       =====           **                                                     
         XC    KEY,KEY                                                          
         MVC   BKEYAM,BAGYMD                                                    
         MVC   BKEYCLT,WORK+3      CLIENT FROM AOR REC                          
         MVC   BKEYPRD,WORK+5      PRODUCT FROM AOR REC                         
         CLI   WORK+8,X'FF'    IS THIS AN ALL ESTIMATE AOR                      
         BE    CKBIL5           CONTAINS REAL EST NUMBER                        
**       NOTE THAT ESTIMATE IN AOR REC IS 2 BYTES (X'FFFF' = ALL)               
**       REAL EST IS IN LAST BYTE                                               
         MVC   BKEYEST,WORK+9    SET EST IN KEY                                 
*                                                                               
CKBIL5   GOTO1 HIGH                                                             
         B     CKBIL10                                                          
*                                                                               
CKBIL6   GOTO1 SEQ                                                              
*                                                                               
CKBIL10  CLC   KEYSAVE(7),KEY     FIRST CHK TRROUGH PRD                         
         BNE   CKBIL20                                                          
*                                                                               
         OC    KEYSAVE+7(1),KEYSAVE+7   SEE IF DELETING EST AOR                 
         BZ    CKBIL12                                                          
         CLC   KEYSAVE+7(1),KEY+7                                               
         BNE   CKBIL20                                                          
CKBIL12  OC    KEY+8(4),KEY+8       SEE IF BILLREC                              
         BZ    CKBIL6               NO - KEEP LOOKING                           
*                                                                               
* CANNOT DELETE                                                                 
*                                                                               
CKBIL15  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(37),=C'CANNOT DELETE // BILL RECORDS PRESENT'            
         GOTO1 ERREX2                                                           
*                                                                               
CKBIL20  MVC   KEY,WORK      RESTORE KEY                                        
         GOTO1 HIGH                                                             
*                                                                               
EXIT     XIT1                                                                   
         SPACE 3                                                                
*        SETSPOT- SET VALUES FOR SPOT FILE READS                                
         SPACE 2                                                                
SETSPOT  DS    0H                                                               
         MVC   LKEY,=H'13'         SET FOR SPOT FILE READS                      
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'24'                                                  
         MVC   SYSFIL,=C'SPTFILE '                                              
         MVC   SYSDIR,=C'SPTDIR  '                                              
         MVI   USEIO,0                                                          
         XC    FILENAME,FILENAME                                                
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
*     VALIDATE KEY ROUTINE                                                      
*                                                                               
VK       DS    0H                                                               
         LA    R6,SVKEY                                                         
         USING AORKEY,R6                                                        
         XC    SVKEY,SVKEY                                                      
         MVC   SVKEY(2),=X'0D45'                                                
*                                                                               
         LA    R2,AGRMEDH          MEDIA                                        
         GOTO1 VALIMED                                                          
         MVC   AORKAGMD,BAGYMD                                                  
*                                                                               
         LA    R2,AGRCLIH          CLIENT                                       
         XC    SVCLT,SVCLT                                                      
         CLI   5(R2),0             CHK FOR INPUT                                
         BNE   VK2                                                              
         CLI   ACTNUM,ACTLIST      NO INPUT OK FOR LIST                         
         BE    VK3                                                              
         CLI   ACTNUM,ACTREP       NO INPUT OK FOR REPORT                       
         BE    VK3                                                              
         CLI   ACTNUM,X'11'        OK IF NOW                                    
         BE    VK3                                                              
VK2      GOTO1 VALICLT                                                          
         MVC   AORKCLT,BCLT                                                     
         MVC   SVCLT,BCLT                                                       
         L     R5,AIO                                                           
         USING CLTHDRD,R5                                                       
         MVC   SVCOFF,COFFICE      SAVE OFFICE FOR PROFILE READ                 
         DROP  R5                                                               
*                                                                               
VK3      XC    SVPRD,SVPRD                                                      
         LA    R2,AGRPROH          PRODUCT                                      
         CLI   5(R2),0             TEST ANY INPUT                               
         BNE   VK4                                                              
         CLI   ACTNUM,ACTLIST      NO, OK IF LIST                               
         BE    VK20                                                             
         CLI   ACTNUM,ACTREP       NO, OK IF REPORTING                          
         BE    VK20                                                             
         CLI   ACTNUM,X'11'        OK IF NOW                       L01          
         BE    VK20                                                L01          
*                                                                               
VK4      DS    0H                                                               
         GOTO1 VALIPRD                                                          
         MVC   AORKPRD,QPRD                                                     
         MVC   SVPRD,QPRD                                                       
*                                                                               
         XC    SVEST,SVEST                                                      
         LA    R2,AGRESTH          ESTIMATE                                     
         CLI   5(R2),0             TEST ANY INPUT                               
         BNE   VK4AA                                                            
         CLI   ACTNUM,ACTLIST     IF LISTING THEN LEAVE SVEST AS O              
         BE    VK5                                                              
         CLI   ACTNUM,ACTREP      IF REPORTING THEN LEAVE AS 0                  
         BE    VK5                                                              
*                                                                               
VK4A     MVC   AORKEST,=X'FFFF'                                                 
         MVC   SVEST,AORKEST                                                    
         B     VK5                                                              
*                                                                               
VK4AA    CLC   8(3,R2),=C'ALL'                                                  
         BE    VK4A               TREAT LIKE NO INPUT                           
         GOTO1 VALIEST                                                          
         MVI   AORKEST,0                                                        
         MVC   AORKEST+1(1),BEST                                                
         MVC   SVEST+1(1),BEST                                                  
*                                                                               
*                                                                               
VK5      DS    0H                                                               
         LA    R2,AGRDPTH          DAYPART                                      
         XC    SVDPT,SVDPT                                                      
         CLI   5(R2),0             SEE IF INPUT                                 
         BNE   VK5AA                                                            
         CLI   ACTNUM,ACTLIST       IF LISTING LEAVE AS O                       
         BE    VK6                                                              
         CLI   ACTNUM,ACTREP        IF REPORTING LEAVE AS 0                     
         BE    VK6                                                              
*                                                                               
VK5A     MVI   AORKDPT,X'FF'                                                    
         MVI   SVDPT,X'FF'                                                      
         B     VK6                                                              
*                                                                               
VK5AA    CLC   8(3,R2),=C'ALL'                                                  
         BE    VK5A                                                             
         GOTO1 ANY                                                              
         MVC   AORKDPT,WORK                                                     
         MVC   SVDPT,AORKDPT                                                    
*                                                                               
VK6      DS    0H                                                               
         XC    SVSTYP,SVSTYP                                                    
         LA    R2,AGRSTPH          STATION TYPE                                 
         CLI   5(R2),0             SEE IF INPUT                                 
         BNE   VK6AA                                                            
         CLI   ACTNUM,ACTLIST     IF LISTING THEN LEAVE AS O                    
         BE    VK20                                                             
         CLI   ACTNUM,ACTREP      IF REPORTING THEN LEAVE AS O                  
         BE    VK20                                                             
*                                                                               
VK6A     MVI   AORKSTYP,X'FF'                                                   
         MVI   SVSTYP,X'FF'                                                     
         B     VK20                                                             
*                                                                               
VK6AA    CLC   8(3,R2),=C'ALL'                                                  
         BE    VK6A              TREAT LIKE NO INPUT                            
         GOTO1 ANY                                                              
         MVC   AORKSTYP,WORK                                                    
         MVC   SVSTYP,AORKSTYP                                                  
*                                                                               
VK20     DS    0H                                                               
         MVI   AORKEXT+2,X'FF'     REST OF KEY FF'S                             
         MVI   AORKEXT+3,X'FF'                                                  
         MVC   KEY(13),SVKEY       SET KEY                                      
*                                                                               
VK900    B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* DISPLAY KEY                                                                   
*                                                                               
DK       DS    0H                                                               
         BAS   RE,CLT0J            CONVERT 0/J CLIENT CODE                      
*                                                                               
         L     R6,AIO                                                           
         USING AORREC,R6                                                        
*                                                                               
         LA    R2,AGRCLIH                                                       
         GOTO1 CLUNPK,DMCB,(X+50,AORKCLT),AGRCLI                                
         OC    AGRCLI,SPACES                                                    
         FOUT  AGRCLIH                                                          
*                                                                               
         LA    R2,AGRPROH                                                       
         MVC   AGRPRO,SPACES                                                    
         MVC   AGRPRO(3),AORKPRD                                                
         FOUT  AGRPROH                                                          
*                                                                               
         MVC   AGREST(3),=C'ALL'                                                
         CLC   AORKEST,=X'FFFF'                                                 
         BE    DK04                                                             
         ZIC   R0,AORKEST+1                                                     
         EDIT  (R0),(3,AGREST),FILL=0                                           
DK04     DS    0H                                                               
         FOUT  AGRESTH                                                          
*                                                                               
         MVC   AGRDPT,SPACES                                                    
         MVC   AGRDPT(1),AORKDPT                                                
         CLI   AORKDPT,X'FF'                                                    
         BNE   *+10                                                             
         MVC   AGRDPT(3),=C'ALL'                                                
         FOUT  AGRDPTH                                                          
*                                                                               
         MVC   AGRSTP,SPACES                                                    
         MVC   AGRSTP(1),AORKSTYP                                               
         CLI   AORKSTYP,X'FF'                                                   
         BNE   *+10                                                             
         MVC   AGRSTP(3),=C'ALL'                                                
         FOUT  AGRSTPH                                                          
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* VALIDATE RECORD                                                               
*                                                                               
VR       DS    0H                                                               
*                                                                               
         MVC   SVKEY,KEY           SAVE THE RECORD KEY                          
         MVI   ELCODE,X'02'        REMOVE ADDRESS ELEM                          
         GOTO1 REMELEM                                                          
         XC    ELEM,ELEM           BUILD NEW ELEMENTS                           
         LA    R6,ELEM                                                          
         USING AORADREL,R6                                                      
         MVI   ELEM,X'02'          ELEM CODE                                    
         MVI   ELEM+1,130          LENGTH                                       
*                                                                               
         LA    R3,AORLIN1                                                       
         LA    R2,AGRLIN1H                                                      
         LA    R4,4                                                             
*                                                                               
VR4      DS    0H                                                               
         MVC   0(30,R3),8(R2)                                                   
         OC    0(30,R3),SPACES                                                  
*                                                                               
         LA    R3,30(R3)           NEXT LINE                                    
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BCT   R4,VR4                                                           
*                                                                               
         GOTO1 ADDELEM                                                          
*                                                                               
         MVI   ELCODE,X'03'        REMOVE AOR INFO ELEM                         
         GOTO1 REMELEM                                                          
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING AORELEM,R6                                                       
         MVI   ELEM,X'03'          ELEM CODE                                    
         MVI   ELEM+1,60           LENGTH                                       
*                                                                               
         LA    R2,AGRPCTH                                                       
         ZIC   R3,5(R2)            LENGTH                                       
         GOTO1 CASHVAL,DMCB,(4,8(R2)),(R3)                                      
         CLI   DMCB,X'FF'                                                       
         BE    VR99                                                             
         MVC   AORPCT,DMCB+4                                                    
*                                                                               
         LA    R2,AGRBASH          BASIS                                        
         MVI   AORBAS,0                                                         
         CLI   5(R2),0                                                          
         BE    VR5                                                              
         CLI   8(R2),C'G'                                                       
         BE    VR5                                                              
         MVI   AORBAS,X'80'                                                     
         CLI   8(R2),C'A'                                                       
         BE    VR5                                                              
         CLI   8(R2),C'N'                                                       
         BNE   VR99                                                             
         MVI   AORBAS,X'40'                                                     
*                                                                               
VR5      LA    R2,AGREFDH          EFFECTIVE DATE                               
         CLI   5(R2),0             TEST ANY                                     
         BE    VR6                                                              
         GOTO1 DATVAL,DMCB,(2,8(R2)),DUB                                        
         OC    DMCB(4),DMCB                                                     
         BZ    VR99                                                             
         MVC   DUB+4(2),=C'01'                                                  
         GOTO1 DATCON,DMCB,DUB,(3,WORK)                                         
         MVC   AOREFF,WORK                                                      
*                                                                               
*                                                                               
VR6      LA    R2,AGRKDTH          KILL DATE                                    
         CLI   5(R2),0             TEST ANY                                     
         BE    VR7                                                              
         GOTO1 DATVAL,DMCB,(2,8(R2)),DUB                                        
         OC    DMCB(4),DMCB                                                     
         BZ    VR99                                                             
         MVC   DUB+4(2),=C'01'                                                  
         GOTO1 DATCON,DMCB,DUB,(3,WORK)                                         
         MVC   AORKILL,WORK                                                     
*                                                                               
VR7      LA    R2,AGRGSTH          GST CODE                                     
         CLI   5(R2),0               ANY INPUT                                  
         BE    VR9                                                              
         CLI   5(R2),1                                                          
         BNE   VR99                                                             
         LA    R3,GSTTAB                                                        
VR7C     CLI   0(R3),X'FF'         END OF TABLE                                 
         BE    VR99                                                             
         CLC   0(1,R3),8(R2)                                                    
         BE    VR7E                                                             
         LA    R3,1(R3)                                                         
         B     VR7C                                                             
*                                                                               
VR7E     MVC   AORGSTCD,8(R2)                                                   
*                                                                               
VR9      DS    0H                                                               
*                                  READ B2A BILLING PROFILE TO SEE              
*                                  IF ACCOUNTS REQUIRED                         
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'SB2A'                                                 
         NI    WORK,X'BF'         MUST MAKE SYSTEM LOWER CASE                   
*                                 NEEDED FOR PROFILES WITH A SUFFIX             
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),AGRMED                                                 
         MVC   WORK+7(3),QCLT       EBCIDIC CLIENT                              
         CLI   SVCOFF,C' '                                                      
         BNH   *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCOFF                                                
         GOTO1 GETPROF,DMCB,WORK,B2APROF,DATAMGR                                
*                                                                               
*                                  GET BAB PROFILE FOR ACCSYS/COMP              
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'SBAB'                                                 
         NI    WORK,X'BF'         MUST MAKE SYSTEM LOWER CASE                   
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),AGRMED                                                 
         MVC   WORK+7(3),QCLT       EBCIDIC CLIENT                              
         CLI   SVCOFF,C' '                                                      
         BNH   *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCOFF                                                
         GOTO1 GETPROF,DMCB,WORK,BABPROF,DATAMGR                                
*                                                                               
         LA    R2,AGRRCAH                                                       
         MVC   AORRCVBL,8(R2)                                                   
         OC    AORRCVBL,SPACES                                                  
         CLI   B2APROF+2,C'Y'      SEE IF INPUT REQUIRED                        
         BNE   VR9C                                                             
         CLI   5(R2),0                                                          
         BNE   VR9C                                                             
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
VR9C     DS    0H                                                               
         CLI   5(R2),0                                                          
         BE    VR9D                                                             
         LA    R3,AORRCVBL                                                      
         BAS   RE,CHKACCT          FIND ON ACCOUNT FILE                         
         CLI   ERROR,0                                                          
         BNE   VR99C                                                            
*                                                                               
VR9D     LA    R2,AGRCMAH                                                       
         MVC   AORCOMM,8(R2)                                                    
         OC    AORCOMM,SPACES                                                   
*                                                                               
         CLI   B2APROF+2,C'Y'      SEE IF ACCOUNTS REQUIRED                     
         BNE   VR9G                                                             
         CLI   5(R2),0             CHK FOR INPUT                                
         BNE   VR9G                                                             
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
VR9G     DS    0H                                                               
         CLI   5(R2),0            ANY INPUT?                                    
         BE    VR9H                                                             
         LA    R3,AORCOMM                                                       
         BAS   RE,CHKACCT          FIND ON ACCOUNT FILE                         
         CLI   ERROR,0                                                          
         BNE   VR99C                                                            
*                                                                               
VR9H     GOTO1 ADDELEM                                                          
         B     VR900                                                            
*                                                                               
VR99     DS    0H                                                               
         MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
VR99C    DS    0H                                                               
         CLI   ERROR,X'FF'         TEST ERROR ALREADY SHOWN                     
         BNE   TRAPERR             NO- LET GENCON DO IT                         
         GOTO1 ERREX2                                                           
*                                                                               
VR900    B     EXIT                                                             
*                                                                               
GSTTAB   DC    C'SUXZ',X'FF'                                                    
         EJECT                                                                  
* DISPLAY RECORD                                                                
DR       DS    0H                                                               
         LA    R2,AGRLIN1H         CLEAR SCREEN                                 
         BAS   RE,CLRSCRN                                                       
*                                                                               
         L     R6,AIO                                                           
         USING AORREC,R6                                                        
*                                                                BUG01          
         USING AORADREL,R6                                                      
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BNE   DR10                                                             
         FOUT  AGRLIN1H,AORLIN1                                                 
         FOUT  AGRLIN2H,AORLIN2                                                 
         FOUT  AGRLIN3H,AORLIN3                                                 
         FOUT  AGRLIN4H,AORLIN4                                                 
*                                                                               
DR10     DS    0H                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'03'        AOR INFO ELEM                                
         BAS   RE,GETEL                                                         
         BNE   DR20                                                             
         USING AORELEM,R6                                                       
*                                                                               
         EDIT  (B4,AORPCT),(10,X),4,ALIGN=LEFT,FLOAT=-                          
         MVC   AGRPCT,X                                                         
         FOUT  AGRPCTH                                                          
*                                                                               
         MVC   AGRBAS(2),=C'G '                                                 
         CLI   AORBAS,0                                                         
         BE    DR15                                                             
         MVC   AGRBAS(2),=C'A '                                                 
         CLI   AORBAS,X'80'                                                     
         BE    DR15                                                             
         MVC   AGRBAS(2),=C'N '                                                 
         CLI   AORBAS,X'40'                                                     
         BE    DR15                                                             
         MVC   AGRBAS(2),=C'XX'                                                 
DR15     FOUT  AGRBASH                                                          
*                                                                               
         MVC   AGREFD,SPACES                                                    
         OC    AOREFF,AOREFF    SEE IF I HAVE A DATE                            
         BZ    DR15C                                                            
         GOTO1 DATCON,DMCB,(3,AOREFF),(9,X)                                     
         MVC   AGREFD(6),X                                                      
DR15C    FOUT  AGREFDH                                                          
*                                                                               
         MVC   AGRKDT,SPACES                                                    
         OC    AORKILL,AORKILL  SEE IF I HAVE A DATE                            
         BZ    DR17C                                                            
         GOTO1 DATCON,DMCB,(3,AORKILL),(9,X)                                    
         MVC   AGRKDT(6),X                                                      
DR17C    FOUT  AGRKDTH                                                          
*                                                                               
         FOUT  AGRGSTH,AORGSTCD,1                                               
*                                                                               
         FOUT  AGRRCAH,AORRCVBL                                                 
         FOUT  AGRCMAH,AORCOMM                                                  
         OI    AGRRCAH+4,X'20'     SET VALIDATED                                
         OI    AGRCMAH+4,X'20'                                                  
*                                                                               
DR20     DS    0H                                                               
DR900    B     EXIT                                                             
         SPACE 3                                                                
*        CHKACCT- READ ACCT FILE                                                
         SPACE 2                                                                
CHKACCT  NTR1                                                                   
         MVI   ERROR,0                                                          
         TM    4(R2),X'20'         TEST NEEDS VALIDATION                        
         BNZ   CHKAX                                                            
*                                  SWITCH TO ACC SYS                            
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,1,BABPROF+2      TEST HAVE ALTERNATE ACCSYS                   
         BZ    CHKA3               NO DO NORMAL SWITCH                          
         GOTO1 (RF),DMCB,((R0),0),0   ELSE SWITHC DIRECTLY                      
         MVC   SVCOMP,BABPROF+5    COMPANY CODE FROM PROF                       
         B     CHKA3D                                                           
*                                                                               
CHKA3    DS    0H                                                               
         GOTO1 (RF),DMCB,=C'ACC',0                                              
         MVC   SVCOMP,0(R1)        SAVE COMPANY CODE                            
*                                                                               
CHKA3D   DS    0H                                                               
         CLI   4(R1),2             TEST NOT OPEN                                
         BNE   CHKA3F                                                           
*                                                                               
         MVI   ERROR,X'FF'                                                      
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(30),=C'**ACCOUNTING SYSTEM NOT OPEN**'                   
         B     CHKA8                                                            
*                                                                               
CHKA3F   DS    0H                                                               
         CLI   4(R1),1             TEST AUTHORIZED                              
         BNE   CHKA4                                                            
*                                                                               
         MVI   ERROR,X'FF'                                                      
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(36),=C'**ACCOUNTING SYSTEM NOT AUTHORIZED**'             
         B     CHKA8                                                            
*                                                                               
CHKA4    DS    0H                                                               
         CLI   4(R1),0             OTHER ERRORS FATAL                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CHKA6    DS    0H                                                               
         MVC   WORK,SPACES                                                      
         MVC   WORK(1),SVCOMP      COMPANY CODE                                 
         MVC   WORK+1(14),0(R3)                                                 
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'ACCOUNT',WORK,AIO2                    
         L     RE,AIO2                                                          
         CLC   0(42,RE),WORK                                                    
         BE    *+12                                                             
         MVI   ERROR,NOTFOUND                                                   
         B     CHKA8                                                            
         OI    4(R2),X'20'         SET ACCT VALIDATED                           
*                                                                               
CHKA8    DS    0H                                                               
         L     RF,ACOMFACS         SWITCH BACK TO NETWORK                       
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,=C'NET',0                                              
         CLI   4(R1),0             ALL ERRORS FATAL                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CHKAX    DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
* LIST RECORDS                                                                  
*          DATA SET SPSFM11    AT LEVEL 011 AS OF 11/14/89                      
*                                                                               
LR       DS    0H                                                               
         LA    R6,KEY                                                           
         USING AORREC,R6                                                        
         MVC   AIO,AIO1                                                         
         OC    KEY,KEY             TEST FIRST TIME                              
         BNZ   LR010                                             BOG01          
*                                                                               
         MVC   AORKTYP,=X'0D45'                                     L01         
         MVC   AORKAGMD,BAGYMD                                                  
         MVC   AORKCLT,SVCLT                   CLIENT              L01          
         MVC   AORKPRD,SVPRD                                      L01           
         MVC   AORKEST,SVEST                                      L01           
         MVC   AORKDPT,SVDPT                                      L01           
         MVC   AORKSTYP,SVSTYP                                    L01           
*                                                                               
LR010    GOTO1 HIGH                                                             
         B     LR030                                                            
*                                                                               
LR020    GOTO1 SEQ                                                              
*                                                                               
LR030    CLC   KEY(3),KEYSAVE      TEST FOR ALL DONE               L01          
         BNE   LR900                                                            
         CLC   KEY(2),=X'0D45'                                                  
         BNE   LR900               ALL DONE                                     
         CLC   KEY+2(1),BAGYMD                                                  
         BNE   LR900               ALL DONE                                     
*                                                                               
         OC    SVCLT,SVCLT         SEE IF CLIENT GIVEN                          
         BZ    LR040                                                            
LR035    CLC   KEY+3(2),SVCLT      TEST FOR END OF CLIENT          L01          
         BNE   LR900                                                            
LR040    GOTO1 GETREC              GET THE AOR RECORD                           
         BAS   RE,CLT0J            CONVERT 0/J CLIENT CODE                      
*                                                                               
         L     R6,AIO                                                           
*&&DO                                                                           
         MVC   BCLT,AORKCLT                                                     
         GOTO1 VLMTDACC                                                         
*&&                                                                             
         GOTO1 VLMTDACC,DMCB,AORKAGMD,AORKCLT                                   
         CLI   DMCB+4,X'FF'                                                     
         BNE   LR045                                                            
*                                                                               
         XC    KEY,KEY                                                          
         ICM   RF,3,AORKCLT                                                     
         LA    RF,1(RF)                                                         
         STCM  RF,3,KEY+3                                                       
         MVC   KEY(3),0(R6)                                                     
         B     LR010                                                            
*                                                                               
LR045    LA    R5,P1               USE P LINES                                  
         CLI   MODE,PRINTREP                                                    
         BE    LR090                                                            
         LA    R5,LISTAR           OR LIST AREA                                 
         MVC   LISTAR,SPACES                                                    
         MVC   SELHED(36),=C'CLT  PRD  EST  DAYPART  STATION TYPE'              
         GOTO1 CLUNPK,DMCB,(X+50,AORKCLT),0(R5)                                 
         MVC   5(3,R5),AORKPRD                                                  
         LA    RE,10(R5)                                                        
         CLC   AORKEST,=X'FFFF'                                                 
         BNE   DOEDIT                                                           
         MVC   0(3,RE),=C'ALL'                                                  
         B     DDISP                                                            
DOEDIT   DS    0H                                                               
         EDIT  (2,AORKEST),(3,(RE))                      L01                    
DDISP    DS    0H                                                               
         CLI   AORKDPT,X'FF'                                                    
         BNE   DDISP5                                                           
         MVC   17(3,R5),=C'ALL'                                                 
         B     SDISP                                                            
*                                                                               
DDISP5   MVC   18(1,R5),AORKDPT                                                 
*                                                                               
SDISP    DS    0H                                                               
         CLI   AORKSTYP,X'FF'                                                   
         BNE   SDISP5                                                           
         MVC   28(3,R5),=C'ALL'                                                 
         B     LR075                                                            
*                                                                               
SDISP5   MVC   29(1,R5),AORKSTYP                                                
LR075    FOUT  SELHEDH                                                          
*                                                                               
LR080    GOTO1 LISTMON                                                          
         B     LR020                                                            
*                                                                               
LR090    DS    0H                  **NB- PRINTREP NOT FULLY CODED               
         GOTO1 SPOOL,DMCB,SPOOLD                                                
         B     LR020                                                            
*                                                                               
LR900    B     EXIT                                                             
         DROP  R6                                                               
*                                                                               
*                                                                               
* PRINT AOR REPORT                                                              
*                                                                               
PR       L     R8,ASPOOLD                                          L01          
         USING SPOOLD,R8                                                        
*                                                                               
         LA    R1,HEDSPECS         SET UP HEADHOOK AND SPECS                    
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         MVC   H2+10(1),QMED                                                    
         MVC   H2+15(10),MEDNM                                                  
*                                                                               
*                                                                               
         LA    R4,KEY              SET UP REGISTER FOR KEY IN DIRECTORY         
         USING AORREC,R4                                                        
         MVC   AIO,AIO1                                                         
         MVC   AORKTYP,=X'0D45'                                                 
         MVC   AORKAGMD,BAGYMD                                                  
         MVC   AORKCLT,SVCLT                                                    
         MVC   AORKPRD,SVPRD                                                    
         MVC   AORKEST,SVEST                                                    
         MVC   AORKDPT,SVDPT                                                    
         MVC   AORKSTYP,SVSTYP                                                  
*                                                                               
         GOTO1 HIGH                                              L01            
         B     PR30                                                             
*                                                                               
PR20     LA    R4,KEY                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
*                                                                               
PR30     CLC   KEY(3),KEYSAVE      TYPE AND AGY/MED                             
         BNE   PR110                                                            
         CLC   KEY(2),=X'0D45'                                                  
         BNE   PR110                                                            
         OC    SVCLT,SVCLT         SEE IF CLIENT GIVEN                          
         BZ    PR30A                                                            
         CLC   KEY+3(2),SVCLT                                                   
         BNE   PR110                                                            
*                                                                               
PR30A    CLI   SVPRD,C' '          SPECIFIC PRODUCT REQUESTED                   
         BNH   PR30C                                                            
         CLC   SVPRD,KEY+5                                                      
         BNE   PR110               END REPORT                                   
*                                                                               
PR30C    OC    SVEST,SVEST         SEE IF ESTIMATE GIVEN                        
         BZ    PR30E                                                            
         CLC   SVEST,KEY+8                                                      
         BNE   PR110                                                            
*                                                                               
PR30E    CLI   SVDPT,0             SEE IF DAYPART GIVEN                         
         BE    PR30F                                                            
         CLC   SVDPT,KEY+10                                                     
         BNE   PR110                                                            
*                                                                               
PR30F    CLI   SVSTYP,0            SEE IF STATION TYPE GIVEN                    
         BE    PR31                                                             
         CLC   SVSTYP,KEY+11                                                    
         BNE   PR110                                                            
*                                                                               
PR31     MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         BAS   RE,CLT0J            CONVERT 0/J CLIENT CODE                      
*                                                                               
         DROP  R4                                                               
         L     R6,AIO                                                           
         USING AORREC,R6                                                        
*                                                                               
         MVI   RECFOUND,C'Y'       YES, FOUND SCHEME REC                        
         GOTO1 CLUNPK,DMCB,(X+50,AORKCLT),P1                                    
         MVC   P1+4(3),AORKPRD                                                  
         CLC   AORKEST,=X'FFFF'                                                 
         BNE   *+14                                                             
         MVC   P1+08(3),=C'ALL'                                                 
         B     PRADDR                                                           
*                                                                               
         OC    AORKEST,AORKEST                                                  
         BZ    PRADDR                                                           
         EDIT  (B2,AORKEST),(4,P1+08)                                           
PRADDR   DS    0H                                                               
         MVC   P1+14(1),AORKDPT                                                 
         CLI   AORKDPT,X'FF'                                                    
         BNE   *+10                                                             
         MVC   P1+14(3),=C'ALL'                                                 
**                                                                              
         MVC   P1+20(1),AORKSTYP                                                
         CLI   AORKSTYP,X'FF'                                                   
         BNE   *+10                                                             
         MVC   P1+20(3),=C'ALL'                                                 
**                                                                              
         USING AORADREL,R6                                                      
**                                                                              
         MVI   ELCODE,02                                                        
         BAS   RE,GETEL            GET THE NAME ELEMENT                         
         BE    *+6                                                              
         DC    H'0'                IF NO NAME ELEMENT, DIE                      
*                                                                               
         MVI   SPACING,2                                                        
         MVC   P1+26(L'AORLIN1),AORLIN1                                         
         CLC   AORLIN2(L'AORLIN2),SPACES                                        
         BNH   *+10                                                             
         MVC   P2+26(L'AORLIN2),AORLIN2                                         
*                                                                               
         CLC   AORLIN3(L'AORLIN3),SPACES                                        
         BNH   *+10                                                             
         MVC   P3+26(L'AORLIN3),AORLIN3                                         
*                                                                               
         CLC   AORLIN4(L'AORLIN4),SPACES                                        
         BNH   *+10                                                             
         MVC   P4+26(L'AORLIN4),AORLIN4                                         
*                                                                               
         MVI   ELCODE,3                                                         
         BAS   RE,NEXTEL           GET THE NAME ELEMENT                         
         BE    *+8                                                              
         B     PR100                                                            
         USING AORELEM,R6                                                       
*                                                                               
         EDIT  (B4,AORPCT),(10,P1+58),4,ALIGN=LEFT,FLOAT=-                      
         MVC   WORK(09),=CL09'OF GROSS'                                         
         CLI   AORBAS,0                                                         
         BE    PR60                                                             
         MVC   WORK(09),=CL09'OF NET   '                                        
         CLI   AORBAS,X'40'                                                     
         BE    PR60                                                             
         MVC   WORK(09),=CL09'OF AGY C.'                                        
         CLI   AORBAS,X'80'                                                     
         BE    PR60                                                             
         DC    H'0'                 BAD COMMISSION BASIS                        
*                                                                               
PR60     MVC   P1+69(09),WORK                                                   
         OC    AOREFF,AOREFF                                                    
         BZ    PR65                                                             
         GOTO1 DATCON,DMCB,(3,AOREFF),(9,P1+79)                                 
PR65     OC    AORKILL,AORKILL                                                  
         BZ    PR70                                                             
         GOTO1 DATCON,DMCB,(3,AORKILL),(9,P1+89)                                
*                                                                               
PR70     DS    0H                                                               
         MVC   P1+98(1),AORGSTCD                                                
*                                                                               
         MVC   P1+102(L'AORRCVBL),AORRCVBL                                      
         MVC   P1+117(L'AORCOMM),AORCOMM                                        
*                                                                               
PR100    GOTO1 SPOOL,DMCB,(R8)                                                  
*        MVI   FORCEHED,C'Y'                                                    
         B     PR20                NEXT RECORD ENTRY                            
*                                                                               
PR110    CLI   RECFOUND,C'Y'       REPORT HAS DATA IN IT                        
         BE    PRX                                                              
******   MVI   HDHOOKOK,C'N'                                                    
         MVC   P1(16),=C'NO RECORDS FOUND'                                      
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PRX      B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
HOOK     NTR1                      HEADLINE ROUTINES                            
*                                                                               
HOOKX    B     EXIT                                                             
*                                                                               
RECFOUND DC    X'0'                                                             
         SPACE 5                                                                
HEDSPECS SSPEC H2,1,C'MEDIA'                                                    
         SSPEC H1,52,C' NETWORK AOR REPORT   '                                  
         SSPEC H2,52,C'-------------------- '                                   
         SSPEC H1,95,AGYNAME                                                    
         SSPEC H2,95,AGYADD                                                     
         SSPEC H4,95,RUN                                                        
         SSPEC H5,95,REPORT                                                     
         SSPEC H1,1,REQUESTOR                                                   
         SSPEC H7,1,C'CLT PRD  EST D/PT STATION ADDRESS'                        
         SSPEC H8,1,C'--- ---  --- ---- ------- -------'                        
*                               1111111111222222222233333333334444444           
*                     01234567890123456789012345678901234567890123456           
*                                                                               
*                                                                               
         SSPEC H6,91,C'KILL   GST'                                              
         SSPEC H7,59,C'COMM. PCT. BASIS     EFF-DATE   DATE   CODE PAY/X        
               REC ACCT'                                                        
         SSPEC H8,59,C'---------- -----     --------  ------  ---- ----X        
               --------'                                                        
*                     5555555666666666677777777778888888888999                  
*                     3456789012345678901234567890123456789012                  
*                                                                               
         SSPEC H7,118,C'INCOME ACCOUNT'                                         
         SSPEC H8,118,C'--------------'                                         
         DC    X'00'                                                            
*                                                                               
         EJECT                                                                  
CLRSCRN  NTR1                                                                   
*                                                                               
*        ROUTINE TO CLEAR THE SCREEN                                            
*        FROM FIELD AT R2                                                       
*                                                                               
         SR    RE,RE                                                            
*                                                                               
CS010    DS    0H                                                               
         IC    RE,0(R2)                                                         
         SH    RE,=H'9'                                                         
         TM    1(R2),X'20'         TEST PROTECTED                               
         BNZ   CS020               YES- DON'T CLEAR                             
         EX    RE,CSCLC                                                         
         BE    CS020                                                            
         EX    RE,CSOC                                                          
         BZ    CS020                                                            
         EX    RE,CSXC                                                          
         OI    6(R2),X'80'                                                      
*                                                                               
CS020    LA    R2,9(RE,R2)                                                      
         CLI   0(R2),9                                                          
         BH    CS010                                                            
         B     EXIT                                                             
*                                                                               
CSCLC    CLC   8(0,R2),SPACES                                                   
CSOC     OC    8(0,R2),8(R2)                                                    
CSXC     XC    8(0,R2),8(R2)                                                    
         EJECT                                                                  
         PRINT GEN                                                              
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
         B     EXIT                                                             
*                                                                               
CLT0J    NTR1                                                                   
         MVC   X(L'KEY),KEY         SAVE AWAY AOR KEY                           
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING CLTHDRD,R6                                                       
*                                                                               
         MVC   CKEYAM,X+2           AGY/MD                                      
         MVC   CKEYCLT,X+3          CLIENT                                      
*                                                                               
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVC   X+50(1),CPROF+6                                                  
         DROP  R6                                                               
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY,X                                                            
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
*                                                                               
CLT0JX   DS    0H                                                               
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE NESFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NESFMD0D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NESFMD1D                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE NESFMWORKD                                                     
*                                                                               
         ORG   SYSSPARE            1K FOR APPLICAITON                           
SVCOMP   DS    X                                                                
SVCLT    DS    CL2                                                              
SVPRD    DS    CL3                                                              
SVEST    DS    CL2                                                              
SVDPT    DS    CL1                                                              
SVSTYP   DS    CL1                                                              
SVCOFF   DS    CL1                                                              
B2APROF  DS    CL16                                                             
BABPROF  DS    CL16                                                             
X        DS    XL100                                                            
         DS    CL10                 SPARE                                       
         SPACE 2                                                                
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
BILHDRD  DSECT                                                                  
       ++INCLUDE SPGENBILL                                                      
         EJECT                                                                  
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENAOR                                                       
         PRINT OFF                                                              
MKTRECD  DSECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDFLDIND                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'117NESFM11   10/31/05'                                      
         END                                                                    
