*          DATA SET SPSFM11    AT LEVEL 052 AS OF 12/04/14                      
*PHASE T21711B,*    ====== NOTE PHASE HAS A SUFFIX OF B                         
         TITLE 'T21711  CHANGE LOG'                                             
*                                                                               
* AKAT 12/4/14  HONOR ANN PROFILE ON CLIENT RECORD                              
*                                                                               
* BPLA 6/97     IF ACCOUNTS NOT INPUT (AND NOT REQUIRED)                        
*               DON'T OPEN OR TRY TO READ THE ACC FILE                          
*                                                                               
* BPLA 12/22/94 FIX GRANT'S BUG  - DISPLAYING NET BASES                         
*                                                                               
* LWEI 11/4/93  ADD PST                                                         
*                                                                               
* BPLA 4/30/91  ADD KILL DATE AND GST CODE TO SCREEN AND RECORD                 
*               AND REPORT                                                      
*                                                                               
* BPLA 8/29/90  DISPLAY NEGATIVE AOR PCTS CORRECTLY                             
*               FLOAT=- ADDED TO AORPCT EDITS                                   
*                                                                               
* ROSA 5/18/89 PROVIDE ABILITY TO PRINT REPORT                                  
*                                                                               
         TITLE 'T21711  AOR INFORMATION RECORDS'                                
T21711   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21711,R7                                                      
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
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
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PR                                                               
         CLI   MODE,RECDEL         DELETE RECORD?                               
         BNE   EXIT                NO                                           
*                                                                               
* READ BILLING RECORD                                                           
* NOTE.. KEY CONTAINS AOR KEY TO BE DELETED                                     
*                                                                               
CKBIL    LA    R6,KEY                                                           
         USING BILLREC,R6                                                       
         MVC   WORK(L'KEY),KEY   FIRST SAVE KEY                                 
*                                                                               
         XC    KEY,KEY                                                          
         MVC   BKEYAM,BAGYMD                                                    
         MVC   BKEYCLT,WORK+3      CLIENT FROM AOR REC                          
         MVC   BKEYPRD,WORK+5      PRODUCT FROM AOR REC                         
         CLI   WORK+8,X'FF'    IS THIS AN ALL ESTIMATE AOR                      
         BE    CKBIL5           CONTAINS REAL EST NUMBER                        
*                                                                               
* NOTE THAT ESTIMATE IN AOR REC IS 2 BYTES (X'FFFF' = ALL)                      
* REAL EST IS IN LAST BYTE                                                      
*                                                                               
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
*                                                                               
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
         MVC   SVCOFF,COFFICE                                                   
         DROP  R5                                                               
*                                                                               
VK3      XC    SVPRD,SVPRD                                                      
         LA    R2,AGRPROH          PRODUCT                                      
         CLI   5(R2),0             TEST ANY INPUT                               
         BNE   VK4                                                              
         CLI   ACTNUM,ACTLIST      NO, OK IF LIST                               
         BE    VK20                                                             
         CLI   ACTNUM,ACTREP       NO, OK IF REPORT                             
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
         CLI   ACTNUM,ACTREP      IF REPORT THEN LEAVE SVEST AS O               
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
VK5      DS    0H                                                               
         MVI   AORKDPT,X'FF'       'ALL' DAYPARTS                               
         MVI   AORKSTYP,X'FF'      'ALL' STATION TYPES                          
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
DK       L     R6,AIO                                                           
         USING AORREC,R6                                                        
*                                                                               
         LA    R2,AGRCLIH                                                       
         GOTO1 CLUNPK,DMCB,AORKCLT,AGRCLI                                       
         OC    AGRCLI,SPACES                                                    
         FOUT  AGRCLIH                                                          
*                                                                               
         LA    R2,AGRCLIH          CLIENT FIELD                                 
         MVI   AGRCLIH+5,3         SET THE LENGTH                               
         MVI   USEIONUM,2          USE AIO2                                     
         GOTO1 VALICLT             READ CLT REC INTO AIO2                       
*                                                                               
         GOTO1 CLUNPK,DMCB,(SVCPROF+6,AORKCLT),AGRCLI                           
         MVC   AIO,AIO1            RESTORE AIO TO AIO1                          
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
         BE    ERRINV                                                           
         MVC   AORPCT,DMCB+4                                                    
*                                                                               
         LA    R2,AGRBASH          BASIS                                        
         MVI   AORBAS,0                                                         
         CLI   5(R2),0                                                          
         BE    VR5                                                              
         CLI   8(R2),C'G'                                                       
         BE    VR5                                                              
         CLI   8(R2),C'A'                                                       
         BNE   *+12                                                             
         MVI   AORBAS,X'80'                                                     
         B     VR5                                                              
         CLI   8(R2),C'N'                                                       
         BNE   ERRINV                                                           
         MVI   AORBAS,X'40'                                                     
*                                                                               
VR5      LA    R2,AGREFDH          EFFECTIVE DATE                               
         CLI   5(R2),0             TEST ANY                                     
         BE    VR6                                                              
         GOTO1 DATVAL,DMCB,(2,8(R2)),DUB                                        
         OC    DMCB(4),DMCB                                                     
         BZ    ERRINV                                                           
         MVC   DUB+4(2),=C'01'                                                  
         GOTO1 DATCON,DMCB,DUB,(3,WORK)                                         
         MVC   AOREFF,WORK                                                      
*                                                                               
VR6      LA    R2,AGRKDTH          KILL DATE                                    
         CLI   5(R2),0             TEST ANY                                     
         BE    VR7                                                              
         GOTO1 DATVAL,DMCB,(2,8(R2)),DUB                                        
         OC    DMCB(4),DMCB                                                     
         BZ    ERRINV                                                           
         MVC   DUB+4(2),=C'01'                                                  
         GOTO1 DATCON,DMCB,DUB,(3,WORK)                                         
         MVC   AORKILL,WORK                                                     
*                                                                               
VR7      LA    R2,AGRGSTH          GST CODE                                     
         CLI   5(R2),0               ANY INPUT                                  
         BE    VR8                                                              
         CLI   5(R2),1                                                          
         BNE   ERRINV                                                           
         LA    R3,GSTTAB                                                        
VR7C     CLI   0(R3),X'FF'         END OF TABLE                                 
         BE    ERRINV                                                           
         CLC   0(1,R3),8(R2)                                                    
         BE    VR7E                                                             
         LA    R3,1(R3)                                                         
         B     VR7C                                                             
*                                                                               
VR7E     MVC   AORGSTCD,8(R2)                                                   
*                                                                               
VR8      LA    R2,AGRPSTH          PST CODE                                     
         CLI   5(R2),0             ANY INPUT?                                   
         BE    VR8A                NO                                           
         LA    R3,AORPST           PST IN AOR RECORD                            
         LA    R5,L'AORPST-1       LENGTH OF PST FIELD -1 FOR EX                
         MVI   BYTE,PSTVALQ        ACTION = VALIDATE                            
         BRAS  RE,PSTVAL           CALL PSTVAL                                  
*                                                                               
VR8A     LA    R2,AGRMPSTH         MAIN PST CODE                                
         CLI   5(R2),0             ANY INPUT?                                   
         BE    VR9                 NO                                           
         LA    R3,AORMPST          MAIN PST IN AOR RECORD                       
         LA    R5,L'AORMPST-1      LENGTH OF MAIN PST FIELD -1 FOR EX           
         MVI   BYTE,PSTVALQ        ACTION = VALIDATE                            
         BRAS  RE,PSTVAL           CALL PSTVAL                                  
*                                                                               
VR9      DS    0H                                                               
*                                  READ B2A BILLING PROFILE TO SEE              
*                                  IF ACCOUNTS REQUIRED                         
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'SB2A'                                                 
         NI    WORK,X'BF'         MUST MAKE SYSTEM LOWER CASE                   
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),AGRMED                                                 
         MVC   WORK+7(3),QCLT       EBCIDIC CLIENT                              
         CLI   SVCOFF,C' '                                                      
         BNH   *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCOFF                                                
         GOTO1 GETPROF,DMCB,WORK,B2APROF,DATAMGR                                
*                                  READ BAB PROFILE FOR COMPANY CODE            
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
         CLI   5(R2),0             ANY INPUT                                    
         BE    VR9D                                                             
         LA    R3,AORRCVBL                                                      
         BAS   RE,CHKACCT          FIND ON ACCOUNT FILE                         
         CLI   ERROR,0                                                          
         BNE   ERRMSG                                                           
*                                                                               
VR9D     LA    R2,AGRCMAH                                                       
         MVC   AORCOMM,8(R2)                                                    
         OC    AORCOMM,SPACES                                                   
         CLI   B2APROF+2,C'Y'      SEE IF INPUT REQUIRED                        
         BNE   VR9G                                                             
         CLI   5(R2),0                                                          
         BNE   VR9G                                                             
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
VR9G     DS    0H                                                               
         CLI   5(R2),0             ANY INPUT?                                   
         BE    VR9H                                                             
         LA    R3,AORCOMM                                                       
         BAS   RE,CHKACCT          FIND ON ACCOUNT FILE                         
         CLI   ERROR,0                                                          
         BNE   ERRMSG                                                           
*                                                                               
VR9H     GOTO1 ADDELEM                                                          
*                                                                               
VR900    B     EXIT                                                             
*                                                                               
GSTTAB   DC    C'SUXZ',X'FF'       VALID GST CODES                              
*                                                                               
* DISPLAY RECORD                                                                
*                                                                               
DR       DS    0H                                                               
         LA    R2,AGRLIN1H         CLEAR SCREEN                                 
         BAS   RE,CLRSCRN                                                       
*                                                                               
         L     R6,AIO                                                           
         USING AORREC,R6                                                        
*                                                                               
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
         EDIT  (B4,AORPCT),(10,AGRPCT),4,ALIGN=LEFT,FLOAT=-                     
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
         GOTO1 DATCON,DMCB,(3,AOREFF),(9,WORK)                                  
         MVC   AGREFD(6),WORK                                                   
DR15C    FOUT  AGREFDH                                                          
*                                                                               
         MVC   AGRKDT,SPACES                                                    
         OC    AORKILL,AORKILL  SEE IF I HAVE A DATE                            
         BZ    DR17C                                                            
         GOTO1 DATCON,DMCB,(3,AORKILL),(9,WORK)                                 
         MVC   AGRKDT(6),WORK                                                   
DR17C    FOUT  AGRKDTH                                                          
*                                                                               
         FOUT  AGRGSTH,AORGSTCD,1                                               
*                                                                               
         LA    R3,AORPST           PST IN AOR RECORD                            
         LA    R2,AGRPSTH          PST SCREEN HEADER                            
         LA    R5,L'AGRPST-1       PST LENGTH ON SCREEN -1 FOR EX               
         LA    R1,L'AORPST-1       PST LENGTH IN RECORD -1 FOR EX               
         BAS   RE,DISPPST          DISPLAY PST                                  
                                                                                
         LA    R3,AORMPST          MAIN PST IN AOR RECORD                       
         LA    R2,AGRMPSTH         MAIN PST SCREEN HEADER                       
         LA    R5,L'AGRMPST-1      MAIN PST LENGTH ON SCREEN -1 FOR EX          
         LA    R1,L'AORMPST-1      MAIN PST LENGTH IN RECORD -1 FOR EX          
         BAS   RE,DISPPST          DISPLAY MAIN PST                             
*                                                                               
         FOUT  AGRRCAH,AORRCVBL                                                 
         FOUT  AGRCMAH,AORCOMM                                                  
         OI    AGRRCAH+4,X'20'     SET VALIDATED                                
         OI    AGRCMAH+4,X'20'                                                  
*                                                                               
DR20     B     EXIT                                                             
***********************************************************************         
*                        DISPLAY PST/MAIN PST                         *         
*        INPUT : R1 = LENGTH OF PST IN THE RECORD                     *         
*              : R2 = SCREEN HEADER                                   *         
*              : R3 = PST IN RECORD                                   *         
*              : R5 = LENGTH OF PST ON THE SCREEN                     *         
***********************************************************************         
DISPPST  NTR1                                                                   
         EX    R1,*+8              EXECUTE                                      
         B     *+10                BRANCH SO ASSEMBLER DOESN'T COMPLAIN         
         OC    0(0,R3),0(R3)       ANYTHING TO DISPLAY IN RECORD?               
         BZ    DISPSTX             NO - EXIT                                    
*                                                                               
         USING AORELEM,R6          AOR RECORD DSECT                             
         LA    R4,AORMPST          MAIN PST IN AOR RECORD                       
         CR    R3,R4               POINTING TO MAIN PST IN AOR RECORD?          
         BNE   DISPST10            NO                                           
         DROP  R6                  DROP R6                                      
*                                                                               
         LA    R3,WORK             BUILD MAIN PST STRING IN WORK                
         XC    WORK(10),WORK       CLEAR WORK                                   
         LLC   R1,0(R4)            POSTION IN STRING                            
         BCTR  R1,0                -1 FOR INDEXING                              
         AR    R1,R3               PUT CHARACTER HERE                           
         MVC   0(1,R1),1(R4)       MOVE THE CHARACTER INTO POSTION              
*                                                                               
DISPST10 MVI   BYTE,PSTFMTQ        ACTION = FORMAT (DISPLAY)                    
         BRAS  RE,PSTVAL           CALL PSTVAL                                  
*                                                                               
DISPSTX  B     EXIT                EXIT                                         
***********************************************************************         
*        CALL PSTVAL TO CHANGE/DISPLAY PST/MAIN PST                   *         
*        INPUT : R2 = SCREEN HEADER                                   *         
*              : R3 = PST IN RECORD                                   *         
*              : R5 = LENGTH OF PST ON SCREEN OR IN THE RECORD        *         
***********************************************************************         
PSTVAL   NTR1                                                                   
         LA    R4,BLOCK            R4 = ELEMENT                                 
         USING PSTBLKD,R4          PST BLOCK DSECT                              
         XC    BLOCK(PSTLNQ),BLOCK CLEAR INTERFACE BLOCK                        
         MVC   PSTACT,BYTE         ACTION = VALIDATE OR DISPLAY                 
         ST    R3,PSTADIN          INPUT ADDRESS IS PST IN RECORD               
         CLI   BYTE,PSTFMTQ        ACTION = FORMAT (DISPLAY)?                   
         BE    *+8                 YES                                          
         ST    R2,PSTADIN          NO - INPUT ADDRESS IS PST ON SCREEN          
         XC    PSTOUT,PSTOUT       CLEAR PST OUTPUT BLOCK                       
         LA    R1,PSTOUT           PST OUTPUT BLOCK                             
         ST    R1,PSTADOUT         OUTPUT ADDRESS                               
         MVC   PSTACOM,ACOMFACS    A(COMFACS)                                   
*                                                                               
         XC    DMCB(24),DMCB       CLEAR DMCB                                   
         MVC   DMCB+4(3),=X'D9000A'                                             
         MVI   DMCB+7,QPSTVAL      CALL PSTVAL TO DISPLAY/VALIDATE PST          
         GOTO1 CALLOV,DMCB         CALL OVERLAY FOR A(PSTVAL)                   
         CLI   4(R1),X'FF'         ANY ERRORS?                                  
         BNE   *+6                 NO                                           
         DC    H'0'                YES - DEATH                                  
         L     RF,DMCB             A(PSTVAL)                                    
         GOTO1 (RF),DMCB,(R4)      CALL PSTVAL                                  
*                                                                               
         LA    R1,8(R2)            MOVE TO SCREEN                               
         CLI   BYTE,PSTFMTQ        ACTION = FORMAT (DISPLAY)?                   
         BE    PSTVAL20            YES - DON'T CHECK FOR ERRORS                 
*                                                                               
         CLI   PSTERR,0            ANY ERRORS RETURNED FROM PSTVAL?             
         BNE   ERRINV              YES - GIVE ERROR INVALID                     
*                                                                               
         LR    R1,R3               MOVE TO RECORD                               
         CHI   R5,L'AORMPST-1      ADD/CHANGE MAIN PST?                         
         BNE   PSTVAL20            NO                                           
*                                                                               
         LA    R1,PSTOUT           RETURNED FROM PSTVAL                         
         LA    R2,10               MAX 10 ENTRIES                               
*                                                                               
PSTVAL15 CLI   0(R1),0             HAVE PST ENTRY?                              
         BNE   PSTVAL16            YES                                          
         LA    R1,1(R1)            NO - BUMP POINTER                            
         BCT   R2,PSTVAL15         LOOP BACK AND CHECK NEXT ONE                 
         DC    H'0'                PSTVAL SHOULD HAVE RETURNED AN ERROR         
*                                                                               
PSTVAL16 MVC   1(1,R3),0(R1)       MOVE PST                                     
         LA    R2,PSTOUT           PST ENTRY                                    
         SR    R1,R2               GET INDEXED POSITION                         
         LA    R1,1(R1)            ADD ONE (POSTION 1 = 1 IN THIS CASE)         
         STC   R1,0(R3)            SAVE THE POSITION IN THE RECORD              
         B     EXIT                AND EXIT                                     
*                                                                               
PSTVAL20 EX    R5,*+8              DISPLAY = L'SCREEN | ADD/CHA = L'REC         
         B     *+10                BRANCH SO ASSEMBLER DOESN'T COMPLAIN         
         MVC   0(0,R1),PSTOUT      MOVE PST/MAIN PST TO SCREEN/RECORD           
         B     EXIT                AND EXIT                                     
         DROP  R4                                                               
*                                                                               
*        CHKACCT- READ ACCT FILE                                                
*                                                                               
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
         GOTO1 (RF),DMCB,((R0),0),0   ELSE SWITCH DIRECTLY                      
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
         CLI   4(R1),1             TEST NOT AUTHORIZED                          
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
         L     RF,ACOMFACS         SWITCH BACK TO SPOT                          
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,=C'SPOT',0                                             
         CLI   4(R1),0             ALL ERRORS FATAL                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CHKAX    B     EXIT                                                             
*                                                                               
* LIST RECORDS                                                                  
*                                                                               
LR       DS    0H                                                               
         LA    R6,KEY                                                           
         USING AORREC,R6                                                        
         MVC   AIO,AIO1                                                         
         OC    KEY,KEY             TEST FIRST TIME                              
         BNZ   LR010                                                            
*                                                                               
         XC    SVBCLI,SVBCLI                                                    
         MVC   AORKTYP,=X'0D45'                                                 
         MVC   AORKAGMD,BAGYMD                                                  
         MVC   AORKCLT,SVCLT                                                    
         MVC   AORKPRD,SVPRD                                                    
         MVC   AORKEST,SVEST                                                    
*                                                                               
LR010    GOTO1 HIGH                                                             
         B     LR030                                                            
*                                                                               
LR020    GOTO1 SEQ                                                              
*                                                                               
LR030    CLC   KEY(3),KEYSAVE      TEST FOR ALL DONE                            
         BNE   LR900                                                            
         CLC   KEY(2),=X'0D45'                                                  
         BNE   LR900               ALL DONE                                     
*                                                                               
         OC    SVCLT,SVCLT         SEE IF CLIENT GIVEN                          
         BZ    LR040                                                            
LRO35    CLC   KEY+3(2),SVCLT      TEST FOR END OF CLIENT                       
         BNE   LR900                                                            
*                                                                               
LR040    CLC    SVBCLI,KEY+3       SAME AS PREVIOUS CLIENT?                     
         BE     LR050              YES                                          
         BAS    RE,AAN             NO - GO READ CLIENT RECORD                   
         MVC    SVBCLI,KEY+3       SAVE OFF CLIENT CODE                         
*                                                                               
LR050    GOTO1 GETREC              GET THE AOR RECORD                           
         L     R6,AIO                                                           
*                                                                               
         LA    R5,P1               USE P LINES                                  
         CLI   MODE,PRINTREP                                                    
         BE    LR090                                                            
         LA    R5,LISTAR           OR LIST AREA                                 
         MVC   LISTAR,SPACES                                                    
         MVC   SELHED(13),=C'CLT  PRD  EST'                                     
*                                                                               
         GOTO1 CLUNPK,DMCB,(CLTAAN,AORKCLT),0(R5)                               
         MVC   5(3,R5),AORKPRD                                                  
         LA    RE,10(R5)                                                        
         CLC   AORKEST,=X'FFFF'                                                 
         BNE   DOEDIT                                                           
         MVC   0(3,RE),=C'ALL'                                                  
         B     DDISP                                                            
DOEDIT   DS    0H                                                               
         EDIT  (2,AORKEST),(3,(RE))                                             
DDISP    DS    0H                                                               
         FOUT  SELHEDH                                                          
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
         TITLE 'PRINT AOR REPORT'                                               
*                                                                               
* PRINT AOR REPORT                                                              
*                                                                               
PR       L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         LA    R1,HEDSPECS         SET UP HEADHOOK AND SPECS                    
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         MVC   H2+15(10),MEDNM                                                  
         MVC   H2+10(1),QMED                                                    
*                                                                               
         LA    R4,KEY              SET UP REGISTER FOR KEY IN DIRECTORY         
         USING AORREC,R4                                                        
         MVC   AIO,AIO1                                                         
*                                                                               
         MVC   AORKTYP,=X'0D45'                                                 
         MVC   AORKAGMD,BAGYMD                                                  
         MVC   AORKCLT,SVCLT                   CLIENT                           
         MVC   AORKPRD,SVPRD                                                    
         MVC   AORKEST,SVEST                                                    
*                                                                               
         GOTO1 HIGH                                                             
         B     PR30                                                             
*                                                                               
PR20     LA    R4,KEY                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
*                                                                               
PR30     CLC   KEY(3),KEYSAVE      CHECK AGY/MED RECORD                         
         BNE   PR110                                                            
         CLC   KEY(2),=X'0D45'                                                  
         BNE   PR110               ALL DONE                                     
         OC    SVCLT,SVCLT         SEE IF CLIENT GIVEN                          
         BZ    PR31                                                             
         CLC   KEY+3(2),SVCLT                                                   
         BNE   PR110                                                            
         CLI   SVPRD,C' '          SPECIFIC PRODUCT REQUESTED                   
         BNH   PR30C                                                            
         CLC   SVPRD,KEY+5                                                      
         BNE   PR110               END REPORT                                   
PR30C    OC    SVEST,SVEST         SEE IF ESTIMATE GIVEN                        
         BZ    PR31                                                             
         CLC   SVEST,KEY+8                                                      
         BNE   PR110               END OF REPORT                                
*                                                                               
PR31     MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         DROP  R4                                                               
         L     R6,AIO                                                           
         USING AORREC,R6                                                        
*                                                                               
         MVI   RECFOUND,C'Y'       YES, FOUND SCHEME REC                        
         GOTO1 CLUNPK,DMCB,AORKCLT,P1                                           
         MVC   P1+6(3),AORKPRD                                                  
         CLC   AORKEST,=X'FFFF'                                                 
         BNE   *+14                                                             
         MVC   P1+12(3),=C'ALL'                                                 
         B     PRADDR                                                           
*                                                                               
         OC    AORKEST,AORKEST                                                  
         BZ    PRADDR                                                           
         EDIT  (B2,AORKEST),(4,P1+12)                                           
         USING AORADREL,R6                                                      
PRADDR   MVI   ELCODE,02                                                        
         BAS   RE,GETEL            GET THE NAME ELEMENT                         
         BE    *+6                                                              
         DC    H'0'                IF NO NAME ELEMENT, DIE                      
*                                                                               
         MVI   SPACING,2                                                        
*                                                                               
         MVC   P1+21(L'AORLIN1),AORLIN1                                         
         CLC   AORLIN2(L'AORLIN2),SPACES                                        
         BNH   *+10                                                             
         MVC   P2+21(L'AORLIN2),AORLIN2                                         
*                                                                               
         CLC   AORLIN3(L'AORLIN3),SPACES                                        
         BNH   *+10                                                             
         MVC   P3+21(L'AORLIN3),AORLIN3                                         
*                                                                               
         CLC   AORLIN4(L'AORLIN4),SPACES                                        
         BNH   *+10                                                             
         MVC   P4+21(L'AORLIN4),AORLIN4                                         
*                                                                               
         MVI   ELCODE,3                                                         
         BAS   RE,NEXTEL           GET THE INFO ELEM                            
         BE    *+8                                                              
         B     PR100                                                            
         USING AORELEM,R6                                                       
*                                                                               
         EDIT  (B4,AORPCT),(10,P1+53),4,FLOAT=-                                 
         MVC   WORK(12),=CL12'OF GROSS'                                         
         CLI   AORBAS,0                                                         
         BE    PR60                                                             
         MVC   WORK(12),=CL12'OF NET  '                                         
         CLI   AORBAS,X'40'                                                     
         BE    PR60                                                             
         MVC   WORK(12),=CL12'OF AGY COMM.'                                     
         CLI   AORBAS,X'80'                                                     
         BE    PR60                                                             
         DC    H'0'                 BAD COMMISSION BASIS                        
*                                                                               
PR60     MVC   P1+64(12),WORK                                                   
         OC    AOREFF,AOREFF        CHK FOR DATE                                
         BZ    PR65                                                             
         GOTO1 DATCON,DMCB,(3,AOREFF),(9,P1+77)                                 
*                                                                               
PR65     OC    AORKILL,AORKILL      CHK FOR DATE                                
         BZ    PR70                                                             
         GOTO1 DATCON,DMCB,(3,AORKILL),(9,P1+87)                                
*                                                                               
PR70     MVC   P1+96(1),AORGSTCD                                                
*                                                                               
PR75     MVC   P1+100(L'AORRCVBL),AORRCVBL                                      
         MVC   P1+115(L'AORCOMM),AORCOMM                                        
*                                                                               
PR100    GOTO1 SPOOL,DMCB,(R8)                                                  
         B     PR20                NEXT RECORD ENTRY                            
*                                                                               
PR110    CLI   RECFOUND,C'Y'       REPORT HAS DATA IN IT                        
         BE    PRX                                                              
         MVC   P1(16),=C'NO RECORDS FOUND'                                      
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PRX      B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        CHECK TO SEE IF CLIENT HAS THE "AAN" OPTION ON                         
***********************************************************************         
AAN      NTR1                                                                   
*                                                                               
         MVI   CLTAAN,C'N'         DEFAULT TO NO!                               
         MVC   SVAORKEY,KEY        SAVE OFF THE AOR KEY                         
         XC    KEY,KEY             CLEAR THE KEY                                
         MVC   KEY+1(1),SVAORKEY+2 A/M                                          
         MVC   KEY+2(2),SVAORKEY+3 CLIENT                                       
*                                                                               
         GOTO1 HIGH                READ THE CLIENT RECORD                       
*                                                                               
         CLC   KEYSAVE(13),KEY     FOUND IT?                                    
         BE    *+6                 YES                                          
         DC    H'0'                NO - DEATH                                   
*                                                                               
         GOTO1 GETREC              GET THE CLIENT RECORD                        
*                                                                               
         L     R6,AIO              CLIENT RECORD                                
         USING CLTHDR,R6           CLIENT RECORD DSECT                          
         MVC   CLTAAN,CPROF+6      SAVE AAN FIELD                               
         DROP  R6                  DROP CLT REC USING                           
         MVC   KEY,SVAORKEY        RESTORE KEY                                  
*                                                                               
         GOTO1 HIGH                RESTORE READ SEQUENCE                        
*                                                                               
AANX     B     EXIT                EXIT                                         
         EJECT                                                                  
HOOK     NTR1                      HEADLINE ROUTINES                            
*                                                                               
HOOKX    B     EXIT                                                             
RECFOUND DC    X'0'                                                             
         SPACE 5                                                                
HEDSPECS SSPEC H2,1,C'MEDIA'                                                    
         SSPEC H1,52,C'SPOT AOR REPORT'                                         
         SSPEC H2,52,C'---------------'                                         
         SSPEC H1,95,AGYNAME                                                    
         SSPEC H2,95,AGYADD                                                     
         SSPEC H4,95,RUN                                                        
         SSPEC H5,95,REPORT                                                     
         SSPEC H1,1,REQUESTOR                                                   
         SSPEC H7,1,C'CLT   PRD   EST      ADDRESS'                             
         SSPEC H8,1,C'---   ---   ---      -------'                             
         SSPEC H6,89,C'KILL   GST'                                              
         SSPEC H7,54,C'COMM. PCT. BASIS        EFF-DATE   DATE   CODE PX        
               AY/REC ACCT'                                                     
         SSPEC H8,54,C'---------- -----        --------  ------  ---- -X        
               ------------'                                                    
         SSPEC H7,116,C'INCOME ACCOUNT'                                         
         SSPEC H8,116,C'--------------'                                         
         DC    X'00'                                                            
*                                                                               
CLRSCRN  NTR1                                                                   
*                                                                               
         SR    RE,RE                                                            
*                                                                               
CS010    DS    0H                                                               
         IC    RE,0(R2)                                                         
         SHI   RE,9                                                             
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
ERRINV   MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
ERRMSG   CLI   ERROR,X'FF'         TEST ERROR ALREADY SHOWN                     
         BNE   TRAPERR             NO- LET GENCON DO IT                         
         GOTO1 ERREX2                                                           
*                                                                               
TRAPERR  GOTO1 ERREX               NEVER TO RETURN                              
         B     EXIT                                                             
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE SPSFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFMC1D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFMD1D                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE SPSFMWORKD                                                     
*                                                                               
         ORG   SYSSPARE         1K APPLICAITON AREA                             
SVCOMP   DS    X                                                                
SVCLT    DS    CL2                                                              
SVPRD    DS    CL3                                                              
SVEST    DS    CL2                                                              
SVCOFF   DS    CL1                                                              
B2APROF  DS    CL16                                                             
BABPROF  DS    CL16                                                             
PSTOUT   DS    CL64                                                             
SVAORKEY DS    XL13                                                             
CLTAAN   DS    CL1                                                              
SVBCLI   DS    XL2                                                              
*                                                                               
       ++INCLUDE DDPSTBLK                                                       
         EJECT                                                                  
       ++INCLUDE DDCOREQUS                                                      
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
BILHDRD  DSECT                                                                  
       ++INCLUDE SPGENBILL                                                      
         EJECT                                                                  
       ++INCLUDE SPGENAOR                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDFLDIND                                                       
       ++INCLUDE FATIOB                                                         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'052SPSFM11   12/04/14'                                      
         END                                                                    
