*          DATA SET PRSFM11S   AT LEVEL 056 AS OF 05/01/02                      
*PHASE T41C11A,*                                                    L01         
         TITLE 'T41C11  CHANGE LOG'                                 L01         
*                                                                               
* SMYE 3/97      DISALLOW MORE THAN 1 PST CODE ENTRY (AT VALPST)                
*                                                                               
* BPLA 3/1/95    ADD NET AS A BASIS                                             
*                                                                               
* BPLA 4/11/94   PST ON LISTING REPORT                                          
*                                                                               
* BPLA 10/15/92  EDIT MAXIMUM PCT OF 100.0000                                   
*                                                                               
* BPLA 5/1/91  ADD LOGIC FOR KILL DATE AND GST CODE                             
*                                                                               
* BPLA 8/28/90 DISPLAY MINUS PERCENTS WITH  '-' IN FRONT                        
*                                                                               
* BPLA 3/9/90  ADD CHECK OF B2A BILLING PROFILE TO SEE IF ACCOUNTS              
*              ARE TO BE REQUIRED                                               
*                                                                               
* ROSA 5/18/89 PROVIDE ABILITY TO PRINT REPORT                      L03         
*                                                                   L03         
*                                                                   L03         
*                                                                   L03         
* ROSA 2/1/89  THERE WAS NO CHECK TO SEE IF THE AOR RECORD COULD BUG02          
*              BE DELETED.. CHECK TO SEE IF BILLING RECORDS ARE  BUG02          
*              PRESENT FOR THAT PRODUCT.. IF NONE CAN DELETE     BUG02          
*                                                                BUG02          
*  ROSA 12/1/88 LIST FUNCTION NOT OPERATING PROPERLY.  PRODUCT   BUG01          
*              AND ESTIMATE NOT BEING SAVED                      BUG01          
*                                                                BUG01          
*  ROSA 9/18/88  ONLY ACCEPT GROSS  PER BRUCE                       L02         
* ROSA 7/26/88    CREATE AOR RECORD FOR PRINT SYSTEM                            
         TITLE 'T41C11  AOR INFORMATION RECORDS'                    L01         
T41C11   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T41C11                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
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
         CLI   MODE,PRINTREP       PRINT REPORT                   L03           
         BE    PR                                                 L03           
*                                                                BUG02          
         CLI   MODE,RECDEL                                       BUG02          
         BNE   EXIT                                              BUG02          
*                                                                BUG02          
* READ BILLING RECORD                                            BUG02          
*                                                                BUG02          
*  NOTE.. KEY CONTAINS AOR KEY TO BE DELETED                     BUG02          
*                                                                BUG02          
*                                                                BUG02          
**       =====           **                                      BUG02          
         LA    R6,KEY                                            BUG02          
         USING PBILRECD,R6                                      BUG02           
         MVC   WORK(32),KEY                                                     
**       =====           **                                      BUG02          
         MVC   PBILKAGY,AGENCY                                   BUG02          
         MVC   PBILKMED,QMED                                     BUG02          
         MVI   PBILKRCD,X'08'                                    BUG02          
         MVC   PBILKCLT,WORK+4     CLIENT FROM AOR REC           BUG02          
         MVC   PBILKPRD,SELPRO                                   BUG02          
         OI    PBILKPRD+2,X'40' SELPRO+2 MAY B BIN0 IF PROD IS 2 BYTES          
         CLI   PBILKPRD+3,X'FF' IS THIS AN ALL ESTIMATE AOR                     
         BE    *+14             CONTAINS REAL EST NUMBER                        
         XC    PBILKPRD+6(13),PBILKPRD+6                                        
         B     GOTHI                                             BUG02          
         XC    PBILKPRD+3(15),PBILKPRD+3  CLEAR X'FFFF'          BUG02          
*                                                                BUG02          
GOTHI    GOTO1 HIGH                                              BUG02          
*                                                                BUG02          
         CLC   KEYSAVE(10),KEY                                   BUG02          
         BNE   LOADUP                                            BUG02          
*                                                                BUG02          
         OC    KEYSAVE+10(2),KEYSAVE+10                          BUG02          
         BZ    CNOTDEL                                           BUG02          
         CLC   KEYSAVE+10(2),KEY+10     DELETING ESTIMATE        BUG02          
         BNE   LOADUP                                            BUG02          
*                                                                BUG02          
* CANNOT DELETE                                                  BUG02          
*                                                                BUG02          
CNOTDEL  XC    CONHEAD,CONHEAD                                   BUG02          
         MVC   CONHEAD(37),=C'CANNOT DELETE // BILL RECORDS PRESENT' 2          
         GOTO1 ERREX2                                            BUG02          
*                                                                BUG02          
*                                                                BUG02          
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*                                                                               
LOADUP   MVC   KEY,WORK                                                         
         GOTO1 HIGH                                                             
         B     EXIT                                                             
*                                                                               
*     VALIDATE KEY ROUTINE                                                      
*                                                                               
VK       DS    0H                                                               
*                                                                               
         LA    R2,AGRMEDH          MEDIA                                        
         GOTO1 VALIMED                                                          
*                                                                               
         LA    R2,AGRCLIH          CLIENT                                       
         XC    SVCLT,SVCLT                                                      
         CLI   5(R2),0             TEST ANY INPUT                               
         BNE   VK2                                                              
         CLI   ACTNUM,ACTLIST      NO, OK IF LIST                               
         BE    VK3                                                              
         CLI   ACTNUM,ACTREP       NO, OK IF REPORT                             
         BE    VK3                                                              
         CLI   ACTNUM,X'11'        OK IF NOW                       L03          
         BE    VK3                                                 L03          
*                                                                               
VK2      GOTO1 VALICLT                                                          
*                                                                   L01         
         MVC   SVCLT,QCLT                                                       
         L     R5,AIO                                                           
         USING CLTHDRD,R5                                                       
         MVC   SVCOFF,PCLTOFF                                                   
         DROP  R5                                                               
*                                                                               
         LA    R6,SVKEY                                                         
         USING AORKEY,R6                                                        
         XC    AORKEY,AORKEY          CLEAR                         L01         
         MVC   AORKAGY,AGENCY   CREATE KEY  -- AGENCY               L01         
         MVC   AORKCLT,QCLT                    CLIENT              L01          
         MVC   AORKMED,QMED                    MEDIA CODE           L01         
         MVI   AORKRCD,X'14'                   ID                               
*                                                                               
VK3      XC    SVPRD,SVPRD                                                      
         LA    R2,AGRPROH          PRODUCT                                      
         CLI   5(R2),0             TEST ANY INPUT                               
         BNE   VK4                                                              
         CLI   ACTNUM,ACTLIST      NO, OK IF LIST                               
         BE    VK6                                                              
         CLI   ACTNUM,ACTREP       NO, OK IF REPORT                             
         BE    VK6                                                              
         CLI   ACTNUM,X'11'        OK IF NOW                       L03          
         BE    VK6                                                 L03          
*                                                                               
VK4      DS    0H                                                               
         GOTO1 VALIPRD                                                          
         MVC   AORKPRD,QPRD                                         L01         
         MVC   SVPRD,QPRD                                           L01         
         XC    SVEST,SVEST                                                      
         LA    R2,AGRESTH                                           L01         
         CLI   5(R2),0         ANY INPUT                            L01         
         BNE   VK4AA                                                L01         
         CLI   ACTNUM,ACTLIST    SEE IF LISTING                                 
         BE    VK6               YES - THEN I CAN LEAVE SVEST ZERO              
         CLI   ACTNUM,ACTREP     SEE IF REPORTING                               
         BE    VK6               YES - THEN I CAN LEAVE SVEST ZERO              
*                                                                               
VK4A     MVC   AORKEST,=2X'FF'   NO EST                             L01         
         MVC   SVEST,AORKEST                                        L01         
         B     VK6                                                  L01         
*                                                                   L01         
VK4AA    CLC   8(3,R2),=C'ALL'  TREAT ALL LIKE NO INPUT             L01         
         BE    VK4A                                                 L01         
         GOTO1 VALIEST                                              L01         
         MVC   AORKEST,BEST                                         L01         
         MVC   SVEST,AORKEST                                        L01         
*                                                                   L01         
VK6      DS    0H                                                               
         MVC   KEY(25),SVKEY       SET KEY                          L01         
*                                                                               
VK900    B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* DISPLAY KEY                                                                   
*                                                                               
DK       L     R6,AIO                                                           
         USING AORREC,R6                                                        
         FOUT  AGRCLIH,AORKCLT,3                                                
*                                                                               
         FOUT  AGRPROH,AORKPRD,3                                 BUG01          
*                                                                BUG01          
         CLC   AORKEST,=X'FFFF'                                                 
         BNE   EDITEST                                                          
         MVC   DUB,=C'ALL'                                                      
         B     FOUTEST                                                          
*                                                                               
EDITEST  EDIT  (B2,AORKEST),(3,DUB),FILL=0                                      
FOUTEST  FOUT  AGRESTH,DUB,3                                                    
         LA    R2,AGRPROH                                                       
         MVC   AGRPRO,SPACES                                                    
         MVC   AGRPRO(3),AORKPRD                                                
         MVI   AGRPROH+5,3                                                      
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
         L     R0,DMCB+4                                                        
*                                                                               
         C     R0,=F'1000000'      SHOULDN'T EXCEED 100.0000                    
         BH    VR99                                                             
         C     R0,=F'-1000000'     SHOULDN'T EXCEED -100.0000                   
         BL    VR99                                                             
*                                                                               
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
         MVI   AORBAS,X'40'                                                     
         CLI   8(R2),C'N'                                                       
         BE    VR5                                                              
         B     VR99                INVALID BASIS                                
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
         BE    VR8                                                              
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
VR8      DS    0H                                                               
         LA    R2,AGRPSTH          PST CODE                                     
         CLI   5(R2),0               ANY INPUT                                  
         BE    VR9                                                              
         BAS   RE,VALPST                                                        
*                                                                               
VR9      DS    0H                                                               
*                                  READ B2A BILLING PROFILE TO SEE              
*                                  IF ACCOUNTS REQUIRED                         
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'PB2A'                                                 
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
*                                  READ BA PROFILE FOR COMPANY CODE             
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'PBAB'                                                 
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
VR9C     LA    R3,AORRCVBL                                                      
         BAS   RE,CHKACCT          FIND ON ACCOUNT FILE                         
         CLI   ERROR,0                                                          
         BNE   VR99C                                                            
*                                                                               
         LA    R2,AGRCMAH                                                       
         MVC   AORCOMM,8(R2)                                                    
         OC    AORCOMM,SPACES                                                   
         CLI   B2APROF+2,C'Y'      SEE IF INPUT REQUIRED                        
         BNE   VR9G                                                             
         CLI   5(R2),0                                                          
         BNE   VR9G                                                             
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
VR9G     LA    R3,AORCOMM                                                       
         BAS   RE,CHKACCT          FIND ON ACCOUNT FILE                         
         CLI   ERROR,0                                                          
         BNE   VR99C                                                            
*                                                                               
*                                                                               
         GOTO1 ADDELEM                                                          
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
*                                                                               
* DISPLAY RECORD                                                                
*                                                                               
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
         BAS   RE,DISPPST         DISPLAY PST VALUES                            
         OC    PSTOUT,PSTOUT                                                    
         BZ    DR18                                                             
         MVC   AGRPST,PSTOUT       OUTPUT                                       
         OI    AGRPSTH+6,X'80'                                                  
*                                                                               
DR18     FOUT  AGRRCAH,AORRCVBL                                                 
         FOUT  AGRCMAH,AORCOMM                                                  
         OI    AGRRCAH+4,X'20'     SET VALIDATED                                
         OI    AGRCMAH+4,X'20'                                                  
*                                                                               
DR20     DS    0H                                                               
DR900    B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*        VALIDATE PST CODES                                                     
*                                                                               
VALPST   NTR1                                                                   
         LA    R6,ELEM                                                          
         USING AORELEM,R6                                                       
         LA    R4,BLOCK                                                         
         USING PSTBLKD,R4                                                       
         XC    0(200,R4),0(R4)     CLEAR INTERFACE BLOCK                        
         XC    200(200,R4),200(R4)                                              
         MVI   PSTACT,PSTVALQ      ACTION = VALIDATE                            
         LA    R1,AGRPSTH                                                       
         ST    R1,PSTADIN          INPUT ADDRESS                                
         XC    PSTOUT,PSTOUT                                                    
         LA    R1,PSTOUT                                                        
         ST    R1,PSTADOUT         OUTPUT ADDRESS                               
         MVC   PSTACOM,ACOMFACS    A(COMFACS)                                   
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A6B'                                           
         GOTO1 CALLOV,DMCB                                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(R4)                                                   
         MVC   ERRDISP,PSTERDSP                                                 
         CLI   PSTERR,0                                                         
         BNE   VR99                                                             
*                              TEST PSTOUT FOR MORE THAN ONE ENTRY              
         SR    RE,RE               COUNT OF NO. OF ENTRIES                      
         LA    R1,PSTOUT                                                        
         LA    R0,10               LOOP COUNTER                                 
VPLUP    CLI   0(R1),0             ANY ENTRY ?                                  
         BE    *+8                 NO                                           
         LA    RE,1(RE)            YES - ADD TO ENTRY COUNT                     
         LA    R1,1(R1)            BUMP TO NEXT BYTE                            
         BCT   R0,VPLUP                                                         
         CH    RE,=H'1'            ONE ENTRY ?                                  
         BH    VR99                NO - ERROR                                   
*                                                                               
         MVC   AORPST,PSTOUT                                                    
         BAS   RE,DISPPST          DISPLAY PST                                  
         OC    PSTOUT,PSTOUT                                                    
         BZ    VPX                                                              
         MVC   AGRPST,PSTOUT       OUTPUT                                       
         OI    AGRPSTH+6,X'80'                                                  
*                                                                               
VPX      B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*        DISPLAY PST CODES                                                      
*                                                                               
DISPPST  NTR1                                                                   
***      LA    R6,ELEM                                                          
         USING AORELEM,R6                                                       
         XC    PSTOUT,PSTOUT                                                    
         OC    AORPST,AORPST   IS THERE ANYTHING TO DISPLAY                     
         BZ    DPX                                                              
*                                                                               
         LA    R4,BLOCK                                                         
         USING PSTBLKD,R4                                                       
         XC    0(200,R4),0(R4)     CLEAR INTERFACE BLOCK                        
         XC    200(200,R4),200(R4)                                              
         MVI   PSTACT,PSTFMTQ      ACTION = FORMAT                              
         LA    R1,AORPST                                                        
         ST    R1,PSTADIN          INPUT ADDRESS                                
         LA    R1,PSTOUT                                                        
         ST    R1,PSTADOUT         OUTPUT ADDRESS                               
         MVC   PSTACOM,ACOMFACS    A(COMFACS)                                   
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A6B'                                           
         GOTO1 CALLOV,DMCB                                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(R4)                                                   
*                                                                               
DPX      B     EXIT                                                             
         SPACE 3                                                                
*        CHKACCT- READ ACCT FILE                                                
         SPACE 2                                                                
CHKACCT  NTR1                                                                   
         MVI   ERROR,0                                                          
*                                                                               
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
         MVC   SVCOMP,0(R1)        COMPANY CODE                                 
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
         GOTO1 (RF),DMCB,=C'PRINT',0                                            
         CLI   4(R1),0             ALL ERRORS FATAL                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CHKAX    DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
* LIST RECORDS                                                                  
*                                                                               
LR       DS    0H                                                               
         LA    R6,KEY                                                           
         USING AORREC,R6                                                        
         MVC   AIO,AIO1                                                         
         OC    KEY,KEY             TEST FIRST TIME                              
         BNZ   LR010                                             BOG01          
*                                                                               
         MVC   AORKAGY,AGENCY   CREATE KEY  -- AGENCY               L01         
         MVC   AORKCLT,SVCLT                   CLIENT              L01          
         MVC   AORKMED,QMED                    MEDIA CODE           L01         
         MVI   AORKRCD,X'14'                   ID                               
         MVC   AORKPRD,SVPRD                                      L01           
         MVC   AORKEST,SVEST                                      L01           
*                                                                               
LR010    GOTO1 HIGH                                                             
         B     LR030                                                            
*                                                                               
LR020    GOTO1 SEQ                                                              
*                                                                               
LR030    CLC   KEY(4),KEYSAVE      TEST FOR ALL DONE               L01          
         BNE   LR900                                                            
         CLI   KEY+3,X'14'         SEE IF AOR REC                               
         BNE   LR900                                                            
         OC    SVCLT,SVCLT         SEE IF CLIENT GIVEN                          
         BZ    LR040                                                            
LRO35    CLC   KEY+4(3),SVCLT      TEST FOR END OF CLIENT          L01          
         BNE   LR900                                                            
LR040    GOTO1 GETREC              GET THE AOR RECORD                           
         L     R6,AIO                                                           
*                                                                               
         LA    R5,P1               USE P LINES                                  
         CLI   MODE,PRINTREP                                                    
         BE    LR090                                                            
         LA    R5,LISTAR           OR LIST AREA                                 
         MVC   LISTAR,SPACES                                                    
         MVC   SELHED(13),=C'CLT  PRD  EST'                        L01          
         MVC   0(3,R5),AORKCLT                                                  
         MVC   5(3,R5),AORKPRD                                                  
         LA    RE,10(R5)                                                        
         CLC   AORKEST,=X'FFFF'                                                 
         BNE   DOEDIT                                                           
         MVC   0(3,RE),=C'ALL'                                                  
         B     DDISP                                                            
DOEDIT   DS    0H                                                               
         EDIT  (2,AORKEST),(3,(RE))                      L01                    
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
         EJECT                                                                  
*          DATA SET SPSFM0B    AT LEVEL 022 AS OF 05/04/88                      
* PRINT SCHEME REPORT                                                           
*                                                                               
PR       L     R8,ASPOOLD                                          L03          
         USING SPOOLD,R8                                                        
*                                                                               
         LA    R1,HEDSPECS         SET UP HEADHOOK AND SPECS                    
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         MVC   H2+15(10),MEDNM                                                  
         MVC   H2+10(1),QMED                                                    
*                                                                               
*                                                                               
         LA    R4,KEY              SET UP REGISTER FOR KEY IN DIRECTORY         
         USING AORREC,R4                                                        
         MVC   AIO,AIO1                                                         
*                                                                               
         MVC   AORKAGY,AGENCY   CREATE KEY  -- AGENCY                           
         MVC   AORKCLT,SVCLT                   CLIENT                           
         MVC   AORKMED,QMED                    MEDIA CODE                       
         MVI   AORKRCD,X'14'                   ID                               
         MVC   AORKPRD,SVPRD                                                    
         MVC   AORKEST,SVEST                                                    
*                                                                               
*                                                                               
*                                                                               
*                                                                               
         GOTO1 HIGH                                              L03            
         B     PR30                                                             
*                                                                               
PR20     LA    R4,KEY                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
*                                                                               
PR30     CLC   KEY(4),KEYSAVE      CHECK AGY/MED RECORD                         
         BNE   PR110                                                            
         CLI   KEY+3,X'14'                                                      
         BNE   PR110                                                            
         OC    SVCLT,SVCLT         SEE IF CLIENT GIVEN                          
         BZ    PR31                                                             
         CLC   KEY+4(3),SVCLT                                                   
         BNE   PR110                                                            
         CLI   SVPRD,C' '          SPECIFIC PRODUCT REQUESTED                   
         BNH   PR30C                                                            
         CLC   SVPRD,KEY+7                                                      
         BNE   PR110               END REPORT                                   
PR30C    OC    SVEST,SVEST         SEE IF ESTIMATE GIVEN                        
         BZ    PR31                                                             
         CLC   SVEST,KEY+10                                                     
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
         MVC   P1(3),AORKCLT                                                    
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
         L     R6,AIO                                                           
         LA    R6,33(R6)                                                        
         MVI   ELCODE,3                                                         
         BAS   RE,FIRSTEL          GET THE NAME ELEMENT                         
         BE    *+8                                                              
         B     PR100                                                            
         USING AORELEM,R6                                                       
*                                                                               
         EDIT  (B4,AORPCT),(10,P1+53),4,FLOAT=-                                 
         MVC   WORK(12),=CL12'OF GROSS'                                         
         CLI   AORBAS,0                                                         
         BE    PR60                                                             
         MVC   WORK(12),=CL12'OF AGY COMM.'                                     
         CLI   AORBAS,X'80'                                                     
         BE    PR60                                                             
         MVC   WORK(12),=CL12'OF NET      '                                     
         CLI   AORBAS,X'40'                                                     
         BE    PR60                                                             
         DC    H'0'                 BAD COMMISSION BASIS                        
*                                                                               
PR60     MVC   P1+64(12),WORK                                                   
         OC    AOREFF,AOREFF        CHK FOR DATE                                
         BZ    PR65                                                             
         GOTO1 DATCON,DMCB,(3,AOREFF),(9,P1+77)                                 
*                                                                               
PR65     OC    AORKILL,AORKILL                                                  
         BZ    PR70                                                             
         GOTO1 DATCON,DMCB,(3,AORKILL),(9,P1+87)                                
*                                                                               
PR70     MVC   P1+96(1),AORGSTCD                                                
         BAS   RE,DISPPST                                                       
         OC    PSTOUT,PSTOUT                                                    
         BZ    PR75                                                             
         MVC   P2+96(L'PSTOUT),PSTOUT                                           
*                                                                               
PR75     MVC   P1+100(L'AORRCVBL),AORRCVBL                                      
         MVC   P1+115(L'AORCOMM),AORCOMM                                        
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
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
HOOK     NTR1                      HEADLINE ROUTINES                            
*                                                                               
*                                                                               
HOOKX    B     EXIT                                                             
RECFOUND DC    X'0'                                                             
         SPACE 5                                                                
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
TRAPERR  OC    ERRDISP,ERRDISP     DO I NEED TO OVERRIDE CURSOR POS.            
         BZ    TRAPEND                                                          
         L     RE,SYSPARMS                                                      
         L     RE,0(RE)            RE=A(TRANSLATOR I/O BLOCK)                   
         USING TIOBD,RE                                                         
         OI    TIOBINDS,TIOBSETC   OVERRIDE CURSOR POSITION                     
         LR    RF,R2                                                            
         SR    RF,RA                                                            
         STCM  RF,3,TIOBCURD       DISPLACEMENT TO FIELD HEADER                 
         MVC   TIOBCURI,ERRDISP+1  DISPLACMENT INTO FIELD                       
*                                                                               
TRAPEND  GOTO1 ERREX               NEVER TO RETURN                              
         B     EXIT                                                             
         EJECT                                                                  
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
HEDSPECS SSPEC H2,1,C'MEDIA'                                                    
         SSPEC H1,52,C' PRINT AOR REPORT   '                                    
         SSPEC H2,52,C'------------------- '                                    
         SSPEC H1,95,AGYNAME                                                    
         SSPEC H2,95,AGYADD                                                     
         SSPEC H4,95,RUN                                                        
         SSPEC H5,95,REPORT                                                     
         SSPEC H1,1,REQUESTOR                                                   
         SSPEC H7,1,C'CLT   PRD   EST      ADDRESS'                             
         SSPEC H8,1,C'---   ---   ---      -------'                             
*                               111111111122222222223                           
*                     0123456789012345678901234567890                           
*                                                                               
*                                                                               
         SSPEC H6,89,C'KILL   GST'                                              
         SSPEC H7,54,C'COMM. PCT. BASIS        EFF-DATE   DATE   CODE PX        
               AY/REC ACCT'                                                     
         SSPEC H8,54,C'---------- -----        --------  ------  ---- -X        
               ------------'                                                    
*                     5555555666666666677777777778888888888999                  
*                     3456789012345678901234567890123456789012                  
*                                                                               
         SSPEC H7,116,C'INCOME ACCOUNT'                                         
         SSPEC H8,116,C'--------------'                                         
         DC    X'00'                                                            
*                                                                               
       ++INCLUDE PRSFMFFD                                          L01          
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFME1D                                          L01          
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFMF1D                                          L01          
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE PRSFMWORKD                                         L01         
*                                                                               
SVCOMP   DS    X                                                                
SVCLT    DS    CL3                                                              
SVPRD    DS    CL3                                                              
SVEST    DS    XL2                                                              
SVCLTAOR DS    XL1      CLIENT AOR BASIS                                        
SVCOFF   DS    CL1                                                              
X        DS    XL100                                                            
B2APROF  DS    CL16                                                             
BABPROF  DS    CL16                                                             
ERRDISP  DS    H                                                                
PSTOUT   DS    CL64                                                             
         DS    CL10      SPARE                                                  
*                                                                               
         EJECT                                                                  
       ++INCLUDE DDPSTBLK                                                       
         SPACE 2                                                                
CLTHDRD  DSECT                                                                  
       ++INCLUDE PCLTREC                                           L01          
         EJECT                                                                  
PRDHDRD  DSECT                                                                  
       ++INCLUDE PPRDREC                                           L01          
         ORG   PPRDBILP                                                         
       ++INCLUDE PBILPROF                                                       
         ORG                                                                    
PAORRECD DSECT                                                                  
       ++INCLUDE PAORREC                                            L01         
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDFLDIND                                                       
       ++INCLUDE FATIOB                                                         
         EJECT                                                                  
PBILRECD DSECT                                                                  
       ++INCLUDE PBILLREC                                          L01          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'056PRSFM11S  05/01/02'                                      
         END                                                                    
