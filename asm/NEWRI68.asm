*          DATA SET NEWRI68    AT LEVEL 004 AS OF 05/01/02                      
*          DATA SET NEWRI68    AT LEVEL 126 AS OF 01/31/95                      
*PHASE T32068A,*                                                                
         TITLE 'T32068 - JOHNSON WAX TAPE'                                      
T32068   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**NW68**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         LA    RA,4095(RB)       RA=2ND BASE REGISTER                           
         LA    RA,1(RA)                                                         
         USING T32068,RB,RA                                                     
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS2          R7-ANETWS2+ANETWS3/WORKING STORAGE           
         LA    R7,300(R7)             (ACTUALLY ANETWS2+300 =WRKSTRG)           
         USING WORKD,R7                                                         
         ST    R2,RELO                                                          
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         L     R5,ANETWS4          R5-NDDEMBLOCK=ANETWS4                        
         ST    R5,NBADEM           ANETWS1=CLIENT RECORD                        
         USING NDDEMBLK,R5                                                      
         LA    R1,HDRTN                                                         
         ST    R1,HEADHOOK                                                      
         L     R1,BOOKVAL                                                       
         A     R1,RELO                                                          
         ST    R1,ANTWKTP                                                       
         L     R1,=A(MYIO)                                                      
         ST    R1,AMYIO                                                         
*                                                                               
         CLI   MODE,VALREC                                                      
         BE    VK                                                               
         CLI   MODE,PRINTREP                                                    
         BE    LR                                                               
         CLI   MODE,RUNLAST                                                     
         BE    LASTRUN                                                          
         CLI   MODE,RUNFRST                                                     
         BE    FIRSTRUN                                                         
EXIT     XIT1                                                                   
         EJECT                                                                  
* - RUNFIRST                                                                    
FIRSTRUN DS    0H                                                               
         L     RE,ATWA                                                          
         MVI   29(RE),2            SET RUNLAST HOOK INDICATOR                   
         B     EXIT                                                             
                                                                                
*                                                                               
* - CLOSES TAPE                                                                 
LASTRUN  DS    0H                                                               
         L     R2,ANTWKTP             IF WE WROTE TAPE                          
         CLC   =X'90EC',0(R2)                                                   
         BE    LASTRX                                                           
         CLOSE ((R2),)                CLOSE IT                                  
LASTRX   B     EXIT                                                             
         EJECT                                                                  
*************************************                                           
* VALIDATE REQUEST SCREEN DATA                                                  
*                                                                               
VK       DS    0H                                                               
         L     R6,ATWA                                                          
         USING T320FFD,R6                                                       
         MVI   NBQINIT,0                                                        
         MVI   FTERMFLG,0          SET REQUIRED FLAG                            
*                                                                               
         LA    R2,SPLCLIH              CLIENT                                   
         NETGO NVCLI,DMCB,SPLCLIN                                               
         OI    SPLCLINH+6,X'80'                                                 
         L     R3,NBAIO                MOVE CLIENT REC TO ANETWS1               
         L     RF,ANETWS1                                                       
         MOVE  ((RF),1250),0(R3)                                                
         L     RF,ANETWS1                                                       
         USING CLTHDR,RF                                                        
         MVC   NETID,CNETID            SAVE  CURRENT UNIQUE ID NO               
         LA    RF,CLIST                AND SET CLIST ADD TO ACLISTSV            
         ST    RF,ACLISTSV                                                      
         DROP  RF                                                               
*                                                                               
*                                                                               
         LA    R2,SPLPROH               PRODUCT                                 
         NETGO NVPRDALL,DMCB,SPLPRON                                            
         OI    SPLPRONH+6,X'80'                                                 
*                                                                               
         LA    R2,SPLESTH                ESTIMATE                               
         NETGO NVESTRNG,DMCB,SPLESTN                                            
*                                                                               
         LA    R2,SPLSDTH          START                                        
         NETGO NVSTRDAT,DMCB                                                    
*                                                                               
         LA    R2,SPLEDTH          END                                          
         NETGO NVENDDAT,DMCB                                                    
*                                                                               
*                                                                               
         MVI   TAPEOPT,C'N'                                                     
         MVI   ERROR,INVALID                                                    
         LA    R2,SPLTAPEH                                                      
         NETGO NVGETFLD,DMCB                                                    
         BZ    VKEXIT                                                           
         MVC   TAPEOPT,FLD                                                      
         CLI   FLD,C'Y'                                                         
         BE    VKEXIT                                                           
         CLI   FLD,C'N'                                                         
         BE    VKEXIT                                                           
         B     ERREX                                                            
*                                                                               
VKEXIT   DS    0H                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*****************************************                                       
LR       DS    0H                                                               
* - INITIALIZE SORTER                                                           
         XC    DMCB,DMCB                                                        
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD                                     
* - OPEN TAPE                                                                   
         CLI   TAPEOPT,C'Y'                                                     
         BNE   LR5                                                              
         L     RE,BOOKVAL          SET DCB TO BOOKVAL/OPEN TAPE                 
         CLC   =X'90EC',0(RE)                                                   
         BNE   LR5                                                              
         LA    RF,NTWKTP                                                        
         MVC   0(128,RE),0(RF)                                                  
         MVC   DSNAME+13(2),NBSELAGY                                            
         L     R4,ATWA                                                          
         USING T320FFD,R4                                                       
         L     RF,TWADCONS                                                      
         L     RF,TDYNALLO-TWADCOND(RF)                                         
         GOTO1 (RF),DMCB,DDNAME,DSNAME                                          
*****    GOTO1 =V(DYNALLOC),DMCB,DDNAME,DSNAME                                  
         L     R2,ANTWKTP                                                       
         OPEN  ((R2),(OUTPUT))                                                  
         DROP  R4                                                               
         B     LR5                                                              
                                                                                
DDNAME   DC    CL8'NTWKTP'                                                      
DSNAME   DC    CL20'NETTAPE.NE0WJAA1'                                           
DCBOPEN  DC    C'N'                                                             
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,19,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=300'                                   
*                                                                               
*                                                                               
         SPACE                                                                  
* - SET UP NETIO READ PARAMETERS                                                
LR5      DS    0H                                                               
         MVI   NBDATA,C'U'                UNITS ONLY                            
         OI    NBSPLOPT,X'C0'             SPLIT EVEN IF POL                     
**       MVI   NBESTOPT,C'P'              EST DEMOS ON PFB                      
         MVI   NBESTOPT,X'04'                                                   
         MVI   NBESTFCT,6                 DO AS EFFICIENCY DOES                 
         MVI   NBACTOPT,C'Y'              ACTUAL DEMOS                          
         MVI   NBSELUOP,C'B'              ESTIMATED+ACTUAL SCHEDULE             
         MVI   NBSEQ,C'P'                 ESTIMATE HIGH (X'94' KEY)             
         LA    R1,DOUNIT                  UNIT HOOK                             
         ST    R1,NBHOOK                                                        
*                                                                               
LR10     NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBMODE,NBREQLST                                                  
         BNE   LR10                                                             
*                                                                               
         BAS   RE,DOTAPE                 DO TAPE/REPORT                         
*                                                                               
         CLI   TAPEOPT,C'Y'                                                     
         BNE   EXIT                                                             
         NETGO NVSETSPT                                                         
         LA    R2,KEY                 AND UPDATE CLIENT NET ID NUMBER           
         USING CLTHDR,R2                                                        
         XC    KEY,KEY                                                          
         L     R3,ANETWS1                                                       
         MVC   KEY(13),0(R3)                                                    
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,NBAIO                                                        
         GOTO1 DATAMGR,DMCB,(X'80',=C'DMRDIR'),=C'SPTFILE ',KEY+14,AIO          
         L     R2,NBAIO                                                         
         USING CLTHDR,R2                                                        
         MVC   CNETID,NETID        SET LATEST NET ID                            
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'SPTFILE ',KEY+14,NBAIO                 
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
*  PROCESS UNIT / PUT TO SORTER                                                 
*                                                                               
DOUNIT   NTR1                                                                   
*                                                                               
         MVI   NBUPUNIT,C'N'                                                    
         MVI   NBNOWRIT,C'N'                                                    
*                                                                               
         CLI   NBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
* - NEW CLIENT                                                                  
         CLI   NBMODE,NBVALCLI                                                  
         BNE   DOUNT0                                                           
         L     RE,NBAIO            MOVE CLIENT REC TO ANETWS1                   
         L     RF,ANETWS1                                                       
         MOVE  ((RF),1250),(RE)                                                 
         L     RF,ANETWS1                                                       
         USING CLTHDR,RF                                                        
         LA    RF,CLIST            AND SET CLIST ADDRESS                        
         ST    RF,ACLISTSV                                                      
         DROP  RF                                                               
* - UNITS ONLY                                                                  
DOUNT0   DS    0H                                                               
         CLI   NBMODE,NBPROCUN                                                  
         BNE   DOUNTX                                                           
         LA    RE,SORTREC                      CLEAR SORTREC AREA               
         L     RF,=F'300'                                                       
         XCEF                                                                   
         CLC   NBPRD,NBSPLPRN      MUST MATCH ONE OF PRODS                      
         BE    DOUNT1              (DON'T KNOW WHY GETTING X'FF')               
         CLI   NBPRD2,0                                                         
         BE    DOUNTX                                                           
         CLC   NBPRD2,NBSPLPRN                                                  
         BE    DOUNT1                                                           
         MVC   CURPRD1,NBSPLPRN    X'FF'/SET SO FORCES LOOKUP NEXTIME           
         B     DOUNTX                                                           
DOUNT1   DS    0H                              PRODUCT                          
         BAS   RE,GETPRD                                                        
         MVC   CURPRD1,NBSPLPRN                                                 
DOUNT2   CLC   CUREST,NBACTEST                 ESTIMATE                         
         BE    DOUNT7                                                           
         BAS   RE,GETEST                    GET EST NAME/ DEMO TARGET           
         MVC   CUREST,NBACTEST                                                  
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'NTI'                                                   
         MVI   DBSELMED,C'N'                                                    
         MVI   DBSELSRC,C'N'                                                    
         NETGO NVDEMCON,DMCB,(0,NDDEMBLK),DBLOCK,(7,CURTDEMN)  TARGET           
         CLI   CURTDEMN+7,C'('               IF IT'S (RT OR (IM                 
         BNE   *+10                                                             
         XC    CURTDEMN+7(3),CURTDEMN+7      CLEAR IT                           
         SPACE                                                                  
DOUNT7   DS    0H                                                               
* - SORTER KEY FIELDS                                                           
         MVC   SRTKPRD,CURPRD3              PROD                                
         EDIT  (B1,NBACTEST),(3,SRTKEST)    EST                                 
         MVC   SRTKDAT,NBACTDAT             DATE                                
         MVC   SRTKLIN,NBACTSUB             SUB-LINE                            
         MVC   SRTKPROG,NBACTPRG            PROGRAM                             
         MVC   SRTKNET,NBACTNET             NETWORK                             
* - DATA FIELDS                                                                 
         MVC   SRTPRD,CURPRD3                          PRODUCT                  
                                                                                
         EDIT  (B1,NBACTEST),(3,SRTESTNO),ALIGN=LEFT   EST NUMBER               
         CLI   SRTESTNO+2,X'40'    SEND ZEROS NOT BLANKS                        
         BNE   *+8                                                              
         MVI   SRTESTNO+2,0                                                     
         CLI   SRTESTNO+1,X'40'    SEND ZEROS NOT BLANKS                        
         BNE   *+8                                                              
         MVI   SRTESTNO+1,0                                                     
                                                                                
         MVC   SRTESTNM,CURESTNM                       EST NAME                 
         LA    R6,SRTESTNM+19        SEND ZERO NOT BLANKS                       
         LA    RE,20                                                            
ZEROEST  CLI   0(R6),X'40'                                                      
         BNE   *+8                                                              
         MVI   0(R6),0                                                          
         BCTR  R6,0                                                             
         BCT   RE,ZEROEST                                                       
                                                                                
         MVC   SRTESTST,CURESTST                       MMDDYYYY                 
         MVC   SRTESTND,CURESTND                       MMDDYYYY                 
         BAS   RE,GETBUYID                                                      
         L     R1,FULL                                                          
         CVD   R1,DUB                                                           
         MVC   SRTBUYID,DUB                                                     
         MVC   SRTDPT,NBACTDP                          DAYPART                  
         GOTO1 DATCON,DMCB,(2,NBACTDAT),(5,WORK)                                
         MVC   SRTMONTH,WORK                           MONTH                    
*        MVC   SRTDAY,NBDAYNAM                         EXPANDED DAY             
         MVI   SRTDAY,0                                                         
         MVC   SRTDAY+1(2),WORK+3                      0NN                      
         CLI   SRTDAY+1,C'0'                           OR 00N                   
         BNE   *+8                                                              
         MVI   SRTDAY+1,0                                                       
                                                                                
         MVC   SRTPROG,NBACTPRG                        PROGRAM                  
         LA    R6,SRTPROG+5        SEND ZERO NOT BLANKS                         
         LA    RE,6                                                             
ZEROPG   CLI   0(R6),X'40'                                                      
         BNE   *+8                                                              
         MVI   0(R6),0                                                          
         BCTR  R6,0                                                             
         BCT   RE,ZEROPG                                                        
                                                                                
         MVC   SRTPRGNM,NBPROGNM                       PROGRAM NAME             
         LA    R6,SRTPRGNM+15        SEND ZERO NOT BLANKS                       
         LA    RE,16                                                            
ZEROPRG  CLI   0(R6),X'40'                                                      
         BNE   *+8                                                              
         MVI   0(R6),0                                                          
         BCTR  R6,0                                                             
         BCT   RE,ZEROPRG                                                       
                                                                                
         MVC   SRTNET,NBACTNET                         NETWORK                  
         CLI   SRTNET+3,X'40'      SEND ZERO                                    
         BNE   *+8                                                              
         MVI   SRTNET+3,0                                                       
                                                                                
         EDIT  (B1,NBLEN),(2,SRTULEN)                  LENGTH                   
         MVC   FULL,NBACTUAL                           ACTUAL COST              
         L     R1,FULL                                                          
         CVD   R1,DUB                                                           
         MVC   SRTACT,DUB+2                                                     
         MVC   FULL,NBINTEG                            INTEGRATION              
         L     R1,FULL                                                          
         CVD   R1,DUB                                                           
         MVC   SRTINTG,DUB+2                                                    
         MVC   HALF,NBESTHOM+2                         EST HH RTG 1 DEC         
         LH    R1,HALF                                                          
         CVD   R1,DUB                                                           
         MVC   SRTEHR,DUB+5                                                     
         MVC   HALF,NBACTHOM+2                         ACT HH RTG 1 DEC         
         LH    R1,HALF                                                          
         CVD   R1,DUB                                                           
         MVC   SRTAHR,DUB+5                                                     
         MVC   FULL,NBESTHOM+4                         EST HH IMP               
         L     R1,FULL                                                          
         CVD   R1,DUB                                                           
         MVC   SRTEHI,DUB+2                                                     
         MVC   FULL,NBACTHOM+4                         ACT HH IMP               
         L     R1,FULL                                                          
         CVD   R1,DUB                                                           
         MVC   SRTAHI,DUB+2                                                     
         MVC   SRTTDEMO,CURTDEMN                                                
         MVC   HALF,NDESTDEM+2                         EST TARGET GRP           
         LH    R1,HALF                                                          
         CVD   R1,DUB                                                           
         MVC   SRTETR,DUB+5                                                     
         MVC   HALF,NDACTDEM+2                         ACT TARGET GRP           
         LH    R1,HALF                                                          
         CVD   R1,DUB                                                           
         MVC   SRTATR,DUB+5                                                     
         MVC   FULL,NDESTDEM+4                         EST TARGET IMP           
         L     R1,FULL                                                          
         CVD   R1,DUB                                                           
         MVC   SRTETI,DUB+2                                                     
         MVC   FULL,NDACTDEM+4                         ACT TARGET IMP           
         L     R1,FULL                                                          
         CVD   R1,DUB                                                           
         MVC   SRTATI,DUB+2                                                     
         L     R6,NBAIO                                ACTIVITY DATE            
         MVI   ELCODE,X'99'                            (MMDDYYYY)               
         BAS   RE,GETEL                                                         
         BNE   DOUNT10                                                          
         USING NUACTD,R6                                                        
         CLI   NUACTCDT,0          IF NO CHANGE DATE                            
         BE    DOUNT9                                                           
         LA    R6,NUACTCDT                                                      
         B     *+8                                                              
DOUNT9   LA    R6,NUACTADT         USE BUY DATE                                 
         GOTO1 DATCON,DMCB,(3,0(R6)),(0,WORK)                                   
         MVC   SRTACTIV(4),WORK+2                                               
         MVC   SRTACTIV+4(2),=C'19'                                             
         CLI   0(R6),80                                                         
         BH    *+10                                                             
         MVC   SRTACTIV+4(2),=C'20'                                             
         MVC   SRTACTIV+6(2),WORK                                               
         DROP  R6                                                               
DOUNT10  L     R6,NBAIO                                 PACKAGE FILTER          
         MVI   ELCODE,8                                                         
         USING NUFILD,R6                                                        
         BAS   RE,GETEL                                                         
DOUNT12  BNE   DOUNT20                                                          
         CLI   NUFILSCM,C'K'                                                    
         BE    DOUNT14                                                          
         BAS   RE,NEXTEL                                                        
         B     DOUNT12                                                          
DOUNT14  MVC   SRTPFILT,NUFILTER                                                
*                                                                               
DOUNT20  DS    0H                                        ISCI                   
         BAS   RE,GETISCIS                          (COMMERCIAL NOS)            
*                                                                               
         MVC   SRTMEDIA,NBSTATYP                         MEDIA                  
         CLI   SRTMEDIA,X'40'                                                   
         BH    *+8                                                              
         MVI   SRTMEDIA,C'N'       DEFAULT TO NETWORK                           
         MVI   SRTUTYP,C'M'                                                     
         TM    NBUNITST,X'02'                            MISSED                 
         BO    DOUNT30                                                          
         MVI   SRTUTYP,C'P'                                                     
         TM    NBUNITST,X'40'                            PREEMPT                
         BO    DOUNT30                                                          
         MVI   SRTUTYP,0                                                        
*                                                                               
DOUNT30  DS    0H                                                               
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
*                                                                               
         B     DOUNT40                                                          
         GOTO1 HEXOUT,DMCB,SORTREC,P,300                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
DOUNT40  CLI   TAPEOPT,C'Y'         IF PRODUCING TAPE                           
         BNE   EXIT                                                             
         CLI   NEWID,C'Y'          AND IF UNIT NOT ALREADY WRITTEN BACK         
         BNE   EXIT                                                             
         MVI   NBUPUNIT,C'Y'       UPDATE UNITS (WITH UNIQUE ID)                
         MVI   NBNOWRIT,C'Y'                                                    
DOUNTX   B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* GET COMMERCIAL NUMBERS (ISCI)                                                 
*                                                                               
GETISCIS NTR1                                                                   
         L     R6,NBAIO                                                         
* - UNIT COMMERCIAL ELEMENT                                                     
         MVI   ELCODE,X'21'                                                     
         BAS   RE,GETEL                                                         
         B     *+12                                                             
GETI10   MVI   ELCODE,X'21'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   GETI20                                                           
         USING NUCMLEL,R6                                                       
         TM    NUCMLFLG,X'E8'      DELETED/ETC                                  
         BNZ   GETI10                                                           
         CLI   NUCML1,X'40'        IF NO ISCI/DEPART                            
         BNH   GETI10                                                           
         TM    NBUNST3,X'40'       IS IT COPYSPLIT                              
         BO    GETI15                                                           
         CLC   NBSPLPRN,NBPRD      NO/ 1ST PRODUCT                              
         BNE   GETI12                                                           
         MVC   WORK(8),NUCML1      1ST COMMERCIAL                               
         XC    WORK+8(4),WORK+8                                                 
         MVI   ELCODE,X'22'        IS IT A FEED                                 
         USING NUFEDEL,R6                                                       
         BAS   RE,NEXTEL                                                        
         BNE   *+10                                                             
         MVC   WORK+8(4),NUFEEDCD                                               
         BAS   RE,ADDISCIS                                                      
         B     GETI10                                                           
GETI12   CLC   NBSPLPRN,NBPRD2     IF 2ND PRODUCT                               
         BNE   GETI10                                                           
         USING NUCMLEL,R6                                                       
         CLI   NUCML2,X'40'        IF 2ND COMMERCIAL                            
         BNH   GETI10                                                           
         MVC   WORK(8),NUCML2                                                   
         XC    WORK+8(4),WORK+8                                                 
         MVI   ELCODE,X'22'        IS IT A FEED                                 
         USING NUFEDEL,R6                                                       
         BAS   RE,NEXTEL                                                        
         BNE   *+10                                                             
         MVC   WORK+8(4),NUFEEDCD                                               
         BAS   RE,ADDISCIS                                                      
         B     GETI10                                                           
*                                                                               
GETI15   DS    0H                  COPYSPLIT                                    
         USING NUCMLEL,R6                                                       
         CLC   NBSPLPRN,NUCMLPRD   UNIT CML MUST = COPY SPLIT CML FLD           
         BNE   GETI10                                                           
         MVC   WORK(8),NUCML1                                                   
         XC    WORK+8(4),WORK+8                                                 
         MVI   ELCODE,X'22'        IS IT A FEED                                 
         USING NUFEDEL,R6                                                       
         BAS   RE,NEXTEL                                                        
         BNE   *+10                                                             
         MVC   WORK+8(4),NUFEEDCD                                               
         BAS   RE,ADDISCIS                                                      
         B     GETI10                                                           
*                                                                               
* - FEED COMMERCIAL ELEMENT                                                     
GETI20   L     R6,NBAIO                                                         
         MVI   ELCODE,X'23'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
GETI22   BAS   RE,NEXTEL                                                        
         BNE   GETI40                                                           
         USING NUFDCEL,R6                                                       
         TM    NUFDCFLG,X'E8'      DELETED/ETC                                  
         BO    GETI22                                                           
         CLI   NUFDCML1,X'40'          IF NO ISCI/DEPART                        
         BNH   GETI22                                                           
         TM    NBUNST3,X'40'       IS IT COPYSPLIT                              
         BO    GETI30                                                           
         CLC   NBSPLPRN,NBPRD      NO/ 1ST PRODUCT                              
         BNE   GETI25                                                           
         MVC   WORK(8),NUFDCML1      1ST COMMERCIAL                             
         MVC   WORK+8(4),NUFDCFED       FEED                                    
         BAS   RE,ADDISCIS                                                      
         B     GETI22                                                           
GETI25   CLC   NBSPLPRN,NBPRD2     IF 2ND PRODUCT                               
         BNE   GETI22                                                           
         CLI   NUFDCML2,X'40'          IF 2ND COMMERCIAL                        
         BNH   GETI22                                                           
         MVC   WORK(8),NUFDCML2                                                 
         MVC   WORK+8(4),NUFDCFED       FEED                                    
         BAS   RE,ADDISCIS                                                      
         B     GETI22                                                           
*                                                                               
GETI30   DS    0H                  COPYSPLIT                                    
         CLC   NBSPLPRN,NUFDCPRD   UNIT CML MUST = COPY SPLIT CML FLD           
         BNE   GETI22                                                           
         MVC   WORK(8),NUFDCML1                                                 
         MVC   WORK+8(4),NUFDCFED  FEED                                         
         BAS   RE,ADDISCIS                                                      
         B     GETI22                                                           
GETI40   DS    0H                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         SPACE                                                                  
* - COMMERCIAL NUMBER (ISCI) SITS IN WORK/ ADD TO STRISCI                       
* - MAX NUMBER OF ISCIS = 10                                                    
ADDISCIS NTR1                                                                   
         LA    R2,SRTISCI                                                       
         LA    R3,10                                                            
ADDI10   CLI   8(R2),0                                                          
         BE    ADDI20                                                           
         LA    R2,12(R2)                                                        
         BCT   R3,ADDI10                                                        
         DC    H'0'                TOO MANY COMMERCIALS                         
ADDI20   MVC   0(4,R2),WORK+8      ADD FEED                                     
         MVC   4(8,R2),WORK        ADD CML                                      
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*        WRITE REPORT / TAPE                                                    
*        R3 POINTS TO REC FROM SORTER                                           
*                                                                               
DOTAPE   NTR1                                                                   
DOT0     LA    RE,SORTREC                                                       
         L     RF,=F'300'                                                       
         XCEF                                                                   
         GOTO1 SORTER,DMCB,=C'GET'       TAKE RECS FROM SORTER                  
         L     R3,4(R1)                                                         
         LA    RF,SORTREC                                                       
         LA    R1,SRTRECL                                                       
         LR    RE,R3                                                            
         MOVE  ((RF),(R1)),(RE)                                                 
         LTR   R3,R3                                                            
         BZ    DOTX                                                             
         LA    R2,P                                                             
         USING PLINED,R2                                                        
         CLC   CURTDEMN,SRTTDEMO   HAS TARGET DEMO CHANGED                      
         BE    *+14                                                             
         MVC   CURTDEMN,SRTTDEMO   YES/SET NEW DEMO                             
         MVI   FORCEHED,C'Y'           AND FORCE TOP OF PAGE                    
         MVC   PPRD,SRTPRD                                                      
         MVC   PEST,SRTESTNO                                                    
**       GOTO1 DATCON,DMCB,(2,SRTKDAT),(5,PESTST)                               
**       MVI   SRTESTND,C'-'                                                    
**       LA    R4,SRTESTND                                                      
**       EDIT  (B1,SRTKLIN),(3,1(R4)),ALIGN=LEFT                                
         MVC   PESTST,SRTESTST                                                  
         MVC   PESTEND,SRTESTND                                                 
         EDIT  (P8,SRTBUYID),(9,PUNIQUE)                                        
         MVC   PDPT+1(1),SRTDPT                                                 
         MVC   PPROG,SRTPROG                                                    
         MVC   PNET,SRTNET                                                      
         MVC   PUNLEN,SRTULEN                                                   
         LA    R4,PHHEST                                                        
         EDIT  (P3,SRTEHR),(5,0(R4)),1                                          
         EDIT  (P6,SRTEHI),(8,6(R4))                                            
         LA    R4,PHHACT                                                        
         EDIT  (P3,SRTAHR),(5,0(R4)),1                                          
         EDIT  (P6,SRTAHI),(8,6(R4))                                            
         LA    R4,PTRGEST                                                       
         EDIT  (P3,SRTETR),(5,0(R4)),1                                          
         EDIT  (P6,SRTETI),(8,6(R4))                                            
         LA    R4,PTRGACT                                                       
         EDIT  (P3,SRTATR),(5,0(R4)),1                                          
         EDIT  (P6,SRTATI),(8,6(R4))                                            
         MVC   PMEDIA+1(1),SRTMEDIA                                             
         MVC   PTYPE+1(1),SRTUTYP                                               
         BAS   RE,SPOOLIT                                                       
         CLI   SRTISCI+8,X'40'       IF WE HAVE ISCIS                           
         BNH   DOT30                                                            
         MVC   PESTST(80),SRTISCI  PRINT THEM                                   
         BAS   RE,SPOOLIT                                                       
DOT30    BAS   RE,WRITAPE                                                       
         B     DOT0                                                             
DOTX     B     EXIT                                                             
         EJECT                                                                  
WRITAPE  NTR1                                                                   
         L     R1,ANTWKTP                                                       
         LA    RE,RECWORK                                                       
         L     RF,=F'300'                                                       
         XCEF                                                                   
         LA    RF,RECWORK                                                       
         LA    R1,SRTRECL1                                                      
         LA    RE,SRTPRD                                                        
         MOVE  ((RF),(R1)),(RE)                                                 
         CLI   TAPEOPT,C'Y'                                                     
         BNE   WRT10                                                            
         L     R1,ANTWKTP                                                       
         PUT   (R1),RECWORK                   WRITE TAPE                        
WRT10    B     EXIT                                                             
         GOTO1 HEXOUT,DMCB,RECWORK,P,300                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
         SPACE                                                                  
*******************************                                                 
*                                                                               
SPOOLIT  NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                                                             
*                                                                               
         SPACE 3                                                                
**********************************                                              
*  TO GET PRD CODE FROM C LIST                                                  
*                                                                               
GETPRD   NTR1                                                                   
         L     R2,ACLISTSV                                                      
GP10     CLI   3(R2),0             IF E-O-F CLIST                               
         BNE   GP12                                                             
         MVC   CURPRD3,=C'UNA'    SET TO UNDEFINED                              
         B     GPX                                                              
GP12     CLC   NBSPLPRN,3(R2)                                                   
         BE    GP14                                                             
         LA    R2,4(R2)            INCREMENT CLIST                              
         B     GP10                RETURN TO LOOP                               
GP14     MVC   CURPRD3,0(R2)      SET 3 CHAR PRINTABLE PRD CODE                 
*                                                                               
GPX      B     EXIT                                                             
         EJECT                                                                  
* - GET NEW EST RECORD / RETURN EST NAME, NEW TARGET DEMO                       
GETEST   NTR1                                                                   
         XC    KEY,KEY                                                          
         NETGO NVSETSPT                                                         
         LA    R2,KEY                                                           
         USING ESTHDR,R2                                                        
         MVC   EKEYAM,NBACTAM                                                   
         MVC   EKEYCLT,NBACTCLI                                                 
         MVC   EKEYPRD,=C'POL'                                                  
         MVC   EKEYEST,NBACTEST                                                 
         BAS   RE,DOHIGH                                                        
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R3,KEY+14                                                        
         L     R4,AMYIO                                                         
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFIL  ',(R3),(R4),MYDMW             
         USING ESTHDR,R4                                                        
         MVC   CURESTNM,EDESC                                                   
         MVC   CURESTST(4),ESTART+2                                             
         MVC   CURESTST+4(2),=C'19'                                             
         CLC   =C'80',ESTART                                                    
         BL    *+10                                                             
         MVC   CURESTST+4(2),=C'20'                                             
         MVC   CURESTST+6(2),ESTART                                             
         MVC   CURESTND(4),EEND+2                                               
         MVC   CURESTND+4(2),=C'19'                                             
         CLC   =C'80',EEND                                                      
         BL    *+10                                                             
         MVC   CURESTND+4(2),=C'20'                                             
         MVC   CURESTND+6(2),EEND                                               
         CLC   NDDEMOS(3),EDEMLST        IS IT SAME TARGET DEMO                 
         BE    GTS10                                                            
         MVC   NDDEMOS(60),EDEMLST           SET NEW TARGET DEMOS               
         MVC   NDDEMOS+60(3),EDEM21           SET NEW TARGET DEMOS              
         MVC   NDWGTLST,EWGTLST                                                 
         MVC   NDWGTNAM,EWGTNM                                                  
         MVC   NDUSRNMS,EUSRNMS                                                 
         MVI   USEIO,C'N'                RESET UNTFILE VALUES                   
         XC    FILENAME,FILENAME                                                
         NETGO NVSETUNT                                                         
         NETGO NBNETVAL,DMCB,NETBLOCK    REVALUE NETBLOCK FOR NEW DEMO          
         B     GTS20                                                            
GTS10    MVI   USEIO,C'N'                                                       
         XC    FILENAME,FILENAME                                                
         NETGO NVSETUNT                                                         
GTS20    MVI   NBFUNCT,NBFRDHI                                                  
         B     EXIT                                                             
         DROP  R2,R4                                                            
*                                                                               
DOHIGH   NTR1                                                                   
         MVC   KEYSAVE,KEY                                                      
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* EACH UNIT REC FOR JOHNSON HAS UNIQUE BUY ID ELEMENT                           
* LATEST ID NUMBER LIVES ON CLIENT RECORD                                       
*                                                                               
GETBUYID NTR1                                                                   
         L     R6,NBAIO            ...DOES UNIT HAVE ID NUMBER                  
         MVI   ELCODE,5                                                         
         BAS   RE,GETEL                                                         
         BNE   GBY10               ...YES                                       
         USING NUIDEL,R6                                                        
         MVC   FULL,NUID                                                        
         MVI   NEWID,0                                                          
         B     GBX                                                              
GBY10    L     R4,NETID            ...NO/GET LAST UNIQE ID NUMBER               
         LA    R4,1(R4)            BUMP IT                                      
         XC    ELEM,ELEM           ADD ID ELEM TO UNIT                          
         MVI   ELEM,5                                                           
         MVI   ELEM+1,10                                                        
         STCM  R4,15,ELEM+2            SET ID NUMBER IN ELEM                    
         GOTO1 ADDELEM                                                          
         ST    R4,NETID             SAVE LATEST ID NUMBER                       
         ST    R4,FULL                                                          
         MVI   NEWID,C'Y'                                                       
GBX      XIT1                                                                   
         EJECT                                                                  
*                                                                               
HDRTN    NTR1                                                                   
         LA    R2,H10                                                           
         USING PLINED,R2                                                        
         MVC   PPRD,=C'PRD'                                                     
         MVC   PEST,=C'EST'                                                     
         MVC   PESTST,=C'ESTIMATE'                                              
         MVC   PESTST+133(5),=C'START'                                          
         MVC   PESTEND,=C'ESTIMATE'                                             
         MVC   PESTEND+134(3),=C'END'                                           
         MVC   PUNIQUE+1(7),=C'UNIT ID'                                         
         MVC   PDPT,=C'DPT'                                                     
         MVC   PPROG,=C'PROGRM'                                                 
         MVC   PNET,=C'NET '                                                    
         MVC   PHHEST+1(12),=C'    HH      '                                    
         MVC   PHHEST+133(12),=C' RTG    IMPS'                                  
         MVC   PHHEST+265(12),=C'  ESTIMATED '                                  
         MVC   PHHACT+1(12),=C'    HH      '                                    
         MVC   PHHACT+133(12),=C' RTG    IMPS'                                  
         MVC   PHHACT+265(12),=C'   ACTUAL   '                                  
         MVC   PTRGEST+3(10),CURTDEMN                                           
         MVC   PTRGEST+133(12),=C' RTG    IMPS'                                 
         MVC   PTRGEST+265(12),=C'  ESTIMATED '                                 
         MVC   PTRGACT+3(10),CURTDEMN                                           
         MVC   PTRGACT+133(12),=C' RTG    IMPS'                                 
         MVC   PTRGACT+265(12),=C'   ACTUAL   '                                 
         MVC   PMEDIA,=C'MED'                                                   
         MVC   PTYPE,=C'P/M'                                                    
*                                                                               
         DS    0H                                                               
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
         LA    R4,BOXCOLS                                                       
         USING PLINED,R4                                                        
         MVI   0(R4),C'L'                                                       
         MVI   PLN1,C'C'                                                        
         MVI   PLN2,C'C'                                                        
         MVI   PLN3,C'C'                                                        
         MVI   PLN4,C'C'                                                        
         MVI   PLN5,C'C'                                                        
         MVI   PLN6,C'C'                                                        
         MVI   PEND,C'R'                                                        
         SPACE                                                                  
         LA    R4,BOXROWS                                                       
         LA    R4,8(R4)                                                         
         MVI   0(R4),C'T'                                                       
         LA    R4,4(R4)                                                         
         MVI   0(R4),C'M'                                                       
         LA    R4,49(R4)                                                        
         MVI   0(R4),C'B'                                                       
         SPACE                                                                  
HDX      B     EXIT                 (XIT1)                                      
         DROP  R2,R4                                                            
         EJECT                                                                  
         SPACE 2                                                                
HEADING  SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,52,C' NETWORK MEDIA TAPE REPORT'                              
         SSPEC H2,52,C' -------------------------'                              
         SSPEC H3,53,PERIOD                                                     
         SSPEC H1,98,AGYNAME                                                    
         SSPEC H2,98,AGYADD                                                     
         SSPEC H3,98,REPORT                                                     
         SSPEC H4,98,RUN                                                        
         SSPEC H5,98,PAGE                                                       
         DC    X'00'                                                            
*                                                                               
NTWKTP   DCB   DDNAME=NTWKTP,                                          X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=00257,                                            X        
               BLKSIZE=02570,                                          X        
               MACRF=PM                                                         
         SPACE 2                                                                
         GETEL (R6),DATADISP,ELCODE                                             
*                                                                               
         LTORG                                                                  
*                                                                               
MYIO     DS    CL2000                                                           
         EJECT                                                                  
*                                                                               
WORKD    DSECT                     MYWORK AREA  ANETWS2                         
*                                                                               
*                                                                               
RELO     DS    F                                                                
MYDMW    DS    CL96                                                             
ACLISTSV DS    F                                                                
ANTWKTP  DS    F                                                                
AMYIO    DS    A                                                                
NETID    DS    F                                                                
TAPEOPT  DS    CL1                                                              
NEWID    DS    CL1                 SET TO N IF NOT WRITING UNIT BACK            
*                                  PIGGYS WILL BLOW DATAMGR IF DOING            
*                                  TWO PUTRECS AFTER ONE GETREC                 
*                                  (AS IN NETIO HOOK)                           
*                                                                               
CUREST   DS    CL1                                                              
CURESTNM DS    CL20                                                             
CURESTST DS    CL8                                                              
CURESTND DS    CL8                                                              
CURPRD1  DS    CL1                                                              
CURPRD3  DS    CL3                                                              
CURPROG  DS    CL6                                                              
CURPRGNM DS    CL16                                                             
CURTDEMO DS    CL3                                                              
CURTDEMN DS    CL10                                                             
*                                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
SORTREC  DS    CL300                                                            
         ORG   SORTREC                                                          
SRTKPRD  DS    CL3                                                              
SRTKEST  DS    CL3                                                              
SRTKDAT  DS    CL2                 DATE(COMPRESSED)                             
SRTKLIN  DS    CL1                 LINE NUMBER                                  
SRTKPROG DS    CL6                                                              
SRTKNET  DS    CL4                                                              
SRTKEYL  EQU   *-SRTKPRD                                                        
SRTPRD   DS    CL3                                                              
SRTCCODE DS    XL1                 COUNTRY CODE=X'00'                           
SRTESTID DS    0CL23                                                            
SRTESTNO DS    CL3                 EST NUMBER                                   
SRTESTNM DS    CL20                ESTNAME                                      
SRTFILL  DS    CL1                 X'00'                                        
SRTESTST DS    CL8                 MMDDYYYY                                     
SRTESTND DS    CL8                 MMDDYYYY                                     
SRTBUYID DS    PL8                 UNIQUE IDENTIFICATION NO                     
SRTDPT   DS    CL1                 DAYPART                                      
SRTMONTH DS    CL3                 JAN/FEB/ETC                                  
SRTDAY   DS    CL3                 MON/TUE/WED                                  
SRTPROG  DS    CL6                 PROGRAM                                      
SRTPRGNM DS    CL16                PROGRAM NAME                                 
SRTNET   DS    CL4                 NETWORK                                      
SRTULEN  DS    CL2                 UNIT LENGTH/30/60/ETC                        
SRTACT   DS    PL6                 ACTUAL COST/DECIMALS                         
SRTINTG  DS    PL6                 INTEGRATION/DECIMALS                         
SRTEHR   DS    PL3                 EST HH RTG/PACKED ONE DEC                    
SRTAHR   DS    PL3                 ACT HH RTG/PACKED ONE DEC                    
SRTEHI   DS    PL6                 EST HH IMPS/PACKED/THOU                      
SRTAHI   DS    PL6                 ACT HH IMPS/PACKED/THOU                      
SRTTDEMO DS    CL10                TARGET DEMO                                  
SRTETR   DS    PL3                 EST TARGET RTG/PACKED 1 DEC                  
SRTATR   DS    PL3                 ACT TARGET RTG/PACKED 1 DEC                  
SRTETI   DS    PL6                 EST TARGET IMPS/PACKED THOUS                 
SRTATI   DS    PL6                 ACT TARGET IMPS/PACKED THOUS                 
SRTACTIV DS    CL8                 LAST ACTIVITY DATE/MMDDYYYY                  
SRTPFILT DS    CL6                 PACKAGE GILTER                               
SRTISCI  DS   0CL96                ISCI REGION CODES 8 * 12                     
         DS    CL4                 REGION FEED                                  
         DS    CL8                 ISCI                                         
         DS    CL4                                                              
         DS    CL8                                                              
         DS    CL4                                                              
         DS    CL8                                                              
         DS    CL4                                                              
         DS    CL8                                                              
         DS    CL4                                                              
         DS    CL8                                                              
         DS    CL4                                                              
         DS    CL8                                                              
         DS    CL4                                                              
         DS    CL8                                                              
         DS    CL4                                                              
         DS    CL8                                                              
SRTMEDIA DS    CL1                 MEDIA TYPE                                   
SRTUTYP  DS    CL1                 P=PREEMPT/M=MISSED                           
SRTRECL  EQU   *-SORTREC           LENGTH OF REC + SRTKEY                       
SRTRECL1 EQU   *-SRTPRD            LENGTH OF REC                                
         DS    CL24                SORTSPARE                                    
*                                                                               
RECWORK  DS    CL300                                                            
         EJECT                                                                  
*                                                                               
PLINED   DSECT                    DSECT FOR PRINTED REPORT                      
PSTR     DS    CL1                                                              
PPRD     DS    CL3                                                              
         DS    CL1                                                              
PEST     DS    CL3                                                              
         DS    CL1                                                              
PESTST   DS    CL8                                                              
         DS    CL1                                                              
PESTEND  DS    CL8                                                              
         DS    CL1                                                              
PUNIQUE  DS    CL9                                                              
         DS    CL1                                                              
PDPT     DS    CL3                                                              
         DS    CL1                                                              
PPROG    DS    CL6                                                              
         DS    CL1                                                              
PNET     DS    CL4                                                              
         DS    CL1                                                              
PUNLEN   DS    CL2                                                              
PLN1     DS    CL1                                                              
PHHEST   DS    CL14                                                             
PLN2     DS    CL1                                                              
PHHACT   DS    CL14                                                             
PLN3     DS    CL1                                                              
PTRGEST  DS    CL14                                                             
PLN4     DS    CL1                                                              
PTRGACT  DS    CL14                                                             
PLN5     DS    CL1                                                              
PMEDIA   DS    CL3                                                              
PLN6     DS    CL1                                                              
PTYPE    DS    CL3                 MISSED/PREEMPT                               
         DS    CL1                                                              
PEND     DS    CL1                                                              
PLENE    EQU   *-PLINED                                                         
*                                                                               
*                                                                               
         EJECT                                                                  
         SPACE                                                                  
         PRINT OFF                                                              
       ++INCLUDE NETINCLS                                                       
       ++INCLUDE NETDEMOT                                                       
       ++INCLUDE DEDBLOCK                                                       
         EJECT                                                                  
       ++INCLUDE NEWRIFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRIDFD                                                       
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDTWADCOND                                                     
       ++INCLUDE DDBIGBOX                                                       
         EJECT                                                                  
*                                                                               
PRDHD    DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE NEGENUNIT                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004NEWRI68   05/01/02'                                      
         END                                                                    
