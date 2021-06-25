*          DATA SET NEMED68S   AT LEVEL 159 AS OF 05/01/02                      
*PHASE T31E68A,+0                                                               
*INCLUDE BINSRCH2                                                               
         TITLE 'T31E68 - LATEST ESTIMATED DEMOS UPDATE/REPORT'                  
************************************************************                    
*  PULLS LATEST ESTIMATED DEMOS OFF PROGRAM RECORD         *                    
*        AND UPDATES UNIT RECORDS                          *                    
*                                                          *                    
* GLOBALS: RA - A(TWA)                                     *                    
*          RB - BASE REG                                   *                    
*          RC - GEND                                       *                    
*          R9 - NETWORK SYSTEM DSECT                       *                    
*          R8 - A(DSECT FOR SPOOL PRINTING)                *                    
*          R7 - WORKING STORAGE                            *                    
*                                                          *                    
************************************************************                    
T31E68   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**NE68**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T31EFFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         LA    R1,HDHOOK                                                        
         ST    R1,HEADHOOK                                                      
         L     R7,ANETWS2          R7= ANETWS2/3  WORKING STORAGE               
         USING WORKD,R7                                                         
         L     R4,ANETWS4          R4= ANETWS4 NDDEMBLK AND DEDBLOCK            
         USING NDDEMBLK,R4                                                      
         ST    R4,NBADEM                                                        
         ST    R2,RELO                                                          
         L     R1,=A(STATLIST)                                                  
         A     R1,RELO                                                          
         ST    R1,NBCNVNTI                                                      
         L     R1,=A(PAKGLIST)                                                  
         A     R1,RELO                                                          
         ST    R1,PKGTADD                                                       
         XC    NBACTNET,NBACTNET   FUDGE FOR STATION LIST/PAKG VAL              
*                                  HAS SET NBACTNET                             
         SPACE                                                                  
* - NEED TO HAVE COPIES WRITTEN TO RECOVERY FILE                                
         ICM   R1,15,TWAMASTC                                                   
         BZ    ENDMST                                                           
         USING MCBLOCK,R1                                                       
         L     R1,MCSSB                                                         
         USING SSBD,R1                                                          
         OI    SSBSTAT2,SSBSROLC   RECOVER OFFLINE COPIES                       
ENDMST   DS    0H                                                               
         DROP  R1                                                               
*                                                                               
         SPACE                                                                  
         BAS   RE,GETPRF                                                        
         SPACE                                                                  
         MVI   UPDATFLG,C'N'                                                    
         CLI   SPLTST,C'Y'                     TEST RUN OPTION                  
         BE    *+8                                                              
         MVI   UPDATFLG,C'Y'                                                    
         CLI   SPLEAT,C'1'         SEED NTI OFF PROGRAM REC                     
         BE    SEED                                                             
         SPACE                                                                  
         MVI   RCSUBPRG,1                                                       
         CLI   SPLEAT,C'C'                                                      
         BE    PAS03                                                            
         CLI   SPLEAT,C'E'                                                      
         BE    PAS03                                                            
         MVI   RCSUBPRG,2                                                       
         SPACE                                                                  
PAS03    MVI   NBDATA,C'B'         READ UNIT RECS                               
         MVI   NBRESUME,NBPROCPK   SET TO START AT PACKAGES                     
         MVI   NBSELUOP,C'A'       READ ACTUAL SCHEDULE                         
         MVI   NBESTOPT,C'M'       ALWAYS DO EST FOR OLD DEMOS                  
         MVI   NBSEQ,C'Q'          READ IN PROGRAM ORDER                        
         MVI   NBREVOPT,0          DON'T REEVALUATE DEMOS                       
         MVI   NBFUNCT,NBFNORM     DO NORMAL FUNCTION                           
         LA    R1,DMWRKSV          PASS NETBLOCK DMWORK SAVE AREA               
         ST    R1,NBADMWRK                                                      
         LA    R6,NBBLKEND-NETBLOCK                                             
         MOVE  (NETBLKSV,(R6)),NETBLOCK   SAVE NETBLOCK                         
         SPACE                                                                  
PAS05    LA    R6,NBBLKEND-NETBLOCK                                             
         MOVE  (NETBLOCK,(R6)),NETBLKSV   RESET NETBLOCK                        
         SPACE                                                                  
PAS06    NETGO NSNETIO,DMCB,NETBLOCK            GET  RECORD                     
         SPACE                                                                  
         CLI   NBMODE,NBVALCLI     TEST IF CHANGE OF CLIENT                     
         BNE   PAS07                                                            
         L     R2,NBAIO            IF YES                                       
         USING CLTHDR,R2           GET CLIENT HEADER REC                        
         MVC   CLTNMSV,CNAME       SAVE CLIENT NAME                             
         MVI   FORCEHED,C'Y'                                                    
         MOVE  (CLISTSV,880),CLIST       SAVE CLIST                             
         MVI   MKGDREV,0                                                        
         BAS   RE,GETPRF           CHK N1 PROFILE ON MAKEGOODS                  
         SPACE                                                                  
PAS07    CLI   NBMODE,NBREQLST                                                  
         BE    PASX                                                             
         CLI   NBMODE,NBPROCPK                                                  
         BE    PACKTAB                                                          
         CLI   NBMODE,NBPROCUN                                                  
         BNE   PAS06                                                            
         B     PAS10                                                            
         EJECT                                                                  
* FILL IN P LINE  *                                                             
PAS10    DS    0H                                                               
         LA    R2,P                                                             
         USING PLINED,R2                                                        
         MVC   SRTCLT,NBCLICOD                                                  
         CLC   NBPRDSV,NBPRD       TEST IF PRD NUMBER CHANGED                   
         BE    *+8                                                              
         BAS   RE,GETPRD           IF YES/GET NEW PRDCODE                       
         MVC   NBPRDSV,NBPRD       SAVE PRD NUMBER                              
         MVC   SRTPRD,PRDCDSV                                                   
         EDIT  (B1,NBACTEST),(3,SRTEST)                                         
         MVC   SRTNET,NBACTNET                                                  
         EDIT  (B1,NBPACK),(3,SRTPKG)                                           
         MVC   SRTPRGCD,NBACTPRG                                                
         GOTO1 DATCON,DMCB,(2,NBACTDAT),(7,SRTDTE)                              
         MVI   SRTDTE+5,C'-'                                                    
         EDIT  (B1,NBACTSUB),(2,SRTDTE+6)                                       
*   GET THE EST DEMOS TO P LINE                                                 
         LA    R5,NDESTDEM                                                      
         LA    R6,SRTODEM                                                       
         EDIT  NBESTSHR,(4,0(R6)),1    SET SHARE FIRST                          
         LA    R6,9(R6)                                                         
         LA    R3,4                    BCT LIMIT FOR NUMBER OF DEMS             
         LA    R1,NDDEMOS                                                       
PAS13    CLI   0(R1),0      FIND HOMES DEMO/ SKIP (=00NN01)                     
         BNE   PAS15                                                            
         CLI   2(R1),1                                                          
         BNE   PAS15                                                            
         CLI   1(R1),C'R'                                                       
         BE    PAS14                                                            
         CLI   1(R1),C'I'                                                       
         BNE   PAS15                                                            
PAS14    LA    R5,8(R5)                                                         
         LA    R1,3(R1)                                                         
         B     PAS13                                                            
PAS15    EDIT  (B2,0(R5)),(4,0(R6))                                             
         LA    R5,8(R5)                                                         
         LA    R6,9(R6)                                                         
         LA    R1,3(R1)                                                         
         BCT   R3,PAS13                                                         
         EJECT                                                                  
         SPACE                                                                  
* REEVALUATE DEMOS/PUT TO PLINE                                                 
* AND UPDATE IF REQUESTED                                                       
PAS20    DS 0H                                                                  
         LA    R6,NBBLKEND-NETBLOCK                                             
         MOVE  (NETBLKSV,(R6)),NETBLOCK               SAVE NETBLK               
         MVI   NBNOWRIT,C'N'                                                    
         MVI   NBUPUNIT,C'N'                                                    
         MVC   NBREVOPT,SPLEAT     SPLEAT=SCRN FIELD(EST/ACT)                   
         CLI   NBREVOPT,C'0'                                                    
         BNE   *+8                                                              
         MVI   NBREVOPT,0                                                       
         MVI   NBSELUOP,C'A'       READ ACTUAL SCHEDULE                         
         MVI   NBESTOPT,C'Y'                                                    
         MVI   NBACTOPT,C'Y'                                                    
         MVI   NBFUNCT,NBFVAL                                                   
         CLI   NBREVOPT,0          RE-LOOK UP ACTUALS                           
         BNE   PAS21B                                                           
         L     R6,NBAIO                                                         
         MVI   ELCODE,X'71'        YES/DELETE NETVALUE ELEMENT                  
         GOTO1 REMELEM                                                          
         L     R6,NBAIO                                                         
         MVI   ELCODE,X'43'        YES/ACT VPH                                  
         GOTO1 REMELEM                                                          
         L     R6,NBAIO                                                         
         MVI   ELCODE,X'45'        YES/ACT HOME                                 
         GOTO1 REMELEM                                                          
         L     R6,NBAIO                                                         
         MVI   ELCODE,X'5E'        YES/ACT NTI                                  
         GOTO1 REMELEM                                                          
PAS21B   CLI   OVERRIDE,C'Y'       IGNORE OVERRIDES                             
         BNE   PAS21C                                                           
         L     R6,NBAIO                                                         
         MVI   ELCODE,X'DD'        YES/DELETE OVERRIDE ELEMENTS                 
         CLI   SPLEAT,C'E'                                                      
         BNE   *+12                                                             
         BAS   RE,DELDDOV                                                       
         B     PAS21C                                                           
         MVI   ELCODE,X'DE'                                                     
         GOTO1 REMELEM                                                          
         SPACE                                                                  
PAS21C   TM    NBUNITST,X'01'      IS IT A MAKEGOOD                             
         BZ    PAS22                                                            
         CLI   MKGDMDGD,C'Y'          IS MAKEGOOD OPTION ON                     
         BNE   PAS22                                                            
         MVI   NBESTOPT,C'P'          DO EST DEMO FOR MAKEGOODS                 
         CLI   MKGDREV,C'Y'           CHK N1 PROFILE                            
         BE    PAS22                                                            
         MVI   NBMGDOPT,C'Y'          USE MADE-GOOD-FOR PROGRAM REC             
*                                                                               
PAS22    LA    R1,NETIOHK          SET NETIO HOOK                               
         ST    R1,NBHOOK                                                        
         NETGO NSNETIO,DMCB,NETBLOCK                                            
         GOTO1 SPOOL,DMCB,(R8)     PRINT REC                                    
         MVI   NBFUNCT,NBFRDHI                                                  
         B     PAS05               GET NEXT REC                                 
         SPACE                                                                  
NETIOHK  NTR1                                                                   
         CLI   UPDATFLG,C'Y'                                                    
         BNE   NHK03                                                            
         CLI   SPLEAT,C'E'                                                      
         BNE   *+8                                                              
         BAS   RE,BLDPROV                                                       
         MVI   NBUPUNIT,C'Y'                                                    
         MVI   NBNOWRIT,C'Y'                                                    
*   GET THE E/A DEMOS TO P LINE                                                 
NHK03    CLI   SPLEAT,C'E'                                                      
         BE    NHK03A                                                           
         CLI   SPLEAT,C'C'                                                      
         BNE   NHK05                                                            
NHK03A   LA    R5,NDESTDEM                                                      
         B     *+8                                                              
NHK05    LA    R5,NDACTDEM                                                      
         LA    R6,SRTNDEM                                                       
         CLI   SPLEAT,C'E'       IF EST USE NBESTSHR                            
         BE    NHK05A                                                           
         CLI   SPLEAT,C'C'                                                      
         BNE   NHK07                                                            
NHK05A   EDIT  NBESTSHR,(4,0(R6)),1                                             
         B     NHK08                                                            
NHK07    EDIT  NBACTSHR,(4,0(R6)),1 ELSE USE NBACTSHR                           
         SPACE                                                                  
NHK08    LA    R6,9(R6)     BUMP OUTPUT FILED                                   
         LA    R2,4         BCT LIMIT FOR NUMBER OF DEMS                        
         LA    R1,NDDEMOS                                                       
NHK10    CLI   0(R1),0      FIND HOMES DEMO AND SKIP (=00NN01)                  
         BNE   NHK15                                                            
         CLI   2(R1),1                                                          
         BNE   NHK15                                                            
         CLI   1(R1),C'I'                                                       
         BE    NHK14                                                            
         CLI   1(R1),C'R'                                                       
         BNE   NHK15                                                            
NHK14    LA    R5,8(R5)     BUMP DEMO VALUES                                    
         LA    R1,3(R1)     BUMP DEMO NAMES                                     
         B     NHK10                                                            
NHK15    EDIT  (B2,0(R5)),(4,0(R6))                                             
         LA    R5,8(R5)                                                         
         LA    R6,9(R6)                                                         
         LA    R1,3(R1)                                                         
         BCT   R2,NHK10                                                         
NHKX     B     PASX                                                             
         SPACE 3                                                                
         DS    0H                                                               
PASX     XIT1                                                                   
         EJECT                                                                  
************************************                                            
* TAKES THE PACKAGE RECORDS AND LOADS THEM                                      
* IN A TABLE.                                                                   
*************************************                                           
         SPACE                                                                  
PACKTAB  L     R5,NBAIO                                                         
         L     R6,PKGTADD                                                       
         USING NPRECD,R5                                                        
         USING PKGTABD,R6                                                       
*                                                                               
         MVC   PKGTNET,NPKNET                                                   
         MVC   PKGTEST,NPKEST                                                   
         MVC   PKGTPKG,NPKPACK                                                  
         MVI   PKGTDTP,C'I'                                                     
         TM    NPAKCNTL,X'40'                                                   
         BO    PACKT50                                                          
         MVI   PKGTDTP,C'V'                                                     
*                                                                               
PACKT50  L     R1,PKGTCNT                                                       
         LA    R1,1(R1)                                                         
***      C     R1,=F'1000'          1000PKG=7000CL                              
         C     R1,=F'2000'                                                      
         BNH   *+6                                                              
         DC    H'0'                                                             
         ST    R1,PKGTCNT                                                       
*                                                                               
         LA    R6,7(R6)                                                         
         ST    R6,PKGTADD                                                       
         B     PAS06                                                            
         DROP  R5,R6                                                            
         EJECT                                                                  
************************************                                            
* READS THE DD ELEMENTS FROM THE UNIT RECORD                                    
* DELETES ONLY THE NON-PROGRAM CREATED OVERRIDES.                               
*************************************                                           
         SPACE                                                                  
DELDDOV  NTR1                                                                   
         USING NUOVD,R5                                                         
*                                                                               
         GOTO1 HELLO,DMCB,(C'G',UNTFILE),(X'DD',(R6)),0                         
         CLI   12(R1),0                                                         
         BNE   DELDDEX                                                          
         L     R5,12(R1)                                                        
*                                                                               
DELDD050 TM    NUOVFLG,X'80'       CHECK IF PROGRAM OVERRIDE                    
         BO    DELDD100                                                         
         MVC   WORK(10),2(R5)                                                   
         GOTO1 HELLO,DMCB,(C'D',UNTFILE),(X'DD',(R6)),(10,WORK)                 
*                                                                               
DELDD100 ZIC   RE,1(R5)                                                         
         AR    R5,RE                                                            
         CLI   0(R5),X'DD'                                                      
         BE    DELDD050                                                         
*                                                                               
DELDDEX  B     PASX                                                             
         DROP  R5                                                               
         EJECT                                                                  
************************************                                            
* READS THE PROGRAM RECORDS MOVES THE                                           
* PROGRAM OVERIDE DEMOS (IMPS, NADS)                                            
* TO THE UNIT RECORD.                                                           
*************************************                                           
         SPACE                                                                  
BLDPROV  NTR1                                                                   
         L     R5,NBAIO            POINT TO UNIT RECORD                         
         USING NURECD,R5                                                        
*                                                                               
         BAS   RE,DELPROV          DELETE CURRENT PROGRAM OVERRIDES             
*GET DEMO TYPE FRTOME PACKAGE TABLE                                             
         XC    DUB,DUB                                                          
         MVC   DUB(4),NUKNET                                                    
         MVC   DUB+4(1),NUKEST                                                  
         MVC   DUB+5(1),NUPACK                                                  
         CLC   DUB(6),PKGTENT                                                   
         BE    BDPROV30                                                         
         L     RE,=A(PAKGLIST)                                                  
         LA    RF,500                                                           
BDPROV10 CLI   0(RE),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(6,RE),DUB                                                      
         BE    BDPROV20                                                         
         LA    RE,7(RE)                                                         
         BCT   RF,BDPROV10                                                      
*                                                                               
BDPROV20 MVC   PKGTENT,0(RE)                                                    
*                                                                               
BDPROV30 XC    KEY,KEY             NOW READ PROGRAM RECORD                      
         LA    R2,KEY                                                           
         USING NPGKEY,R2                                                        
         MVC   NPGKTYP,=X'0D20'                                                 
         MVC   NPGKAM,NBACTAM                                                   
         MVC   NPGKNET,NBMARKET                                                 
         MVC   NPGKPROG,NBACTPRG                                                
         MVC   NPGKEND,NBACTDAT                                                 
* CHECK IF MAKE-GOOD INF SHOULD BE USED                                         
         CLI   NBMGDOPT,C'Y'                                                    
         BNE   BDPROV50                                                         
         CLI   NBMGFDAT,0                                                       
         BE    BDPROV50                                                         
         MVC   NPGKPROG,NBMGFPCD                                                
         MVC   NPGKEND,NBMGFDAT                                                 
*                                                                               
BDPROV50 MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=CL8'DMRDHI',=CL8'SPTDIR',KEY,KEY,0                 
         CLC   KEY(11),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                PROGRAM MUST EXIST                           
         CLC   NPGKEND,KEYSAVE+11                                               
         BNL   *+6                                                              
         DC    H'0'                PROGRAM MUST EXIST                           
         L     R6,=A(PROGRCSV)                                                  
         A     R6,RELO                                                          
         CLC   KEY(13),0(R6)       DO WE ALREADY HAVE THE RECORD                
         BE    BDPROV60                                                         
         LA    R3,KEY+14                                                        
         GOTO1 DATAMGR,DMCB,=CL8'GETREC',=CL8'SPTFIL',(R3),(R6),DMWRKPR         
*                                                                               
BDPROV60 MVI   ELCODE,X'DD'                                                     
         MVC   NBDTADSP,=H'24'                                                  
*                                                                               
         USING NPGELDD,R6                                                       
         BAS   RE,GETEL                                                         
         BNE   BDPROVEX                                                         
*                                                                               
BDPROV70 OC    NPGDAMT,NPGDAMT                                                  
         BZ    BDPROV90                                                         
         CLI   PKGTENT+6,C'I'      CHECK FOR IMP BASE                           
         BE    BDPROV75                                                         
* VPH FILTER                                                                    
         CLI   NPGDMOD,C'R'                                                     
         BE    BDPROV80                                                         
         CLI   NPGDMOD,C'V'                                                     
         BE    BDPROV80                                                         
         B     BDPROV90                                                         
* IMP FILTER                                                                    
BDPROV75 CLI   NPGDMOD,C'T'                                                     
         BE    BDPROV80                                                         
         CLI   NPGDMOD,C'H'                                                     
         BE    BDPROV80                                                         
         B     BDPROV90                                                         
BDPROV80 GOTO1 HELLO,DMCB,(C'P',UNTFILE),(X'DD',(R5)),(R6)                      
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
BDPROV90 ZIC   RE,1(R6)            GET NEXT 'DD' ELEMENT                        
         AR    R6,RE                                                            
         CLI   0(R6),X'DD'                                                      
         BE    BDPROV70                                                         
*                                                                               
BDPROVEX MVC   NBDTADSP,=H'27'     RESET DATADISP                               
         B     PASX                                                             
         DROP  R2,R5,R6                                                         
         EJECT                                                                  
************************************                                            
* READS THE DD ELEMENTS FROM THE UNIT RECORD                                    
* DELETES ONLY THE PROGRAM CREATED OVERRIDES.                                   
*************************************                                           
         SPACE                                                                  
DELPROV  NTR1                                                                   
         USING NUOVD,R6                                                         
*                                                                               
         GOTO1 HELLO,DMCB,(C'G',UNTFILE),(X'DD',(R5)),0                         
         CLI   12(R1),0                                                         
         BNE   DELPREX                                                          
         L     R6,12(R1)                                                        
*                                                                               
DELPR050 CLI   NUOVMOD,C'U'        DONT DELETE UNIVERSES                        
         BE    DELPR100                                                         
         TM    NUOVFLG,X'80'       CHECK IF PROGRAM OVERRIDE                    
         BZ    DELPR100                                                         
         MVC   WORK(10),2(R6)                                                   
         GOTO1 HELLO,DMCB,(C'D',UNTFILE),(X'DD',(R5)),(10,WORK)                 
         B     DELPR050                                                         
*                                                                               
DELPR100 ZIC   RE,1(R6)                                                         
         AR    R6,RE                                                            
         CLI   0(R6),X'DD'                                                      
         BE    DELPR050                                                         
*                                                                               
DELPREX  B     PASX                                                             
         DROP  R6                                                               
         EJECT                                                                  
************************************                                            
* READS N1 PROFILE TO SEE IF                                                    
* MAKEGOODS HAVE THEIR OWN DEMOS                                                
* IF YES, NEED TO REEVALUATE DEMOS                                              
* FROM PROGRAM RECORD                                                           
* IF NO, DO NOT RELOOKUP DEMOS                                                  
*    OUTPUT: SETS MKGDREV=Y IF RELOOKUP                                         
*************************************                                           
         SPACE                                                                  
GETPRF   NTR1                                                                   
         XC    KEY,KEY             GET PROFILE CHK MKGD OPTION                  
         MVC   KEY(4),=C'SON1'                                                  
         MVC   KEY+4(2),NBEFFAGY                                                
         MVI   KEY+6,C'N'                                                       
         GOTO1 NBCLUNPK,DMCB,NBACTCLI,KEY+7                                     
         DS    0H                                                               
         L     R3,DATAMGR                                                       
         DS    0H                                                               
         GOTO1 GETPROF,DMCB,KEY,WORK,(R3)                                       
         CLI   WORK+1,C'Y'                                                      
         BNE   *+8                                                              
         MVI   MKGDREV,C'Y'        SET MAKEGOOD OPTION                          
         B     PASX                                                             
         EJECT                                                                  
**********************************                                              
*  TO GET PRD CODE FROM C LIST   *                                              
*                                *                                              
**********************************                                              
GETPRD   NTR1                                                                   
         LA    R2,CLISTSV                                                       
         CLI   NBPRD,0                                                          
         BNE   GP10                                                             
         MVC   PRDCDSV,=C'***'     PRODUCT IS UNALLOCATED                       
         B     GPX                                                              
         SPACE                                                                  
GP10     CLI   3(R2),0             IF E-O-F CLIST                               
         BNE   GP12                SET TO UNDEFINED                             
         XC    PRDCDSV,PRDCDSV                                                  
         MVC   PRDCDSV,=C' * '                                                  
         B     GPX                                                              
GP12     CLC   3(1,R2),NBPRD                                                    
         BE    GP14                                                             
         LA    R2,4(R2)            INCREMENT CLIST                              
         B     GP10                RETURN TO LOOP                               
GP14     MVC   PRDCDSV,0(R2)      SET 3 CHAR PRINTABLE PRD CODE                 
         SPACE                                                                  
GPX      B     PASX                                                             
         EJECT                                                                  
*                                                                               
* READS PROGRAM RECORD FOR NTI CODE                                             
*                                                                               
GETNTI   NTR1                                                                   
         MVC   MYKEY,KEY           SAVE CURRENT KEY                             
         B     SKIPSTR                                                          
*                                                                               
         L     R2,=A(PROGRCSV)                                                  
         A     R2,RELO                                                          
         USING STAREC,R2           READ STATION REC TO GET MKT NUMBER           
         MVI   STAKTYPE,C'S'                                                    
         MVI   STAKMED,C'N'                                                     
         MVC   STAKCALL(4),NBACTNET                                             
         MVI   STAKCALL+4,C'N'                                                  
         MVC   STAKAGY,NBSELAGY                                                 
         MVC   STAKCLT,=CL3'000'                                                
         MVC   STAKFILL,=CL5'00000'                                             
         GOTO1 DATAMGR,DMCB,=CL8'DMREAD',=C'STATION ',(R2),(R2),0               
         PACK  DUB,SMKT                                                         
         CVB   R1,DUB                                                           
         STH   R1,NETMKT                                                        
*                                                                               
SKIPSTR  XC    KEY,KEY             NOW READ PROGRAM RECORD                      
         LA    R2,KEY                                                           
         USING NPGKEY,R2                                                        
         MVC   NPGKTYP,=X'0D20'                                                 
         MVC   NPGKAM,NBACTAM                                                   
*        MVC   NPGKNET,NETMKT                                                   
         MVC   NPGKNET,NBMARKET                                                 
         MVC   NPGKPROG,NBACTPRG                                                
         MVC   NPGKEND,NBACTDAT                                                 
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=CL8'DMRDHI',=CL8'SPTDIR',KEY,KEY,0                 
BNT3     CLC   KEY(11),KEYSAVE                                                  
         BNE   GNTX                                                             
         CLC   NPGKEND,NBACTDAT                                                 
         BL    GNTX                                                             
         L     R6,=A(PROGRCSV)                                                  
         A     R6,RELO                                                          
         LA    R3,KEY+14                                                        
         GOTO1 DATAMGR,DMCB,=CL8'GETREC',=CL8'SPTFIL',(R3),(R6),DMWRKSV         
         MVI   ELCODE,X'92'                                                     
         MVC   NBDTADSP,=H'24'                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NPGELEM,R6                                                       
         MVC   PRNTI,NPGPPNO                                                    
GNTX     MVC   NBDTADSP,=H'27'     RESET DATADISP                               
         MVC   KEY,MYKEY           RESTORE KEY                                  
         MVI   NBFUNCT,NBFRDHI                                                  
         B     PASX                                                             
         EJECT                                                                  
         SPACE                                                                  
* SUB-ROUTINE TO DELETE ELMENT (R6 POINTS TO RECORD)                            
*                                                                               
DELELM   DS    0H                                                               
         LR    R0,RE                                                            
         GOTO1 HELLO,DMCB,(C'D',UNTFILE),(ELCODE,(R6)),0                        
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TP PUT ELEMENT (R4 POINTS TO ELEMENT)                             
*                                                                               
PUTELM   DS    0H                                                               
         LR    R0,RE                                                            
         GOTO1 HELLO,DMCB,(C'P',UNTFILE),(ELCODE,(R6)),(R4)                     
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 2                                                                
*                                                                               
         GETEL (R6),NBDTADSP,ELCODE                                             
         SPACE 2                                                                
*                                                                               
UNTFILE  DC    CL8'UNTFIL '                                                     
         EJECT                                                                  
* READ NTI CODE OFF PROGRAM REC AND SEED INTO UNITS                             
*                                                                               
SEED     DS    0H                                                               
         MVI   RCSUBPRG,3                                                       
         MVI   NBDATA,C'U'         READ UNIT RECS                               
         MVI   NBSELUOP,C'A'       READ ACTUAL SCHEDULE                         
         MVI   NBREVOPT,0          DON'T REEVALUATE DEMOS                       
         MVI   NBSEQ,C'Q'          PROGRAM ORDER                                
         LA    R1,SEEDHK                                                        
         ST    R1,NBHOOK                                                        
SD10     NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBMODE,NBREQLST                                                  
         BE    SEEDX                                                            
         B     SD10                                                             
*                                                                               
SEEDHK   NTR1                                                                   
         CLI   NBMODE,NBPROCUN                                                  
         BNE   SDHKX                                                            
         CLI   UPDATFLG,C'Y'                                                    
         BNE   SDHK10                                                           
         MVI   NBUPUNIT,C'Y'                                                    
         MVI   NBNOWRIT,C'Y'                                                    
SDHK10   LA    R2,P                                                             
         USING PLIN,R2                                                          
         BAS   RE,GETNTI                                                        
         OC    PRNTI,PRNTI         IF NO NTI                                    
         BZ    SDHK30              SKIP UNIT                                    
         L     R6,NBAIO                                                         
         CLI   OVERRIDE,C'Y'        REPLACE OVERRIDES                           
         BNE   SDHK12                                                           
         MVI   ELCODE,X'DD'        YES                                          
         GOTO1 REMELEM                                                          
SDHK12   MVC   PLCLT,NBCLICOD                                                   
         EDIT  (B1,NBACTEST),(3,PLEST)                                          
         MVC   PLNET,NBACTNET                                                   
         MVC   PLPROG,NBACTPRG                                                  
         EDIT  (B1,NBPACK),(3,PLPKG)                                            
         GOTO1 DATCON,DMCB,(2,NBACTDAT),(5,PLDATE)                              
         MVI   PLDATE+5,C'-'                                                    
         EDIT  (B1,NBACTSUB),(2,PLDATE+6),ALIGN=LEFT                            
         EDIT  (B2,NBNTI),(6,PLNTI)                                             
         EDIT  (B2,PRNTI),(6,PLNEWNTI)                                          
         L     R6,NBAIO                                                         
         USING NUMAINEL,R6                                                      
         MVI   ELCODE,1                                                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   NUNTI,PRNTI                                                      
         XC    PRNTI,PRNTI                                                      
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     SDHKX                                                            
*                                                                               
SDHK30   DS    0H                                                               
         MVI   NBUPUNIT,C'N'                                                    
         MVI   NBNOWRIT,C'N'                                                    
SDHKX    B     SEEDX                                                            
*                                                                               
SEEDX    B     PASX                                                             
         EJECT                                                                  
         SPACE                                                                  
HDHOOK   NTR1                                                                   
         CLI   UPDATFLG,C'Y'                                                    
         BE    *+10                                                             
         MVC   H4+58(14),=C'** TEST RUN **'                                     
         CLI   RCSUBPRG,3                                                       
         BE    HDHK10                                                           
         MVC   H3+10(3),NBCLICOD                                                
         MVC   H3+16(20),CLTNMSV                                                
         SPACE                                                                  
* GET DEMO NAMES TO HEADER *                                                    
         MVC   DBCOMFCS,ACOMFACS   *SET FOR DEMOCON                             
         MVC   DBFILE,=C'NTI'      *                                            
         MVI   DBSELMED,C'N'       *                                            
         MVI   DBSELSRC,C'N'       *                                            
         SPACE                                                                  
         CLI   NDNDEMOS,0          CHK NUMBER OF DEMOS                          
         BE    HDHK10                                                           
         LA    R3,H7                                                            
         USING PLINED,R3                                                        
         LA    R3,SRTODEM                                                       
         MVC   0(5,R3),=C'SHARE'   SHARE IS ALWAYS FIRST                        
         MVC   46(5,R3),=C'SHARE'                                               
         MVC   133(5,R3),SPACES                                                 
         MVC   178(5,R3),SPACES                                                 
         LA    R3,9(R3)                                                         
         SPACE                                                                  
         LA    R5,4                BCT LIMIT FOR DEMO CATEGORIES                
         SR    R2,R2               COUNTER FOR DEMO NUMBER(POSITION)            
         SPACE                                                                  
*        LA    R1,5                *SET FOR VPHS IN NDDEMOS                     
*        LA    R6,NDDEMOS          *                                            
*HDHK05   MVI   1(R6),C'V'          *                                           
*         LA    R6,3(R6)            *                                           
*         BCT   R1,HDHK05           *                                           
         SPACE                                                                  
GETDEMO  NETGO NVDEMCON,DMCB,((R2),NDDEMBLK),DBLOCK,(7,WORK)                    
         MVC   0(7,R3),WORK        FOR OLD DEMOS                                
         MVC   45(7,R3),WORK       FOR NEW DEMOS                                
         CLC   WORK+8(3),=C'RTG'   FORCE TO VPH                                 
         BNE   *+10                                                             
         MVC   WORK+8(3),=C'VPH'                                                
         MVC   133(5,R3),WORK+7                                                 
         MVC   178(5,R3),WORK+7                                                 
         CLC   0(4,R3),=C'HOME'                                                 
         BNE   GTD5                IF HOME,SKIP                                 
         LA    R2,1(R2)                                                         
         B     GETDEMO                                                          
GTD5     LA    R2,1(R2)                                                         
         LA    R3,9(R3)                                                         
         BCT   R5,GETDEMO                                                       
         SPACE                                                                  
*  SET UP BOXES PARAMETERS *                                                    
HDHK10   CLI   BOXSET,C'Y'                                                      
         BE    HDX                                                              
         CLI   RCSUBPRG,3                                                       
         BNE   HDX                                                              
         MVI   BOXSET,C'Y'                                                      
         L     R1,ABOX                                                          
         USING BOXD,R1                                                          
         MVC   BOXCOLS,SPACES                                                   
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0           (ONLY FOR SPOOF)                              
         SPACE                                                                  
         LA    R5,BOXCOLS                                                       
         USING PLIN,R5                                                          
         MVI   39(R5),C'L'                                                      
         MVI   PLDATE+8,C'C'                                                    
         LA    R5,PLNEQU(R5)                                                    
         MVI   0(R5),C'R'                                                       
         SPACE                                                                  
         LA    R5,BOXROWS                                                       
         LA    R5,6(R5)                                                         
         MVI   0(R5),C'T'                                                       
         LA    R5,2(R5)                                                         
         MVI   0(R5),C'M'                                                       
         LA    R5,49(R5)                                                        
         MVI   0(R5),C'B'                                                       
         SPACE                                                                  
HDX      B     PASX                                                             
         EJECT                                                                  
         LTORG                                                                  
         SPACE                                                                  
*  SAVE AREA FOR NETBLOCK   *                                                   
NETBLKSV DS    CL2000                                                           
PROGRCSV DS    CL2000                                                           
STATLIST DS    CL2000                                                           
PAKGLIST DS    CL7000          PGK LIST                                         
         DS    CL7000          PKG LIST                                         
         EJECT                                                                  
*                                                                               
*** MY WORKING STORAGE USING ANETWS2 AND ANETWS3 ***                            
*                                                                               
WORKD    DSECT                                                                  
CLISTSV  DS    CL880        *** FIELDS PASSED FROM EDIT MODULE                  
CLTNMSV  DS    CL20         ***                                                 
RELO     DS    F            ***                                                 
MKGDMDGD DS    CL1          ***    Y=MAKEGOODS GET LATEST EST DEMO              
OVERRIDE DS    CL1          ***    Y=REPLACE OVERRIDES                          
*                                                                               
NETMKT   DS    CL2                                                              
NBPRDSV  DS    CL1                                                              
BOXSET   DS    CL1                                                              
UPDATFLG DS    CL1                                                              
FRST     DS    CL1                                                              
MKGDREV  DS    CL1                                                              
PRDCDSV  DS    CL3                                                              
DMWRKSV  DS    CL96                                                             
DMWRKPR  DS    CL96                                                             
MYKEY    DS    CL40                                                             
PRNTI    DS    CL2                                                              
*                                                                               
PKGTADD  DS    F                   ADDRESS OF PACKAGE TABLE                     
PKGTCNT  DS    F                   NUMBER OF PACKAGES IN TABLE                  
PKGTENT  DS    CL7                 LAST ENTRY OF PACKAGE TABLE                  
*                                                                               
         SPACE                                                                  
*                                                                               
PLINED   DSECT                                                                  
SRTCLT   DS    CL3                 CLIENT CODE (PRINTABLE)                      
         DS    CL1                                                              
SRTPRD   DS    CL3                 PRODUCT     (PRINTABEL)                      
         DS    CL1                                                              
SRTEST   DS    CL4                 ESTIMATE                                     
         DS    CL1                                                              
SRTNET   DS    CL4                 NETWORK                                      
         DS    CL1                                                              
SRTPKG   DS    CL3                 PACKAGE                                      
         DS    CL1                                                              
SRTPRGCD DS    CL6                 PROGRAM CODE                                 
         DS    CL1                                                              
SRTDTE   DS    CL8                 DATE-SUBLINE                                 
         DS    CL1                                                              
SRTODEM  DS    CL45                OLD DEMS                                     
         DS    CL2                                                              
SRTNDEM  DS    CL45                NEW DEMS                                     
         DS    CL2                                                              
*                                                                               
PLIN     DSECT                                                                  
         DS    CL40                                                             
PLCLT    DS    CL3                 CLIENT CODE (PRINTABLE)                      
         DS    CL2                                                              
PLEST    DS    CL4                 ESTIMATE                                     
         DS    CL2                                                              
PLNET    DS    CL4                 NETWORK                                      
         DS    CL2                                                              
PLPKG    DS    CL3                 PACKAGE                                      
         DS    CL2                                                              
PLPROG   DS    CL6                 PROGRAM CODE                                 
         DS    CL2                                                              
PLDATE   DS    CL6                 DATE-SUBLINE                                 
         DS    CL3                                                              
PLNTI    DS    CL6                 OLD NTI                                      
         DS    CL3                                                              
PLNEWNTI DS    CL6                 NEW NTI                                      
PLNEQU   EQU   *-PLIN                                                           
*                                                                               
PKGTABD  DSECT                                                                  
PKGTNET  DS    CL4                 NETWORK                                      
PKGTEST  DS    CL1                 ESTIMATE                                     
PKGTPKG  DS    CL1                 PACKAGE                                      
PKGTDTP  DS    CL1                 DEMO TYPE I/V                                
*                                                                               
         EJECT                                                                  
       ++INCLUDE NETINCLS                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE NEMEDFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDE8D                                                       
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDMASTC                                                        
       ++INCLUDE FASSB                                                          
         EJECT                                                                  
         PRINT ON                                                               
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE NEGENPACK                                                      
NDBLK    DSECT                                                                  
       ++INCLUDE NETDEMOD                                                       
       ++INCLUDE DEDBLOCK                                                       
         EJECT                                                                  
       ++INCLUDE DDBIGBOX                                                       
         EJECT                                                                  
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPGENPROG                                                      
       ++INCLUDE SPGENSTA                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'159NEMED68S  05/01/02'                                      
         END                                                                    
