*          DATA SET NEWRI83    AT LEVEL 025 AS OF 05/01/02                      
*PHASE T32083A,+0                                                               
*INCLUDE CLPACK                                                                 
*INCLUDE CLUNPK                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*                                                                               
         TITLE 'T32083 - NETWORK CLIENT DAILY ACTIVITY REPORT'                  
         SPACE 2                                                                
T32083   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T32083*,RA                                                    
         USING T32083,RB,RA                                                     
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R6,ATWA                                                          
         USING T320FFD,R6                                                       
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS3                                                       
         USING MYWORKD,R7                                                       
         LA    R2,NDDEMBLK                                                      
         ST    R2,NBADEM                                                        
                                                                                
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R5,SORTER                                                        
         ST    R5,ASORTER          SAVE SORTER ADDRESS                          
         LA    R5,HEDSPECS                                                      
         ST    R5,SPECS                                                         
         DROP  R8                                                               
*                                                                               
         L     R8,=A(MYIO)                                                      
         USING RECD,R8                                                          
         LA    RE,28(R8)           POINT PAST RECV HEADER                       
         ST    RE,AUNITREC         UNIT RECORD POINTER                          
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BE    MAINLINE                                                         
         CLI   MODE,VALREC                                                      
         BE    EDLINE                                                           
EXIT     XIT1                                                                   
         EJECT                                                                  
EDLINE   DS    0H                                                               
         MVI   NBQINIT,0           DO ALL VALIDATIONS EACH TIME                 
*                                                                               
         LA    R2,SPLCLIH                CLIENT                                 
         NETGO NVCLI,DMCB                                                       
         MVC   ONECLT,SPLCLI                                                    
         MVC   AMSAVE,NBACTAM                                                   
*                                                                               
*                                                                               
         LA    R2,SPLPROH                PRODUCT                                
         NETGO NVPRDALL,DMCB                                                    
         MVI   POLRUN,0                                                         
         CLC   =C'POL',SPLPRO                                                   
         BNE   *+10                                                             
         MVC   POLRUN,=C'POL'                                                   
*                                                                               
         LA    R2,SPLESTH                ESTIMATE                               
         NETGO NVESTRNG,DMCB,0,NDDEMBLK                                         
         B     EXIT                                                             
*                                                                               
*                                                                               
MAINLINE DS    0H                                                               
         SPACE                                                                  
         BAS   RE,INIT                                                          
         BAS   RE,NET1         READ RECOVERY / PUT RECS TO TSAROFF              
         CLOSE (RECVIN)                             CLOSE RECOVERY              
         FREEPOOL RECVIN                                                        
         BAS   RE,NET200       GET RECS FROM TSAROFF / PUT TO SORT              
         BAS   RE,NET300       GET RECS FROM SORT / WRITE REPORT                
*                                                                               
         GOTO1 ASORTER,DMCB,=C'END'              CLOSE SORTER                   
*                                                                               
         L     R1,TSARBUFF                       RETURN MEMORY                  
         L     R2,TSARBUFL                       RETURN MEMORY                  
         FREEMAIN RC,LV=(2),A=(1)                                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         B     EXIT                                                             
*                                                                               
COPY     EQU   1                                                                
CHANGE   EQU   2                                                                
ADDS     EQU   3                                                                
*                                                                               
         EJECT                                                                  
                                                                                
INIT     NTR1                                                                   
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(3,TODAYB)                                     
*****    MVC   TODAYB,=X'60040A'                                                
*                                                                               
         MVI   MYSPACES,X'40'                                                   
         MVC   MYSPACES+1(L'MYSPACES-1),MYSPACES                                
         MVC   MYP,MYSPACES                                                     
         MVC   MYP2,MYSPACES                                                    
*                                                                               
* - SET UP SORTER                                                               
         XC    DMCB(12),DMCB                                                    
         GOTO1 ASORTER,DMCB,SORTCARD,RECCARD                                    
         B     INIT12                                                           
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,26,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=300'                                   
                                                                                
* - INITIALIZE TSAROFF                                                          
INIT12   DS    0H                                                               
         L     RF,ATWA                                                          
         L     RF,TWAMASTC-TWATASK(RF)       POINT PASTC                        
         L     RF,MCVLOADR-MCBLOCK(RF)      GET LOADER ADDRESS                  
         GOTO1 (RF),DMCB,=CL8'T00A7D',0    TSAROFF                              
         MVC   ATSAROFF,DMCB+4                                                  
         OC    ATSAROFF,ATSAROFF                                                
         BNO   *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R0,TSARBUFL                                                      
         GETMAIN RU,LV=(0),LOC=(ANY,ANY)                                        
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         ST    R1,TSARBUFF                                                      
*                                                                               
         XC    TSAREA,TSAREA                                                    
         LA    R2,TSAREA                                                        
         USING TSARD,R2                                                         
*                                                                               
         MVI   TSOFFACT,TSAINI                                                  
         MVC   TSABUF,TSARBUFF                                                  
         MVC   TSAREC,TSARBUFL                                                  
         LA    R0,MYTSKEYL                                                      
         STC   R0,TSKEYL                                                        
         LA    R0,MYTSRECL                                                      
         STH   R0,TSRECL                                                        
         GOTO1 ATSAROFF,(R2)                                                    
*                                                                               
***      MVC   WORK(4),TSARBUFF                                                 
***      GOTO1 =V(PRNTBL),DMCB,=C'GTM',WORK,C'DUMP',4,=C'1D'                    
*                                                                               
         CLI   TSERRS,0                                                         
         BE    INIT6                                                            
         DC    H'0'                                                             
*                                                                               
INIT6    L     RE,=A(ESTABLE)      CLEAR ESTABLE                                
         SR    RF,RF                                                            
         LA    RF,ESTBLN(RF)                                                    
         XCEF                                                                   
*                                                                               
INITX    OPEN  (RECVIN,(INPUT))                                                 
         B     EXIT                                                             
         EJECT                                                                  
********************************************************                        
* THIS ROUTINE GETS THE RECORDS FROM THE RECOVERY FILE                          
* IF SIGNIFICANT CHANGE TO REC SEND COPY/CHA TO TSAROFF                         
*                                                                               
*  INPUT:  RECS FROM RECOVERY FILE                                              
* OUTPUT:  PUTS RECS TO SORTER                                                  
*                                                                               
********************************************************                        
         SPACE 2                                                                
NET1     NTR1                                                                   
*                                                                               
GET      LA    R1,RECVIN                                                        
         L     R0,=A(MYIO)                                                      
         GET   (1),(0)                                                          
         SPACE                                                                  
         CLI   RFILTY,X'2A'       TEST UNTFILE                                  
         BNE   GET                                                              
         CLC   RDATE,TODAYB        ONLY TODAY'S REC                             
         BNE   GET                                                              
         CLI   RTASKID,X'FF'       BACKED OUT AFTER A DUMP                      
         BE    GET                 YES/SKIP IT                                  
         CLI   RPRG,X'14'          TEST PFM                                     
         BE    GET                                                              
         L     R5,AUNITREC         POINT TO UNIT RECORD - R5                    
         USING NURECD,R5                                                        
         CLI   NUKTYPE,X'04'       TEST UNIT RECORD                             
         BNE   GET                                                              
         CLI   NUKSUB,X'C1'        SKIP TRAFFIC RECS                            
         BNL   GET                                                              
* TEST                                                                          
*        CLC   =C'CNN',NUKNET                                                   
*        BNE   GET                                                              
*        CLC   =C'4NAN',NUKPROG                                                 
*        BNE   GET                                                              
*        CLC   NUKDATE,=X'BE56'                                                 
*        BNE   GET                                                              
*        TM    NUUNITST,X'02'     ONLY MISSED                                   
*        BNO   GET                                                              
*        CLI   NUKEST,78                                                        
*        BNE   GET                                                              
*                                                                               
         SPACE                                                                  
* SET ZERO ELEMENT CODE AT E-O-R                                                
         LH    R1,REC-4                                                         
         LA    R1,REC-4(R1)                                                     
         XC    0(2,R1),0(R1)                                                    
                                                                                
* - TEST IF AGENCY                                                              
         CLC   NUKAM,AMSAVE        IS IT REQUESTED AGENCY                       
         BNE   GET                                                              
         CLI   PKCLISV,0           DO I HAVE PACKED CLIENT CODE                 
         BNE   NET10                                                            
         OC    ONECLT,MYSPACES                                                  
         GOTO1 =V(CLPACK),DMCB,ONECLT,PKCLISV                                   
         CLI   DMCB,0                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
NET10    CLC   PKCLISV,NUKCLT      CHECK CLIENT CODE                            
         BNE   GET                                                              
*                                                                               
NET12    DS    0H                                                               
         TM    NUPACKST,X'01'      TEST NO-SHOW DELETE                          
         BO    NET12C              IF YES/TREAT AS DELETED                      
         MVI   DELETFLG,0                                                       
         TM    NURSTAT,X'80'       TEST IF DELETED                              
         BNO   NET12D                                                           
NET12C   MVI   DELETFLG,C'D'                                                    
         B     NET13                                                            
         DROP  R5                                                               
NET12D   L     R6,AUNITREC                                                      
         MVI   ELCODE,X'01'                                                     
         USING NUMAINEL,R6                                                      
         MVC   DATADISP,=H'27'                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    NUUNITST,X'40'      IS IT PREEMPTED                              
         BNO   *+12                                                             
         MVI   DELETFLG,C'P'                                                    
         B     NET13                                                            
         TM    NUUNITST,X'02'     MISSED                                        
         BNO   *+12                                                             
         MVI   DELETFLG,C'M'                                                    
         B     NET13                                                            
         EJECT                                                                  
NET13    L     R6,AUNITREC         TEST IF COPY                                 
         USING NURECD,R6                                                        
         CLI   RRECTY,COPY         TEST IF COPY                                 
         BE    NET15                                                            
         CLI   RRECTY,CHANGE       TEST IF CHA                                  
         BE    NET20                                                            
         CLI   RRECTY,ADDS         TEST IF ADD                                  
         BE    NET17                                                            
         DC    H'0'                                                             
                                                                                
* - COPY                                                                        
NET15    DS    0H                                                               
         CLI   PREVREC,COPY       IF PREVIOUS REC WAS A COPY                    
         BNE   *+8                 THAT REC IS AN ERROR                         
         BAS   RE,COPYERR          SO PRINT OUT PREVIOUS COPY AS ERROR          
         MVC   PREVIOUS,RFILTY     SAVE CURRENT COPY                            
         MVI   PREVREC,COPY        SET FLAGE                                    
*                                                                               
NET17    L     R4,=A(RECAREA)          AND PROCESS THIS COPY                    
         USING AUDITRD,R4                                                       
* - GETDEMVL RETURNS R1 TO DEMO VALUES                                          
         BAS   RE,GETDEMVL                                                      
         USING ESTABLED,R1                                                      
         MVI   NDNDEMOS,1                                                       
         MVC   NDDEMOS(3),ESTDEMO                                               
         MVC   NDWGTLST(1),ESTWTLST                                             
         MVC   NDWGTNAM(7),ESTWTNM                                              
         MVC   NDUSRNMS(7),ESTUSRNM                                             
         DROP  R1                                                               
* - CALL NETVALUE TO FILL NETBLOCK                                              
         BAS   RE,FILLDBLK                                                      
         MVC   AU2UNTK,NUKEY       UNIT KEY - AUNITREC                          
         GOTO1 =V(CLUNPK),DMCB,NUKCLT,AU2CLT                                    
         MVC   AU2PRD,NBPRD        PRODUCT                                      
         MVC   AU2PRD2,NBPRD2      PRODUCT 2                                    
         MVC   AU2LEN,NBLEN        SPOT LENGTH TOTAL                            
         CLI   NBPRD2,0            ONLY ONE PROD                                
         BE    NET18               YES                                          
         CLI   NBLEN1,0            ..IF LEN NOT INPUT                           
         BNE   NET17D                                                           
         ZIC   R1,NBLEN            ..ASSUME HALF THE LENGTH                     
         SRL   R1,1                                                             
         STC   R1,NBLEN1                                                        
NET17D   MVC   AU2LEN,NBLEN1       NO/SPOT LENGTH FIRST PIGGY                   
         ZIC   R1,NBLEN            TOTAL                                        
         ZIC   R0,NBLEN1           FIRST                                        
         SR    R1,R0               SUBTRACT                                     
         STC   R1,AU2LEN1          SET SPOT LENGTH SECOND PIGGY                 
NET18    MVC   AU2TIME,NBTIME      TIME                                         
         MVC   AU2PRGNM,NBPROGNM   PROGRAM NAME                                 
         MVC   AU2ACT,NBACTUAL     ACTUAL DOLLARS                               
         MVC   AU2TDEMO,NDDEMOS                                                 
         MVC   AU2TRTG,NDESTDEM+2                                               
         MVC   AU2PKG,NBPACK       PACKAGE                                      
         MVC   AU2UNTST,NBUNITST   UNIT STATUS                                  
         TM    NBUNITST,X'02'          IS IT MISSED MISSED GET PRIORITY         
         BO    NET18B                  YES                                      
         TM    NBUNITST,X'01'          IS IT A MAKE GOOD                        
         BNO   NET19                   NO                                       
         MVC   AU2MGBF(16),NBMGFPNM    YES/PROG NAME                            
         MVC   AU2MGBF+16(2),NBMGFDAT      DATE                                 
         MVC   AU2MGBF+18(1),NBMGFSUB      SUB-LINE                             
         MVC   AU2MGBF+19(6),NBMGFPCD      PROG CODE                            
         B     NET19                                                            
NET18B   DS    0H                            MISSED     \                       
         MVC   AU2MGBF(16),NBMGBPNM    YES/PROG NAME                            
         MVC   AU2MGBF+16(2),NBMGBDAT      DATE                                 
         MVC   AU2MGBF+18(1),NBMGBSUB      SUB-LINE                             
         B     NET19                                                            
*                                                                               
NET19    DS    0H                                                               
         CLI   DELETFLG,X'40'      IF DELETED/MISSED ETC                        
         BNH   NET19D                                                           
         MVC   AU2TYPE,DELETFLG      FLAG RECORD                                
         MVI   DELETFLG,0           CLEAR INDICATOR                             
         B     GET                  AND GET NEXT RECORD                         
NET19D   CLI   RRECTY,ADDS         IF ADD                                       
         BNE   GET                                                              
         MVI   AU2TYPE,C'A'          FLAG AS ADDED                              
         BE    NET24               ADD IT AS CHANGE ALSO                        
         EJECT                                                                  
* - CHANGE                                                                      
NET20    DS    0H                                                               
         CLI   PREVREC,COPY       PREVIOUS REC MUST BE A COPY                   
         BE    *+12                                                             
         BAS   RE,CHANGERR         ELSE PRINT THIS REC AS ERROR                 
         B     GET                 AND GET NEXT REC                             
NET24    L     R4,=A(RECAREA)                                                   
         USING AUDITRD,R4                                                       
* - GETDEMVL RETURNS R1 TO DEMO VALUES                                          
         BAS   RE,GETDEMVL                                                      
         USING ESTABLED,R1                                                      
         MVI   NDNDEMOS,1                                                       
         MVC   NDDEMOS(3),ESTDEMO                                               
         MVC   NDWGTLST(1),ESTWTLST                                             
         MVC   NDWGTNAM(7),ESTWTNM                                              
         MVC   NDUSRNMS(7),ESTUSRNM                                             
         DROP  R1                                                               
* - CALL NETVALUE TO FILL IN NETBLOCK                                           
         BAS   RE,FILLDBLK                                                      
         MVC   AU1UNTK,NUKEY       UNIT KEY -AUNITREC                           
         GOTO1 =V(CLUNPK),DMCB,NUKCLT,AU1CLT                                    
         MVC   AU1PRD,NBPRD        PRODUCT                                      
         MVC   AU1PRD2,NBPRD2      PRODUCT 2                                    
         MVC   AU1LEN,NBLEN        SPOT LENGTH TOTAL                            
         CLI   NBPRD2,0            ONLY ONE PROD                                
         BE    NET26               YES                                          
         CLI   NBLEN1,0            ..IF LEN NOT INPUT                           
         BNE   NET25                                                            
         ZIC   R1,NBLEN            ..ASSUME HALF LENGTH                         
         SRL   R1,1                                                             
         STC   R1,NBLEN1                                                        
NET25    MVC   AU1LEN,NBLEN1       NO/SPOT LENGTH FIRST PIGGY                   
         ZIC   R1,NBLEN            TOTAL                                        
         ZIC   R0,NBLEN1           FIRST                                        
         SR    R1,R0               SUBTRACT                                     
         STC   R1,AU1LEN1          SET SPOT LENGTH SECOND PIGGY                 
NET26    MVC   AU1TIME,NBTIME      TIME                                         
         MVC   AU1PRGNM,NBPROGNM   PROGRAM NAME                                 
         MVC   AU1ACT,NBACTUAL     ACTUAL DOLLARS                               
         MVC   AU1TDEMO,NDDEMOS                                                 
         MVC   AU1TRTG,NDESTDEM+2                                               
         MVC   AU1PKG,NBPACK       PACKAGE                                      
         MVC   AU1UNTST,NBUNITST   UNIT STATUS                                  
         TM    NBUNITST,X'02'         IS IT MISSED MISSED GET PRIORITY          
         BO    NET26B                  NO                                       
         TM    NBUNITST,X'01'          IS IT MAKEGOOD                           
         BNO   NET26D                  NO                                       
         MVC   AU1MGBF(16),NBMGFPNM    YES/PROG NAME                            
         MVC   AU1MGBF+16(2),NBMGFDAT      DATE                                 
         MVC   AU1MGBF+18(1),NBMGFSUB      SUB-LINE                             
         MVC   AU1MGBF+19(6),NBMGFPCD      PROG CODE                            
         B     NET26D                                                           
NET26B   DS    0H                        MISSED                                 
         MVC   AU1MGBF(16),NBMGBPNM    YES/PROG NAME                            
         MVC   AU1MGBF+16(2),NBMGBDAT      DATE                                 
         MVC   AU1MGBF+18(1),NBMGBSUB      SUB-LINE                             
         B     NET26D                                                           
*                                                                               
NET26D   MVI   PREVREC,0          CLEAR REC FLAG                                
         CLI   DELETFLG,X'40'      IF DELETED/MISSED ETC                        
         BNH   NET27                                                            
         MVC   AU1TYPE,DELETFLG      FLAG RECORD                                
         MVI   DELETFLG,0           CLEAR INDICATOR                             
         B     NET28                                                            
NET27    CLI   RRECTY,ADDS         IF ADD                                       
         BNE   NET28                                                            
         MVI   AU1TYPE,C'A'          FLAG AS ADDED                              
         B     NET30                AND ADD TO TSAROFF FILE                     
         EJECT                                                                  
* - COMPARE CHANGE TO COPY FOR SIGNIFICANT CHANGES                              
*                                                                               
NET28    DS    0H                                                               
         CLC   AU1UNTK(AU1LENE),AU2UNTK                                         
         BE    NET50                       NO SIGNIFICANT CHANGES               
         BNE   NET30                                                            
*                                                                               
* - SIGNIFICANT CHANGES / TSAROFF TIME                                          
* - RECORDS SIT OUT ON TSAROFF AS CHANGE + COPY                                 
* - SINCE I NEED FIRST COPY AND LAST CHANGE, I CHECK IF                         
* - CURRENT COPY MATCHES A TSAROFF CHANGE. IF IT DOES                           
* - PROGRAM USES THE TSAROFF COPY (THE ORIGINAL COPY) INSTEAD                   
* - OF THE CURRENT ONE                                                          
                                                                                
NET30    DS    0H                                                               
         LA    R2,TSAREA                                                        
         USING TSARD,R2                                                         
         XC    MYTSREC(MYTSRECL),MYTSREC                                        
* - FIRST ADD A DUMMY REC WITH COPY KEY TO TEST IF THAT MATCHES                 
* - A PREVIOUS CHANGE ON TSAROFF FILE                                           
         MVC   MYTSREC(AU1LENE),AU2UNTK     USE COPY KEY                        
         MVI   TSOFFACT,TSAADD     ADD WILL COME BACK WITH DUP ERROR            
         LA    RE,MYTSREC                                                       
         ST    RE,TSAREC                                                        
         GOTO1 ATSAROFF,(R2)                                                    
         CLI   TSERRS,0                                                         
         BNE   NET40                                                            
* - ADDED - NO ERRORS SO NO PREVIOUS COPY/CHANGES                               
* - DELETE THE DUMMY AND ADD CURRENT COPY/CHANGE                                
         MVI   TSOFFACT,TSADEL                                                  
         GOTO1 ATSAROFF,(R2)                                                    
         CLI   TSERRS,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   MYTSREC(AUDLENE),AU1UNTK   NOW ADD CHANGE/COPY                   
         MVI   TSOFFACT,TSAADD                                                  
         GOTO1 ATSAROFF,(R2)                                                    
         CLI   TSERRS,0                                                         
         BE    NET50                                                            
         DC    H'0'                                                             
*                                                                               
NET40    CLI   TSERRS,TSEDUP       DUPLICATE KEY/CURRENT COPY MATCHES           
*                                  A PREVIOUS CHANGE                            
         BE    *+6                                                              
         DC    H'0'                                                             
* - GET THE RECORD FROM TSAROFF AND USE ITS COPY                                
         MVI   TSOFFACT,TSAGET                                                  
         GOTO1 ATSAROFF,(R2)                                                    
         CLI   TSERRS,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,TSAREC                                                        
         LA    R1,AU1LENE(R1)                POINT TO TSAROFF COPY              
         MVC   AU2UNTK(AU2LENE),0(R1)        MOVE IT TO CURRENT COPY            
         CLI   AU2TYPE,C'A'        *** IF COPY = ADDED                          
         BE    NET47                                                            
         B     NET49                                                            
*        LA    R1,AU2LENE(R1)      .IF ORIGINAL COPY = ADDED                    
*        CLI   0(R1),C'A'                                                       
*        BNE   NET49                                                            
*        CLI   AUTYPE,X'40'        .AND CURRENT STATUS IS NORMAL                
*        BH    NET47                                                            
*        MVI   AUTYPE,C'A'         KEEP ADDED STATUS FOR CURRENT REC            
*        B     NET49                                                            
*                                                                               
NET47    CLI   AU1TYPE,C'D'         .AND CURRENT STATUS IS DELETED              
         BNE   NET49                                                            
         MVI   TSOFFACT,TSADEL      DELETE TSAROFF RECORD                       
         GOTO1 ATSAROFF,(R2)                                                    
         CLI   TSERRS,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     NET50               AND SKIP THIS ONE ALSO                       
                                                                                
* - PREPARE TO WRITE CURRENT CHANGE AND ORIGINAL COPY                           
NET49    MVC   MYTSREC(AUDLENE),AU1UNTK      MOVE CHA/COPY TO IOAREA            
         LA    R1,MYTSREC                                                       
         ST    R1,TSAREC                                                        
         MVI   TSOFFACT,TSAWRT               WRITE IT                           
         GOTO1 ATSAROFF,(R2)                                                    
         CLI   TSERRS,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
NET50    L     R1,=A(RECAREA)                CLEAR RECAREA                      
         XC    0(AUDLENE,R1),0(R1)                                              
         B     GET                       AND GET NEXT RECORD                    
         DROP  R2                                                               
         EJECT                                                                  
* -  DEMOS                                                                      
GETDEMVL NTR1                                                                   
         XC    DEMFCTOR,DEMFCTOR                                                
* - FIRST CHECK IF DEMO GUARANTEE                                               
         MVC   DATADISP,=H'27'                                                  
         L     R6,AUNITREC                                                      
         MVI   ELCODE,X'B4'                                                     
         BAS   RE,GETEL                                                         
         BNE   GD03                                                             
         USING NUNDGD,R6                                                        
         LA    R1,WORK                                                          
         XC    0(ESTLENE,R1),0(R1)                                              
         USING ESTABLED,R1                                                      
         MVC   ESTDEMO,NUNDGDEM    DEMO                                         
         MVC   DEMFCTOR,NUNDGFAC   GUARANTEE FACTOR                             
         L     R4,AUNITREC                                                      
         USING NURECD,R4                                                        
         MVC   ESTCLT,NUKCLT                                                    
         MVC   ESTEST,NUKEST                                                    
         B     GDX                                                              
* - PACKAGE GUARANTEE                                                           
GD03     L     R6,AUNITREC                                                      
         USING NUNGUD,R6                                                        
         MVI   ELCODE,X'B3'                                                     
         BAS   RE,GETEL                                                         
         BNE   GD05                                                             
         MVC   DEMFCTOR,NUNGUFAC   PKG GUARNATEE                                
         DROP  R6                                                               
* - NO DEMO GUARANTEE/ GET POL DEMO                                             
GD05     L     R4,AUNITREC                                                      
         MVC   KEY(2),NUKCLT                                                    
         MVC   KEY+2(1),NUKEST                                                  
         L     R1,=A(ESTABLE)                                                   
GD10     CLC   0(3,R1),KEY         .DO I ALREADY HAVE DEMO                      
         BE    GDX                 .YES                                         
         LA    R1,ESTLENE(R1)      .BUMP ESTABLE ENTRY                          
         CLI   0(R1),0             .NO MORE ENTRIES                             
         BE    GD20                                                             
         CLC   0(2,R1),=X'FFFF'    ..IF EST TABLE FULL                          
         BNE   GD10                                                             
         L     RE,=A(ESTABLE)      ..CLEAR IT AND START OVER                    
         SR    RF,RF                                                            
         LA    RF,ESTBLN(RF)                                                    
         XCEF                                                                   
         L     R1,=A(ESTABLE)                                                   
GD20     XC    KEY,KEY             READ EST HEADER TO GET TARGET                
         MVC   KEY+1(3),NUKAM      A/M + CLIENT                                 
         MVC   KEY+4(3),=C'POL'                                                 
         MVC   KEY+7(1),NUKEST                                                  
         BAS   RE,MYHIGH                                                        
         MVC   KEYSAVE(13),KEY                                                  
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,=A(TEMPIO2)                                                   
         ST    R3,AIO                                                           
         BAS   RE,MYGETREC                                                      
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ESTHDR,R3                                                        
* - R1 POINTS TO NEXT AVAILABLE ESTABLE ENTRY ESTABLISHED ABOVE                 
         MVC   ESTCLT,NUKCLT                                                    
         MVC   ESTEST,NUKEST                                                    
         MVC   ESTDEMO,EDEMLST     DEMO                                         
         MVC   ESTWTLST,EWGTLST    WEIGHT LIST                                  
         MVC   ESTWTNM,EWGTNM      WEIGHTED DEMO NAME                           
         MVC   ESTUSRNM,EUSRNMS    USER NAME                                    
*                                                                               
GDX      XIT1  REGS=(R1)              PASS BACK ADDRESSABILITY TO DEMOS         
         DROP  R4,R3,R1                                                         
                                                                                
         EJECT                                                                  
*                                                                               
FILLDBLK NTR1                                                                   
         MVC   NBAIO,AUNITREC       SET NBAIO TO START OF UNIT REC              
         MVI   NBESTOPT,C'M'        RETURN DEMOS UNCONDITIONALLY                
         GOTO1 NBNETVAL,DMCB,NETBLOCK                                           
         CLI   NBERROR,NBGOOD                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    NBUNITST,X'42'      IF PREEMPTED OR MISSED SET TO ZERO           
         BZ    FILLX                                                            
FILLX    B     EXIT                                                             
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
         EJECT                                                                  
         EJECT                                                                  
*****************************************************                           
*                                                   *                           
* THIS ROUTINE TAKES COPY/CHANGES RECS FROM TSAROFF *                           
* SENDS THEM TO SORTER                              *                           
* EACH REC IS PRINTED                               *                           
* AT KEYBREAK TOTS ARE PRINTED AND ROLLED           *                           
*****************************************************                           
         SPACE                                                                  
NET200   NTR1                                                                   
         LA    R2,TSAREA                                                        
         USING TSARD,R2                                                         
         XC    MYTSREC(MYTSRECL),MYTSREC           CLEAR I/O AREA               
         MVI   TSOFFACT,TSAGET     READ BY NUMBER                               
         LA    R0,1                                                             
NT210    STH   R0,TSRNUM                                                        
         LA    R0,MYTSREC                                                       
         ST    R0,TSAREC                                                        
         GOTO1 ATSAROFF,(R2)                                                    
         TM    TSERRS,TSEEOF                                                    
         BO    NT200X                                                           
         L     R3,=A(RECAREA)                                                   
         USING SORTRECD,R3                                                      
         LR    RE,R3                                                            
         L     RF,=F'300'                                                       
         XCEF                                                                   
* - DO COPY FIRST                                                               
         LA    R4,MYTSREC                                                       
         USING AUDITRD,R4                                                       
         LA    R4,AU2UNTK                                                       
         USING NURECD,R4                                                        
         GOTO1 =V(CLUNPK),DMCB,NUKCLT,SRTCLT                                    
         MVC   SRTDATE,NUKDATE                                                  
         MVC   SRTEST,NUKEST                                                    
         MVC   SRTNET,NUKNET                                                    
         MVC   SRTPROG,NUKPROG                                                  
         MVC   SRTLNUM,NUKSUB                                                   
*                                                                               
         LA    R4,MYTSREC                                                       
         USING AUDITRD,R4                                                       
         MVC   SRTPKG,AU2PKG                                                    
* - GET PRINTABLE PRODUCT CODE                                                  
         MVC   WORK(3),AU2UNTK+1   PASS A/M,CLIENT                              
         MVC   WORK+3(2),AU2PRD     PASS 1-BYTE PRODUCT CODES                   
         BAS   RE,GETPROD                                                       
         MVC   SRTPROD,WORK        RETURNED 3-BYTE PRODUCT CODES                
         MVC   SRTDEMO,AU2TDEMO                                                 
         MVC   SRTRTG,AU2TRTG                                                   
         MVC   SRTACT,AU2ACT                                                    
         MVC   SRTLEN,AU2LEN      TOTAL SPOT LENGTH                             
         MVC   SRTLEN1,AU2LEN1    LENGTH OF 1ST PROD                            
         MVC   SRTTIME,AU2TIME     TIME                                         
         MVC   SRTPRGNM,AU2PRGNM   PROGRAM NAME                                 
         MVC   SRTMGBF,AU2MGBF     MAKE GOOD BY/FOR DATA                        
         MVC   SRTTYP1,AU2TYPE     DEL/MISSED/ADD/PREEMPT                       
* - NOW DO CHANGE                                                               
         LA    R4,MYTSREC                                                       
         USING AUDITRD,R4                                                       
         LA    R4,AU1UNTK                                                       
         USING NURECD,R4                                                        
         GOTO1 =V(CLUNPK),DMCB,NUKCLT,SRTCLT2                                   
         MVC   SRTDATE2,NUKDATE                                                 
         MVC   SRTEST2,NUKEST                                                   
         MVC   SRTNET2,NUKNET                                                   
         MVC   SRTPROG2,NUKPROG                                                 
         MVC   SRTLNUM2,NUKSUB                                                  
*                                                                               
         LA    R4,MYTSREC                                                       
         USING AUDITRD,R4                                                       
         MVC   SRTPKG2,AU1PKG                                                   
* - GET PRINTABLE PRODUCT CODE                                                  
         MVC   WORK(3),AU1UNTK+1   PASS A/M,CLIENT                              
         MVC   WORK+3(2),AU1PRD     PASS 1-BYTE PRODUCT CODES                   
         BAS   RE,GETPROD                                                       
         MVC   SRTPROD2,WORK       RETURNED 3-BYTE PRODUCT CODES                
         MVC   SRTDEMO2,AU1TDEMO                                                
         MVC   SRTRTG2,AU1TRTG                                                  
         MVC   SRTACT2,AU1ACT                                                   
         MVC   SRTLEN2,AU1LEN       TOTAL SPOT LENGTH                           
         MVC   SRTLEN21,AU1LEN1     LENGTH OF 1ST PROD                          
         MVC   SRTTIME2,AU1TIME                                                 
         MVC   SRTPRGN2,AU1PRGNM                                                
         MVC   SRTMGBF2,AU1MGBF     MAKE GOOD BY/FOR DATA                       
         MVC   SRTTYP2,AU1TYPE      DEL/MISSED/PREEMPT/ADD                      
         CLI   AU1TYPE,C'M'        ,,IF CHANGE MISSED                           
         BNE   NT212                                                            
         CLI   AU2TYPE,C'M'        ,,AND COPY = MISSED                          
         BNE   NT212                                                            
         CLC   SRTMGBF2,SRTMGBF    ,,AND MAKE GOOD BYS ARE =                    
         BE    NT210              ,,SKIP - NO NEED TO SHOW CHANGES TO           
*                                 ,,       MISSED UNITS                         
**********NOTE SHOULDN'T THIS GO TO NT220 INSTEAD TO BUMP                       
**********NUMBER BEFORE READ? **** THIS NOTE IS ONLY CHANGE AT                  
**********THIS LEVEL                                                            
         DS    0H                                                               
NT212    MVI   SRTORD1,1           ADDED MAKEGOODS GET 2                        
*                                  SO THEY SORT AFTER THEIR MISSED              
                                                                                
* PREPARE TO PUT RECORD TO SORT BY SETTING UP SRTORDER FIELD                    
         CLI   SRTTYP1,C'A'                 ..IF COPY ADDED                     
         BNE   *+10                                                             
         MVC   SRTCLT(10),SRTCLT2           ..SET CLT/PRD = BOTH RECS           
                                                                                
         MVC   SRTORDER,SRTCLT              SET HIGER SORT FIELD                
*                                                                               
* - FOR NEWLY CREATED MAKEGOODS FIND MISSED UNIT AND SET ITS DATE               
* - IN SRTORDER SO MAKEGOOD AND MISSED APPEAR TOGETHER                          
         TM    AU1UNTST,X'02'    ,,IF MISSED SKIP MAKE GOOD CHECK               
*                                  MGBF FIELD HAS MADE GOOD BY DATA             
         BO    NT219                                                            
         TM    AU1UNTST,X'01'    ,,IF CHANGE IS A MAKE GOOD                     
         BNO   NT219                                                            
         CLI   SRTTYP1,C'A'        ,,AND COPY IS ADDED                          
         BE    NT214                 GET MADEGOOD FOR DATA FOR SRTORD           
         TM    AU2UNTST,X'01'    ,,OR - AND IF COPY IS NOT A MAKE GOOD          
         BO    NT219                                                            
NT214    DS    0H                                                               
*        GOTO1 =V(PRNTBL),DMCB,=C'TT2',AU1UNTK,C'DUMP',200,=C'1D'               
         LA    RF,SRTMGBF2       ,,GET MADE GOOD FOR DATA(MISSED)               
         LA    RE,SRTORDER       ,,AND SET IT INTO SRTORDER                     
*                                ,,SO SORT PUTS MAKEGOOD AFTER MISSED           
         DROP  R3                                                               
         USING SRTCLT,RE                                                        
         MVC   SRTDATE,16(RF)      DATE                                         
         MVC   SRTLNUM,18(RF)      LINENUMBER                                   
         MVC   SRTPROG,19(RF)      PROGRAM CODE                                 
         DROP  RE                                                               
         USING SORTRECD,R3                                                      
         MVI   SRTORD1,2           SO MAKEGOOD SORTS AFTER MISSED               
                                                                                
* - GET MISSED RECORD AND SET ITS PRODUCT INTO SORT IN CASE MISSED/             
* - MAKEGOOD HAD A PRODUCT CHANGE                                               
         LA    RE,AU1UNTK          SET UP KEY OF MISSED RECORD                  
         USING NURECD,RE                                                        
         XC    KEY,KEY                                                          
         MVI   KEY,X'84'                                                        
         MVC   KEY+1(3),1(RE)      A/M,CLIENT                                   
         MVC   KEY+4(4),NUKNET                                                  
         MVC   KEY+8(6),19(RF)      MISSED PROG CODE                            
         MVC   KEY+14(2),16(RF)      MISSED DATE                                
         MVC   KEY+16(1),NUKEST       ESTIMATE                                  
         MVC   KEY+17(1),18(RF)       MISSED SUB-LINE                           
         MVC   KEY+18(1),NUKDP         DAYPART                                  
         DROP  RE                                                               
         GOTO1 HIGH                NOW FIND MISSED REC                          
         CLC   KEY(20),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,=A(TEMPIO2)                                                   
         ST    R1,AIO                                                           
         GOTO1 GETREC                                                           
         L     RE,AIO                                                           
         USING NURECD,RE                                                        
         MVC   WORK(3),1(RE)       PASS A/M,CLIENT CODE                         
         LA    RE,27(RE)                                                        
         CLI   0(RE),1                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NUMAINEL,RE                                                      
         MVC   WORK+3(2),NUPRD     AND PRODUCT CODES                            
         DROP  RE                                                               
         BAS   RE,GETPROD                                                       
         MVC   SRTORDER+3(7),WORK  SET AAA/BBB PROD CODES                       
*                                                                               
NT219    DS    0H                                                               
*        GOTO1 =V(PRNTBL),DMCB,=C'SRT',SRTORDER,C'DUMP',300,=C'1D'              
         GOTO1 ASORTER,DMCB,=C'PUT',(R3)                                        
                                                                                
* IF PRODUCTS CHANGED PUT OUT UNDER OLD AND NEW PRODUCTS                        
         CLC   =C'POL',POLRUN      IF POL REQUEST                               
         BE    NT220               DON'T SEND DOUBLE PRODS                      
         CLI   SRTTYP1,C'D'                 ..IF DELETED                        
         BE    NT220                             SEND ONLY ONCE                 
         CLC   SRTPROD,SRTPROD2             ..ELSE IF PRODUCTS CHANGE           
         BE    NT220                                                            
         MVC   SRTORDER,SRTCLT2                 SET NEW PROD IN KEY             
         GOTO1 ASORTER,DMCB,=C'PUT',(R3)                                        
                                                                                
NT220    LH    R0,TSRNUM                                                        
         A     R0,=F'1'                                                         
         B     NT210                                                            
*                                                                               
NT200X   B     EXIT                                                             
         DROP  R3,R4                                                            
         EJECT                                                                  
*- GET 3 BYTE PROD CODE(S) / WORK HAS A/M,CLT,PRD1.PRD2                         
GETPROD  NTR1                                                                   
         L     R2,=A(TEMPIO)                                                    
         ST    R2,AIO                                                           
         CLI   0(R2),0                                                          
         BNE   *+14                                                             
         CLC   WORK(3),1(R2)       SAME CLIENT                                  
         BE    GTP10                                                            
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),WORK       A/M,CLT                                      
         BAS   RE,MYHIGH                                                        
         CLC   KEYSAVE(13),KEY                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CLTHDR,R2                                                        
         BAS   RE,MYGETREC                                                      
GTP10    LA    R1,255                                                           
         LA    R2,CLIST                                                         
         LA    R3,WORK+3           FIRST PRODUCT                                
GTP20    CLC   0(1,R3),3(R2)                                                    
         BE    GTP30                                                            
         LA    R2,4(R2)                                                         
         CLI   0(R2),0             END OF LIST                                  
         BE    GTP25                                                            
         BCT   R1,GTP20                                                         
GTP25    MVC   WORK(3),=C'***'     UNKNOWN                                      
         B     *+10                                                             
GTP30    MVC   WORK(3),0(R2)                                                    
         CLI   WORK+4,0            IS THERE 2ND PRODUCT                         
         BNE   GTP35                                                            
         XC    WORK+3(4),WORK+3    NO/CLEAR REST OF WORK                        
         B     GTPX                                                             
*                                                                               
GTP35    L     R2,AIO              YES/RESET REGISTERS                          
         LA    R2,CLIST                                                         
         LA    R1,255                                                           
         LA    R3,WORK+4           POINT TO SECOND PRODUCT                      
GTP40    CLC   0(1,R3),3(R2)                                                    
         BE    GTP50                                                            
         LA    R2,4(R2)                                                         
         CLI   0(R2),0                                                          
         BE    GTP45                                                            
         BCT   R1,GTP40                                                         
GTP45    MVC   WORK+4(3),=C'***'   UNKNOWN                                      
         B     GTPX                                                             
GTP50    MVC   WORK+4(3),0(R2)                                                  
*                                                                               
GTPX     MVI   WORK+3,0                                                         
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
* - GET RECS FROM SORTER AND PRINT REPORT                                       
NET300   NTR1                                                                   
         MVI   FRST,C'Y'                                                        
         SPACE                                                                  
NET320   GOTO1 ASORTER,DMCB,=C'GET'                                             
         L     R6,4(R1)                                                         
         LTR   R6,R6                                                            
         BNZ   NET330                                                           
         SPACE                                                                  
         CLI   FRST,C'Y'           NO RECS/TEST FIRST TIME                      
         BE    ERR                 YES/ERROR                                    
         BAS   RE,PRNTLOGE         NO/PRINT ENDING LOGO                         
         B     EXIT                                                             
ERR      BAS   RE,PRNTLOGS                                                      
         MVC   MYP(29),=C'NO RECORDS RECEIVED FROM SORT'                        
         BAS   RE,PRINTIT                                                       
         BAS   RE,PRNTLOGE                                                      
         B     EXIT                                                             
         SPACE 2                                                                
NET330   DS    0H                                                               
         L     R3,=A(RECAREA)                                                   
         USING SORTRECD,R3                                                      
         LR    RF,R3                                                            
         LR    RE,R6                                                            
         LA    R1,SORTRLEN                                                      
         MOVE  ((RF),(R1)),(RE)                                                 
         CLI   SRTTYP2,X'40'             ..IF SIMPLE CHANGE                     
         BH    *+14                                                             
         CLC   SRTCLT(SRTSIGE),SRTCLT2   ..AND REC RETURN TO ORIGINAL           
         BE    NET320                    ..AFTER ALL IS DONE/SKIP IT            
         B     NET331                                                           
*                                                                               
NET331   CLI   FRST,C'Y'                     TEST FIRST TIME                    
         BNE   NET340                                                           
* - FIRST TIME                                                                  
         MVC   SAVECLT,SRTORDER                                                 
         MVC   SAVEPROD(3),=C'POL'                                              
         CLC   =C'POL',POLRUN                                                   
         BE    *+10                                                             
         MVC   SAVEPROD,SRTORDER+3                                              
         MVC   SAVEEST,SRTEST2                                                  
         MVC   SAVEDEM(3),SRTDEMO                                               
         MVC   SAVEDEM+3(3),SRTDEMO2                                            
         BAS   RE,CHKCLT                                                        
         BAS   RE,CHKPROD                                                       
         BAS   RE,CHKEST                                                        
         BAS   RE,PRNTLOGS                                                      
         BAS   RE,PAGETOP          PRINT HEADS                                  
         MVI   FRST,C'N'                                                        
         B     NET344                                                           
                                                                                
* - CHECK CLIENT/PRODUCT/ESTIMATE BREAKS                                        
NET340   CLC   SAVECLT,SRTORDER                                                 
         BE    NET340B             NO                                           
         XC    SAVEPROD,SAVEPROD   YES/FORCE ALL TO BREAK                       
         XC    SAVEEST,SAVEEST                                                  
* - CLIENT BREAK                                                                
         MVC   SAVECLT,SRTORDER                                                 
         BAS   RE,CHKCLT                                                        
         MVI   BYTE,C'Y'           SET BREAK BYTE                               
*                                                                               
* - PRODUCT BREAK                                                               
NET340B  CLC   =C'POL',POLRUN          IF REQ FOR POL                           
         BNE   NET340C                                                          
         MVC   SAVEPROD(3),=C'POL'     DONT BREAK FOR PRODUCT                   
         B     NET340D                                                          
NET340C  CLC   SAVEPROD,SRTORDER+3                                              
         BE    NET340D                                                          
         MVC   SAVEPROD,SRTORDER+3                                              
         BAS   RE,CHKPROD                                                       
         MVI   BYTE,C'Y'                                                        
*                                                                               
* - ESTIMATE BREAK                                                              
NET340D  CLC   SRTEST2,SAVEEST                                                  
         BE    NET340E                                                          
         MVC   SAVEEST,SRTEST2                                                  
         BAS   RE,CHKEST                                                        
         MVI   BYTE,C'Y'                                                        
*                                                                               
* - DEMO NAME                                                                   
NET340E  CLC   SRTDEMO,SAVEDEM                                                  
         BNE   *+14                                                             
         CLC   SRTDEMO2,SAVEDEM+3                                               
         BE    NET340F                                                          
         MVC   SAVEDEM(3),SRTDEMO                                               
         MVC   SAVEDEM+3(3),SRTDEMO2                                            
         MVI   BYTE,C'Y'                                                        
*                                                                               
NET340F  CLI   BYTE,C'Y'                                                        
         BNE   NET340G                                                          
         XC    SAVENET,SAVENET     CLEAR NET ON PAGE BREAK                      
         BAS   RE,PAGETOP                                                       
         MVI   BYTE,0                                                           
         B     NET344                                                           
*                                                                               
         EJECT                                                                  
                                                                                
* PRINT THE REC *                                                               
NET340G  DS    0H                                                               
* FOR TESTING                                                                   
*        GOTO1 =V(PRNTBL),DMCB,=C'SRT',SRTORDER,C'DUMP',31,=C'1D'               
         L     RE,ASPOOLD                                                       
         USING SPOOLD,RE                                                        
         CLI   LINE,50                                                          
         BNH   NET344                                                           
         DROP  RE                                                               
         BAS   RE,PAGETOP                                                       
         B     NET345                                                           
                                                                                
* - NETWORK AS MIDLINE                                                          
NET344   CLC   SAVENET,SRTNET2           NETWORK CHANGE                         
         BE    *+8                                                              
         BAS   RE,CHKMID                                                        
NET345   MVI   BYTE,0                                                           
         LA    R2,MYP                                                           
         USING PLINED,R2                                                        
*                                                                               
*                                                                               
         CLI   SRTTYP1,C'A'        IF ADDED                                     
         BNE   NET345B                                                          
         CLI   FIRSTADD,C'N'                                                    
         BE    *+12                                                             
         BAS   RE,PRINTIT                                                       
         MVI   FIRSTADD,C'N'                                                    
         MVC   PDATE+1(23),=C'*********ADDED*********'                          
         CLI   SRTMGBF2,X'40'      IS IT MAKEGOOD                               
         BNH   NET357                                                           
         MVC   PTIME(7),=C'M/G FOR' YES/SET LABEL                               
         B     NET345C                  GO SET IN DATA                          
*                                                                               
NET345B  CLI   SRTTYP2,C'M'        IF MISSED                                    
         BNE   NET346                                                           
         LA    R2,PLENE(R2)                                                     
         MVC   PDATE+1(23),=C'********MISSED*********'                          
         CLI   SRTMGBF2,X'40'      IS IT MADE GOOD                              
         BNH   NET357                                                           
         MVC   PTIME(6),=C'M/G BY'                                              
         MVC   PTIME+8(16),SRTMGBF2                                             
         GOTO1 DATCON,DMCB,(2,SRTMGBF2+16),(7,PTIME+25)                         
         MVI   BYTE,2             ONLY DO THIS HALF                             
         LA    R3,SRT1LENE(R3)    BUMP TO CHANGED REC AREA                      
         B     NET349             PUT OUT CHANGED REC IN ORIGINAL AREA          
*                                                                               
NET345C  MVC   PTIME+8(16),SRTMGBF2                                             
         GOTO1 DATCON,DMCB,(2,SRTMGBF2+16),(7,PTIME+25)                         
         B     NET357              PUT OUT CHANGED REC                          
*                                                                               
NET346   CLI   SRTTYP2,X'40'       IS IT REGULAR CHANGED REC                    
         BNH   NET350              YES                                          
         MVI   BYTE,2              NO                                           
         LA    R2,PLENE(R2)        BUMP  P LINE TO SECOND HALF                  
*                                                                               
         CLI   SRTTYP2,C'D'        IF DELETED                                   
         BNE   NET346D                                                          
         CLI   FIRSTDEL,C'N'                                                    
         BE    *+12                                                             
         BAS   RE,PRINTIT                                                       
         MVI   FIRSTDEL,C'N'                                                    
         MVC   PDATE+1(23),=C'********DELETED********'                          
         B     NET349                                                           
*                                                                               
NET346D  CLI   SRTTYP2,C'P'        IF PRE-EMPTED                                
         BNE   *+14                                                             
         MVC   PDATE+1(23),=C'*****PRE-EMPTED********'                          
         B     NET349                                                           
*                                                                               
NET349   LA    R2,MYP              RESET PRINT LINE TO START                    
*                                                                               
NET350   GOTO1 DATCON,DMCB,(2,SRTDATE),(4,PDATE)                                
***      MVI   PDATE+133,C'-'                       * DON'T PRINT               
***      LA    R4,PDATE+134                         * LINE NUMBER               
***      EDIT  (B1,SRTLNUM),(3,0(R4)),ALIGN=LEFT    * FOR NOW                   
         EDIT  (B1,SRTPKG),(3,PPKG)                                             
         MVC   PPROD,SRTPROD                                                    
         CLI   SRTPROD+4,X'40'                                                  
         BNH   *+10                                                             
         MVC   PPROD+132(3),SRTPROD+4                                           
***      MVC   PPROG(6),SRTPROG               * DON'T PRINT PROG CODE           
***      MVC   PPROG+132(16),SRTPRGNM         * FOR NOW                         
         MVC   PPROG(16),SRTPRGNM                                               
         GOTO1 UNTIME,DMCB,SRTTIME,PTIME                                        
         EDIT  (B1,SRTLEN),(3,PLENTH)                                           
         CLI   SRTLEN1,0                                                        
         BE    NET355                                                           
         LA    R4,PLENTH+132                                                    
         EDIT  (B1,SRTLEN1),(3,0(R4))                                           
NET355   DS    0H                                                               
* - RATING                                                                      
         LA    R4,PRTG                                                          
         EDIT  (B2,SRTRTG),(5,0(R4)),1,ALIGN=LEFT                               
* - DOLLARS                                                                     
         SR    R0,R0                                                            
         ICM   R1,15,SRTACT                                                     
         D     R0,=F'100'                                                       
         EDIT  (R1),(7,PDOLLAR)                                                 
NET357   CLI   BYTE,2              HAVE WE DONE BOTH HALVES                     
         BE    NET360                                                           
         LA    R2,PLENE(R2)        BUMP P LINE                                  
         LA    R3,SRT1LENE(R3)     SORTREC HALVES ARE INDENTICAL                
         MVI   BYTE,2                                                           
         B     NET350                                                           
         SPACE                                                                  
NET360   MVI   BYTE,0                                                           
         BAS   RE,PRINTIT                                                       
         BAS   RE,PRINTIT          SKIP A LINE                                  
         B     NET320              GET NEXT REC FROM SORTER                     
         DROP  R2,R3                                                            
*                                                                               
         EJECT                                                                  
* PRINT AN AGENCY LOGO                                                          
PRNTLOGS NTR1                                                                   
*                                                                               
         B     EXIT                                                             
         SPACE 2                                                                
* PRINT AN AGENCY LOGO                                                          
PRNTLOGE NTR1                                                                   
         B     EXIT                                                             
         EJECT                                                                  
* SET TO PRINT HEADLINES *                                                      
PAGETOP  NTR1                                                                   
         L     R5,ASPOOLD          RA - DPRINT                                  
         USING SPOOLD,R5                                                        
         MVI   LINE,99                                                          
         MVC   HEAD1+54(26),=C'NETWORK CLIENT AUDIT TRAIL'                      
         MVC   HEAD2+54(26),=C'--------------------------'                      
         MVC   HEAD4+56(12),=C'ACTIVITY FOR'                                    
         GOTO1 DATCON,DMCB,(3,TODAYB),(5,HEAD4+69)                              
         MVC   HEAD2(6),=C'CLIENT'                                              
         MVC   HEAD2+10(3),SAVECLT                                              
         MVC   HEAD2+18(20),CLIENTNM                                            
         MVC   HEAD3(7),=C'PRODUCT'                                             
         MVC   HEAD3+10(7),SAVEPROD                                             
         MVC   HEAD3+18(20),PRODNAME                                            
         MVC   HEAD4(8),=C'ESTIMATE'                                            
         EDIT  (B1,SAVEEST),(3,HEAD4+10),ALIGN=LEFT                             
         MVC   HEAD4+18(20),ESTNAME                                             
         MVC   HEAD6+29(8),=C'ORIGINAL'                                         
         LA    R1,HEAD6+10                                                      
         LA    R1,PLENE(R1)                                                     
         MVC   20(7,R1),=C'CURRENT'                                             
         MVC   HEAD7(62),=65C'-'                                                
         LA    R1,HEAD7                                                         
         LA    R1,PLENE(R1)                                                     
         MVC   0(62,R1),=65C'-'                                                 
*                                                                               
* PRINT NETWORK AS MIDLINE                                                      
         L     R3,=A(RECAREA)                                                   
         USING SORTRECD,R3                                                      
         XC    MID1,MID1                                                        
         MVC   MID1(4),SRTNET2                                                  
         CLC   SAVENET,SRTNET2           NETWORK CHANGE                         
         BNE   *+10                                                             
         MVC   MID1+6(11),=C'(CONTINUED)'   NO - PRINT 'CONTINUED'              
         MVC   SAVENET,SRTNET2                                                  
         MVC   MID2(20),=20C'-'                                                 
*                                                                               
* - LOOP THROUGH P LINE TWICE                                                   
         LA    R2,P                                                             
         USING PLINED,R2                                                        
         MVI   BYTE,0                                                           
         OC    SRTDEMO,SRTDEMO     IF NO SRTDEMO                                
         BZ    PGT20               DON'T PRINT THIS HALF OF LINE                
         MVC   WORK(3),SRTDEMO     SET 1ST DEMO                                 
PGT10    MVC   PDATE(4),=C'DATE'                                                
         MVC   PPKG,=C'PKG'                                                     
         MVC   PPROD,=C'PRD'                                                    
         MVC   PPROG(7),=C'PROGRAM'                                             
         MVC   PTIME(4),=C'TIME'                                                
         MVC   PLENTH,=C'LEN'                                                   
         BAS   RE,GETDEMN                                                       
         MVC   PRTG-1(7),WORK                                                   
         MVC   PDOLLAR+1(4),=C'COST'                                            
PGT20    LA    R2,PLENE(R2)        BUMP TO 2ND HALF OF P LINE                   
         MVC   WORK(3),SRTDEMO2    SET DEMO FOR 2ND HALF                        
         CLI   BYTE,0              HAVE WE DONE 2ND HALF                        
         BNE   *+12                YES                                          
         MVI   BYTE,1              NO                                           
         B     PGT10                                                            
*                                                                               
         BAS   RE,PRINTER                                                       
         LA    R2,P                                                             
         USING PLINED,R2                                                        
         MVC   PDATE(PLEND),=65C'-'                                             
         LA    R2,PLENE(R2)                                                     
         MVC   PDATE(PLEND),=65C'-'                                             
         BAS   RE,PRINTER                                                       
         B     EXIT                                                             
         DROP  R5                                                               
*                                                                               
GETDEMN  NTR1                                                                   
         MVC   NDDEMOS(3),WORK                                                  
         MVI   NDDEMOS+1,C'R'      FORCE RATING                                 
         MVI   DBSELMED,C'N'                                                    
         MVC   DBFILE,=C'NTI'                                                   
         MVC   DBCOMFCS,ACOMFACS                                                
         L     RF,NBDEMCON                                                      
         L     R5,=A(TEMPIO2)                                                   
         GOTO1 (RF),DMCB,(0,NDDEMOS),(2,WORK),(C'S',DBLOCK),(R5)                
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
CHKCLT   NTR1                                                                   
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),AMSAVE                                                  
         GOTO1 =V(CLPACK),DMCB,SAVECLT,PKCLISV                                  
         MVC   KEY+2(2),PKCLISV                                                 
         BAS   RE,MYHIGH                                                        
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,=A(TEMPIO)                                                    
         ST    R2,AIO                                                           
         BAS   RE,MYGETREC                                                      
         USING CLTHDR,R2                                                        
         MVC   CLIENTNM,CNAME                                                   
         B     EXIT                                                             
*                                                                               
CHKPROD  NTR1                                                                   
         CLI   SAVEPROD,C'*'                                                    
         BNE   *+14                                                             
         MVC   PRODNAME(11),=C'UNALLOCATED'                                     
         B     CHKPRDX                                                          
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),AMSAVE                                                  
         MVC   KEY+2(2),PKCLISV                                                 
         MVC   KEY+4(3),SAVEPROD                                                
         BAS   RE,MYHIGH                                                        
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,=A(TEMPIO)                                                    
         ST    R2,AIO                                                           
         BAS   RE,MYGETREC                                                      
         USING PRDHDR,R2                                                        
         MVC   PRODNAME,PNAME                                                   
CHKPRDX  B     EXIT                                                             
*                                                                               
CHKEST   NTR1                                                                   
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),AMSAVE                                                  
         MVC   KEY+2(2),PKCLISV                                                 
         MVC   KEY+4(3),SAVEPROD                                                
         CLI   SAVEPROD,C'*'       UNALLOCATED                                  
         BNE   *+10                                                             
         MVC   KEY+4(3),=C'POL'    USE POL HEADER                               
         MVC   KEY+7(1),SAVEEST                                                 
         BAS   RE,MYHIGH                                                        
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,=A(TEMPIO)                                                    
         ST    R2,AIO                                                           
         BAS   RE,MYGETREC                                                      
         USING ESTHDR,R2                                                        
         MVC   ESTNAME,EDESC                                                    
         B     EXIT                                                             
         EJECT                                                                  
MYHIGH   NTR1                                                                   
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR',KEYSAVE,KEY                   
         B     GETRECX                                                          
*                                                                               
MYGETREC NTR1                                                                   
         LA    RF,=C'GETREC'                                                    
         B     GETREC2                                                          
*                                                                               
GETREC2  ST    RF,DMCB                                                          
         GOTO1 DATAMGR,DMCB,,=C'SPTFILE',KEY+14,AIO,DMWORK                      
*                                                                               
GETRECX  CLI   8(R1),0             SET CC ON EXIT                               
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
* - PRINTD ONLY HAS ONE PRINT LINE/USE MY OWN                                   
PRINTIT  NTR1                                                                   
         L     R5,ASPOOLD          RA - DPRINT                                  
         USING SPOOLD,R5                                                        
         MVC   P,MYP                                                            
         BAS   RE,PRINTER                                                       
         MVC   MYP,SPACES                                                       
         CLC   P,MYP2              ANYTING ON 2ND PRINT LINE                    
         BE    PRXX                                                             
         MVC   P,MYP2                                                           
         BAS   RE,PRINTER                                                       
         MVC   MYP2,SPACES                                                      
PRXX     B     EXIT                                                             
*                                                                               
PRINTER  NTR1                                                                   
         L     R5,ASPOOLD                                                       
         USING SPOOLD,R5                                                        
         GOTO1 SPOOL,DMCB,(R5)                                                  
         B     EXIT                                                             
         DROP  R5                                                               
*                                                                               
* - PRINT NETWORK MIDLINE                                                       
CHKMID   NTR1                                                                   
         L     R5,ASPOOLD          RA - DPRINT                                  
         USING SPOOLD,R5                                                        
         L     R3,=A(RECAREA)                                                   
         USING SORTRECD,R3                                                      
         MVI   SPACING,3           SKIP 3 LINES                                 
         BAS   RE,PRINTER                                                       
         MVI   SPACING,1           RESET SPACING                                
         MVC   P+1(4),SRTNET2                                                   
         BAS   RE,PRINTER                                                       
         MVC   P+1(20),=20C'-'                                                  
         BAS   RE,PRINTER                                                       
         BAS   RE,PRINTER                                                       
         MVC   SAVENET,SRTNET2                                                  
CHKMX    XIT1                                                                   
         DROP  R3,R5                                                            
*                                                                               
         EJECT                                                                  
*                                                                               
RECVIN   DCB   DDNAME=RECVIN,                                          X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=4048,                                             X        
               MACRF=GM,                                               X        
               EODAD=EXIT                                                       
               SPACE                                                            
         SPACE                                                                  
UTL      DC    F'0',X'00'                                                       
SSB      DC    F'2'                                                             
                                                                                
* - COPY WITH NO CHANGE                                                         
COPYERR  NTR1                                                                   
         MVC   MYP(16),=C'COPY - NO CHANGE'                                     
         GOTO1 HEXOUT,DMCB,PREVIOUS,MYP+20,44                                   
         BAS   RE,PRINTIT                                                       
         B     EXIT                                                             
                                                                                
* - CHANGE WITH NO PREVIOUS COPY                                                
CHANGERR NTR1                                                                   
         MVC   MYP(16),=C'CHANGE - NO COPY'                                     
         GOTO1 HEXOUT,DMCB,RFILTY,MYP+20,44                                     
         BAS   RE,PRINTIT                                                       
         B     EXIT                                                             
*                                                                               
* -  CONSTANTS FOR TSAROFF                                                      
TSARBUFF DS    A                   ADDRESS OF GETMAIN BUFF                      
TSARBUFL DC    A(AUDLENE*9000)   AUDLENE=LEN OF AUDIT TRAIL RECS                
*                                  1000 PAIRS OF CHANGE/COPY RECS               
*                                                                               
TSAREA   DS    XL64                                                             
*                                                                               
MYTSREC  DS    0D                                                               
*                                                                               
MYTSKEY  DS    CL(L'AU1UNTK)                                                    
MYTSKEYL EQU   *-MYTSKEY                                                        
MYTSDATA DS    CL(AUDTEND-AU1CLT)                                               
MYTSRECL EQU   *-MYTSREC                                                        
*                                                                               
         EJECT                                                                  
*                                                                               
HEDSPECS SSPEC H1,1,C'MEDIA     NETWORK T.V.'                                   
         SSPEC H1,99,AGYNAME                                                    
         SSPEC H2,99,AGYADD                                                     
         SSPEC H4,120,PAGE                                                      
         DC    X'00'                                                            
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
                                                                                
         EJECT                                                                  
*                                                                               
         DS    0D                                                               
         DC    CL8'**RECA**'                                                    
RECAREA  DS    CL300              AREA FOR CHANGE/COPY RECS                     
*                                                                               
         DC    CL8'**TEMP**'                                                    
TEMPIO DS      2000C              IO AREA                                       
*                                                                               
         DC    CL8'**TEMP2*'                                                    
TEMPIO2  DS    2000C              TEMP WORK IO AREA                             
*                                                                               
         DS    0D                                                               
         DC    CL8'*ESTABL*'                                                    
ESTABLE  DS    CL(100*ESTLENE)                                                  
ESTBLN   EQU   *-ESTABLE                                                        
         DC    X'FFFF'                                                          
*                                                                               
         DS    0D                                                               
         DC    CL8'**MYIO**'                                                    
MYIO     DS    0D                                                               
         DS    4048C                                                            
         EJECT                                                                  
         SPACE 2                                                                
*                                                                               
AUDITRD  DSECT                                                                  
* - CHANGE DATA                                                                 
AU1UNTK  DS    CL20                UNIT KEY                                     
AU1CLT   DS    CL3                 CLIENT CODE                                  
AU1PRD   DS    CL1                 PROD CODE                                    
AU1PRD2  DS    CL1                 SECOND PRODUCT                               
AU1LEN   DS    CL1                 TOTAL SPOT LENGTH                            
AU1LEN1  DS    CL1                 1ST PROD SPOT LENGTH                         
AU1ACT   DS    CL4                 ACTUAL DOLLARS                               
AU1TDEMO DS    CL3                 TARGET DEMO                                  
AU1TRTG  DS    CL2                 TARGET RATING                                
AU1PKG   DS    CL1                 PACKAGE                                      
AU1TIME  DS    CL4                 TIME                                         
AU1PRGNM DS    CL16                PROGRAM NAME                                 
AU1MGBF  DS    CL25                MAKE-GOOD (CL16=PROG/CL2=DAT/CL1=SL)         
*                                            (CL6=PROG CODE)                    
AU1UNTST DS    CL1                 UNIT STATUS (=NBUNITST)                      
AU1TYPE  DS    CL1                 D=DEL/A=ADD/P=PREEMPT                        
AU1LENE  EQU   *-AU1UNTK                                                        
* - COPY DATA                                                                   
AU2UNTK  DS    CL20                UNIT KEY                                     
AU2CLT   DS    CL3                 CLIENT CODE                                  
AU2PRD   DS    CL1                 PROD CODE                                    
AU2PRD2  DS    CL1                 SECOND PRODUCT                               
AU2LEN   DS    CL1                 TOTAL SPOT LENGTH                            
AU2LEN1  DS    CL1                 1ST PROD SPOT LENGTH                         
AU2ACT   DS    CL4                 ACTUAL DOLLARS                               
AU2TDEMO DS    CL3                 TARGET DEMO                                  
AU2TRTG  DS    CL2                 TARGET RATING                                
AU2PKG   DS    CL1                 PACKAGE                                      
AU2TIME  DS    CL4                 TIME                                         
AU2PRGNM DS    CL16                PROGRAM NAME                                 
AU2MGBF  DS    CL25                MAKE-GOOD (CL16=PROG/CL2=DAT/CL1=SL)         
*                                            (CL6=PROG CODE)                    
AU2UNTST DS    CL1                 UNIT STATUS (=NBUNITST)                      
AU2TYPE  DS    CL1                 D=DEL/A=ADD/P=PREEMPT                        
AU2LENE  EQU   *-AU2UNTK                                                        
AUDTEND  EQU   *                                                                
AUDLENE  EQU   *-AU1UNTK                                                        
                                                                                
*                                                                               
                                                                                
                                                                                
* - DSECT COVERING TARGET DEMOS FOR POL ESTIMATES                               
ESTABLED DSECT                                                                  
ESTCLT   DS    CL2                                                              
ESTEST   DS    CL1                                                              
ESTDEMO  DS    CL3                                                              
ESTWTLST DS    CL1                 WEIGHT                                       
ESTWTNM  DS    CL7                 WEIGHTED DEMO NAME                           
ESTUSRNM DS    CL7                 USER DEMO NAME                               
ESTLENE  EQU   *-ESTCLT                                                         
*                                                                               
         EJECT                                                                  
*                                                                               
SORTRECD DSECT                                                                  
         DS    0CL300                                                           
SRTORDER DS    CL25                CLIENT/PRODUCT                               
SRTORD1  DS    CL1                 FOR MAKEGOODS                                
*                                                                               
SRTCLT   DS    CL3                 COPY REC                                     
SRTPROD  DS    CL7                                                              
SRTEST   DS    CL1                                                              
SRTNET   DS    CL4                                                              
SRTPKG   DS    CL1                                                              
SRTDATE  DS    CL2                                                              
SRTLNUM  DS    CL1                 UNIT LINE NUMBER                             
SRTPROG  DS    CL6                                                              
SRTPRGNM DS    CL16                                                             
SRTTIME  DS    CL4                                                              
SRTLEN   DS    CL1                 TOTAL SPOT LENGTH                            
SRTLEN1  DS    CL1                 LENGTH OF FIRST PORDUCT                      
SRTDEMO  DS    CL3                                                              
SRTRTG   DS    CL2                                                              
SRTACT   DS    CL4                 DOLLARS                                      
SRTMGBF  DS    CL25                MAKE-GOOD BY/FOR                             
SRTTYP1  DS    CL1                                                              
SRTSIGE  EQU   *-SRTCLT                                                         
         DS    CL42                SPARE                                        
SRT1LENE EQU   *-SRTORDER                                                       
*                                                                               
SRTORD2  DS    CL25                                                             
SRTORD2A DS    CL1                                                              
SRTCLT2  DS    CL3                 CHANGED REC                                  
SRTPROD2 DS    CL7                                                              
SRTEST2  DS    CL1                                                              
SRTNET2  DS    CL4                                                              
SRTPKG2  DS    CL1                                                              
SRTDATE2 DS    CL2                                                              
SRTLNUM2 DS    CL1                 UNIT LINE NUMBER                             
SRTPROG2 DS    CL6                                                              
SRTPRGN2 DS    CL16                                                             
SRTTIME2 DS    CL4                                                              
SRTLEN2  DS    CL1                 TOTAL SPOT LENGTH                            
SRTLEN21 DS    CL1                 LENGTH OF 1ST PORD                           
SRTDEMO2 DS    CL3                                                              
SRTRTG2  DS    CL2                                                              
SRTACT2  DS    CL4                 DOLLARS                                      
SRTMGBF2 DS    CL25                MAKE-GOOD BY/FOR                             
SRTTYP2  DS    CL1                 DELETED/ADDED                                
         DS    CL42                SPARE                                        
SORTRLEN EQU   *-SRTORDER                                                       
         EJECT                                                                  
*                                                                               
PLINED   DSECT                                                                  
PDATE    DS    CL5                 DATE                                         
         DS    CL1                                                              
PPKG     DS    CL3                 PACKAGE                                      
         DS    CL1                                                              
PPROD    DS    CL3                 PRODUCT                                      
         DS    CL1                                                              
PPROG    DS    CL16                PROGRAM                                      
         DS    CL1                                                              
PTIME    DS    CL11                START TIME                                   
         DS    CL1                                                              
PLENTH   DS    CL3                                                              
         DS    CL2                                                              
PRTG     DS    CL5                 RATING                                       
         DS    CL2                                                              
PDOLLAR  DS    CL7                 DOLLARS                                      
PLEND    EQU   *-PDATE                                                          
         DS    CL8                 LEAVE AS BREAK                               
PLENE    EQU   *-PLINED            LENGTH                                       
                                                                                
         EJECT                                                                  
                                                                                
MYWORKD  DSECT                                                                  
TOTAL    DS    D                                                                
AUNITREC DS    A                   UNIT RECORD POINTER                          
ATSAROFF DS    A                                                                
ASORTER  DS    A                                                                
*                                                                               
PREVREC  DS    CL1                                                              
SAVECLT  DS    CL3                                                              
SAVEPROD DS    CL7                                                              
SAVEEST  DS    CL1                                                              
SAVENET  DS    CL4                                                              
SAVEDEM  DS    CL6                                                              
USERNUM  DS    CL2                                                              
POLRUN   DS    CL3                                                              
DIVSOR   DS    F                                                                
ADDER    DS    F                                                                
MULTER   DS    F                                                                
PMULTER  DS    PL8                                                              
PDIVSOR  DS    PL8                                                              
PADDER   DS    PL8                                                              
PWORK    DS    PL16                                                             
PREVIOUS DS    CL44                                                             
*                                                                               
*                                                                               
*                                                                               
AMSAVE   DS    CL1                                                              
AGYNAMSV DS    CL33                                                             
AGYADRSV DS    CL33                                                             
PKCLISV  DS    CL2                 CLIENT FILTER                                
CLIENTNM DS    CL20                                                             
PRODNAME DS    CL20                                                             
ESTNAME  DS    CL20                                                             
*                                                                               
RCVRYSV  DS    CL60                                                             
TODAYB   DS    CL3                                                              
PREVFLG  DS    CL1                                                              
DELETFLG DS    CL1                                                              
BRKBYTE  DS    CL1                                                              
EOFSRTRC DS    CL1                                                              
FRST     DS    CL1                                                              
MIDTXT   DS    CL6                                                              
WRITEFLG DS    CL1                                                              
ONEAGY   DS    CL2                                                              
ONECLT   DS    CL3                                                              
FIRSTADD DS    CL1                                                              
FIRSTDEL DS    CL1                                                              
DEMFCTOR DS    CL4                 DEMO/PKG GUARANTEE FACTOR                    
                                                                                
MYP      DS    CL132                                                            
MYP2     DS    CL132                                                            
MYSPACES DS    CL132                                                            
*                                                                               
*        NETDEMOD                                                               
*        DEDBLOCK                                                               
         PRINT OFF                                                              
       ++INCLUDE NETDEMOD                                                       
       ++INCLUDE DEDBLOCK                                                       
         PRINT ON                                                               
MYWRKDLE EQU   *-MYWORKD                                                        
*                                                                               
EVND     DSECT                                                                  
         SPACE 1                                                                
PIO      DS    0C                  AREA TO BUILD EVN RECORDS                    
EVNKEY   DS    CL22                (PHONY KEY)                                  
         SPACE 1                                                                
PUEL     DS    0CL243              UNIVERSE ELEMENT                             
         DS    XL3                 X'31F344'                                    
UNIVS    DS    CL240               UNIVERSES                                    
         SPACE 1                                                                
PVEL     DS    0CL243              VPH ELEMENT                                  
         DS    XL3                 X'33F302'                                    
VPHS     DS    CL240               VPHS                                         
         SPACE 1                                                                
PREL     DS    0CL9                RATING/HUT/SHARE ELEMENT                     
         DS    XL3                 X'350902'                                    
RATING   DS    XL2                 RATING                                       
HUT      DS    XL2                 HUT                                          
SHARE    DS    XL2                 SHARE                                        
         SPACE 1                                                                
PBEL     DS    CL7                 BOOK ELEMENT                                 
         SPACE 1                                                                
OVERAREA DS    0CL1                40 OVERRIDE ELEMENTS FOR NAD DEMOS           
*                                  PLUS 10 OVERRIDES PLUS EOR                   
OVEREL   DS    0CL12               OVERRIDE ELEMENT                             
*                                                                               
                                                                                
*                                                                               
RECD     DSECT                                                                  
RECLN    DS    XL2                                                              
         DS    CL2                                                              
REC      DS    0C                                                               
         EJECT                                                                  
       ++INCLUDE DMRCVRHDR                                                      
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE NETINCLS                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE DDTSARD                                                        
         PRINT ON                                                               
       ++INCLUDE NEWRIFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRICFD                                                       
*                                                                               
       ++INCLUDE DDMASTC                                                        
       ++INCLUDE DDGENTWA                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'025NEWRI83   05/01/02'                                      
         END                                                                    
