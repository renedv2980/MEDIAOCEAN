*          DATA SET NEWRI3C    AT LEVEL 003 AS OF 03/14/18                      
*          DATA SET NEWRI3C    AT LEVEL 069 AS OF 08/07/96                      
*PHASE T3203CA,+0                                                               
*INCLUDE BINSRCH2                                                               
*INCLUDE NETNET                                                                 
         TITLE 'T3203C - FLOWCHART'                                             
T3203C   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T3203C,RR=R2                                                   
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         LA    RA,4095(RB)                                                      
         LA    RA,1(RA)                                                         
         USING T3203C,RB,RA                                                     
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R7,ANETWS2                                                       
         USING MYWORKD,R7                                                       
         L     R6,ANETWS4                                                       
         ST    R6,ACLIST                                                        
         LA    R6,1000(R6)                                                      
         USING NDDEMBLK,R6                                                      
         ST    R6,NBADEM                                                        
         ST    R2,RELO                                                          
         LA    R2,HOOK                                                          
         ST    R2,HEADHOOK                                                      
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BE    REPMOD                                                           
XIT      XIT1                                                                   
         EJECT                                                                  
*                                                                               
REPMOD   DS    0H                                                               
*                                                                               
         MVI   MEDBRK,0                                                         
         XC    DMCB(12),DMCB                                                    
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD                                     
         B     REP3                                                             
SORTCARD DC    CL80'SORT FIELDS=(1,40,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=40'                                    
*                                                                               
REP3     DS    0H                                                               
*                                                                               
         MVI   PPNAM,C'Y'          PRINT PKG NAME SWITCH                        
         CLI   NETOPT,C'S'                                                      
         BE    *+8                                                              
         MVI   PPNAM,C'N'                                                       
*                                                                               
         CLI   EQVOPT,X'FF'        EQUIVALENCE OPTION                           
         BE    *+10                                                             
         MVC   NBUSER2(1),EQVOPT                                                
*                                                                               
         MVI   NBDATA,C'U'         GET UNITS                                    
         OI    NBSPLOPT,X'80'      SPLIT IF NOT POL                             
         MVI   NBSELUOP,C'A'       ACTUAL SCHEDULE                              
         MVI   NBESTOPT,C'A'       USE ESTIMATED (DEFAULT)                      
         CLI   DEMOPT,C'A'                                                      
         BNE   PROCDAT                                                          
         MVI   NBACTOPT,C'Y'                                                    
         MVI   NBESTOPT,0                                                       
*                                                                               
PROCDAT  NETGO NSNETIO,DMCB,NETBLOCK   PROCESS DATE                             
         CLI   NBERROR,0                                                        
         BNE   PROCERR                                                          
         CLI   NBMODE,NBVALDAT                                                  
         BE    GOTDATE                                                          
         B     PROCDAT                                                          
*                                                                               
GOTDATE  DS    0H                                                               
         BAS   RE,INITMON          INITIALIZE MONTH (WEEK) LIST                 
         L     RE,=A(TABLES)                                                    
         A     RE,RELO                                                          
         LA    RF,TABLENDE                                                      
         XCEF                                                                   
*                                                                               
GETUNIT  NETGO NSNETIO,DMCB,NETBLOCK    NOW DO UNIT RECORDS                     
         CLI   NBERROR,0                                                        
         BNE   PROCERR                                                          
         CLI   NBMODE,NBREQLST     LAST ONE                                     
         BE    REP10                                                            
         CLI   NBMODE,NBPROCUN     IF A UNIT                                    
         BNE   GETUNIT                                                          
*                                  PUT TO SORTER                                
         CLI   AMSV,0                                                           
         BNE   *+16                                                             
         MVC   AMSV,NBACTAM                                                     
         MVC   CLISV,NBACTCLI                                                   
         LA    R4,MYWORK                                                        
         XC    MYWORK,MYWORK                                                    
         USING BINTBLD,R4                                                       
         CLI   MEDOPT,C'Y'                                                      
         BNE   *+10                                                             
         MVC   BTMEDIA,NBSTATYP    STATION TYPE                                 
         MVC   BTEST,NBACTEST                                                   
         MVC   BTNET1,NBACTNET                                                  
         CLI   NBPRD,0             IF UNALLOCATED, DON'T USE SPLPRN             
         BE    *+10                                                             
         MVC   BTSPLPRN,NBSPLPRN   SINCE =X'FF' 1ST TIME FROM NETVAL            
         CLI   NETOPT,C'S'                                                      
         BNE   *+10                                                             
         XC    BTNET1,BTNET1                                                    
         MVC   BTNET,NBACTNET                                                   
         MVC   BTPKG,NBPACK                                                     
         MVC   BTSPTLEN,NBLEN                                                   
         CLI   LENOPT,C'S'                                                      
         BNE   *+10                                                             
         MVC   BTLEN1,NBLEN                                                     
         CLI   PRDOPT,C'A'                                                      
         BNE   PUT3                                                             
         LA    R3,BTPRD1                                                        
         MVC   WORK(1),NBPRD                                                    
         CLI   WORK,0              IF UNALLOCATED                               
         BE    PUT2D                                                            
         CLC   =C'POL',REQPRD      SKIP NBSPLPRN SINCE NETVALUE                 
         BE    *+10                MAKES IT X'FF' AND THIS                      
         MVC   WORK(1),NBSPLPRN    IS PROBLEM WHEN CALLING NETVAL               
PUT2D    BAS   RE,GETPRD           OUTSIDE OF NETIO                             
PUT3     MVC   BTMONEY,NBASSIGN                                                 
         CLI   COSTOPT,C'A'        IS IT ACTUAL OPTION                          
         BNE   *+10                                                             
         MVC   BTMONEY,NBACTUAL                                                 
         CLI   DOLLOPT,C'Y'       BREAK ON DIFFERENT DOLLAR COST                
         BNE   *+10                                                             
         XC    BTMONEY,BTMONEY     NO, DON'T BREAK/SORT ON DOLLARS              
         L     R2,AIO                                                           
         USING NUKDKEY,R2                                                       
         MVC   BTDAY,NUKDDAY                                                    
         MVC   BTTIME,NUKDTIME                                                  
         MVC   BTPROG,NUKDPROG                                                  
         MVC   BTDSKADR,NBKEY+21                                                
         CLI   NETOPT,C'S'                                                      
         BE    PUT5                                                             
         XC    BTEST,BTEST                                                      
         XC    BTNET,BTNET                                                      
         XC    BTPKG,BTPKG                                                      
         B     PUT10                                                            
PUT5     DS    0H                                                               
PUT10    GOTO1 SORTER,DMCB,=C'PUT',(R4)                                         
         B     GETUNIT                                                          
         DROP  R2,R4                                                            
*                                                                               
REP10    DS    0H                                                               
         USING BINTBLD,R2                                                       
REP12    GOTO1 SORTER,DMCB,=C'GET'                                              
         MVI   LEVEL,4                                                          
         MVI   NEWPAGE,0                                                        
         CLI   DOLLOPT,C'Y'                                                     
         BNE   *+8                                                              
         MVI   LEVEL,0                                                          
         L     R2,4(R1)                                                         
         LTR   R2,R2                                                            
         BZ    REP20                                                            
         OC    PREVIOUS(7),PREVIOUS     FIRST TIME                              
         BNZ   REP13                                                            
         MVC   PRDSV,1(R2)              YES/SET PRODUCT                         
         MVC   PREVIOUS,0(R2)               SET PREVIOUS                        
*                                                                               
         CLI   MEDOPT,C'Y'                                                      
         BNE   REP12B                                                           
         MVC   MEDSV,0(R2)                  SET MEDIA                           
*                                                                               
REP12B   CLI   NETALL,C'Y'                                                      
         BNE   REP12C                                                           
         MVC   P+1(4),PREVIOUS+10                                               
         CLI   NETOPT,C'S'                                                      
         BE    *+10                                                             
         MVC   P+1(4),PREVIOUS+5                                                
         BAS   RE,WRITIT                                                        
*                                                                               
REP12C   BAS   RE,GETPAK           AND GET PACKAGE NAME                         
         MVC   PAKNAMSV,NBPAKNAM                                                
         BAS   RE,PRINTPNM                                                      
         B     REP17                                                            
*                                                                               
REP13    CLI   MEDOPT,C'Y'         MEDIA SORTING                                
         BNE   REP1300                                                          
         CLC   PREVIOUS(1),0(R2)                                                
         BE    REP1300                                                          
         BAS   RE,PRINTIT                                                       
         BAS   RE,PKGTOT                                                        
         CLI   PRDOPT,C'A'         PRODUCT ALSO                                 
         BNE   *+12                                                             
         MVI   PRDBRK,C'Y'         YES/DO PRODUCT                               
         BAS   RE,PKGTOT                                                        
         MVI   PRDBRK,0                                                         
         MVI   SPACING,2                                                        
         BAS   RE,WRITIT                                                        
         MVI   MEDBRK,C'Y'         SET MED BREAK                                
         BAS   RE,PKGTOT                                                        
         MVI   MEDBRK,0                                                         
         MVC   MEDSV,0(R2)                                                      
         MVC   PRDSV,1(R2)                                                      
         MVI   FORCEHED,C'Y'                                                    
         B     REP13D                                                           
*                                                                               
REP1300  CLI   PRDOPT,C'A'         PRODUCT SORTING                              
         BNE   REP13A                                                           
         CLC   PREVIOUS+1(3),1(R2)                                              
         BE    REP13A                                                           
         BAS   RE,PRINTIT                                                       
         BAS   RE,PKGTOT                                                        
         MVI   PRDBRK,C'Y'                                                      
         BAS   RE,PKGTOT                                                        
         MVI   PRDBRK,0                                                         
         MVC   PRDSV,1(R2)                                                      
         MVI   FORCEHED,C'Y'                                                    
         B     REP13D                                                           
*                                                                               
REP13A   CLI   NETOPT,C'S'                                                      
         BE    REP13B                                                           
         CLC   PREVIOUS+5(4),5(R2)       NETWORK SORTING                        
         BE    REP14                                                            
         B     REP13C                                                           
*                                                                               
REP13B   CLC   PREVIOUS+9(6),9(R2)       EST/NET/PKG                            
         BE    REP14                                                            
*                                                                               
REP13C   BAS   RE,PRINTIT                                                       
         BAS   RE,PKGTOT                                                        
         MVI   SPACING,2           SKIP LINES BEFORE                            
         BAS   RE,WRITIT                                                        
*                                                                               
REP13D   CLI   NETALL,C'Y'                                                      
         BNE   REP13E                                                           
         MVC   P+1(4),10(R2)                                                    
         CLI   NETOPT,C'S'                                                      
         BE    *+10                                                             
         MVC   P+1(4),5(R2)                                                     
         BAS   RE,WRITIT                                                        
REP13E   BAS   RE,GETPAK                                                        
         MVC   PAKNAMSV,NBPAKNAM                                                
         BAS   RE,PRINTPNM                                                      
         MVC   PREVIOUS,0(R2)                                                   
         B     REP17                                                            
*                                                                               
REP14    CLC   BTPROG,PREVIOUS+17      PROGRAM                                  
         BE    REP14B                                                           
         B     REP15                                                            
REP14B   CLC   BTMONEY,PREVIOUS+24     DOLLAR AMTS EQUAL                        
         BE    REP14D                                                           
         MVI   LEVEL,4                                                          
         B     REP15                                                            
REP14D   OC    OPTIONS,OPTIONS         IS IT OPTIONS=DATE/PROD                  
         BZ    REP17                                                            
         CLC   PREVIOUS+23(1),23(R2)   SPT LEN                                  
         BE    REP17                                                            
REP15    BAS   RE,PRINTIT                                                       
         MVC   PREVIOUS,0(R2)                                                   
REP17    DS    0H                                                               
         BAS   RE,GETUNTS                                                       
         BAS   RE,POST                                                          
         MVC   PRDSV,BTPRD1                                                     
         B     REP12                                                            
REP20    DS    0H                                                               
         BAS   RE,PRINTIT          YES/CLEAR BUFF                               
         BAS   RE,PKGTOT           AND DO PKG                                   
         CLC   NBSELPRD,=C'ALL'                                                 
         BNE   REP20B                                                           
         MVI   PRDBRK,C'Y'                                                      
         BAS   RE,PKGTOT                                                        
         MVI   SPACING,3                                                        
         BAS   RE,WRITIT                                                        
REP20B   CLI   MEDOPT,C'Y'                                                      
         BNE   REP20C                                                           
         MVI   MEDBRK,C'Y'                                                      
         BAS   RE,PKGTOT                                                        
         MVI   SPACING,3                                                        
         BAS   RE,WRITIT                                                        
REP20C   MVI   FORCEHED,C'Y'       PRINT OVERALL TOTS ON SEP PAGE               
         MVI   PRDOPT,0            DONT PRINT PRD/MED ON TOTALS                 
         MVI   MEDOPT,0                                                         
         BAS   RE,SUBS             AND DO SUBS                                  
         BAS   RE,TOTS             AND DO TOTALS                                
         GOTO1 SORTER,DMCB,=C'END' '                                            
         B     XIT                                                              
*                                                                               
PRINTPNM NTR1                                                                   
         CLI   PPNAM,C'Y'                                                       
         BNE   XIT                                                              
         LA    R2,P                                                             
         USING PLINED,R2                                                        
         MVC   PLPROG,PAKNAMSV                                                  
         BAS   RE,WRITIT                                                        
         MVC   PAKNAMSV,NBPAKNAM                                                
         B     XIT                                                              
         DROP  R2                                                               
*                                                                               
PROCERR  DC    H'0'                                                             
         EJECT                                                                  
****************************************************************                
*  BUILD THE MONTH LIST. SET VALUES BASED ON CONTROLS.                          
*  IF WEEKS SPECIFIED, BUT SPAN IS GREATER THAN MONLIST ALLOWS, USE             
*      MONTHS INSTEAD.                                                          
*                                                                               
*  OUTPUTS:  MONLIST - LIST OF BEGIN-END DATE SETS FOR PERIOD.                  
*            NUMMONS - NUMBER OF USABLE DATE SETS IN LIST                       
*   LOCALS:                                                                     
*        R3 - A(FIRST DATE SET IN LIST)                                         
*        R4 - CURRENT NUMBER OF USABLE DATE SETS IN LIST                        
*                                                                               
INITMON  NTR1                                                                   
         MVI   PERTYPE,C'W'        WEEK OR MONTH                                
         CLI   TYPOPT,C'M'                                                      
         BNE   *+8                                                              
         MVI   PERTYPE,C'M'                                                     
         MVI   PERTYPE+1,1         SET SO MONS USED IF TOO MANY WKS             
         MVI   PERTYPE+2,0         DONT USE QUARTERS                            
         LA    R4,MAXMONTS                                                      
         ST    R4,NUMMONS          MAX NUMBER OF MONTHS                         
         NETGO NVWKLST,DMCB,NUMMONS,MONLIST,PERTYPE                             
IMXIT    B     XIT                                                              
*                                                                               
         SPACE 2                                                                
GETUNTS  NTR1                                                                   
*  R2 POINTS TO RECORD IN BINSRCH                                               
*        XC    KEY,KEY           SETSURV IN NETIO ONLY FOR NBFGET               
*        MVC   KEY+21(4),28(R2)  5/13/92                                        
*        MVI   NBFUNCT,NBFVAL                                                   
*        GOTO1 DATAMGR,DMCB,=C'GETREC',=C'UNTFIL',28(R2),NBAIO,                 
*              (0,DMWORK)                                                       
                                                                                
                                                                                
         XC    NBKEY,NBKEY                                                      
         MVC   NBKEY+21(4),28(R2)                                               
         MVI   NBFUNCT,NBFGET                                                   
         MVI   NBSPLPRN,0                 PXZ 17OCT95                           
         NETGO NSNETIO,DMCB,NETBLOCK                                            
****     GOTO1 =V(PRNTBL),DMCB,=C'NETIO',NBAIO,C'DUMP',40,=C'1D'                
                                                                                
SKIPIT0  CLC   =C'POL',REQPRD                                                   
         BE    GETUNTX                                                          
         MVI   NBWHERE,0                                                        
         MVI   NBSPLPRN,0                                                       
         MVC   NBSPLPRN,NBPRD                                                   
         LA    R4,NETBLOCK                                                      
         GOTO1 NBNETVAL,DMCB,(R4)                                               
         CLC   NBSPLPRN,32(R2)     IS IT PROD WE WANT                           
         BE    GETUNTX                                                          
         MVC   NBSPLPRN,NBPRD2                                                  
         GOTO1 NBNETVAL,DMCB,(R4)                                               
         CLC   NBSPLPRN,32(R2)                                                  
         BE    GETUNTX                                                          
         MVC   WORK(40),0(R2)                                                   
         DC    H'0'                                                             
GETUNTX  DS    0H                  GOTO NETACC TO HANDLE RATE TYPES             
         GOTO1 =V(NETNET),DMCB,(NBRTTYPE,NBASSIGN),DUB,RR=RELO                  
         MVC   NBASSIGN,DUB                                                     
         GOTO1 =V(NETNET),DMCB,(NBRTTYPE,NBACTUAL),DUB,RR=RELO                  
         MVC   NBACTUAL,DUB                                                     
         B     XIT                                                              
*                                                                               
GETPAK   NTR1                                                                   
         CLI   PPNAM,C'Y'                                                       
         BNE   XIT                                                              
*  R2 POINTS TO RECORD FROM SORTER                                              
         USING BINTBLD,R2                                                       
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING NPRECD,R3                                                        
         MVI   NPKTYPE,X'02'                                                    
         MVC   NPKAM,AMSV                                                       
         MVC   NPKCLT,CLISV                                                     
         MVC   NPKNET,BTNET                                                     
         MVC   NPKEST,BTEST                                                     
         MVC   NPKPACK,BTPKG                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(20),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   NBKEY,KEY                                                        
         MVI   NBFUNCT,NBFGET                                                   
         NETGO NSNETIO,DMCB,NETBLOCK                                            
         B     XIT                                                              
         DROP  R2,R3                                                            
*                                                                               
         EJECT                                                                  
************************************                                            
*  POST - PROCESS A UNIT RECORD                                                 
*                                                                               
POST     NTR1                                                                   
         L     R3,=A(TEMPSTR)                                                   
         A     R3,RELO                                                          
         USING TEMPD,R3                                                         
         LA    R4,6                MAXIMUM NUMBER OF SPOT LENGTHS               
*                                                                               
POST2    OC    3(1,R3),3(R3)                                                    
         BZ    POST3                                                            
         CLC   NBLEN,3(R3)        CHK LENGTH                                    
         BE    POST3                                                            
         LA    R3,TPLENE(R3)                                                    
         BCT   R4,POST2                                                         
         DC    H'0'                CRASH,TOO MANY SPOT LENGTHS                  
*                                                                               
POST3    MVC   3(1,R3),NBLEN                                                    
         L     R1,TPUNITS                                                       
         LA    R1,1(R1)                                                         
         ST    R1,TPUNITS                                                       
         L     R1,NBASSIGN                                                      
         CLI   COSTOPT,C'A'                                                     
         BNE   *+8                                                              
         L     R1,NBACTUAL                                                      
         CLI   DOLOPT,C'Y'                                                      
         BNE   POST3A                                                           
*--ROUND TO NEAREST DOLLAR                                                      
         LR    RE,R1                                                            
         SR    RF,RF                                                            
         SRDA  RE,31                                                            
         D     RE,=F'100'                                                       
         LTR   RF,RF                                                            
         BM    *+8                                                              
         AH    RF,=H'1'                                                         
         SRA   RF,1                                                             
         LR    R1,RF                                                            
POST3A   A     R1,TPDOLS                                                        
         ST    R1,TPDOLS                                                        
         L     R1,TPRTG                                                         
         MVC   HALF,NDESTDEM+2     ADD TO TOTAL                                 
         CLI   DEMOPT,C'A'                                                      
         BNE   *+10                                                             
         MVC   HALF,NDACTDEM+2                                                  
         AH    R1,HALF                                                          
         ST    R1,TPRTG                                                         
         L     R2,NUMMONS                                                       
         LA    R4,MONLIST                                                       
POST4    CLC   NBACTDAT,0(R4)                                                   
         BL    *+14                                                             
         CLC   NBACTDAT,2(R4)                                                   
         BNH   POST6                                                            
         LA    R4,4(R4)            BUMP MONLIST                                 
         LA    R3,8(R3)            BUMP TEMP                                    
         BCT   R2,POST4                                                         
         B     POSTX           COULD BE UNITS OUTSIDE ESTIMATE DATES            
POST6    LA    R2,TPBUKTS                                                       
         L     R1,4(R2)            ADD UNIT                                     
         LA    R1,1(R1)                                                         
         ST    R1,4(R2)                                                         
         L     R1,0(R2)                                                         
         MVC   HALF,NDESTDEM+2     ADD GRP                                      
         AH    R1,HALF                                                          
         ST    R1,0(R2)                                                         
*                                                                               
POSTX    TM    OPTIONS,X'03'       IS IT DATES/PROD OPTION                      
         BZ    *+8                                                              
         BAS   RE,POSTOPT                                                       
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
*                                                                               
POSTOPT  NTR1                                                                   
         TM    OPTIONS,X'01'       IS IT DATES                                  
         BZ    POPT10                                                           
         L     R2,NUMMONS                                                       
         LA    R4,MONLIST                                                       
         LA    R3,DATETBL                                                       
POPT2    CLC   NBACTDAT,0(R4)                                                   
         BL    *+14                                                             
         CLC   NBACTDAT,2(R4)                                                   
         BNH   POPT4                                                            
         LA    R4,4(R4)            BUMP MONLIST                                 
         LA    R3,62(R3)           BUMP DATETBL                                 
         BCT   R2,POPT2                                                         
         B     POPTX               COULD BE DATES OUTSIDE ESTIMATE              
POPT4    CLI   0(R3),0                                                          
         BE    POPT6                                                            
         LA    R3,2(R3)                                                         
         B     POPT4                                                            
POPT6    MVC   0(2,R3),NBACTDAT                                                 
         B     POPTX                                                            
*                                                                               
POPT10   TM    OPTIONS,X'02' PRODUCTS                                           
         BZ    POPTX                                                            
         L     R2,NUMMONS                                                       
         LA    R4,MONLIST                                                       
         LA    R3,DATETBL                                                       
         A     R3,=F'1000'         POINT TO PROD TABLE                          
POPT12   CLC   NBACTDAT,0(R4)                                                   
         BL    *+14                                                             
         CLC   NBACTDAT,2(R4)                                                   
         BNH   POPT14                                                           
         LA    R4,4(R4)             BUMP MONLIST                                
         LA    R3,186(R3)           BUMP PRODTABLE                              
         BCT   R2,POPT12                                                        
         B     POPTX               COULD BE DATES OUTSIDE ESTIMATE              
POPT14   CLI   0(R3),0                                                          
         BE    POPT16                                                           
         LA    R3,6(R3)                                                         
         B     POPT14                                                           
POPT16   CLC   =C'POL',REQPRD                                                   
         BE    POPT16C                                                          
         MVC   WORK(1),NBSPLPRN                                                 
         BAS   RE,GETPRD                                                        
         B     POPTX                                                            
POPT16C  MVC   WORK(1),NBPRD                                                    
         BAS   RE,GETPRD                                                        
         CLI   NBPRD2,0                                                         
         BE    POPTX                                                            
         LA    R3,3(R3)                                                         
         MVC   WORK(1),NBPRD2                                                   
         BAS   RE,GETPRD                                                        
         B     POPTX                                                            
*                                                                               
POPTX    B     XIT                                                              
         EJECT                                                                  
         SPACE                                                                  
*  R3 POINTS TO OUTPUT AREA, WORK HAS 1 BYTE PROD CODE                          
*                                                                               
GETPRD   NTR1                                                                   
         L     R2,ACLIST                                                        
PRD2     CLI   0(R2),0                                                          
         BNE   PRD4                                                             
         MVC   0(3,R3),=C'UNA'     UNDEFINED                                    
         B     PRDX                                                             
PRD4     CLC   3(1,R2),WORK                                                     
         BE    *+12                                                             
         LA    R2,4(R2)                                                         
         B     PRD2                                                             
         MVC   0(3,R3),0(R2)                                                    
PRDX     B     XIT                                                              
*                                                                               
*********************************                                               
         EJECT                                                                  
*                                                                               
PRINTIT  NTR1                                                                   
         LA    R2,P                                                             
         USING PLINED,R2                                                        
         L     R3,=A(TEMPSTR)                                                   
         A     R3,RELO                                                          
         USING TEMPD,R3                                                         
         OC    0(4,R3),0(R3)                                                    
         BZ    XIT                                                              
*        CLC   PVPROG,NBACTPRG                                                  
         OC    NBPROGNM,=16X'40'                                                
         CLC   PVPROG,NBPROGNM                                                  
         BE    PRT3                                                             
*        MVC   PVPROG,NBACTPRG                                                  
         MVC   PVPROG,NBPROGNM                                                  
         MVC   PLPROG,NBPROGNM                                                  
PRT3     EDIT  (B4,TPRTG),(5,PLRTG),1                                           
         EDIT  (B4,TPUNITS),(3,PLUNT)                                           
         EDIT  (B4,TPSEC),(3,PLLEN)                                             
         CLI   LEVEL,4                                                          
         BE    PRT3C                                                            
         CLI   DOLOPT,C'Y'                                                      
         BE    PRT3B                                                            
         LA    R4,PLCOST-1                                                      
         EDIT  (B4,TPDOLS),(11,0(R4)),2                                         
******   EDIT  (B4,TPDOLS),(10,PLCOST),2                                        
         B     PRT3E                                                            
PRT3B    EDIT  (B4,TPDOLS),(10,PLCOST)                                          
         B     PRT3E                                                            
PRT3C    L     R5,NBASSIGN                                                      
         CLI   COSTOPT,C'A'                                                     
         BNE   *+8                                                              
         L     R5,NBACTUAL                                                      
         CLI   DOLOPT,C'Y'                                                      
         BE    PRT3D                                                            
         LA    R4,PLCOST-1             PXZ CHANGE                               
         EDIT  (R5),(11,0(R4)),2                                                
****     EDIT  (R5),(10,PLCOST),2                                               
***      LA    R5,PLCOST+132                                                    
         LA    R5,PLCOST+131                                                    
***      EDIT  (B4,TPDOLS),(10,0(R5)),2                                         
         EDIT  (B4,TPDOLS),(11,0(R5)),2                                         
         B     PRT3E                                                            
*                                                                               
*--ROUND TO NEAREST DOLLAR                                                      
PRT3D    LR    RE,R5                                                            
         SR    RF,RF                                                            
         SRDA  RE,31                                                            
         D     RE,=F'100'                                                       
         LTR   RF,RF                                                            
         BM    *+8                                                              
         AH    RF,=H'1'                                                         
         SRA   RF,1                                                             
         LR    R5,RF                                                            
         EDIT  (R5),(10,PLCOST)                                                 
         LA    R5,PLCOST+132                                                    
         EDIT  (B4,TPDOLS),(10,0(R5))                                           
*                                                                               
PRT3E    TM    OPTIONS,X'03'                                                    
         BZ    PRT4                                                             
         BAS   RE,PRTOPTS                                                       
         B     PRTROLL                                                          
*                                                                               
PRT4     LA    R5,TPBUKTS                                                       
         BCTR  R2,0                                                             
         CLI   UNTOPT,C'Y'                                                      
         BNE   *+8                                                              
         LA    R5,4(R5)                                                         
         L     R4,NUMMONS                                                       
PRT5     EDIT  (B4,0(R5)),(5,PLDATA),1                                          
         CLI   UNTOPT,C'Y'                                                      
         BNE   PRT5B                                                            
         EDIT  (B4,0(R5)),(5,PLDATA)                                            
PRT5B    OC    0(4,R5),0(R5)                                                    
         BNZ   *+10                                                             
         MVC   PLDATA(5),=C'  -  '                                              
         LA    R5,8(R5)                                                         
         LA    R2,5(R2)                                                         
         BCT   R4,PRT5                                                          
         BAS   RE,WRITIT                                                        
         LA    R3,TPLENE(R3)                                                    
         LA    R2,P                                                             
         OC    0(4,R3),0(R3)                                                    
         BNZ   PRT3                                                             
*                                                                               
PRTROLL  DS    0H                                                               
         L     R2,=A(TEMPSTR)          ROLL TO SUBTOTS                          
         A     R2,RELO                                                          
PRT6     L     R3,=A(SUBTOTS)                                                   
         A     R3,RELO                                                          
         LA    R4,36                                                            
         LA    R5,6                                                             
PRT6A    OC    0(4,R3),0(R3)                                                    
         BZ    PRT6C                                                            
         CLC   0(4,R3),0(R2)                                                    
         BE    PRT6C                                                            
         LA    R3,TPLENE(R3)                                                    
         BCT   R5,PRT6A                                                         
         DC    H'0'                SHOULD NEVER GET HERE                        
PRT6C    MVC   0(4,R3),0(R2)       SET SPOT LENGTH                              
         B     PRT7C                                                            
PRT7     L     R1,0(R2)                                                         
         A     R1,0(R3)                                                         
         ST    R1,0(R3)                                                         
PRT7C    LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R4,PRT7                                                          
         OC    0(4,R2),0(R2)                                                    
         BNZ   PRT6                                                             
*                                                                               
         L     R2,=A(TEMPSTR)          ROLL TO GRANTOTS                         
         A     R2,RELO                                                          
PRT8     L     R3,=A(GRANTOTS)                                                  
         A     R3,RELO                                                          
         LA    R2,4(R2)            BUMP PAST SECS LENGTH                        
         LA    R4,35                                                            
PRT9     L     R1,0(R2)                                                         
         A     R1,0(R3)                                                         
         ST    R1,0(R3)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R4,PRT9                                                          
         OC    0(4,R2),0(R2)                                                    
         BNZ   PRT8                                                             
*                                                                               
         L     R2,=A(TEMPSTR)          ROLL TO PKG TOTAL                        
         A     R2,RELO                                                          
PRT8A    L     R3,=A(PKGTEMP)                                                   
         A     R3,RELO                                                          
         LA    R2,4(R2)            BUMP PAST SECS LENGTH                        
         LA    R4,35                                                            
PRT9A    L     R1,0(R2)                                                         
         A     R1,0(R3)                                                         
         ST    R1,0(R3)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R4,PRT9A                                                         
         OC    0(4,R2),0(R2)                                                    
         BNZ   PRT8A                                                            
*                                                                               
         CLC   NBSELPRD,=C'ALL'        IF PROD=ALL                              
         BNE   PRT11                                                            
         L     R2,=A(TEMPSTR)          ROLL TO PRD TOTAL                        
         A     R2,RELO                                                          
PRT10    L     R3,=A(PRDTEMP)                                                   
         A     R3,RELO                                                          
         LA    R2,4(R2)            BUMP PAST SECS LENGTH                        
         LA    R4,35                                                            
PRT10A   L     R1,0(R2)                                                         
         A     R1,0(R3)                                                         
         ST    R1,0(R3)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R4,PRT10A                                                        
         OC    0(4,R2),0(R2)                                                    
         BNZ   PRT10                                                            
*                                                                               
PRT11    CLI   MEDOPT,C'Y'             IF MEDIA OPTION                          
         BNE   PRT12                                                            
         L     R2,=A(TEMPSTR)          ROLL TO MED TOTAL                        
         A     R2,RELO                                                          
PRT11A   L     R3,=A(MEDTEMP)                                                   
         A     R3,RELO                                                          
         LA    R2,4(R2)            BUMP PAST SECS LENGTH                        
         LA    R4,35                                                            
PRT11B   L     R1,0(R2)                                                         
         A     R1,0(R3)                                                         
         ST    R1,0(R3)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R4,PRT11B                                                        
         OC    0(4,R2),0(R2)                                                    
         BNZ   PRT11A                                                           
*                                                                               
PRT12    L     R4,=A(TEMPSTR)          CLEAR TEMPSTR                            
         A     R4,RELO                                                          
PRT14    XC    0(TPLENE,R4),0(R4)                                               
         LA    R4,TPLENE(R4)                                                    
         OC    0(4,R4),0(R4)                                                    
         BNZ   PRT14                                                            
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
PRTOPTS  NTR1                                                                   
         TM    OPTIONS,X'01'       DATES                                        
         BZ    POP10                                                            
         MVI   ISITHIT,0                                                        
         LA    R3,31                                                            
         LA    R5,DATETBL                                                       
POP1     ST    R5,DATPOINT                                                      
         L     R4,NUMMONS                                                       
POP2     CLI   0(R5),0                                                          
         BE    POP2C                                                            
         MVI   ISITHIT,1                                                        
         GOTO1 DATCON,DMCB,(2,0(R5)),(4,WORK)                                   
         MVC   PLDATA+1(2),WORK+3                                               
POP2C    LA    R2,5(R2)                                                         
         LA    R5,62(R5)                                                        
         BCT   R4,POP2                                                          
         CLI   ISITHIT,1           DID WE PUT DATA TO PRINT LINE                
         BNE   POP5                                                             
         CLI   PLINUM,2            YES/IS IT P2                                 
         BE    POP3                                                             
         MVI   PLINUM,2            NO/SET UP FOR P2                             
         B     POP5                                                             
POP3     BAS   RE,WRITIT                                                        
         MVI   ISITHIT,0                                                        
         MVI   PLINUM,1                                                         
POP5     L     R5,DATPOINT                                                      
         LA    R5,2(R5)                                                         
         LA    R2,P                                                             
         CLI   PLINUM,2                                                         
         BNE   *+8                                                              
         LA    R2,132(R2)                                                       
         BCT   R3,POP1                                                          
         B     POPX                                                             
*                                                                               
POP10    TM    OPTIONS,X'02'       PRODUCTS                                     
         BZ    POPX                                                             
         MVI   ISITHIT,0                                                        
         LA    R3,31                                                            
         LA    R5,DATETBL                                                       
         LA    R5,1000(R5)                                                      
POP12    ST    R5,DATPOINT                                                      
         L     R4,NUMMONS                                                       
POP14    CLI   0(R5),0                                                          
         BE    POP16                                                            
         MVI   ISITHIT,1                                                        
         MVC   PLDATA(3),0(R5)                                                  
         CLI   3(R5),0             IS THERE A PIGGY                             
         BE    POP16                                                            
         MVI   PLDATA+3,C'/'       YES/SET PIGGY MARK                           
         MVC   PLDATA+132(3),3(R5) MOVE IN PIGGY                                
         MVI   PLINUM,2                                                         
POP16    LA    R2,5(R2)                                                         
         LA    R5,186(R5)                                                       
         BCT   R4,POP14                                                         
         CLI   ISITHIT,1           DID WE PUT DATA TO PRINT LINE                
         BNE   POP17                                                            
         CLI   PLINUM,2            YES/IS IT P2                                 
         BE    POP16C                                                           
         MVI   PLINUM,2            NO/SO SET FOR P2                             
         B     POP17                                                            
POP16C   BAS   RE,WRITIT                                                        
         MVI   ISITHIT,0                                                        
         MVI   PLINUM,1                                                         
POP17    L     R5,DATPOINT                                                      
         LA    R5,6(R5)                                                         
         LA    R2,P                                                             
         CLI   PLINUM,2                                                         
         BNE   *+8                                                              
         LA    R2,132(R2)                                                       
         BCT   R3,POP12                                                         
*                                                                               
POPX     LA    RE,DATETBL                                                       
         L     RF,=F'4000'                                                      
         XCEF                                                                   
         B     XIT                                                              
         EJECT                                                                  
PKGTOT   NTR1                                                                   
         MVI   ALLOWLIN,10                                                      
         BAS   RE,WRITIT                                                        
         LA    R2,P                                                             
         USING PLINED,R2                                                        
         CLI   MEDBRK,C'Y'                                                      
         BNE   PKGTOT0                                                          
         L     R3,=A(MEDTEMP)                                                   
         A     R3,RELO                                                          
         MVI   P3+1,0                                                           
         MVC   PLPROG(11),=C'MEDIA TOTAL'                                       
         B     PKG2                                                             
*                                                                               
PKGTOT0  L     R3,=A(PKGTEMP)                                                   
         A     R3,RELO                                                          
         MVI   P3+1,0                                                           
         MVC   PLPROG(13),=C'PACKAGE TOTAL'                                     
         CLI   NETOPT,C'S'                                                      
         BE    *+10                                                             
         MVC   PLPROG(7),=C'NETWORK'                                            
         CLI   PRDBRK,C'Y'         IS IT PROD BREAK                             
         BNE   PKG2                                                             
         L     R3,=A(PRDTEMP)                                                   
         A     R3,RELO                                                          
         MVC   PLPROG(7),=C'PRODUCT'                                            
PKG2     MVC   PLPROG+396(10),=C'UNIT TOTAL'                                    
         LA    R4,PLPROG+15                                                     
         EDIT  (B4,4(R3)),(7,0(R4)),1  RTG                                      
         LA    R4,PLRTG+6                                                       
         EDIT  (B4,0(R3)),(4,0(R4))    UNITS                                    
         LA    R4,PLLEN+3                                                       
         CLI   DOLOPT,C'Y'                                                      
         BE    PKG2A                                                            
         EDIT  (B4,8(R3)),(12,0(R4)),2                                          
         B     PKG2B                                                            
PKG2A    EDIT  (B4,8(R3)),(12,0(R4))                                            
PKG2B    LA    R4,12(R3)                                                        
         LA    R2,PLCOST+10                                                     
         L     R5,NUMMONS                                                       
         MVI   SKIP,1                                                           
PKG6     CLI   SKIP,1              SET RTGS ON ALTERNATE LINES                  
         BE    PKG12                                                            
         MVI   SKIP,1                                                           
         OC    0(4,R4),0(R4)                                                    
         BNZ   *+12                                                             
         MVI   3(R2),C'-'                                                       
         B     PKG13                                                            
         EDIT  (B4,0(R4)),(6,0(R2)),1                                           
         B     PKG13                                                            
PKG12    OC    0(4,R4),0(R4)                                                    
         BNZ   PKG12B                                                           
         MVI   135(R2),C'-'                                                     
         MVI   SKIP,0                                                           
         B     PKG13                                                            
PKG12B   EDIT  (B4,0(R4)),(6,132(R2)),1                                         
         MVI   SKIP,0                                                           
PKG13    OC    4(4,R4),4(R4)                                                    
         BNZ   *+12                                                             
         MVI   399(R2),C'-'                                                     
         B     PKG13C                                                           
         EDIT  (B4,4(R4)),(5,396(R2))                                           
PKG13C   LA    R4,8(R4)                                                         
         LA    R2,5(R2)                                                         
         BCT   R5,PKG6                                                          
         BAS   RE,WRITIT                                                        
         CLI   PRDBRK,C'Y'        IS IT PRODUCT BREAKS                          
         BE    PKG15C                                                           
         CLI   MEDBRK,C'Y'        IS IT MED     BREAKS                          
         BE    PKG15C                                                           
         LA    R3,P                DRAW A LINE AFTER PACKAGE                    
         BCTR  R2,0                                                             
         SR    R2,R3                                                            
         LA    R3,P                                                             
PKG15    MVI   1(R3),C'-'                                                       
         LA    R3,1(R3)                                                         
         BCT   R2,PKG15                                                         
         MVI   SPACING,2                                                        
         BAS   RE,WRITIT                                                        
PKG15C   L     R2,=A(PKGTEMP)          CLEAR PKGTEMP                            
         A     R2,RELO                                                          
         LA    R3,35                                                            
         XC    0(4,R2),0(R2)                                                    
         LA    R2,4(R2)                                                         
         BCT   R3,*-10                                                          
         CLI   PRDBRK,C'Y'         HAVE WE FINISHED A PRD BREAK                 
         BNE   PKG20                                                            
         L     R2,=A(PRDTEMP)      YES/CLEAR PRDTEMP                            
         A     R2,RELO                                                          
         LA    R3,35                                                            
         XC    0(4,R2),0(R2)                                                    
         LA    R2,4(R2)                                                         
         BCT   R3,*-10                                                          
PKG20    CLI   MEDBRK,C'Y'         HAVE WE FINISHED A MED BREAK                 
         BNE   PKGXIT                                                           
         L     R2,=A(MEDTEMP)      YES/CLEAR MEDTEMP                            
         A     R2,RELO                                                          
         LA    R3,35                                                            
         XC    0(4,R2),0(R2)                                                    
         LA    R2,4(R2)                                                         
         BCT   R3,*-10                                                          
PKGXIT   B     XIT                                                              
         EJECT                                                                  
*                                                                               
SUBS     NTR1                  ** SUB TOTALS **                                 
         BAS   RE,WRITIT                                                        
         L     R3,=A(SUBTOTS)                                                   
         A     R3,RELO                                                          
         LA    R2,P                                                             
         USING PLINED,R2                                                        
         USING TEMPD,R3                                                         
         MVC   PLPROG(15),=C'** SUB TOTAL **'                                   
SUB2     LA    R4,PLPROG+15                                                     
         EDIT  (B4,TPRTG),(7,0(R4)),1                                           
         LA    R4,PLRTG+5                                                       
         EDIT  (B4,TPUNITS),(5,0(R4))                                           
         LA    R4,PLLEN                                                         
         EDIT  (B4,TPSEC),(3,0(R4))                                             
         LA    R4,PLLEN+3                                                       
         CLI   DOLOPT,C'Y'                                                      
         BE    SUB2A                                                            
         EDIT  (B4,TPDOLS),(12,0(R4)),2            DOLLARS                      
         B     SUB2B                                                            
SUB2A    EDIT  (B4,TPDOLS),(12,0(R4))              DOLLARS                      
SUB2B    LA    R2,PLCOST+10                                                     
         LA    R4,TPBUKTS          POINT R4 TO GRPS/UNIT BUCKETS                
         L     R5,NUMMONS                                                       
         MVI   SKIP,1                                                           
SUB6     CLI   SKIP,1              SET RTGS ON ALTERNATE LINES                  
         BE    SUB12                                                            
         MVI   SKIP,1                                                           
         OC    0(4,R4),0(R4)                                                    
         BNZ   *+12                                                             
         MVI   3(R2),C'-'                                                       
         B     SUB13                                                            
         EDIT  (B4,0(R4)),(6,0(R2)),1                                           
         B     SUB13                                                            
SUB12    OC    0(4,R4),0(R4)                                                    
         BNZ   SUB12B                                                           
         MVI   135(R2),C'-'                                                     
         MVI   SKIP,0                                                           
         B     SUB13                                                            
SUB12B   EDIT  (B4,0(R4)),(6,132(R2)),1                                         
         MVI   SKIP,0                                                           
SUB13    OC    4(4,R4),4(R4)                                                    
         BNZ   *+12                                                             
         MVI   266(R2),C'-'                                                     
         B     SUB13C                                                           
         EDIT  (B4,4(R4)),(5,264(R2))                                           
SUB13C   LA    R4,8(R4)                                                         
         LA    R2,5(R2)                                                         
         BCT   R5,SUB6                                                          
         MVI   SPACING,2                                                        
         BAS   RE,WRITIT                                                        
         LA    R2,P                                                             
         LA    R3,TPLENE(R3)                                                    
         OC    0(4,R3),0(R3)                                                    
         BNZ   SUB2                                                             
*                                                                               
         L     RE,=A(SUBTOTS)                CLEAR SUBTOTS                      
         A     RE,RELO                                                          
         LA    RF,SUBTLENE                                                      
         XCEF                                                                   
*                                                                               
SUBX     B     XIT                                                              
         EJECT                                                                  
*                                                                               
TOTS     NTR1                      GRANDTOTS                                    
         BAS   RE,WRITIT                                                        
         LA    R2,P                                                             
         USING PLINED,R2                                                        
         L     R3,=A(GRANTOTS)                                                  
         A     R3,RELO                                                          
         MVC   PLPROG(18),=C'** GRAND TOTALS **'                                
         LA    R4,PLPROG+15                                                     
         EDIT  (B4,4(R3)),(7,0(R4)),1  RTG                                      
         LA    R4,PLRTG+6                                                       
         EDIT  (B4,0(R3)),(4,0(R4))    UNITS                                    
         LA    R4,PLLEN+3                                                       
         CLI   DOLOPT,C'Y'                                                      
         BE    TOTS2                                                            
         EDIT  (B4,8(R3)),(12,0(R4)),2                                          
         B     TOTS2A                                                           
TOTS2    EDIT  (B4,8(R3)),(12,0(R4))                                            
TOTS2A   LA    R4,12(R3)                                                        
         LA    R2,PLCOST+10                                                     
         L     R5,NUMMONS                                                       
         MVI   SKIP,1                                                           
GDT6     CLI   SKIP,1              SET RTGS ON ALTERNATE LINES                  
         BE    GDT12                                                            
         MVI   SKIP,1                                                           
         OC    0(4,R4),0(R4)                                                    
         BNZ   *+12                                                             
         MVI   3(R2),C'-'                                                       
         B     GDT13                                                            
         EDIT  (B4,0(R4)),(6,0(R2)),1                                           
         B     GDT13                                                            
GDT12    OC    0(4,R4),0(R4)                                                    
         BNZ   GDT12B                                                           
         MVI   135(R2),C'-'                                                     
         MVI   SKIP,0                                                           
         B     GDT13                                                            
GDT12B   EDIT  (B4,0(R4)),(6,132(R2)),1                                         
         MVI   SKIP,0                                                           
GDT13    OC    4(4,R4),4(R4)                                                    
         BNZ   *+12                                                             
         MVI   266(R2),C'-'                                                     
         B     GDT13C                                                           
         EDIT  (B4,4(R4)),(5,264(R2))                                           
GDT13C   LA    R4,8(R4)                                                         
         LA    R2,5(R2)                                                         
         BCT   R5,GDT6                                                          
         BAS   RE,WRITIT                                                        
         B     XIT                                                              
         EJECT                                                                  
***********************************                                             
*              HEADLINE ROUTINES                                                
*                                                                               
HOOK     NTR1                                                                   
         L     R5,ATWA                                                          
         USING T320FFD,R5                                                       
         MVC   H4(6),=C'CLIENT'                                                 
         MVC   H4+10(3),SPLCLI                                                  
         MVC   H4+15(20),SPLCLIN                                                
         MVC   H5(7),=C'PRODUCT'                                                
         MVC   H5+10(6),SPLPRO                                                  
         MVC   H5+18(20),SPLPRON                                                
*        CLC   SPLPRO(3),=C'ALL'                                                
         CLI   PRDOPT,C'A'                                                      
         BNE   *+16                                                             
         MVC   H5+10(3),PRDSV                                                   
         XC    H5+18(20),H5+18                                                  
         MVC   H6(8),=C'ESTIMATE'                                               
         MVC   H6+10(8),SPLEST                                                  
         MVC   H6+20(20),SPLESTN                                                
         MVC   H7(7),=C'NETWORK'                                                
         MVC   H7+10(4),SPLNET                                                  
         CLI   H7+10,X'40'                                                      
         BH    *+10                                                             
         MVC   H7+10(3),=C'ALL'                                                 
         MVC   H6+56(7),=C'DAYPART'                                             
         MVC   H6+64(8),SPLDPTN                                                 
         CLI   H6+64,X'40'                                                      
         BH    *+10                                                             
         MVC   H6+64(3),=C'ALL'                                                 
         MVC   H5+97(6),=C'TARGET'                                              
         CLI   MEDOPT,C'Y'                                                      
         BNE   *+16                                                             
         MVC   H6+97(6),=C'MEDIA='                                              
         MVC   H6+103(1),MEDSV                                                  
         DROP  R5                                                               
*                                                                               
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'NTI'                                                   
         MVI   DBSELMED,C'N'                                                    
         MVI   DBSELSRC,C'N'                                                    
         SR    R3,R3                                                            
         NETGO NVDEMCON,DMCB,((R3),NDDEMBLK),DBLOCK,(7,WORK)                    
         MVC   H5+104(7),WORK                                                   
*                                                                               
         CLI   QTITLE,C'Y'                                                      
         BNE   *+14                                                             
         MVC   WORK(40),TITLE                                                   
         B     *+10                                                             
         MVC   WORK(40),=CL40'NETWORK FLOWCHART'                                
         GOTO1 CENTER,DMCB,WORK,40                                              
         MVC   HEAD1+43(40),WORK                                                
*                                                                               
HK1      DS    0H                                                               
*                                                                               
         LA    R2,H10                                                           
         USING PLINED,R2                                                        
         MVC   PLPROG(12),=C'PROGRAM NAME'                                      
         MVC   PLRTG+1(3),=C'RTG'                                               
         MVC   PLRTG+6(4),=C'UNTS'                                              
         MVC   PLLEN,=C'LEN'                                                    
         MVC   PLCOST+5(4),=C'COST'                                             
         LA    R2,H9                                                            
         MVC   PLRTG,=C'TOTAL'                                                  
         MVC   PLRTG+7(2),=C'NO'                                                
         MVC   PLCOST+4(6),=C'ACTUAL'                                           
         CLI   COSTOPT,C'A'                                                     
         BE    *+10                                                             
         MVC   PLCOST+2(8),=C'ASSIGNED'                                         
*                                                                               
         LA    R3,MONLIST                                                       
         L     R4,NUMMONS                                                       
         LA    R2,PLDATA                                                        
HK3      GOTO1 DATCON,DMCB,(2,0(R3)),(4,WORK)                                   
         MVC   0(3,R2),WORK                                                     
         MVC   133(2,R2),WORK+3                                                 
         LA    R2,5(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R4,HK3                                                           
*                                      BOXES                                    
         CLI   BOXOPT,C'N'                                                      
         BE    HOOKX                                                            
         L     R4,ABOX                                                          
         LTR   R4,R4                                                            
         BZ    HOOKX                                                            
         USING BOXD,R4                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
         MVC   BOXROWS,SPACES                                                   
         LA    R3,BOXROWS                                                       
         LA    R3,7(R3)                                                         
         MVI   0(R3),C'T'                                                       
         LA    R3,3(R3)                                                         
         MVI   0(R3),C'M'                                                       
         LA    R3,65(R3)                                                        
         MVI   0(R3),C'B'                                                       
         MVC   BOXCOLS,SPACES                                                   
         DROP  R2                                                               
         LA    R3,BOXCOLS                                                       
         USING PLINED,R3                                                        
         MVI   0(R3),C'L'                                                       
         LA    R3,PLDATA                                                        
         L     R5,NUMMONS                                                       
HK13     LA    R3,5(R3)                                                         
         BCT   R5,HK13                                                          
         MVI   0(R3),C'R'                                                       
HOOKX    B     XIT                                                              
         EJECT                                                                  
HEADING  SSPEC H2,1,REQUESTOR                                                   
         SSPEC H3,50,PERIOD                                                     
         SSPEC H1,98,AGYNAME                                                    
         SSPEC H2,98,AGYADD                                                     
         SSPEC H3,98,REPORT                                                     
         SSPEC H4,98,RUN                                                        
         SSPEC H5,125,PAGE                                                      
         DC    X'00'                                                            
         SPACE                                                                  
WRITIT   NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
*                                                                               
         GETEL R2,DATADISP,ELCODE                                               
*                                                                               
         EJECT                                                                  
               LTORG                                                            
         SPACE 3                                                                
DATETBL  DS    CL1000              2CL DATE X 31 DAYS X 16 COLUMNS              
PRODTBL  DS    CL3000              3CL PRODS X 2(PIGGY) X 31 X 16               
*                                                                               
TABLES   DS    0F                                                               
TEMPSTR  DS    216F                6 SEC LENS X 36F                             
         DS    F                                                                
SUBTOTS  DS    216F                                                             
         DS    F                                                                
PKGTEMP  DS    35F                 UNITS/HHRTG/$/16X2F(GRPS/UNITS)              
         DS    F                                                                
SUBTLENE EQU   *-SUBTOTS                                                        
GRANTOTS DS    35F                 UNITS/HHRTG/$/16X2F(GRPS/UNITS)              
         DS    F                                                                
PRDTEMP  DS    35F                 UNITS/HHRTG/$/16X2F(GRPS/UNITS)              
         DS    F                                                                
MEDTEMP  DS    35F                 UNITS/HHRTG/$/16X2F(GRPS/UNITS)              
         DS    F                                                                
TABLENDE EQU   *-TABLES                                                         
*                                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
MYWORKD  DSECT                                                                  
QTITLE   DS    CL1                 *** FROM EDIT                                
TITLE    DS    CL40                ***                                          
TYPOPT   DS    CL1                 ***                                          
NETALL   DS    CL1                 ***                                          
OPTIONS  DS    CL1                 *** X'01'=DATES, X'02'=PRODUCTS              
UNTOPT   DS    CL1                 ***  Y=UNITS                                 
BOXOPT   DS    CL1                 ***                                          
NETOPT   DS    CL1                 ***  A=NETWORK AS PRIMARY SORT               
COSTOPT  DS    CL1                 ***  A=ACTUAL COST                           
DOLLOPT  DS    CL1                 ***  Y=NO BREAK/SORT ON COST DOLLS           
DEMOPT   DS    CL1                 ***  A=ACTUAL DEMOS                          
LENOPT   DS    CL1                 ***  A=LENGTH AS PRIMARY SORT                
*                                       OR NUMER AS FILTER                      
PRDOPT   DS    CL1                 ***                                          
EQVOPT   DS    CL1                 ***                                          
DOLOPT   DS    CL1                 ***                                          
REQPRD   DS    CL3                 ***                                          
ACLIST   DS    F                   ***                                          
MEDOPT   DS    CL1                 ***                                          
*                                                                               
RELO     DS    F                                                                
NUMMONS  DS    F                   NUMBER OF MONTHS IN LIST                     
DATPOINT DS    F                                                                
PRDPOINT DS    F                                                                
PRDSV    DS    CL3                                                              
PPNAM    DS    CL1                                                              
NEWPAGE  DS    CL1                                                              
AMSV     DS    CL1                                                              
CLISV    DS    CL2                                                              
ISITHIT  DS    CL1                                                              
PLINUM   DS    CL1                                                              
PKGBRK   DS    CL1                                                              
SKIP     DS    CL1                                                              
LEVEL    DS    CL1                                                              
PRDBRK   DS    CL1                                                              
MEDBRK   DS    CL1                                                              
MEDSV    DS    CL1                                                              
MYWORK   DS    CL40                                                             
PREVIOUS DS    CL40                                                             
PVPROG   DS    CL16                                                             
PAKNAMSV DS    CL16                                                             
MAXMONTS EQU   16                  MAXIMUM NUMBER OF MONTHS (WEEKS)             
MONLIST  DS    CL(4*MAXMONTS)      MONTH (WEEK) LIST                            
PERTYPE  DS    CL3                 PERIOD TYPE AND CONTROL BYTES                
*                                                                               
TEMPD    DSECT                                                                  
TPSEC    DS    CL4                 SECONDS LENGTH                               
TPUNITS  DS    CL4                 TOTAL UNITS                                  
TPRTG    DS    CL4                 HH RATING                                    
TPDOLS   DS    CL4                 DOLLARS                                      
TPBUKTS  DS    CL128               2FX16 (GRP/UNITS)                            
TPLENE   EQU   *-TPSEC                                                          
*                                                                               
BINTBLD  DSECT                                                                  
BTMEDIA  DS    CL1                                                              
BTPRD1   DS    CL3                                                              
BTLEN1   DS    CL1                                                              
BTNET1   DS    CL4                                                              
BTEST    DS    CL1                                                              
BTNET    DS    CL4                                                              
BTPKG    DS    CL1                                                              
BTDAY    DS    CL1                                                              
BTTIME   DS    CL1                                                              
BTPROG   DS    CL6                                                              
BTSPTLEN DS    CL1                                                              
BTMONEY  DS    CL4                                                              
BTDSKADR DS    CL4                                                              
BTSPLPRN DS    CL1                                                              
BINRLENE EQU   *-BTEST                                                          
*                                                                               
PLINED   DSECT                                                                  
         DS    CL1                                                              
PLPROG   DS    CL16                                                             
         DS    CL1                                                              
PLRTG    DS    CL5                                                              
         DS    CL2                                                              
PLUNT    DS    CL3                                                              
         DS    CL1                                                              
PLLEN    DS    CL3                                                              
         DS    CL2                                                              
PLCOST   DS    CL10                                                             
         DS    CL2                                                              
PLDATA   DS    CL4                                                              
         DS    CL1                                                              
         DS    CL75                                                             
         PRINT OFF                                                              
       ++INCLUDE NETINCLS                                                       
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE NEGENPACK                                                      
       ++INCLUDE NEWRIFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRIECD                                                       
       ++INCLUDE DDBIGBOX                                                       
DEMBLK   DSECT                                                                  
       ++INCLUDE NETDEMOT                                                       
       ++INCLUDE DEDBLOCK                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003NEWRI3C   03/14/18'                                      
         END                                                                    
