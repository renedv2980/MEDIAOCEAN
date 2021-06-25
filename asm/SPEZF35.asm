*          DATA SET SPEZF35    AT LEVEL 173 AS OF 03/26/20                      
*PHASE T23035A                                                                  
*INCLUDE DLFLD                                                                  
*INCLUDE UNSCAN                                                                 
***********************************************************************         
*                                                                     *         
* TITLE: T23035 - EASI INVOICE SUMMARY REPORT                         *         
*                                                                     *         
* REGISTER USAGE:                                                     *         
* -------- -----                                                      *         
* R5 - EZWRKIOB                                                       *         
* R7 - EZBLOCKD                                                       *         
* R8 - SPOOLD                                                         *         
* R9 - SYSD                                                           *         
* RA - TWA                                                            *         
* RB - BASE (ONE BASE REGISTER USED THROUGHOUT THE PROGRAM)           *         
* RC - GEND                                                           *         
*                                                                     *         
***********************************************************************         
*                                                                               
***********************************************************************         
         TITLE 'T23035 - EASI INVOICE SUMMARY REPORT'                           
*                                                                               
T23035   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**3035**,RR=R2,CLEAR=YES                                       
         PRINT NOGEN                                                            
*                                                                               
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
*                                                                               
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R2,RELO                                                          
         ST    RC,SVRC                                                          
*                                                                               
         L     R7,AIO2                                                          
         USING EZBLOCKD,R7                                                      
*                                                                               
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
*                                                                               
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
*                                                                               
         LAY   R5,EZWRKIOB                                                      
         USING WRKIOB,R5                                                        
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BNE   *+12                                                             
         BRAS  RE,VKEY                                                          
         B     EQXIT                                                            
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BNE   *+12                                                             
         BRAS  RE,LIST                                                          
         B     EQXIT                                                            
*                                                                               
EQXIT    CR    RB,RB                                                            
         J     EXIT                                                             
NEQXIT   LTR   RB,RB                                                            
EXIT     XIT1                                                                   
*                                                                               
*                                                                               
*                                                                               
* VALIDATE KEY                                                                  
*                                                                               
VKEY     NTR1  BASE=*,LABEL=*                                                   
         XC    FILTRS(FILTRLQ),FILTRS                                           
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CGETFACT-COMFACSD(,RF)                                        
         GOTO1 (RF),DMCB,0                                                      
*                                                                               
         L     RE,DMCB             FAFACTS                                      
         USING FACTSD,RE                                                        
*                                                                               
         CLI   FAOVSYS,02          SPOT                                         
         BNE   *+12                                                             
         MVI   FILTSYS,C'S'                                                     
         B     VK05                                                             
*                                                                               
         CLI   FAOVSYS,03          NET                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   FILTSYS,C'N'                                                     
         DROP  RE                                                               
*                                                                               
* BY DEFAULT, FILTER UID AND AGENCY ARE SET TO LOGIN UID/AGY VALUES             
*                                                                               
VK05     DS    0H                                                               
         XC    LKAGYBLK,LKAGYBLK                                                
         MVC   LKAGYBID,TWAORIG                                                 
*                                                                               
         GOTO1 VLKAGY,LKAGYBLK     LOOK UP CHARACTER LOGIN ID                   
         JNE   *+2                                                              
*                                                                               
         CLC   LKAGYAGY,TWAAGY                                                  
         JNE   *+2                                                              
*                                                                               
         MVC   FILTAGY,TWAAGY      LOGIN AGENCY                                 
         MVC   FILTBUID,TWAORIG    LOGIN ID - BINARY                            
         MVC   FILTUID,LKAGYUID    LOOKED UP CHAR LOGIN ID                      
*                                                                               
* USER ID FIELD                                                                 
*                                                                               
VK10     DS    0H                                                               
         MVI   ERROR,INVALID                                                    
         LA    R2,CPEUIDH                                                       
*                                                                               
         CLI   5(R2),0             FIELD EMPTY?                                 
         BE    VK100               YES - USE DEFAULT UID, AGY                   
*                                                                               
         CLC   =C'ALL',8(R2)       FIRST CHECK FOR THE WORD "ALL"               
         BNE   VK20                                                             
         CLI   5(R2),3                                                          
         BNE   VK20                                                             
*                                                                               
* HAVE "ALL" UID FILTERE HERE                                                   
* NOTE: AGENCY FILTER REMAINS IN PLACE BY DEFAULT                               
         XC    FILTBUID,FILTBUID                                                
         XC    FILTUID,FILTUID                                                  
         B     VK100                                                            
*                                                                               
VK20     DS    0H                  SPECIFIC USER ID ENTERED                     
         XC    LKAGYBLK,LKAGYBLK                                                
         MVC   LKAGYUID,8(R2)                                                   
         GOTO1 VLKAGY,LKAGYBLK                                                  
         BNE   VKERR                                                            
*                                                                               
         CLC   TWAAGY,LKAGYAGY     LOOKED UP AGY MUST MATCH TWA AGY             
         BNE   VKERR                                                            
*                                                                               
         MVC   FILTUID,LKAGYUID    CHAR USER ID                                 
         MVC   FILTBUID,LKAGYBID   BINARY USER ID                               
*                                                                               
* MEDIA - TAKEN AS IS (ALPHA CHARACTER ONLY)                                    
*                                                                               
VK100    DS    0H                                                               
         MVI   ERROR,INVALID                                                    
         LA    R2,CPEMEDH                                                       
*                                                                               
         CLI   5(R2),0                                                          
         BE    VK300                                                            
*                                                                               
         CLI   5(R2),1                                                          
         BNE   VKERR                                                            
*                                                                               
         LA    R1,8(R2)                                                         
         BRAS  RE,ISALPHA                                                       
         BNE   VKERR                                                            
         MVC   FILTMED,8(R2)                                                    
*                                                                               
* STATION - TAKEN AS IS, CALL LETTERS NOT VALIDATED                             
*                                                                               
VK300    DS    0H                                                               
         MVI   ERROR,INVALID                                                    
         LA    R2,CPESTAH                                                       
*                                                                               
         CLI   5(R2),0                                                          
         BE    VK400                                                            
*                                                                               
         CLC   =C'ALL',CPESTA                                                   
         BNE   *+12                                                             
         CLI   5(R2),3                                                          
         BE    VK400                                                            
*                                                                               
         GOTO1 VREADSTA                                                         
         BNE   VKERR                                                            
*                                                                               
         MVC   FILTSTA,FLDRDSTA                                                 
*                                                                               
* SOURCE                                                                        
*                                                                               
VK400    DS    0H                                                               
         LA    R2,CPESRCH                                                       
         CLI   5(R2),0                                                          
         BE    VK500                                                            
*                                                                               
         MVC   FILTSRCE,CPESRC                                                  
         OC    FILTSRCE,SPACES                                                  
*                                                                               
* MOS                                                                           
*                                                                               
VK500    DS    0H                                                               
         MVI   ERROR,MISSING                                                    
         LA    R2,CPEMOSH                                                       
*                                                                               
         CLC   =C'SOON',CONWHEN    REQUESTING SOON?                             
         BNE   VK510                                                            
* IF RUNNING SOON, MOS IS A REQUIRED FIELD                                      
         CLI   5(R2),0                                                          
         BE    VKERR                                                            
         B     VK520                                                            
*                                                                               
VK510    DS    0H                  NOT RUNNING SOON HERE                        
         CLI   5(R2),0                                                          
         BE    VK600                                                            
*                                                                               
VK520    DS    0H                                                               
         MVI   ERROR,INVALID                                                    
         GOTO1 DATVAL,DMCB,(2,CPEMOS),WORK                                      
         CLC   =C'000000',WORK                                                  
         BE    VKERR                                                            
         GOTO1 DATCON,(R1),(0,WORK),(X'21',FILTMOS)                             
*                                                                               
* START DATE                                                                    
*                                                                               
VK600    DS    0H                                                               
         MVI   ERROR,INVALID                                                    
         LA    R2,CPEDATH                                                       
         CLI   5(R2),0                                                          
         BE    VK700                                                            
*                                                                               
         GOTO1 DATVAL,DMCB,(0,8(R2)),WORK                                       
         ICM   R3,15,DMCB                                                       
         BZ    VKERR                                                            
         GOTO1 DATCON,DMCB,(0,WORK),(2,FILTSD)                                  
*                                                                               
         CLM   R3,1,5(R2)          ONLY 1 DATE ENTERED                          
         BE    VK700                YES                                         
         LA    R3,1+8(R2,R3)                                                    
*                                                                               
         GOTO1 DATVAL,DMCB,(0,(R3)),WORK                                        
         OC    DMCB,DMCB                                                        
         BZ    VKERR                                                            
         GOTO1 DATCON,DMCB,(0,WORK),(2,FILTED)                                  
*                                                                               
VK700    DS    0H                                                               
         MVI   ERROR,INVALID                                                    
         LA    R2,CPESTTH                                                       
         CLI   5(R2),0             FIELD EMPTY?                                 
         BE    VK900                                                            
*                                                                               
         MVC   FILTSTAT,8(R2)                                                   
*                                                                               
         CLI   FILTSTAT,C'C'                                                    
         BE    VK900                                                            
         CLI   FILTSTAT,C'U'                                                    
         BE    VK900                                                            
         CLI   FILTSTAT,C'D'                                                    
         BE    VK900                                                            
         CLI   FILTSTAT,C'R'                                                    
         BNE   VKERR               INVALID STATUS                               
*                                                                               
VK900    DS    0H                                                               
         B     VKXIT                                                            
*                                                                               
VKERR    GOTO1 ERREX                                                            
*                                                                               
VKXIT    DS    0H                                                               
         J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
LIST     NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,GENINIT                                                       
         BRAS  RE,INITEZB                                                       
         BRAS  RE,INITDL                                                        
*                                                                               
* FORCE PAGE BREAK FOR DOWNLOADABLE REPORTS                                     
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 SPOOL,DMCB,(R8)     SKIP A PAGE AFTER REQ DETAILS                
*                                                                               
         ZAP   INVCOUNT,=P'0'                                                   
*                                                                               
* SET UP TO READ WORKER FILE INDICES                                            
*                                                                               
         XC    WRKEZKEY,WRKEZKEY                                                
         MVC   WRKEZUID,FILTBUID  WILL BE EMPTY, IF RUNNING FOR ALL IDS         
*                                                                               
         OC    FILTUID,FILTUID     RUNNING FOR ONE UID?                         
         BNZ   *+14                YES - DO NOT READ OTHER FILES                
         MVI   WRKINDS,WRKIWFNQ    INDICATE PASSING WORKER FILE NAME            
         MVC   WRKIFILE,FILTWKF                                                 
*                                                                               
LIST20   DS    0H                                                               
         OC    FILTUID,FILTUID     RUNNING FOR ONE UID?                         
         BNZ   *+10                                                             
         XC    WRKEZUID,WRKEZUID                                                
*                                                                               
         MVI   WRKIACTN,WRKIANDX                                                
         GOTO1 VWRKIO,WRKIOB                                                    
         CLI   WRKIERRS,0                                                       
         BE    LIST30                                                           
         CLI   WRKIERRS,WRKIEEOF                                                
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* REACHED EOF HERE                                                              
*                                                                               
         OC    FILTUID,FILTUID     RUNNING FOR ONE UID?                         
         BNZ   LIST200             YES - DO NOT READ OTHER FILES                
*                                                                               
         BRAS  RE,BUMPWKRF         BUMP TO NEXT WORKER FILE                     
         BNE   LIST200             ALREADY ON THE LAST ONE                      
*                                                                               
         XC    WRKEZKEY,WRKEZKEY   CLEAR OUT THE INDEX                          
         B     LIST20              READ FIRST INDEX FOR NEW FILE                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *             
* HERE WE HAVE A WORKER FILE INDEX IN WRKRINDX                                  
* DO THE WORKER INDEX FILTERING HERE                                            
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *             
LIST30   DS    0H                                                               
*        BRAS  RE,TRACEIND                                                      
*                                                                               
* APPLY FILTERS                                                                 
*                                                                               
         BRAS  RE,FILTIND                                                       
         BNE   LIST20                                                           
*                                                                               
* UPDATE EZBLOCK'S AGENCY AND BAGYMD                                            
         BRAS  RE,UPDTEZB                                                       
*                                                                               
         BRAS  RE,FAKEIND          POPULATE EZBLOCK INDEX VALUES                
*                                                                               
* READ WORKER FILE RECORDS FOR THIS BATCH                                       
*                                                                               
         MVI   WRKIACTN,WRKIAGET                                                
         GOTO1 VWRKIO,WRKIOB                                                    
         CLI   WRKIERRS,WRKIEEOF                                                
         BE    LIST20              EOF ON FIRST GET - READ NEXT INDEX           
         CLI   WRKIERRS,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OC    FILTSRCE,FILTSRCE                                                
         BZ    *+14                                                             
         CLC   FILTSRCE,WRKEZDSC+10                                             
         BNE   LIST20                                                           
*                                                                               
         MVC   SVSRCE,WRKEZDSC+10 SOURCE                                        
*                                                                               
         GOTO1 VEZMOD,DMCB,EZBLOCKD                                             
*                                                                               
         B     LIST20                                                           
*                                                                               
*                                                                               
*****************************************************************               
* DONE READING WORKER FILES                                                     
*****************************************************************               
LIST200  DS    0H                                                               
         LARL  R2,OUTFILE                                                       
         CLOSE ((R2),)                                                          
         FREEPOOL (R2)                                                          
*                                                                               
* SORT THE INVOICES                                                             
*                                                                               
         BRAS  RE,INITICE                                                       
*                                                                               
         LARL  R1,ICEPAR1          COUNT OPERATOR VIA PARAMETER LIST            
         LINK  EP=ICETOOL          CALL ICETOOL                                 
         LTR   RF,RF               SUCCESSFUL ICETOOL CALL?                     
         BZ    *+6                                                              
         DC    H'0'                NO                                           
*                                                                               
* OUTPUT THE INVOICES                                                           
*                                                                               
* PRINT HEADERS FIRST                                                           
* POINT AOUTREC AT COLUMN HEADER LIST                                           
         LARL  RE,HDRLIST                                                       
         ST    RE,AOUTREC                                                       
*                                                                               
         LARL  R1,HDRTAB           HEADER TABLE FOR PDENTRY                     
         BRAS  RE,PDENTRY                                                       
* RESTORE AOUTREC                                                               
         LA    RE,OUTREC                                                        
         ST    RE,AOUTREC                                                       
*                                                                               
         LARL  R2,OUTFILE2                                                      
         OPEN  ((R2),INPUT)                                                     
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
LIST220  LARL  R2,OUTFILE2                                                      
         GET   (R2),OUTREC                                                      
*                                                                               
         LARL  R1,COLTAB           COLUMN TABLE FOR PDENTRY                     
         BRAS  RE,PDENTRY                                                       
         B     LIST220                                                          
*                                                                               
OUTF2EOF DS    0H                                                               
         LARL  R2,OUTFILE2                                                      
         CLOSE ((R2),)                                                          
         FREEPOOL (R2)                                                          
*                                                                               
* END OF DOWNLOADABLE REPORT                                                    
*                                                                               
         MVC   P,SPACES                                                         
         MVI   DLCB+DLCBACT-DLCBD,C'R'                                          
         GOTO1 =V(DLFLD),DLCB                                                   
*                                                                               
* DYNAMICALLY UNALLOCATE THE TEMP DATASETS.                                     
         L     RF,ACOMFACS                                                      
         L     RF,CDYNALOC-COMFACSD(,RF)                                        
         GOTO1 (RF),DMCB,(C'U',=CL8'CTL1CNTL'),0                                
         GOTO1 (RF),DMCB,(C'U',=CL8'OUTFILE'),0                                 
         GOTO1 (RF),DMCB,(C'U',=CL8'OUTFILE2'),0                                
*                                                                               
LISTX    DS    0H                                                               
         J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
         DS    0D                                                               
OUTFILE  DCB   DDNAME=OUTFILE,                                         X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=000,                                              X        
               MACRF=(GM,PM)                                                    
*                                                                               
         DS    0D                                                               
OUTFILE2 DCB   DDNAME=OUTFILE2,                                        X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=000,                                              X        
               MACRF=(GM,PM),                                          X        
               EODAD=OUTF2EOF                                                   
*                                                                               
         DS    0D                                                               
CTL1CNTL DCB   DDNAME=CTL1CNTL,                                        X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=80,                                               X        
               MACRF=PM                                                         
*                                                                               
*                                                                               
*                                                                               
ICEPAR1  DC    A(0)                ICETOOL PARAMETER LIST                       
ICEPARST DC    A(CNTSA0L)          A(ICETOOL PARAMETER)                         
         DC    A(0)                                                             
         DC    X'FFFFFFFF'         EOL                                          
*                                                                               
* STATEMENT 0                                                                   
*                                                                               
CNTSA0L  DC    AL2(CNTSA0E-CNTSA0S)    L'STATEMENT                              
CNTSA0S  DC    C'SORT FROM(OUTFILE) TO(OUTFILE2) USING(CTL1)'                   
         DC    CL(80-(*-CNTSA0S))' '   BLANK PAD TO 80-BYTES                    
CNTSA0E  EQU   *                                                                
*                                                                               
USING1   DS    0C                  DEFAULT                                      
         DC    C' SORT FIELDS=(1,'                                              
USING1KL DC    C'000'              SORT KEY LENGTH                              
         DC    CL(80-(*-USING1))',BI,A)'                                        
         DC    X'FF'               EOT                                          
*                                                                               
*                                                                               
*                                                                               
MYHEAD   BR    RE                  DUMMY HEADHOOK                               
*                                                                               
*                                                                               
*                                                                               
MYPRINT  BR    RE                  DUMMY EZPRINT                                
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* GENERAL INITIALIZATION                                                        
***********************************************************************         
GENINIT  NTR1  BASE=*,LABEL=*                                                   
         MVC   AIO,AIO1                                                         
*                                                                               
         LARL  RE,MYHEAD                                                        
         A     RE,RELO                                                          
         ST    RE,HEADHOOK                                                      
*                                                                               
         XC    DMCB,DMCB                                                        
         MVC   DMCB+4(4),=X'D9000A2C' DDWRKIO                                   
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VWRKIO,0(R1)                                                     
*                                                                               
* SET OUTFILE'S RECORD LENGTH                                                   
*                                                                               
         LHI   R0,SUMRECDLQ                                                     
         LARL  RF,OUTFILE          INITIAL FILE                                 
         USING IHADCB,RF                                                        
         STH   R0,DCBLRECL         LRECL                                        
         LARL  RF,OUTFILE2         SORTED FILE                                  
         STH   R0,DCBLRECL         LRECL                                        
         DROP  RF                                                               
*                                                                               
* DYNAMICALLY ALLOCATE DATASETS                                                 
*                                                                               
         L     RF,TWAMASTC                                                      
         OC    MCREMPQK-MASTD(,RF),MCREMPQK-MASTD(RF)   SOON RUN?               
         BNZ   GENI010             YES                                          
*                                                                               
* FOR OVERNIGHT PROCESSING ALLOCATE DATASETS                                    
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CDYNALOC-COMFACSD(,RF)                                        
         GOTO1 (RF),DMCB,(X'FD',=CL8'DFSMSG'),(X'80',=CL21' ')                  
         GOTO1 (RF),DMCB,(X'FD',=CL8'TOOLMSG'),(X'80',=CL21' ')                 
         GOTO1 (RF),DMCB,(X'FD',=CL8'SYMNOUT'),(X'80',=CL21' ')                 
         B     GENI011                                                          
*                                                                               
* FOR SOON PROCESSING GENERATE DD DUMMY                                         
*                                                                               
GENI010  DS    0H                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CDYNALOC-COMFACSD(,RF)                                        
         GOTO1 (RF),DMCB,(C'N',=CL8'DFSMSG'),0                                  
         GOTO1 (RF),DMCB,(C'N',=CL8'TOOLMSG'),0                                 
         GOTO1 (RF),DMCB,(C'N',=CL8'SYMNOUT'),0                                 
*                                                                               
GENI011  DS    0H                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CDYNALOC-COMFACSD(,RF)                                        
         GOTO1 (RF),DMCB,(X'80',=CL8'CTL1CNTL'),(X'80',=AL3(1,1)),0             
         GOTO1 (RF),DMCB,(X'80',=CL8'OUTFILE'),(X'40',=AL3(10,10)),0            
         GOTO1 (RF),DMCB,(X'80',=CL8'OUTFILE2'),(X'40',=AL3(10,10)),0           
*                                                                               
         LARL  R2,OUTFILE                                                       
         OPEN  ((R2),OUTPUT)                                                    
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* IF WE'RE NOT FILTERING BY USER ID, WE NEED TO READ ALL WORKER FILES           
* STARTING WITH WORKER FILE 1                                                   
* MAKE FAKE INDEX CALL TO OBTAIN WORKER FILE NAME FROM WORKIO                   
*                                                                               
         BRAS  RE,INITWKB                                                       
*                                                                               
         XC    WRKEZKEY,WRKEZKEY                                                
         MVC   WRKEZUID,TWAORIG                                                 
         MVI   WRKIACTN,WRKIANDX                                                
         GOTO1 VWRKIO,WRKIOB                                                    
         CLI   WRKIERRS,WRKIEEOF                                                
         BE    *+14                                                             
         CLI   WRKIERRS,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   FILTWKF,WRKIFILE                                                 
         MVI   FILTWKF+4,C'1'      FORCE 1ST WORKER FILE                        
*                                                                               
         LA    RE,OUTREC                                                        
         ST    RE,AOUTREC                                                       
*                                                                               
GENINITX J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* INITIALIZE WORKIO BLOCK                                                       
***********************************************************************         
INITWKB  NTR1  BASE=*,LABEL=*                                                   
         XC    WRKIOB(WRKIOBL),WRKIOB                                           
         MVC   WRKIACOM,ACOMFACS                                                
         MVC   WRKIAREC,AIO                                                     
         LAY   RE,WRKFBUFR                                                      
         ST    RE,WRKIABUF                                                      
         MVI   WRKIFTYP,WRKIFTEZ                                                
*                                                                               
INITWKBX J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* INITIALIZE EZBLOCK FOR EZMOD                                                  
***********************************************************************         
INITEZB  NTR1  BASE=*,LABEL=*                                                   
         LA    RE,EZBLOCKD                                                      
         LHI   RF,EZBLOCKL                                                      
         XCEFL                                                                  
*                                                                               
         GOTO1 CALLOV,DMCB,(X'10',0),ATWA  LOAD T23010 (EZMOD)                  
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         ST    RF,VEZMOD                                                        
*                                                                               
         MVI   EZWRKIOF,C'Y'                                                    
*                                                                               
         LAY   R1,WRKFBUFR                                                      
         ST    R1,EZWKRBUF                                                      
*                                                                               
         L     RF,AIO                                                           
         ST    RF,EZWKRREC                                                      
*                                                                               
         LA    RF,2048(RF)                                                      
         ST    RF,EZAREC                                                        
*                                                                               
         MVC   EZCOMFCS,ACOMFACS                                                
*        MVI   EZLOOKSW,X'20'+X'40'+X'80'  NO CMML/PRD/EST LOOKUP               
         MVI   EZLOOKSW,X'20'      DON'T LOOK UP COMMLS                         
         MVI   EZTEST,C'Y'                                                      
         MVI   EZWRITE,C'N'                                                     
         MVC   EZUTL,UTL                                                        
         MVC   EZAGY,TWAAGY                                                     
         L     RF,=A(EZMPROC)                                                   
         A     RF,RELO                                                          
         ST    RF,EZHOOK                                                        
         L     RF,=A(MYPRINT)          PRINT ROUTINE FOR TRACE                  
         ST    RF,EZPRINT                                                       
         MVC   EZWRKIO,VWRKIO                                                   
*                                                                               
         BRAS  RE,INITWKB                                                       
*                                                                               
INITEZBX J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* UPDATE EZBLOCK WITH CURRENT AGENCY/MEDIA VALUES                               
***********************************************************************         
UPDTEZB  NTR1  BASE=*,LABEL=*                                                   
         MVC   EZAGY,LKAGYAGY                                                   
         MVC   EZBAGYMD,BAGYMD                                                  
*                                                                               
UPDTEZBX J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *             
* FAKEIND - ROUTINE TO FILL IN INDEX FIELDS WITH DDWRKIOD DATA                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *             
FAKEIND  NTR1  BASE=*,LABEL=*                                                   
         LAY   R1,WRKEZKEY                                                      
         USING WRKEZKEY,R1                                                      
         LA    R2,EZWKRIND                                                      
         USING UKINDEX,R2                                                       
*                                                                               
         MVC   UKUSRID,WRKEZUID                                                 
         MVC   UKSYSPRG(L'WRKEZSCL),WRKEZSCL                                    
         MVC   UKDAY,WRKEZDAY                                                   
         MVC   UKCLASS,WRKEZMED                                                 
* !!! UKTYPE AND UKATTB SACRIFICED TO FIT IN THE 4-BYTE SEQ NUMBER !!!          
         MVC   UKFILENO(L'WRKEZSQN),WRKEZSQN                                    
         MVC   UKSTAT,WRKEZSTA                                                  
         MVC   UKAGELD,WRKEZBDT                                                 
         MVC   UKUDATA,WRKEZUDT                                                 
*                                                                               
         DROP  R2,R1                                                            
*                                                                               
         J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* EZHOOK                                                                        
***********************************************************************         
EZMPROC  NTR1  BASE=*,LABEL=*                                                   
         CLI   EZMODE,EZINVL                                                    
         JNE   EQXIT                                                            
*                                                                               
         AP    INVCOUNT,=P'1'                                                   
*                                                                               
         BRAS  RE,FILTINV                                                       
         JNE   EQXIT                                                            
*                                                                               
         LA    R4,OUTREC                                                        
         USING SUMRECD,R4                                                       
*                                                                               
         LA    RE,OUTREC                                                        
         LHI   RF,SUMRECDLQ                                                     
         XCEFL                                                                  
* USER ID                                                                       
         MVC   SUMUID,LKAGYUID                                                  
*                                                                               
* BATCH DATE                                                                    
         GOTO1 DATCON,DMCB,(2,WRKEZBDT),(10,SUMBDAT)                            
* BATCH SEQUENCE NUMBER                                                         
         EDIT  (B4,WRKEZSQN),(6,SUMSEQ),ALIGN=LEFT                              
* BATCH SOURCE                                                                  
         MVC   SUMSRCE,SVSRCE                                                   
* MOS                                                                           
         GOTO1 DATCON,DMCB,(1,WRKEZMOS),(6,SUMMOS)                              
* STATION                                                                       
         MVC   SUMSTA,PRTSTA7C                                                  
* INVOICE                                                                       
         MVC   SUMINV,EZIHINV                                                   
         OC    SUMINV,SPACES                                                    
**                                                                              
**                                                                              
**                                                                              
* CLIENT                                                                        
         XC    SVCLTCOD,SVCLTCOD                                                
         XC    WORK2,WORK2                                                      
         LA    R6,WORK2                                                         
*                                                                               
         CLC   EZIHADVN,SPACES                                                  
         BNH   EZMP02                                                           
*                                                                               
         MVC   0(4,R6),=AL4(L'EZIHADVN)                                         
         LA    RF,EZIHADVN                                                      
         ST    RF,4(R6)                                                         
         LA    R6,8(R6)                                                         
         MVC   0(4,R6),=X'FFFFFFFF'                                             
*                                                                               
EZMP02   DS    0H                                                               
         MVC   0(4,R6),=AL4(5)                                                  
         MVI   WORK,C'('                                                        
         MVC   WORK+1(3),=C'***'                                                
         MVI   WORK+4,C')'                                                      
*                                                                               
         CLC   EZIHAAID,SPACES                                                  
         BNH   *+16                                                             
         MVC   WORK+1(3),EZIHAAID                                               
         MVC   SVCLTCOD,EZIHAAID                                                
*                                                                               
         LA    RF,WORK                                                          
         ST    RF,4(R6)                                                         
         LA    R6,8(R6)                                                         
         MVC   0(4,R6),=X'FFFFFFFF'                                             
*                                                                               
         TM    EZIHCVST,EZIHCOVR                                                
         BZ    EZMP06                                                           
*                                                                               
         MVC   0(4,R6),=AL4(8)                                                  
         MVC   WORK+10(4),=C'OVR('                                              
         GOTO1 CLUNPK,DMCB,EZIHCVAD,WORK+14                                     
         MVC   SVCLTCOD,WORK+14                                                 
         MVI   WORK+17,C')'                                                     
         LA    RF,WORK+10                                                       
         ST    RF,4(R6)                                                         
         LA    R6,8(R6)                                                         
         MVC   0(4,R6),=X'FFFFFFFF'                                             
*                                                                               
EZMP06   DS    0H                                                               
         OC    WORK2(4),WORK2                                                   
         BZ    EZMP10                                                           
*                                                                               
         GOTO1 =A(CONCAT),DMCB,WORK2,SUMCLT                                     
         OC    SUMCLT,SPACES                                                    
*                                                                               
* PRODUCT                                                                       
*                                                                               
EZMP10   DS    0H                                                               
         XC    WORK2,WORK2                                                      
         LA    R6,WORK2                                                         
*                                                                               
         CLC   EZIHPRDN,SPACES                                                  
         BNH   EZMP12                                                           
*                                                                               
         MVC   0(4,R6),=AL4(L'EZIHPRDN)                                         
         LA    RF,EZIHPRDN                                                      
         ST    RF,4(R6)                                                         
         LA    R6,8(R6)                                                         
*                                                                               
EZMP12   DS    0H                                                               
* PRE-POPULATE WITH ASTERISKS, IN CASE PRODUCT CODE IS MISSING                  
         MVC   0(4,R6),=AL4(5)                                                  
         MVC   WORK(5),=C'(***)'                                                
         LA    RF,WORK                                                          
         ST    RF,4(R6)                                                         
         MVC   8(4,R6),=X'FFFFFFFF'                                             
*                                                                               
         CLC   EZIHAPID,SPACES     PRODUCT CODE?                                
         BNH   EZMP14                                                           
*                                                                               
* CONCAT 1: '(' + EZIHAPID                                                      
         MVC   0(4,R6),=AL4(1+L'EZIHAPID)                                       
         MVI   WORK,C'('                                                        
         MVC   WORK+1(L'EZIHAPID),EZIHAPID                                      
         LA    RF,WORK                                                          
         ST    RF,4(R6)                                                         
         LA    R6,8(R6)                                                         
*                                                                               
* CONCAT 2: ')'                                                                 
         MVC   0(4,R6),=AL4(1)                                                  
         MVI   WORK+L'EZIHAPID+1,C')'                                           
         LA    RF,WORK+L'EZIHAPID+1                                             
         ST    RF,4(R6)                                                         
         MVC   8(4,R6),=X'FFFFFFFF'                                             
*                                                                               
EZMP14   DS    0H                                                               
         TM    EZIHCVST,EZIHCOVR                                                
         BZ    EZMP16                                                           
*                                                                               
         LA    R6,8(R6)                                                         
         MVC   0(4,R6),=AL4(12)                                                 
         MVC   WORK+10(4),=C'OVR('                                              
*                                                                               
         LAY   RF,EZIHSAV          2ND SAVE FIELD                               
         MVC   WORK+14(3),(EZIHSPRD-EZIHSAV)(RF)                                
         MVI   WORK+17,C')'                                                     
*                                                                               
         OC    (EZIHSPR2-EZIHSAV)(3,RF),(EZIHSPR2-EZIHSAV)(RF)                  
         BZ    EZMP15                                                           
         MVC   WORK+17(3),(EZIHSPR2-EZIHSAV)(RF)                                
         MVI   WORK+20,C')'                                                     
*                                                                               
EZMP15   DS    0H                                                               
         LA    RF,WORK+10                                                       
         ST    RF,4(R6)                                                         
         LA    R6,8(R6)                                                         
         MVC   0(4,R6),=X'FFFFFFFF'                                             
*                                                                               
EZMP16   DS    0H                                                               
         OC    WORK2(4),WORK2                                                   
         BZ    EZMP20                                                           
*                                                                               
         GOTO1 =A(CONCAT),DMCB,WORK2,SUMPRD                                     
         OC    SUMPRD,SPACES                                                    
*                                                                               
* ESTIMATE                                                                      
EZMP20   DS    0H                                                               
         XC    WORK2,WORK2                                                      
         LA    R6,WORK2                                                         
*                                                                               
         CLC   EZIHEST,SPACES                                                   
         BNH   EZMP23                                                           
*                                                                               
         MVC   0(4,R6),=AL4(L'EZIHEST)                                          
         LA    RF,EZIHEST                                                       
         ST    RF,4(R6)                                                         
         LA    R6,8(R6)                                                         
         MVC   0(4,R6),=X'FFFFFFFF'                                             
*                                                                               
EZMP23   DS    0H                                                               
         TM    EZIHCVST,EZIHCOVR                                                
         BZ    EZMP25                                                           
*                                                                               
         EDIT  (B1,EZIHCVES),(3,FULL)                                           
         XC    WORK,WORK                                                        
         MVC   0(4,R6),=AL4(8)                                                  
         MVC   WORK(4),=C'OVR('                                                 
         MVC   WORK+4(3),FULL                                                   
         MVI   WORK+7,C')'                                                      
         LA    RF,WORK                                                          
         ST    RF,4(R6)                                                         
         LA    R6,8(R6)                                                         
         MVC   0(4,R6),=X'FFFFFFFF'                                             
*                                                                               
EZMP25   DS    0H                                                               
         OC    SUMEST,SPACES                                                    
         OC    WORK2(4),WORK2                                                   
         BZ    EZMP30                                                           
*                                                                               
         GOTO1 =A(CONCAT),DMCB,WORK2,SUMEST                                     
         OC    SUMEST,SPACES                                                    
*                                                                               
* NUMBER OF SPOTS                                                               
EZMP30   DS    0H                                                               
         EDIT  (B4,EZIHTSPN),(5,SUMSPOTS),ALIGN=LEFT                            
* GROSS                                                                         
         LAY   R3,EZITPACT                                                      
         EDIT  (P8,0(R3)),(16,SUMGRS),2,COMMAS=YES,ZERO=NOBLANK,       X        
               MINUS=YES,ALIGN=LEFT                                             
* NET                                                                           
         LAY   R3,EZITPDUE                                                      
         EDIT  (P8,0(R3)),(16,SUMNET),2,COMMAS=YES,ZERO=NOBLANK,       X        
               MINUS=YES,ALIGN=LEFT                                             
* ORDER TYPE                                                                    
         MVC   SUMOTYPE,EZIHORD                                                 
         OC    SUMOTYPE,SPACES                                                  
* PAYING REP                                                                    
*        MVC   SUMPREP,EZIHLREP                                                 
         LAY   RF,EZIHPREP                                                      
         MVC   SUMPREP,0(RF)                                                    
         OC    SUMPREP,SPACES                                                   
*                                                                               
* INVOICE STATUS                                                                
         MVI   SUMSTAT,C'U'                                                     
*                                                                               
         TM    EZIHCVST,EZIHCDEL   DELETED?                                     
         BZ    *+12                                                             
         MVI   SUMSTAT,C'D'                                                     
         B     EZMP80                                                           
*                                                                               
         TM    EZIHCVST,EZIHCVQ    CONVERTED?                                   
         BZ    *+8                                                              
         MVI   SUMSTAT,C'C'                                                     
*                                                                               
         TM    EZIHCVST,EZIHRCVQ   RE-CONVERTED?                                
         BZ    *+8                                                              
         MVI   SUMSTAT,C'R'                                                     
*                                                                               
* CONVERTED DATE                                                                
EZMP80   DS    0H                                                               
         MVC   SUMCDAT,SPACES                                                   
         OC    EZIHCVDT,EZIHCVDT                                                
         BZ    EZMP90                                                           
         GOTO1 DATCON,DMCB,(1,EZIHCVDT),(21,SUMCDAT)                            
*                                                                               
EZMP90   DS    0H                                                               
*                                                                               
         DROP  R4                                                               
*                                                                               
         BRAS  RE,LKMNAM                                                        
*                                                                               
         LARL  R2,OUTFILE                                                       
         PUT   (R2),(R4)                                                        
*                                                                               
         J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
ISALPHA  CLI   0(R1),C'A'                                                       
         JL    ISNEQX                                                           
         CLI   0(R1),C'Z'                                                       
         JH    ISNEQX                                                           
         J     ISEQX                                                            
*                                                                               
ISNUM    CLI   0(R1),C'0'                                                       
         JL    ISNEQX                                                           
         CLI   0(R1),C'9'                                                       
         JH    ISNEQX                                                           
*                                                                               
ISEQX    CR    RB,RB                                                            
         BR    RE                                                               
ISNEQX   LTR   RB,RB                                                            
         BR    RE                                                               
*                                                                               
*                                                                               
*                                                                               
INITDL   NTR1  BASE=*,LABEL=*                                                   
         XC    HEADHOOK,HEADHOOK                                                
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,0                                                       
*                                                                               
         MVI   DNFIRST,C'Y'                                                     
         USING DLCBD,R2                                                         
         LA    R2,DLCB                                                          
         OI    DLCBFLG1,DLCBFXTN                                                
         MVC   DLCXTND,MAXLINE                                                  
         MVC   DLCBAPR,=A(DNPRINT)                                              
         LA    R0,P                                                             
         ST    R0,DLCBAPL                                                       
         MVC   P,SPACES                  JUST IN CASE                           
         MVI   DLCBACT,C'I'              START AND INTIALIZE REPORT             
         GOTO1 =V(DLFLD),DLCB                                                   
         DROP  R2                                                               
         J     EQXIT                                                            
* DLCXTND(8) CONTENTS                                                           
MAXLINE  DC    H'132'                                                           
DELIM    DC    C' '        FIELD DELIMITER                                      
EOTCHR   DC    C'"'        END OF TEXT FIELD DELIMITER                          
EOTALT   DC    C''''       END OF TEXT CHR ALTERNATE                            
EOLCHR   DC    X'5E'       END OF LINE CHAR - SEMI-COLON                        
EORCHR   DC    C':'        END OF REPORT CHR                                    
         DC    X'00'       SPARE                                                
         LTORG                                                                  
*                                                                               
*                                                                               
DNPRINT  NTR1  BASE=*,LABEL=*                                                   
         MVI   LINE,0                                                           
         MVI   FORCEHED,C'N'                                                    
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   FORCEHED,C'N'                                                    
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* FILTER EASI INVOICES                                                          
***********************************************************************         
FILTINV  NTR1  BASE=*,LABEL=*                                                   
         OC    FILTSTAT,FILTSTAT                                                
         BZ    FINVQX                                                           
*                                                                               
         CLI   FILTSTAT,C'U'                                                    
         BNE   *+16                                                             
         TM    EZIHCVST,EZIHCVQ+EZIHCDEL+EZIHRCVQ                               
         BNZ   FINVNQX                                                          
         B     FINVQX                                                           
*                                                                               
         CLI   FILTSTAT,C'C'                                                    
         BNE   *+16                                                             
         TM    EZIHCVST,EZIHCVQ                                                 
         BZ    FINVNQX                                                          
         B     FINVQX                                                           
*                                                                               
         CLI   FILTSTAT,C'D'                                                    
         BNE   *+16                                                             
         TM    EZIHCVST,EZIHCDEL                                                
         BZ    FINVNQX                                                          
         B     FINVQX                                                           
*                                                                               
         CLI   FILTSTAT,C'R'                                                    
         BNE   *+16                                                             
         TM    EZIHCVST,EZIHRCVQ                                                
         BZ    FINVNQX                                                          
         B     FINVQX                                                           
*                                                                               
FINVQX   J     EQXIT                                                            
FINVNQX  J     NEQXIT                                                           
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* FILTER EASI BATCH INDICES                                                     
* R2 EXPECTED TO ADDRESS THE INDEX                                              
***********************************************************************         
FILTIND  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
* MAKE SURE EASI BATCH                                                          
*                                                                               
         CLI   WRKEZDAY,WRKEZDYQ                                                
         BNE   FINQX               NO - NEXT INDEX                              
*                                                                               
* TRY FILTERING ON UID FIRST                                                    
*                                                                               
         OC    FILTBUID,FILTBUID                                                
         BZ    *+14                                                             
         CLC   WRKEZUID,FILTBUID                                                
         BNE   FINQX                                                            
*                                                                               
* ALWAYS FILTER BY AGENCY                                                       
*                                                                               
         XC    LKAGYBLK,LKAGYBLK                                                
         MVC   LKAGYBID,WRKEZUID                                                
         GOTO1 VLKAGY,LKAGYBLK                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   FILTAGY,LKAGYAGY                                                 
         BNE   FINQX                                                            
*                                                                               
* CHECK BATCH DATE FILTER                                                       
*                                                                               
         OC    FILTSD,FILTSD                                                    
         BZ    FI10                                                             
*                                                                               
         OC    FILTED,FILTED       SECOND DATE?                                 
         BNZ   FI05                YES - CHECK DATE RANGE                       
*                                                                               
* ONLY ONE DATE                                                                 
         CLC   FILTSD,WRKEZBDT                                                  
         BNE   FINQX                                                            
         B     FI10                                                             
*                                                                               
FI05     DS    0H                                                               
         CLC   FILTSD,WRKEZBDT                                                  
         BH    FINQX                                                            
         CLC   FILTED,WRKEZBDT                                                  
         BL    FINQX                                                            
*                                                                               
FI10     DS    0H                                                               
*                                                                               
* CHECK MOS FILTER                                                              
*                                                                               
         OC    FILTMOS,FILTMOS     ARE WE FILTERING BY MOS?                     
         BZ    *+14                                                             
         CLC   FILTMOS,WRKEZMOS                                                 
         BNE   FINQX               WRONG MOS - READ NEXT INDEX                  
*                                                                               
* LOOKUP EQUIVALENT STATION                                                     
*                                                                               
         MVC   SRCESTA(4),WRKEZSCL                                              
         CLI   SRCESTA+3,C' '                                                   
         BH    *+8                                                              
         MVI   SRCESTA+3,C' '                                                   
         MVC   SRCESTA+4(1),WRKEZMED                                            
         GOTO1 VEQVSTA                                                          
*                                                                               
* APPLY SYSTEM FILTER                                                           
*                                                                               
         CLI   LKAGYCTR,C'C'       CANADA?                                      
         BE    *+14                CANADA NET INVOICES LIVE IN SPOT             
         CLC   FILTSYS,EQVSYS                                                   
         BNE   FINQX               NO - NEXT INDEX                              
*                                                                               
* CHECK MEDIA FILTER                                                            
*                                                                               
         OC    FILTMED,FILTMED                                                  
         BZ    *+14                                                             
         CLC   FILTMED,EQVMED                                                   
         BNE   FINQX               NO - NEXT INDEX                              
*                                                                               
* CHECK STATION FILTER                                                          
*                                                                               
         OC    FILTSTA,FILTSTA                                                  
         BZ    *+14                                                             
         CLC   FILTSTA,EQUISTA5                                                 
         BNE   FINQX               WRONG STATION - READ NEXT INDEX              
*                                                                               
FIQX     J     EQXIT                                                            
FINQX    J     NEQXIT                                                           
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* INCREMENT WORKER FILE NAME                                                    
* WORKER FILE NAME EXPECTED TO BE STORED IN WRKFILE (CSECT)                     
***********************************************************************         
BUMPWKRF NTR1  BASE=*,LABEL=*                                                   
         LA    R1,WRKIFILE                                                      
         A     R1,RELO                                                          
         CLI   4(R1),C'F'      ARE WE ON THE LAST FILE?                         
         BE    BUMPNEQX        YES                                              
*                                                                               
* NO - INCREMENT THE WORKER FILE NUMBER                                         
*                                                                               
         CLI   4(R1),C'9'      GO TO NEXT FILE                                  
         BNE   *+12                                                             
         MVI   4(R1),C'A'                                                       
         B     BUMPEQX                                                          
*                                                                               
         ZIC   R0,4(R1)                                                         
         AHI   R0,1                                                             
         STC   R0,4(R1)                                                         
*                                                                               
BUMPEQX  J     EQXIT                                                            
BUMPNEQX J     NEQXIT                                                           
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* INITICE                                                                       
* BUILD CTL1CNTL DATASET FOR ICETOOL                                            
***********************************************************************         
INITICE  NTR1  BASE=*,LABEL=*                                                   
         LARL  R3,USING1KL                                                      
         LHI   R0,SUMSRTKLQ        LENGTH OF SORT KEY                           
         EDIT  (R0),(L'USING1KL,0(R3)),FILL=0                                   
*                                                                               
         LARL  R3,USING1                                                        
         LARL  R2,CTL1CNTL                                                      
         OPEN  ((R2),OUTPUT)                                                    
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
II30     CLI   0(R3),X'FF'                                                      
         BE    II30A                                                            
         PUT   (R2),0(R3)                                                       
         LA    R3,80(R3)                                                        
         B     II30                                                             
*                                                                               
II30A    DS    0H                                                               
         CLOSE ((R2),)                                                          
         FREEPOOL (R2)                                                          
         J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
TRACEIND NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R2,WRKEZKEY                                                      
         GOTO1 HEXOUT,DMCB,(R2),P,12,=C'N'                                      
         MVC   P+40(2),=C'..'                                                   
         MVC   P+42(4),2(R2)                                                    
         MVC   P+46(1),=C'.'                                                    
         MVC   P+47(1),7(R2)                                                    
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
* PRINT INVOICE IN DOWNLOADABLE FORMAT                                          
* OUTREC EXPECTED TO HAVE SUMRECD INVOICE ENTRY                                 
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
PDENTRY  NTR1  BASE=*,LABEL=*                                                   
         USING DLCBD,R4                                                         
         LA    R4,DLCB                                                          
*                                                                               
         LR    R7,R1                                                            
         USING COLTABD,R7                                                       
*                                                                               
PDENT10  CLI   0(R7),X'FF'         END OF COLTAB?                               
         BE    PDENT99                                                          
*                                                                               
         CLI   0(R7),X'00'         NOT PRINTING THIS COLUMN?                    
         BE    PDENT20                                                          
*                                                                               
         MVC   DLCBFLD,SPACES                                                   
         MVI   DLCBTYP,C'T'        TEXT FIELD                                   
         MVI   DLCBACT,DLCBPUT                                                  
*                                                                               
         L     RE,AOUTREC                                                       
         ICM   RF,15,COLTDISP      DISPLACEMENT TO DATA                         
         AR    RE,RF                                                            
*                                                                               
         LLC   RF,COLTLEN          LENGTH OF DATA                               
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   DLCBFLD(0),0(RE)                                                 
*                                                                               
         GOTO1 =V(DLFLD),DLCB      SEND FIELD                                   
*                                                                               
PDENT20  DS    0H                                                               
         LA    R7,COLTABDLQ(R7)                                                 
         B     PDENT10                                                          
*                                                                               
PDENT99  DS    0H                                                               
         MVI   DLCBACT,DLCBEOL           SEND END OF LINE                       
         GOTO1 =V(DLFLD),DLCB                                                   
*                                                                               
PDNTRYX  J     EQXIT                                                            
         LTORG                                                                  
         DROP  R4                                                               
*                                                                               
*                                                                               
*                                                                               
COLTABD  DSECT                                                                  
COLTLEN  DS    X                   SUMREC ENTRY LENGTH                          
COLTDISP DS    AL4                 ENTRY'S DISPLACEMENT INTO SUMREC             
COLTABDLQ EQU  *-COLTABD                                                        
*                                                                               
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
* COLUMN TABLE FOR PRINTING                                                     
* MAKE SURE TO UPDATE, IF SUMRECD CHANGES                                       
* GOES TOGETHER WITH HDRTAB - TABLE OF HEADERS                                  
*                                                                               
* AL1 - LENGTH OF ENTRY                                                         
* AL4 - ENTRY'S DISPLACEMENT INTO SUMRECD                                       
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
T23035   CSECT                                                                  
         DS    0H                                                               
COLTAB   DS    0XL5                                                             
         DC    AL1(L'SUMUID),AL4(SUMUID-SUMRECD)                                
         DC    AL1(L'SUMBDAT),AL4(SUMBDAT-SUMRECD)                              
         DC    AL1(L'SUMSEQ),AL4(SUMSEQ-SUMRECD)                                
         DC    AL1(L'SUMSRCE),AL4(SUMSRCE-SUMRECD)                              
         DC    AL1(L'SUMMOS),AL4(SUMMOS-SUMRECD)                                
         DC    AL1(L'SUMMNAM),AL4(SUMMNAM-SUMRECD)                              
         DC    AL1(L'SUMSTA),AL4(SUMSTA-SUMRECD)                                
         DC    AL1(L'SUMCLT),AL4(SUMCLT-SUMRECD)                                
         DC    AL1(L'SUMPRD),AL4(SUMPRD-SUMRECD)                                
         DC    AL1(L'SUMEST),AL4(SUMEST-SUMRECD)                                
         DC    AL1(L'SUMINV),AL4(SUMINV-SUMRECD)                                
         DC    AL1(L'SUMSPOTS),AL4(SUMSPOTS-SUMRECD)                            
         DC    AL1(L'SUMGRS),AL4(SUMGRS-SUMRECD)                                
         DC    AL1(L'SUMNET),AL4(SUMNET-SUMRECD)                                
         DC    AL1(L'SUMOTYPE),AL4(SUMOTYPE-SUMRECD)                            
         DC    AL1(L'SUMPREP),AL4(SUMPREP-SUMRECD)                              
         DC    AL1(L'SUMSTAT),AL4(SUMSTAT-SUMRECD)                              
         DC    AL1(L'SUMCDAT),AL4(SUMCDAT-SUMRECD)                              
         DC    X'FF'                                                            
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
* HEADER TABLE FOR PRINTING WITH PDENTRY, SAME FORMAT AS COLTAB                 
* MAKE SURE TO UPDATE, IF SUMRECD CHANGES                                       
*                                                                               
* AL1 - LENGTH OF ENTRY                                                         
* AL4 - ENTRY'S DISPLACEMENT INTO SUMRECD                                       
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
         DS    0H                                                               
HDRTAB   DS    0XL5                                                             
         DC    AL1(HDRUIDLQ),AL4(HDRUID-HDRLIST)                                
         DC    AL1(HDRBDATLQ),AL4(HDRBDAT-HDRLIST)                              
         DC    AL1(HDRSEQLQ),AL4(HDRSEQ-HDRLIST)                                
         DC    AL1(HDRSRCELQ),AL4(HDRSRCE-HDRLIST)                              
         DC    AL1(HDRMOSLQ),AL4(HDRMOS-HDRLIST)                                
         DC    AL1(HDRMNMLQ),AL4(HDRMNM-HDRLIST)                                
         DC    AL1(HDRSTALQ),AL4(HDRSTA-HDRLIST)                                
         DC    AL1(HDRCLTLQ),AL4(HDRCLT-HDRLIST)                                
         DC    AL1(HDRPRDLQ),AL4(HDRPRD-HDRLIST)                                
         DC    AL1(HDRESTLQ),AL4(HDREST-HDRLIST)                                
         DC    AL1(HDRINVLQ),AL4(HDRINV-HDRLIST)                                
         DC    AL1(HDRSPTSLQ),AL4(HDRSPTS-HDRLIST)                              
         DC    AL1(HDRGRSLQ),AL4(HDRGRS-HDRLIST)                                
         DC    AL1(HDRNETLQ),AL4(HDRNET-HDRLIST)                                
         DC    AL1(HDROTYPLQ),AL4(HDROTYP-HDRLIST)                              
         DC    AL1(HDRPREPLQ),AL4(HDRPREP-HDRLIST)                              
         DC    AL1(HDRSTATLQ),AL4(HDRSTAT-HDRLIST)                              
         DC    AL1(HDRCDATLQ),AL4(HDRCDAT-HDRLIST)                              
         DC    X'FF'                                                            
*                                                                               
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
* LIST OF COLUMN HEADERS TO BE PRINTED WITH PDENTRY                             
* MAKE SURE TO UPDATE, IF SUMRECD CHANGES                                       
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
HDRLIST  DS    0H                                                               
HDRUID   DC    C'USER ID'                                                       
HDRUIDLQ EQU  *-HDRUID                                                          
HDRBDAT  DC    C'BATCH DATE'                                                    
HDRBDATLQ EQU  *-HDRBDAT                                                        
HDRSEQ   DC    C'SEQ'                                                           
HDRSEQLQ EQU   *-HDRSEQ                                                         
HDRSRCE  DC    C'SOURCE'                                                        
HDRSRCELQ EQU  *-HDRSRCE                                                        
HDRMOS   DC    C'MOS'                                                           
HDRMOSLQ EQU   *-HDRMOS                                                         
HDRMNM   DC    C'MARKET NAME'                                                   
HDRMNMLQ EQU   *-HDRMNM                                                         
HDRSTA   DC    C'STATION'                                                       
HDRSTALQ EQU   *-HDRSTA                                                         
HDRCLT   DC    C'CLIENT'                                                        
HDRCLTLQ EQU   *-HDRCLT                                                         
HDRPRD   DC    C'PRODUCT'                                                       
HDRPRDLQ EQU   *-HDRPRD                                                         
HDREST   DC    C'ESTIMATE'                                                      
HDRESTLQ EQU   *-HDREST                                                         
HDRINV   DC    C'INVOICE'                                                       
HDRINVLQ EQU   *-HDRINV                                                         
HDRSPTS  DC    C'SPOTS'                                                         
HDRSPTSLQ EQU *-HDRSPTS                                                         
HDRGRS   DC    C'GROSS AMOUNT'                                                  
HDRGRSLQ EQU *-HDRGRS                                                           
HDRNET   DC    C'NET AMOUNT'                                                    
HDRNETLQ EQU *-HDRNET                                                           
HDROTYP  DC    C'ORDER TYPE'                                                    
HDROTYPLQ EQU *-HDROTYP                                                         
HDRPREP  DC    C'PAYING REP'                                                    
HDRPREPLQ EQU *-HDRPREP                                                         
HDRSTAT  DC    C'STATUS'                                                        
HDRSTATLQ EQU *-HDRSTAT                                                         
HDRCDAT  DC    C'CONV. DATE'                                                    
HDRCDATLQ EQU *-HDRCDAT                                                         
HDRLISTLQ EQU  *-HDRLIST                                                        
*                                                                               
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
* STRING CONCATENATION                                                          
*                                                                               
* P1 - LIST OF STRINGS                                                          
*      LIST ENTRY FORMAT:                                                       
*          XL4 - LENGTH OF STRING                                               
*          XL4 - A(STRING)                                                      
*          LIST TERMINATED BY X'FFFFFFFF'                                       
* P2 - A(OUTPUT AREA) - BOUNDARY NOT CHECKED                                    
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
CONCAT   NTR1  BASE=*,LABEL=*                                                   
         L     R9,0(R1)           STRING LIST POINTER                           
         L     R5,4(R1)           A(OUTPUT AREA)                                
*                                                                               
         LTR   R5,R5                                                            
         JZ    EQXIT              NO OUTPUT AREA ADDRESS                        
*                                                                               
CONC10   CLC   =X'FFFFFFFF',0(R9) END OF STRING LIST?                           
         JE    EQXIT                                                            
*                                                                               
         MVC   DMCB(4),0(R9)                                                    
         MVC   DMCB+4(4),4(R9)                                                  
         GOTO1 =A(SQUEEZE),DMCB                                                 
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ICM   RE,15,DMCB+4           START OF NEW STRING                       
         ICM   RF,15,DMCB             LENGTH OF STRING TO MOVE                  
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),0(RE)                                                    
*                                                                               
         AR    R5,RF                                                            
         AHI   R5,1                TO COMPENSATE FOR BCTR                       
         MVI   0(R5),C' '          LEAVE A SPACE                                
         AHI   R5,1                                                             
*                                                                               
CONC90   DS    0H                                                               
         LA    R9,8(R9)                                                         
         B     CONC10                                                           
*                                                                               
         J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
* SQUEEZE LEADING AND TRAILING WHITESPACES OUT OF THE STRING                    
*                                                                               
* P1 - MAX STRING LENGTH                                                        
* P2 - A(STRING)                                                                
*                                                                               
* ON EXIT                                                                       
* P1 - NEW STRING LENGTH                                                        
* P2 - A(NON-WHITESPACE STRING START)                                           
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
SQUEEZE  NTR1  BASE=*,LABEL=*                                                   
         LR    R9,R1              SAVE PARAMETERS                               
*                                                                               
         ICM   R2,15,4(R1)        A(STRING)                                     
         JZ    NEQXIT                                                           
         ICM   R3,15,0(R1)        L'STRING                                      
         JZ    NEQXIT                                                           
*                                                                               
* ELIMINATE LEADING SPACES                                                      
*                                                                               
         LR    RE,R2              ADDRESS OF STRING                             
         LR    R0,R2              ADDRESS OF STRING                             
         AR    R0,R3              R0 -> 1 BYTE PAST STRING'S END                
*                                                                               
SQZE20   DS    0H                                                               
         CR    RE,R0               WENT PAST END OF STRING?                     
         BNL   SQZE90              STRING CONSISTS OF ALL SPACES                
*                                                                               
         CLI   0(RE),C' '                                                       
         BH    SQZE30              FOUND FIRST NON-SPACE CHARACTER              
         LA    RE,1(RE)                                                         
         B     SQZE20                                                           
*                                                                               
SQZE30   DS    0H                  RE POINTS TO FIRST NON-SPACE CHAR            
*                                                                               
* NOW ELIMINATE TRAILING SPACES                                                 
*                                                                               
         LR    RF,R2               ADDRESS OF STRING                            
         AR    RF,R3               PLUS MAX LENGTH                              
         BCTR  RF,0                RF -> LAST CHARACTER OF THE STRING           
         LR    R0,R2               ADDRESS OF STRING                            
         BCTR  R0,0                R0 -> ONE CHAR BEFORE STRING START           
*                                                                               
SQZE40   DS    0H                                                               
         CR    RF,R0                                                            
         JH    *+6                 STRING CONSISTS OF ALL SPACES                
         DC    H'0'                THIS SHOULD HAVE BEEN CAUGHT EARLIER         
*                                                                               
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,SQZE40                                                        
*                                                                               
* AT THIS POINT RF POINTS TO LAST NON-SPACE CHARACTER OF THE STRING             
* CALCULATE RESULTING STRING LENGTH, UPDATE RETURN PARMS AND EXIT               
*                                                                               
         SR    RF,RE               RF = L'STRING TO - 1                         
         AHI   RF,1                                                             
         LTR   RF,RF                                                            
         BP    *+6                                                              
         DC    H'0'        STRING LENGTH NEGATIVE, SOMETHING WENT WRONG         
*                                                                               
         ST    RF,0(R9)            STORE LENGTH OF NEW STRING                   
         ST    RE,4(R9)            START OF NEW STRING                          
         J     EQXIT                                                            
*                                                                               
* HAVE ALL-SPACES STRING HERE                                                   
*                                                                               
SQZE90   DS    0H                                                               
         XC    0(4,R9),0(R9)       XC RESULTING STRING LENGTH                   
         J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
* LOOK UP MARKET NAME                                                           
* P1=5-CHAR STA, P2-OUTPUT AREA                                                 
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
LKMNAM   NTR1  BASE=*,LABEL=*                                                   
         LA    R4,OUTREC                                                        
         USING SUMRECD,R4                                                       
         MVC   SUMMNAM,SPACES                                                   
*                                                                               
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING STAREC,R2                                                        
*                                                                               
         MVI   KEY,C'0'            FILL KEY WITH CHAR ZEROES                    
         MVC   KEY+1(L'STAKEY-1),KEY                                            
*                                                                               
         MVI   STAKTYPE,STAKTYPQ   C'S'                                         
         MVC   STAKMED,EQVMED                                                   
         MVC   STAKCALL,EQUISTA                                                 
         OC    STAKCALL,SPACES                                                  
         MVC   STAKAGY,AGENCY                                                   
         OC    SVCLTCOD,SVCLTCOD                                                
         BZ    *+10                                                             
         MVC   STAKCLT,SVCLTCOD                                                 
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'STATION',KEY,AIO3                     
         CLI   8(R1),0             ANY ERROR                                    
         JNE   LKMNAM10             NO                                          
*                                                                               
         L     R2,AIO3                                                          
         CLC   STAKEY,KEY                                                       
         JE    LKMNAM10                                                         
*                                                                               
* CLIENT-SPECIFIC LOOKUP FAILED, NOW READ NO-CLIENT STATION RECORD              
         LA    R2,KEY                                                           
         MVC   STAKCLT,=CL3'000'                                                
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'STATION',KEY,AIO3                     
         CLI   8(R1),0             ANY ERROR                                    
         JNE   NEQXIT               NO                                          
*                                                                               
         L     R2,AIO3                                                          
         CLC   STAKEY,KEY                                                       
         JNE   NEQXIT                                                           
*                                                                               
LKMNAM10 DS    0H                                                               
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING MKTREC,R3                                                        
         MVI   KEY,C'0'            FILL KEY WITH CHAR ZEROES                    
         MVC   KEY+1(L'MKTKEY-1),KEY                                            
*                                                                               
         MVI   MKTKTYPE,MKTKTYPQ   C'M'                                         
         MVC   MKTKMED,EQVMED                                                   
         MVC   MKTKMKT,SMKT                                                     
         MVC   MKTKAGY,AGENCY                                                   
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'STATION',KEY,AIO3                     
         CLI   8(R1),0             ANY ERROR                                    
         JNE   NEQXIT               NO                                          
*                                                                               
         L     R3,AIO3                                                          
         CLC   MKTKEY,KEY                                                       
         JNE   NEQXIT                                                           
*                                                                               
         MVC   SUMMNAM,MKTNAME                                                  
*                                                                               
         J     EQXIT                                                            
         LTORG                                                                  
         DROP  R4,R3,R2                                                         
                                                                                
*                                                                               
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
* SHARED EASI WORKING STORAGE                                                   
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
       ++INCLUDE SPEZFWORKD                                                     
*                                                                               
*                                                                               
SYSD     DSECT                                                                  
         ORG   SYSSPARE                                                         
RELO     DS    A                                                                
SVRC     DS    A                                                                
VWRKIO   DS    A                                                                
WORK2    DS    XL128                                                            
*                                                                               
* DLFLD PARAMETER BLOCK                                                         
*                                                                               
DLCB     DS    XL256                                                            
DNLINE   DS    CL132                                                            
DNLINE2  DS    CL132                                                            
DNLINE3  DS    CL132                                                            
DNFIRST  DS    X                                                                
*                                                                               
*                                                                               
INVCOUNT DS    PL8                                                              
*                                                                               
SVSRCE   DS    CL4                                                              
SVCLTCOD DS    CL3                                                              
*                                                                               
AOUTREC  DS    A                                                                
*                                                                               
OUTREC   DS    (SUMRECDLQ)X                                                     
*                                                                               
* AGENCY LOOKUP  BLOCK                                                          
       ++INCLUDE EZLKAGYBLK                                                     
*                                                                               
WRKFBUFR DS    0D                                                               
         DS    14336X                                                           
WRKFEND  EQU   *                                                                
*                                                                               
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
* SCREEN                                                                        
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
       ++INCLUDE SPEZFFFD                                                       
       ++INCLUDE DDGENTWA                                                       
         ORG CONTAGH                                                            
       ++INCLUDE SPEZFA5D                                                       
*                                                                               
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
* TWA SAVED STORAGE                                                             
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
         ORG   CPEWORK                                                          
SVTWA    DS    0H                                                               
SVTWALQ  EQU   AFRSTKEY-SVTWA                                                   
                                                                                
*                                                                               
FILTRS   DS    0X                                                               
*                                                                               
FILTSYS  DS    C                                                                
FILTAGY  DS    CL2                                                              
FILTUID  DS    XL10                                                             
FILTBUID DS    XL2                                                              
*                                                                               
FILTMOS  DS    XL2                                                              
         DS    X                   SPACE FOR PACKED DAY (IGNORED)               
*                                                                               
FILTMED  DS    C                                                                
FILTSTA  DS    CL5                                                              
FILTSD   DS    XL2                                                              
FILTED   DS    XL2                                                              
FILTSRCE DS    CL4                                                              
FILTCDAT DS    XL3                                                              
FILTSTAT DS    C                                                                
*                                                                               
FILTWKF  DS    CL7                                                              
FILTRLQ  EQU   *-FILTRS                                                         
*                                                                               
SVTWAEND DS    0X                                                               
*                                                                               
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
* SUMMARY RECORD DSECT                                                          
* IF MAKING CHANGES, DON'T FORGET TO UPDATE COLTAB AND HDRTAB!                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
SUMRECD  DSECT                                                                  
SUMUID   DS    CL10                USER ID                                      
SUMBDAT  DS    CL8                 MM/DD/YY                                     
SUMSEQ   DS    CL6                 BATCH SEQUENCE NUMBER                        
SUMSRCE  DS    CL4                 BATCH SOURCE                                 
SUMMOS   DS    CL6                 MMM/YY                                       
SUMSTA   DS    CL7                                                              
SUMINV   DS    CL10                                                             
*                                                                               
SUMSRTKLQ EQU  *-SUMRECD           SORT KEY LENGTH                              
*                                                                               
SUMCLT   DS    CL40                                                             
SUMPRD   DS    CL40                                                             
SUMEST   DS    CL20                                                             
SUMSPOTS DS    CL5                                                              
SUMGRS   DS    CL16                                                             
SUMNET   DS    CL16                                                             
SUMOTYPE DS    CL15                                                             
SUMPREP  DS    CL3                                                              
SUMSTAT  DS    C                   CONVERSION STATUS                            
SUMCDAT  DS    CL10                CONVERRT DATE MMMDD/YYYY                     
SUMMNAM  DS    CL(L'MKTNAME)       MARKET NAME                                  
SUMRECDLQ EQU  *-SUMRECD                                                        
*                                                                               
*                                                                               
*                                                                               
         DCBD  DSORG=PS,DEVD=DA                                                 
*                                                                               
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
* INCLUDES                                                                      
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DMWRKFD                                                        
       ++INCLUDE DMWRKFK                                                        
       ++INCLUDE FAXTRAINF                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE SPSNVRCRD                                                      
       ++INCLUDE DDDLCB                                                         
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE SPGENEZ                                                        
       ++INCLUDE EZBLOCK                                                        
       ++INCLUDE SPEZDSCTS                                                      
       ++INCLUDE DDWRKIOD                                                       
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE SPGENMKT                                                       
*                                                                               
MASTD    DSECT                                                                  
       ++INCLUDE DDMASTC                                                        
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'173SPEZF35   03/26/20'                                      
         END                                                                    
