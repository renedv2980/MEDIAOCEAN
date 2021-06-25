*          DATA SET TAREP0B    AT LEVEL 001 AS OF 05/18/15                      
*PHASE T7030BC,*                                                                
         TITLE 'T7030B - O+M PURCHASE ORDER ADDER'                              
*----------------------------------------------------------------------         
* THIS REPLACES TAREP02O PANBOOK AND T70302O PHASE        2015 MAY - GH         
*----------------------------------------------------------------------         
T7030B   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T7030B                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC             RC=A(CONTROLLER W/S)                         
         L     RA,ATWA                                                          
         USING T703FFD,RA          RA=A(SCREEN)                                 
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          R9=A(SYSTEM W/S)                             
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8           R8=A(SPOOL DSECT)                            
         LA    R7,BUFF                                                          
         LA    R7,8(R7)                                                         
         USING TFD,R7              R7=A(LOCAL W/S)                              
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
*                                                                               
         GOTO1 INITIAL,DMCB,0      INITIALIZE                                   
*                                                                               
         CLI   MODE,PRINTREP       PROCESS REPORT                               
         BNE   XIT                                                              
         BAS   RE,OPENDISK         OPEN THE DISK                                
         BAS   RE,PREP             PRINT THE CONTENTS                           
         BAS   RE,CLOSDISK         CLOSE IT                                     
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE CONTROLS REPORT GENERATION                               
         SPACE 1                                                                
PREP     NTR1                                                                   
         LA    R1,MYSPECS          SET A(SPECS) FOR PRINTING                    
         ST    R1,SPECS                                                         
         XC    MATCHD,MATCHD                                                    
         XC    COUNT,COUNT                                                      
         XC    FCNT,FCNT                                                        
*                                                                               
PREP010  DS    0H                                                               
         BAS   RE,GETDREC                                                       
         CLI   MYREC,X'FF'                                                      
         BE    PREPX                                                            
*                                                                               
         CLC   MYREC(2),=C'99'     IGNORE THESE                                 
         BE    PREP010                                                          
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,MYRECLN                                                     
         AHI   RF,-5               4 + 1 FOR EX                                 
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   P(0),MYREC                                                       
         BAS   RE,PRNTIT                                                        
*                                                                               
         L     RF,FCNT             INCREMENT FILE COUNTER                       
         LA    RF,1(RF)                                                         
         ST    RF,FCNT                                                          
*                                                                               
         XC    ELEMENT,ELEMENT     CLEAR ELEMENT                                
         LA    R4,ELEMENT                                                       
         LA    R2,MYREC                                                         
         USING RECD,R2                                                          
         CLC   RECID,=C'10'        PO HEADER                                    
         BE    PREP030                                                          
*        CLI   POUPDAT,C'Y'        IF PO EXISTS, DON'T BOTHER                   
*        BE    PREP010             SKIP TO NEXT PO                              
*                                                                               
         CLC   RECID,=C'20'        PO ITEM                                      
         BE    PREP050                                                          
*                                                                               
         CLC   RECID,=C'30'        PO SUMMARY                                   
         BE    PREP070                                                          
*                                                                               
         B     PREP010             IGNORE EVERYTHING ELSE                       
*                                                                               
         USING TLPUD,R3                                                         
PREP030  DS    0H                                                               
         MVI   POUPDAT,C'N'                                                     
         L     R3,AIO                                                           
         LA    RF,TLPUELEM-TLPUD                                                
         EX    RF,*+8                                                           
         B     *+10                                                             
         XC    0(0,R3),0(R3)                                                    
         MVI   TLPUCD,TLPUCDQ      X'24'                                        
         MVI   TLPUSCD,TLPUSCDQ    ACTIVE KEY                                   
         MVC   TLPUAGY,=CL6'OMNY'                                               
         MVC   TLPUPO,RECPO                                                     
         OC    TLPUPO,=CL10'0000000000'                                         
*                                                                               
         MVC   KEY,TLPUKEY                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLPUKEY),KEYSAVE                                           
         BNE   PREP040                                                          
*                                                                               
         MVC   P(25),=CL25'** UPDATE ** PO EXISTS:'                             
         MVC   P+25(6),TLPUAGY                                                  
         MVI   P+32,C'/'                                                        
         MVC   P+34(10),TLPUPO                                                  
         BAS   RE,PRNTIT                                                        
         MVI   POUPDAT,C'Y'                                                     
         GOTO1 GETREC                                                           
         MVI   ELCODE,TAPUELQ      DELETE THE OLD DETAILS                       
         GOTO1 REMELEM                                                          
*                                                                               
PREP040  MVC   SVCURR,RECCURR                                                   
         MVC   SVCPY,RECCC                                                      
         GOTO1 DATCON,DMCB,(10,RECDATE),(1,SVCDATE)                             
         LA    RF,TLPUELEM-TLPUD                                                
         STC   RF,TLPULEN                                                       
*                                                                               
         B     PREP010                                                          
*                                                                               
         USING TAPUD,R4                                                         
PREP050  DS    0H                                                               
         MVI   TAPUEL,TAPUELQ      X'44'                                        
         MVI   TAPULEN,TAPUILNQ                                                 
         MVI   TAPUTYPE,TAPUTITM   ITEM                                         
         MVC   TAPUINUM,RECPOITM   ITEM NUMBER                                  
         MVC   TAPUICOD,RECPOICD   ITEM CODE                                    
         MVC   TAPUIDES,RECPOIDS   ITEM DESCRIPTION                             
         XC    TAPUIAMT,TAPUIAMT                                                
         GOTO1 CASHVAL,DMCB,RECPOIAM,13                                         
         CLI   0(R1),X'FF'         INVALID?                                     
         BNE   PREP054             YES, LEAVE IT AS ZEROS                       
*                          1234567890123456789012345                            
         MVC   P(25),=CL25'* ERROR / ITEM AMOUNT *'                             
         MVC   P+25(6),TLPUAGY                                                  
         MVI   P+32,C'/'                                                        
         MVC   P+34(10),TLPUPO                                                  
         BAS   RE,PRNTIT                                                        
*                                                                               
PREP054  MVC   TAPUIAMT,4(R1)      ITEM AMOUNT                                  
         MVC   TAPUIQTY,=X'0001'                                                
*                                                                               
         CLC   RECPOITX,SPACES     ANY ITEM TEXT?                               
         BNH   PREP059             NO, JUST ADD THIS ONE                        
         LA    RF,L'RECPOITX                                                    
         LA    RE,RECPOITX+L'RECPOITX-1                                         
PREP055  CLI   0(RE),C' '                                                       
         BH    PREP057                                                          
         BCTR  RE,0                                                             
         BCT   RF,PREP055                                                       
*                                                                               
PREP057  BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   TAPUITX(0),RECPOITX                                              
*                                                                               
         AHI   RF,TAPUILNQ+1                                                    
         STC   RF,TAPULEN                                                       
*                                                                               
PREP059  GOTO1 ADDELEM                                                          
         B     PREP010                                                          
*                                                                               
PREP070  DS    0H                                                               
         MVI   TAPUEL,TAPUELQ      X'24'                                        
         MVI   TAPULEN,TAPUSLNQ                                                 
         MVI   TAPUTYPE,TAPUTSUM   SUMMARY                                      
         MVC   TAPUSCUR,SVCURR     CURRENCY                                     
         MVC   TAPUSCPY,SVCPY      COMPANY CODE                                 
         MVC   TAPUSCDT,SVCDATE    CREATION DATE                                
         GOTO1 CASHVAL,DMCB,RECPOSAM,13                                         
         CLI   0(R1),X'FF'         INVALID?                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   TAPUSAMT,4(R1)                                                   
         GOTO1 ADDELEM                                                          
*                                                                               
         CLI   POUPDAT,C'Y'        UPDATING RECORD?                             
         BNE   PREP080             NO, ADD THE RECORD                           
         GOTO1 PUTREC                                                           
         B     PREP090                                                          
*                                                                               
PREP080  GOTO1 ADDREC              OUTPUT NEW RECORD                            
*                                                                               
PREP090  L     R3,AIO                                                           
         SR    RF,RF                                                            
         ICM   RF,3,TLPULEN                                                     
         GOTO1 TRACE,DMCB,AIO,(RF),=C'PO RECORD',9                              
         B     PREP010                                                          
*                                                                               
PREPX    BAS   RE,PRNTIT                                                        
         MVC   P(35),=CL35'NUMBER OF RECORDS ON FILE:'                          
         EDIT  FCNT,(8,P+40)                                                    
         BAS   RE,PRNTIT                                                        
         B     XIT                                                              
         EJECT                                                                  
         EJECT                                                                  
*              ROUTINE TO PRINT A LINE                                          
PRNTIT   NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         EJECT                                                                  
*              ERRORS, EXITS                                                    
         SPACE 2                                                                
FLDINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     THEEND                                                           
         SPACE 1                                                                
THEEND   GOTO1 ERREX                                                            
         SPACE 1                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 3                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 1                                                                
ECACNT   DC    PL6'0'              TOTAL N'ECAST RECORDS ADDED                  
DUPCNT   DC    PL6'0'              N'ECAST RECORDS ALREADY ON FILE              
NREC     DC    PL6'10'             N'RECORDS TO TRACE                           
HEXFFS   DC    6X'FF'                                                           
COMPLM2  DC    X'FFFFFFFFFFFF'                                                  
RECHD1   DC    C'----+----1----+----2----+----3----+----4----+----5'            
RECHD2   DC    C'----+----6----+----7----+----8----+----9----+----0'            
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
OPENDISK NTR1                                                                   
         OPEN  (DISKIN,(INPUT))                                                 
         B     XIT                                                              
*                                                                               
CLOSDISK NTR1                                                                   
         CLOSE (DISKIN,)                                                        
         B     XIT                                                              
*                                                                               
GETDREC  NTR1                                                                   
         LA    R0,MYRECLN          GET RECORD FROM DISK                         
         GET   DISKIN,(0)                                                       
         B     XIT                                                              
*                                                                               
DISKEOF  MVI   MYREC,X'FF'                                                      
         B     XIT                                                              
         EJECT                                                                  
*              REPORT SPECS                                                     
         SPACE 1                                                                
MYSPECS  DS    0H                                                               
         SSPEC H1,2,RUN                                                         
         SSPEC H1,100,REPORT                                                    
         SSPEC H1,117,PAGE                                                      
         SSPEC H2,100,REQUESTOR                                                 
*                                                                               
         SSPEC H1,55,C'O+M PURCHASE ORDERS'                                     
         SSPEC H2,55,19X'BF'                                                    
*                                                                               
         DC    X'00'                                                            
*                                                                               
*                                                                               
DISKIN   DCB   DDNAME=DISKIN,DSORG=PS,RECFM=VB,MACRF=GM,EODAD=DISKEOF           
         EJECT                                                                  
*              DSECT TO COVER RECORD FROM DISK                                  
RECD     DSECT                                                                  
RECID    DS    CL2                                                              
*                                                                               
RECPO    DS    CL10                                                             
RECCC    DS    CL4                 C'2010'                                      
RECCURR  DS    CL3                 C'USD'                                       
RECDATE  DS    CL10                YYYY-MM-DD                                   
*                                                                               
         ORG   RECPO                                                            
RECPOITM DS    CL10                PO ITEM NUMBER                               
RECPOICD DS    CL10                PO ITEM CODE                                 
RECPOIDS DS    CL30                PO ITEM DESCRIPTION                          
RECPOIAM DS    CL13                AMOUNT                                       
RECPOIQT DS    CL5                 QUANTITY                                     
RECPOIUN DS    CL2                 C'EA'                                        
RECPOITX DS    CL50                                                             
*                                                                               
         ORG   RECPO                                                            
RECPOSAM DS    CL13                NET AMOUNT                                   
         EJECT                                                                  
*              DSECT TO COVER LOCAL WORKING STORAGE                             
         SPACE 1                                                                
TFD      DSECT                                                                  
*                                                                               
MATCHD   DS    F                                                                
COUNT    DS    F                                                                
FCNT     DS    F                                                                
TFOPTS   DS    XL1                 OPTIONS                                      
TFTRACE  EQU   X'80'               TRACE ACTIVE                                 
*                                                                               
SVTGINV  DS    CL6                                                              
BINDATE  DS    XL3                                                              
DUEDATE  DS    XL3                                                              
BILLDATE DS    XL3                                                              
CHKDATE  DS    XL3                                                              
CHKRDTE  DS    XL3                                                              
RUNDATE  DS    XL3                                                              
INVNO    DS    XL6                                                              
INVAMT   DS    F                                                                
MYGETRET DS    A                                                                
EPISTAT  DS    X                   SAVED EPISODE STATUS                         
SVEPI    DS    CL2                 SAVED EPISODE NUMBER                         
SVCURR   DS    CL1                 SAVED CURRENCY                               
SVCPY    DS    CL4                 SAVED COMPANY CODE                           
SVCDATE  DS    XL3                 SAVED CREATION DATE                          
POUPDAT  DS    C                                                                
MYBYTE   DS    CL1                                                              
PTRBLK   DS    CL(2*L'TLDRREC+1)                                                
EPITAB   DS    CL(MAXEPIS*EPITABL)   TABLE OF EPISODES AND AIR DATES            
MAXEPIS  EQU   300                   MAX N'EPISODES IN EPITAB                   
*                                                                               
MYRECLN  DS    F                                                                
MYREC    DS    2048C                                                            
TFLNQ    EQU   *-TFD                                                            
         EJECT                                                                  
*              DSECT OF EPISODE TABLE                                           
         SPACE                                                                  
EPITABD  DSECT                                                                  
EPIAGY   DS    CL6                 AGENCY                                       
EPINUM   DS    XL2                 EPISODE NUM                                  
EPIADT   DS    XL3                 AIR DATE                                     
EPITABL  EQU   *-EPITABD                                                        
         EJECT                                                                  
       ++INCLUDE TAREPFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPEAD                                                       
         SPACE 3                                                                
         PRINT OFF                                                              
* DDGENTWA   (MUST FOLLOW LAST SCREEN)                                          
* DDPERVAL                                                                      
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* DDBIGBOX                                                                      
* TAGENFILE                                                                     
* TASYSDSECT                                                                    
* TASYSEQUS                                                                     
* TAREPWORKD                                                                    
* DDCOMFACSD                                                                    
* DDGETRETD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAREPWORKD                                                     
       ++INCLUDE DDCOMFACSD                                                     
       ++INCLUDE DDGETRETD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001TAREP0B   05/18/15'                                      
         END                                                                    
