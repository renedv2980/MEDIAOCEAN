*          DATA SET PPREPCH02  AT LEVEL 019 AS OF 07/18/18                      
*PHASE PPCH02A                                                                  
*INCLUDE GETUSER                                                                
*INCLUDE DDUCOM                                                                 
*INCLUDE PPFMTINO                                                               
*INCLUDE PPBVAL                                                                 
*INCLUDE PERVAL                                                                 
         TITLE 'PPCH02 - CHOICE HOTELS INTERFACE'                               
*                                                                               
*        CHANGE LOG                                                             
*********************************************************************           
* USER    JIRA       DATE                  CHANGE LOG                           
* ---- ----------  -------- -----------------------------------------           
* SMUR SPEC-11729  05/15/18 NEW (D)IGITAL AUDIO (18.3)                          
*                                                                               
* MAR/16 AKAT - SUPPORT MEDIA O (OUTDOOR)                                       
*                                                                               
* MAR/13 BPLA - AGENCY ADDRESS CHANGE                                           
*                                                                               
* OCT/13 BPLA - REMOVE *INCLUDE CPUINFO AND SKIP ZERO INVOICES                  
*                                                                               
* OCT/13 BPLA - RE-LINK NEW MQRPT WITH CPUINFO                                  
*                                                                               
* MAY/13 BPLA - CHANGES FOR SENDING VIA THE HUB                                 
*                                                                               
* SEP/14 BPLA - CHANGE TO CUSTOMER ID                                           
*                                                                               
*        QOPT6 Y= TEST RUN - NO TAPE, AND CONTINUE IF ERRORS                    
*                            ARE FOUND                                          
*        QOPT7 N= NO MQ NOTIFICATION                                            
*              T= REST MQ NOTIFICATION                                          
*              Y= PDUMP RECORDS                                                 
*              P= PRODUCTION MQ NOTIFICATION                                    
*                                                                               
*        QSTART(6) = PERIOR START                                               
*        QEND(6) = PERIOD END - MAY BE BLANK                                    
*                                                                               
*        NOTE: PROGRAM USES THE BRAND'S ADDRESS DATA                            
*              NOT THE ONES FROM PRODUCT AAA (LIKE THE SE PROGRAM)              
*              IT ALSO READS BILLS  - NOT INSERTIONS                            
*                                                                               
PPCH02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PPCH02,RR=R9                                                   
*                                                                               
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         MVI   RC2DSECT,C'Y'      2ND DSECT                                     
         L     R9,PPWORK2C                                                      
         USING PPWORK2D,R9                                                      
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         LA    R8,SPACEND                                                       
         USING PPCHWRKD,R8                                                      
         MVC   ADMASTC,VMASTC     SAVE VMASTC                                   
         MVC   ACONIO1,ACONIO                                                   
*                                                                               
         LA    R7,PPCH02+4095                                                   
         LA    R7,1(R7)                                                         
         USING PPCH02+4096,R7     **NOTE USE OF R7 AS BASE REG*                 
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    INITIAL                                                          
         CLI   MODE,REQFRST                                                     
         BE    FIRSTB                                                           
         CLI   MODE,FBUYCLI                                                     
         BE    FCLI                                                             
         CLI   MODE,FBILEST                                                     
         BE    FEST                                                             
         CLI   MODE,FBUYPRO                                                     
         BE    FPRD                                                             
         CLI   MODE,REQLAST                                                     
         BE    PUTBUFF                                                          
         CLI   MODE,RUNLAST                                                     
         BE    TOTALS                                                           
         CLI   MODE,PROCBIL                                                     
         BE    PROCESS                                                          
*                                                                               
         CLI   MODE,CLILAST       END OF CLIENT                                 
         BE    LCLI                                                             
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*                         RUN FIRST                                             
INITIAL  DS    0H                                                               
*                                                                               
         L     R0,=V(GETUSER)                                                   
         A     R0,RELO                                                          
         ST    R0,VGETUSER                                                      
         L     R0,=V(DDUCOM)                                                    
         A     R0,RELO                                                          
         ST    R0,VDDUCOM                                                       
         L     R0,=V(PPFMTINO)                                                  
         A     R0,RELO                                                          
         ST    R0,AFMTINO                                                       
         L     R0,=V(PERVAL)                                                    
         A     R0,RELO                                                          
         ST    R0,APERVAL                                                       
         L     R0,=V(PPBVAL)                                                    
         A     R0,RELO                                                          
         ST    R0,APPBVAL                                                       
*                                                                               
         L     RF,ADMASTC            USE MASTC'S AGYID                          
         USING MASTD,RF                                                         
         MVC   AMQRPT,MCVMQRPT                                                  
         DROP  RF                                                               
*                                                                               
*                                                                               
         XC    MYDUMP,MYDUMP                                                    
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 DATCON,DMCB,(5,0),(0,TODAY1)       YYMMDD                        
         GOTO1 DATCON,DMCB,(5,0),(X'14',TODAYY)   YYYYMMDD                      
         ZAP   TOTCNT,=P'0'                                                     
         MVI   COLSW,C'N'                                                       
         MVI   IHDRSW,C'N'                                                      
         MVI   FRSTBILL,C'N'                                                    
         MVI   LASTBILL,C'N'                                                    
         ZAP   INVTOTD,=P'0'                                                    
         ZAP   INVRCNT,=P'0'                                                    
*                                                                               
         MVI   CHOPENSW,C'N'      SET BK FILE NOT OPEN                          
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(20,CTODAY) TODAY - YYYYMMDD                   
*                                                                               
*        GET TIME OF DAY                                                        
         TIME                                                                   
*                                                                               
*        R0 NOW HAS TIME HHMMSSHS  (PWOS)                                       
*                                                                               
         ST    R0,FULL                                                          
         SRL   R0,4                                                             
         ST    R0,MYFULL                                                        
         XC    DUB,DUB                                                          
         MVC   DUB+5(3),MYFULL                                                  
         OI    DUB+7,X'0F'                                                      
         CVB   R6,DUB                                                           
         EDIT  (R6),(5,TIMEOFD),2,FILL=0                                        
         MVI   TIMEOFD+5,C'.'                                                   
         UNPK  WORK(3),FULL+2(2)                                                
         MVC   TIMEOFD+6(2),WORK     HH.MM.SS                                   
*                                                                               
         MVI   ZEROS,C'0'                                                       
         MVC   ZEROS+1(L'ZEROS-1),ZEROS                                         
*                                                                               
         MVI   DMINBTS,X'08'                                                    
         MVI   DMOUTBTS,X'FD'                                                   
         MVI   TAPESW,0                                                         
         MVI   ETAPESW,0                                                        
         MVI   ALLOWSW,0     DYNAMIC ALLOCATION INV. FILE NOT DONE              
         MVI   CHSFTP,C'Y'   DOING SFTP FILE                                    
*                                                                               
INIT5    DS    0H                                                               
         L     R0,=A(TITLES)                                                    
         A     R0,RELO                                                          
         ST    R0,ATITLES                                                       
*                                                                               
         L     R0,=A(LENTAB)                                                    
         A     R0,RELO                                                          
         ST    R0,ALENTAB                                                       
*                                                                               
         L     R2,ALENTAB          ZAP ACCUMS                                   
         LA    R3,2                                                             
INIT7L   ZAP   4(4,R2),=P'0'                                                    
         LA    R2,8(R2)                                                         
         BCT   R3,INIT7L                                                        
*                                                                               
INIT70   DS    0H                                                               
**                                                                              
INITX    B     EXIT                                                             
         EJECT                                                                  
*                       REQUEST FIRST                                           
FIRSTB   DS    0H                                                               
*                                                                               
         LA    RF,PCLTREC                                                       
         ST    RF,ADCLT                                                         
         LA    RF,PPRDREC                                                       
         ST    RF,ADPRD                                                         
         LA    RF,PESTREC                                                       
         ST    RF,ADEST                                                         
*                                                                               
         MVI   TESTMQ,C'P'         SET TO PROD MQ NOTIFICATION                  
         CLI   QOPT7,C'N'                                                       
         BNE   *+8                                                              
         MVI   TESTMQ,C'N'         SUPPRESS MQ                                  
         CLI   QOPT7,C'Y'          TRACING OUTPUT RECORDS                       
         BNE   *+8                 ALSO SUPPRESS MQ                             
         MVI   TESTMQ,C'N'         SUPPRESS MQ                                  
         CLI   QOPT7,C'T'                                                       
         BNE   *+8                                                              
         MVI   TESTMQ,C'T'         TEST MQ                                      
*                                                                               
REQF10   DS    0H                                                               
         XC    LBILLKEY,LBILLKEY                                                
         XC    LESTOUT,LESTOUT                                                  
         MVC   SVQOPT1,QOPT1      SAVE FILE TYPE                                
         MVC   SVQOPT6,QOPT6      SAVE DO TAPE OPTION                           
         MVC   SVQOPT7,QOPT7      SAVE PDUMPING OPTION                          
*                                                                               
         CLC   QEST(2),=C'NO'                                                   
         BE    REQF65                                                           
         CLC   QEST(3),=C'ALL'                                                  
         BE    REQF65                                                           
*                                                                               
REQF50   CLC   QEST(6),SPACES                                                   
         BNE   REQF55                                                           
         B     REQF65                                                           
REQF55   CLC   QESTEND,SPACES                                                   
         BNE   REQF60                                                           
         MVC   QESTEND,QEST      IF ONLY ONE EST SET QESTEND TO IT              
REQF60   PACK  DUB,QEST                                                         
         CVB   R0,DUB                                                           
         STH   R0,MYBEST                                                        
         PACK  DUB,QESTEND                                                      
         CVB   R0,DUB                                                           
         STH   R0,MYBESTE          NEED TO SET NOW FOR TBCLTF                   
         B     REQF70                                                           
*                                                                               
REQF65   MVC   MYBEST,=H'1'                                                     
         MVC   MYBESTE,=H'999'     MAX ESTIMATE                                 
REQF70   DS    0H                                                               
*                                                                               
         CLI   QOPT7,C' '        IF EMPTY, SET FOR PROD                         
         BNE   *+8                                                              
         MVI   QOPT7,C'P'        SET FOR PROD MQ                                
*                                                                               
         CLI   QOPT6,C'Y'        SEE IF TEST REQUEST                            
         BE    FIRSTB0                                                          
         CLI   TAPESW,C'N'       SEE IF A PRIOR REQUEST WAS TEST                
         BE    MIXERR                                                           
         MVI   TAPESW,C'Y'       SET TAPE BEING PRODUCED                        
*                                                                               
*                                                                               
REQF13B6 CLI   CHOPENSW,C'Y'    IS CH FILE ALREADY OPEN?                        
         BE    REQF13M          IF SO, SKIP CODE BELOW                          
*                                                                               
         MVC   DSNAME,SPACES                                                    
         MVC   DSNAME+0(4),=C'BIL.'                                             
         MVC   DSNAME+4(3),=C'PRT'                                              
         MVI   DSNAME+7,C'.'                                                    
         L     RF,ADMASTC            USE MASTC'S AGYID                          
         USING MASTD,RF                                                         
         L     R1,MCAEXTRA                                                      
         MVC   DSNAME+8(4),MCAGYCOD-MCEXTRA(R1)                                 
         DROP  RF                                                               
*                                                                               
         MVC   DSNAME+12(2),=C'.D'                                              
         MVC   DSNAME+14(6),CTODAY+2    YYMMDD                                  
         MVC   DSNAME+20(2),=C'.T'                                              
         MVC   DSNAME+22(2),TIMEOFD         WITHOUT .'S                         
         MVC   DSNAME+24(2),TIMEOFD+3                                           
         MVC   DSNAME+26(2),TIMEOFD+6                                           
         MVC   MQMAPNM,=C'SFTPDISK.PROD.'                                       
         CLI   QOPT7,C'P'   PROD RUN                                            
         BE    REQF13K                                                          
         CLI   QOPT7,C'Y'                                                       
         BE    REQF13H                                                          
         CLI   QOPT7,C'N'   NO MQ NOTIFICATION                                  
         BE    REQF13H      ALSO PUT 'TEST' IN MQMAPNM                          
         CLI   TESTMQ,C'T'  PUTTING TO TEST BROKER?                             
         BNE   *+10                                                             
REQF13H  MVC   MQMAPNM+9(4),=C'TEST'                                            
*                                                                               
REQF13K  MVI   BYTE,X'45'         X'04' = BIG NAMES                             
         MVC   DUB,=X'000005000001'                                             
         CLI   CHSFTP,C'Y'        SEE IF DOING CH SFTP FILE                     
         BNE   REQF13M                                                          
         GOTO1 DYNALLOC,DMCB,(X'80',=C'PCHTAPE '),(BYTE,DUB),          X        
               (X'80',MQMAPNM)                                                  
*                                                                               
         OPEN  (PCHTAPE,(OUTPUT))                                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVI   CHOPENSW,C'Y'     SET BK FILE OPEN                               
*                                                                               
REQF13M  DS   0H                                                                
*                                                                               
REQF75   B     FIRSTB0X                                                         
*                                                                               
FIRSTB0  DS    0H                                                               
         MVI   QOPT7,C'N'       TEST RUN - NO MQ MESSAGE                        
         CLI   TAPESW,C'Y'      SEE IF A PRIOR REQUEST WAS LIVE                 
         BE    MIXERR                                                           
         MVI   TAPESW,C'N'                                                      
         B     FIRSTB0X                                                         
*                                                                               
MIXERR   MVC   P1(37),=C'*** MIX OF TEST AND LIVE REQUESTS ***'                 
         MVC   P2(37),=C'*** THIS REQUEST HAS BEEN SKIPPED ***'                 
         GOTO1 REPORT                                                           
         MVI   MODE,REQLAST    SKIP TO NEXT REQUEST                             
         B     EXIT                                                             
*                                                                               
FIRSTB0X DS    0H                                                               
*                             SET MYUSER FROM AGYTAB                            
         LA    R1,AGYTAB                                                        
FIRSTB1  CLI   0(R1),X'FF'            END OF TABLE                              
         BNE   *+6                                                              
         DC    H'0'                   INVALID AGENCY                            
*                                                                               
         CLC   0(2,R1),QAGENCY                                                  
         BE    FIRSTB1D                                                         
         LA    R1,AGYTABL(R1)                                                   
         B     FIRSTB1                                                          
*                                                                               
FIRSTB1D MVC   MYUSER,2(R1)         USER FIELDS IN USE                          
*                                                                               
FIRSTB2  DS    0H                                                               
         XC    ESTU1,ESTU1          CLEAR USER FIELDS                           
         XC    ESTU2,ESTU2                                                      
         XC    PRDU1,PRDU1                                                      
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         ZAP   CINVGRS,=P'0'                                                    
         ZAP   CINVBIL,=P'0'                                                    
         ZAP   CINVCD,=P'0'                                                     
         ZAP   CINVRCV,=P'0'                                                    
         MVI   CINVSW,0                                                         
*                                                                               
*                                                                               
         CLC   QEND,SPACES                                                      
         BNE   FIRSTB3C                                                         
         MVC   QEND(6),QSTART     SET END TO START,IF NOT ENTERED               
*                                                                               
FIRSTB3C DS    0H                                                               
         MVC   SVSTART(12),QSTART                                               
*                                                                               
FIRSTB3X LA    R0,BUFREC                                                        
         ST    R0,BUFFIO                                                        
         L     R0,=A(BUFFALOC)                                                  
         A     R0,RELO                                                          
         ST    R0,BUFFBUFF                                                      
         GOTO1 BUFFALO,DMCB,=C'SET',BUFFBUFF                                    
         B     EXIT                                                             
*                                                                               
FIRSTB4  DS    0H                                                               
FIRSTBX  B     EXIT                                                             
*                                                                               
         EJECT                                                                  
*                                  LAST FOR CLIENT                              
LCLI     DS    0H                                                               
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
*                                   FIRST FOR CLIENT                            
FCLI     DS    0H                                                               
*                                                                               
         BAS   RE,FNDAAA      FIND PRD AAA AND STORE ITS ADDRESS                
*                                                                               
         XC    B1PROF,B1PROF        FIRST READ B1 AND B1X PROFILES              
         XC    B1XPROF,B1XPROF      B1X PROFILE                                 
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'POB1'                                                 
         MVC   WORK+4(2),PCLTKAGY                                               
         MVC   WORK+6(1),PCLTKMED                                               
         MVC   WORK+7(3),PCLTKCLT                                               
         CLI   PCLTOFF,C' '                                                     
         BNH   FBC1                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),PCLTOFF                                               
*                                                                               
FBC1     DS    0H                                                               
         GOTO1 GETPROF,DMCB,WORK,B1PROF,DATAMGR                                 
         MVC   WORK(4),=C'PB1X'                                                 
         NI    WORK,X'BF'          MAKE SYS LOWER CASE FOR PAGE A               
         GOTO1 GETPROF,DMCB,WORK,B1XPROF,DATAMGR                                
*                                                                               
         XC    LINVFULL,LINVFULL      CLEAR FOR NEW REQ                         
         XC    LBQDATE,LBQDATE                                                  
         MVI   FRSTBILL,C'Y'                                                    
         MVI   LASTBILL,C'N'                                                    
         ZAP   INVTOTD,=P'0'     CLEAR TOTAL AMOUNT DUE                         
         XC    INVTAB,INVTAB     CLEAR INVOICE DETAIL TABLE                     
*                                                                               
TBCF60   DS    0H                                                               
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
*                                                                               
FPRD     DS    0H                  FIRST BUY FOR PRODUCT                        
*                                                                               
*                                      COMPANY AND OFFICE CODE                  
*                                      FROM PRODUCT INTERFACE                   
*                                                                               
         MVC   SVPRDIFC,SPACES                                                  
         LA    R2,PPRDREC+33                                                    
         USING PPRDICEL,R2                                                      
         MVI   ELCODE,X'30'      LOOK FOR INTERFACE CODE ELEMENT                
         BAS   RE,NEXTEL                                                        
         BNE   FPRD5                                                            
         MVC   SVPRDIFC,PPRDINFC    SAVE FIRST 4 CHARACTERS                     
*                                                                               
         DROP  R2                                                               
*                                                                               
FPRD5    DS    0H                SAVE BRAND'S ADDRESSES                         
         MVC   BPRDBILL,PPRDBILL                                                
         MVC   BPRDBIL2,PPRDBIL2                                                
         MVC   BPRDLIN1,PPRDLIN1                                                
****     MVC   BPRDLIN1,SPACES    NOT USED FOR CHOICE HOTELS NEXTEL             
*                                 SINCE LINE ONE IS A P.O. BOX                  
         MVC   BPRDLIN2,PPRDLIN2                                                
         MVC   BPRDATTN,PPRDATTN                                                
*                                                                               
         XC    PRDU1,PRDU1                                                      
         CLI   MYUSER,C'Y'        SEE IF USING USER FIELDS                      
         BNE   FPRDSAVE                                                         
         CLC   PPRDKPRD,=C'AAA'    NOT FOR PRD=AAA                              
         BE    FPRDSAVE                                                         
*                                                                               
         GOTO1 VGETUSER,DMCB,(C'P',ADCLT),(C'P',ADPRD),(C':',PRDU1),0           
         CLI   DMCB,X'FF'                                                       
         BE    FPRDERR                                                          
         CLI   PRDU1+21,C' '    MUST FIND DATA                                  
         BNH   FPRDERR                                                          
         B     FPRDSAVE                                                         
*                                                                               
FPRDERR  DS    0H                                                               
         B     FPRDSAVE                                                         
*                                                                               
*        NOT AN ERROR FOR CHOICE HOTELS                                         
*                                                                               
****     MVC   P1,SPACES                                                        
****     MVC   P1(36),=C'*** MISSING PRODUCT USER FIELD 1 ***'                  
****     MVC   P1+40(3),PPRDKPRD                                                
****     MVI   SPACING,2                                                        
****     GOTO1 REPORT                                                           
****     CLI   QOPT6,C'Y'            SEE IF TEST RUN                            
****     BE    FPRDSAVE              CONTINUE - ELSE DIE                        
****     MVC   P1(23),=C'*** REQUEST STOPPED ***'                               
****     GOTO1 REPORT                                                           
****     DC    H'0'              MUST DIE                                       
*                                                                               
FPRDSAVE DS    0H                                                               
*                                                                               
         CLC   PPRDKPRD,=C'AAA'                                                 
         BNE   FPRDFORM                                                         
*                                                                               
         MVI   AAAPRD,C'Y'                                                      
         MVC   AAAFORMU(5),PPRDBILP         MOVE FORMULA FOR AAA                
         B     FPRDX                                                            
*                                                                               
FPRDFORM DS    0H                                                               
         MVC   PRDFORMU(5),PPRDBILP                                             
*                                                                               
*                                                                               
FPRDX    B     EXIT                                                             
         EJECT                                                                  
*                                                                               
FEST     DS    0H                  ESTIMATE FIRST                               
         XC    PREGREC,PREGREC     CLEAR REGION RECORD                          
         CLI   QOPT1,C'E'          ESTIMATE FILE?                               
         BE    FESTX               NOTHINGTO DO                                 
*                                                                               
FEST0    DS    0H                                                               
*                                                                               
         XC    ESTU1,ESTU1       CLEAR EST USER FIELDS                          
         XC    ESTU2,ESTU2                                                      
         GOTO1 VGETUSER,DMCB,(C'P',ADCLT),(C'E',ADEST),(C':',ESTU1),(C'+        
               :',ESTU2)                                                        
*                                                                               
FESTX    B     EXIT                                                             
*                                                                               
         EJECT                                                                  
PUTBUFF  DS    0H      FIRST PRINT TOTAL LINE FOR CURRENT INVS                  
         TM    CINVSW,1                                                         
         BZ    PUTB2                                                            
         MVI   RCSUBPRG,10                                                      
         BAS   RE,MYRPT                                                         
         MVC   P1+28(7),=C'*TOTAL*'                                             
         EDIT  (P8,CINVGRS),(14,P1+37),2,COMMAS=YES,FLOAT=-                     
         EDIT  (P8,CINVBIL),(14,P1+53),2,COMMAS=YES,FLOAT=-                     
         EDIT  (P8,CINVCD),(14,P1+69),2,COMMAS=YES,FLOAT=-                      
         EDIT  (P8,CINVRCV),(14,P1+85),2,COMMAS=YES,FLOAT=-                     
         MVI   P1+51,C'*'                                                       
         MVI   P1+67,C'*'                                                       
         MVI   P1+83,C'*'                                                       
         MVI   P1+99,C'*'                                                       
         BAS   RE,MYRPT                                                         
PUTB2    MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,20                                                      
*                                  PUT BUFFALO RECS TO TAPE                     
*                                  AT LBUYREQ                                   
         ZAP   GTTOTCD,=P'0'                                                    
         ZAP   GTTOTAMT,=P'0'                                                   
         ZAP   GTTOTCOM,=P'0'                                                   
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
TOTALS   DS    0H                                                               
*                                                                               
TOT0     DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,0                                                       
         L     R4,ATITLES                                                       
         L     R3,ALENTAB                                                       
         LA    R6,2                FOR BCT                                      
TOT2     MVC   P1+7(30),0(R4)                                                   
         EDIT  (P4,4(R3)),(9,P1+40),0,COMMAS=YES                                
         BAS   RE,MYRPT                                                         
         LA    R4,30(R4)                                                        
         LA    R3,8(R3)                                                         
         BCT   R6,TOT2                                                          
         BAS   RE,MYRPT            SKIP A LINE                                  
         MVC   P1+17(13),=C'TOTAL RECORDS'                                      
         EDIT  TOTCNT,(9,P1+40),0,COMMAS=YES                                    
         MVI   P1+49,C'*'                                                       
         BAS   RE,MYRPT                                                         
*                                                                               
         CLI   TAPESW,C'Y'          SEE IF PRODUCING INV. TAPE                  
         BNE   TOT5                                                             
         CLOSE (PCHTAPE,)                                                       
*                                                                               
RUNLBK2  DS    0H                                                               
         CLI   TESTMQ,C'N'       SEE IF SUPRESSING MQ NOTIFICATION              
         BE    RUNLBK5                                                          
* SEND MQ MESSAGE WITH FILE NAME                                                
         LA    R5,ELEM                                                          
         USING MQMSGD,R5                                                        
         MVI   ELEM,C' '                                                        
         MVC   ELEM+1(MQMSGLNQ-1),ELEM                                          
         LA    R1,MQMAPNM                                                       
         MVC   MQFILE(34),14(R1)   BIL.SYS.AGID.DYYMMDD.THHMMSS                 
*                                  SYS=SYSTEM,AGID= 4 CHARACTER AGY ID          
*                          14(R1) TO GET PAST SPTPDISK.PROD (OR TEST)           
         MVC   MQDATE(6),28(R1)    YYMMDD OF FILE NAME                          
         MVC   MQTIME(6),36(R1)    HHMMSS OF FILE NAME                          
*                                                                               
         BRAS  RE,MQOPEN                                                        
*                                                                               
         MVC   MQHID,=CL6'DANOT1'                                               
         MVC   MQSYS,MQFILE+4  SYSTEM (+4 PAST BIL.)                            
*                                                                               
         MVC   MQQUAL(07),=C'BILLING'                                           
RUNLBK2B L     RF,ADMASTC                                                       
         L     RF,MCAEXTRA-MASTD(RF)                                            
         MVC   MQAGYID,MCAGYCOD-MCEXTRA(RF)                                     
*                                                                               
RUNLBK4  GOTO1 AMQRPT,DMCB,(0,=C'PUT'),ELEM,MQMSGLNQ,0                          
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DCHO                                                                   
*                                                                               
         GOTO1 AMQRPT,DMCB,(0,=C'CLOSE'),0,0,0                                  
         CLI   DMCB+8,0                                                         
         BE    RUNLBK5                                                          
         DCHO                CLOSE ERROR                                        
*                                                                               
RUNLBK5  MVC   P1+1(49),MQMAPNM       WHOLE FILE NAME                           
         BRAS  RE,MYRPT                                                         
         B     EXIT                                                             
*                                                                               
         DROP  R5                                                               
*                                                                               
TOT5     B     EXIT                                                             
*                                                                               
         EJECT                                                                  
PROCESS  DS    0H       PROCESS BILL                                            
*                                                                               
         CP    PBILLRCV,=P'0'      ZKIP ZERO BILLS                              
         BE    EXIT                                                             
*                                                                               
         TM    PBILCMSW,X'02'      IS IT A COMMISION ONLY BILL                  
         BNZ   PRB09               YES, OK                                      
         CLI   QOPT6,C'C'          NO, ARE WE SKIPPING OTHERS                   
         BE    EXIT                                                             
         B     PRB09A                                                           
*                                                                               
PRB09    DS    0H                                                               
         CLI   QOPT6,C'N'          EXCLUDING COMMISSION ONLY?                   
         BE    EXIT                                                             
PRB09A   DS    0H                                                               
*                                                                               
         CLI   QOPT6,C' '            CHECK AOR OPTION                           
         BE    PRB09D                                                           
         CLI   QOPT6,C'A'            ONLY AOR                                   
         BNE   PRB09B                                                           
         TM    PBILCMSW,X'20'                                                   
         BNO   EXIT                                                             
         B     PRB09D                                                           
*                                                                               
PRB09B   CLI   QOPT6,C'B'            AOR AND AOR/CLIENT                         
         BNE   PRB09C                                                           
         TM    PBILCMSW,X'30'                                                   
         BNO   EXIT                                                             
         B     PRB09D                                                           
*                                                                               
PRB09C   CLI   QOPT6,C'X'            SEE IF EXCLUDING AOR                       
         BNE   PRB09D                                                           
         TM    PBILCMSW,X'20'                                                   
         BO    EXIT                                                             
*                                                                               
PRB09D   DS    0H                                                               
*                                                                               
         CLC   PBILLDAT,QSTART     DATES FILTER                                 
         BL    EXIT                                                             
         CLI   QEND,C' '                                                        
         BE    PRB09D5                                                          
         CLC   PBILLDAT,QEND                                                    
         BH    EXIT                                                             
*                                                                               
PRB09D5  DS    0H                   MUST BE SURE ESTIMATE IS THERE              
         MVC   WORK(12),PBILLREC                                                
         MVI   WORK+3,7                                                         
         CLC   PESTREC(12),WORK        TEST HAVE EST HDR                        
         BE    PRB09D6             YES                                          
         MVC   WORK(64),KEY        SAVE KEY FOR SEQ                             
         XC    KEY,KEY                                                          
         MVC   KEY(12),PBILLREC                                                 
         MVI   KEY+3,7                                                          
         GOTO1 READ                                                             
         GOTO1 GETEST                                                           
         MVC   KEY(64),WORK                                                     
         GOTO1 HIGH                                                             
*                                                                               
PRB09D6  DS    0H                                                               
*                                                                               
         XC    ESTU1,ESTU1       CLEAR EST USER FIELDS                          
         XC    ESTU2,ESTU2                                                      
         GOTO1 VGETUSER,DMCB,(C'P',ADCLT),(C'E',ADEST),(C':',ESTU1),(C'+        
               :',ESTU2)                                                        
*                                                                               
PRB09DX  GOTO1 =V(PPBVAL),DMCB,(C'B',PBILLREC),PPBVALD                          
*****                                                                           
*****    SET EFFECTIVE VALUES INTO PBILLREC                                     
*****                                                                           
         MVC   PBILLGRS,PPBVEBG                                                 
         MVC   PBILLBIL,PPBVEBB                                                 
         MVC   PBILLNET,PPBVEBN                                                 
*                                 SET MYGST AND MYPST AND MYHST                 
*****    ZAP   MYGST,=P'0'                                                      
*****    ZAP   MYPST,=P'0'                                                      
         L     R0,PPBVGST                                                       
         CVD   R0,DUB                                                           
         ZAP   MYGST,DUB                                                        
         L     R0,PPBVPST                                                       
         CVD   R0,DUB                                                           
         ZAP   MYPST,DUB                                                        
         L     R0,PPBVHST                                                       
         CVD   R0,DUB                                                           
         ZAP   MYHST,DUB                                                        
*                                                                               
         GOTO1 =A(BLDREC)                                                       
         B     EXIT            DONE WITH THIS BILL                              
*                                                                               
NEXTEL   DS    0H                                                               
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    NEXTEL2                                                          
         CLC   ELCODE1,0(R2)                                                    
         BER   RE                                                               
         B     NEXTEL                                                           
*                                                                               
NEXTEL2  DS    0H                                                               
         LTR   R2,R2                                                            
         BR    RE                  SET CC NOT EQ                                
         SPACE 2                                                                
*                                                                               
         EJECT                                                                  
*                                                                               
FNDAAA   NTR1                                                                   
         MVC   APRDBILL(124),SPACES                                             
*                                                                               
         MVC   PPGKEY,KEY                                                       
         XC    KEY,KEY                                                          
         MVC   KEY(7),PCLTKAGY     AGY/MED/CODE/CLT                             
         MVI   KEY+3,X'06'                                                      
         MVC   KEY+7(3),=C'AAA'                                                 
         CLC   PPRDKAGY(7),KEY                                                  
         BE    FNDP10                                                           
         GOTO1 HIGH                                                             
         CLC   KEY(10),KEYSAVE                                                  
         BE    FNDP08                                                           
         B     FNDAX      PRODUCT AAA NOT ON FILE                               
*                                                                               
FNDP08   LA    RE,PPRDREC                                                       
         ST    RE,AREC                                                          
         GOTO1 GETPRT                                                           
*                                                                               
FNDP10   MVC   APRDBILL,PPRDBILL                                                
         MVC   APRDBIL2,PPRDBIL2                                                
         MVC   APRDLIN1,PPRDLIN1                                                
         MVC   APRDLIN1,SPACES    NOT USED FOR CHOICE HOTELS NEXTEL             
*                                 SINCE LINE ONE IS A P.O. BOX                  
         MVC   APRDLIN2,PPRDLIN2                                                
         MVC   APRDATTN,PPRDATTN                                                
*                                                                               
FNDAX    MVC   KEY,PPGKEY                                                       
         GOTO1 HIGH                                                             
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*        AGENCY TABLE                                                           
*                                                                               
*        AGENCY CODE/VENDOR/COMPANY/MYUSER VALUE                                
*                                                                               
*                                                                               
AGYTAB   DC    C'FM',C'Y'        FMNY                                           
         DC    C'SJ',C'Y'        SJR                                            
         DC    X'FFFF'                                                          
*                                                                               
AGYTABL  EQU   3                                                                
         EJECT                                                                  
*                                                                               
         DS    1200C   SO THAT MYPRT AND MWRITE CAN BE ACCESSED                 
         PRINT GEN                                                              
**VBAPE  DCB   DDNAME=PCHTAPE,DSORG=PS,RECFM=VB,LRECL=01200,                    
**VB           BLKSIZE=06004,MACRF=PM                                           
PCHTAPE  DCB   DDNAME=PCHTAPE,DSORG=PS,RECFM=FB,LRECL=01000,           X        
               BLKSIZE=01000,MACRF=PM                                           
         PRINT NOGEN                                                            
*                      BY OTHER ROUTINES                                        
MYRPT    NTR1                                                                   
*                                                                               
         CLI   QOPT6,C'Y'              SEE IF TEST RUN                          
         BE    MYRPT2                                                           
         CLI   SVQOPT6,C'Y'            SEE IF TEST RUN                          
         BNE   *+10                                                             
*                                                                               
MYRPT2   MVC   HEAD4+50(12),=C'**TEST RUN**'                                    
         CLI   MODE,RUNLAST                                                     
         BNE   *+10                                                             
         MVC   QSTART(12),SVSTART                                               
*                                                                               
MYRPT5   GOTO1 REPORT                                                           
         XIT1                                                                   
*                                                                               
*                          AND BLDREC AS IT'S NOW COVERED BY R7 - THE           
*                          SECOND BASE REGISTER                                 
         EJECT                                                                  
MWRITE   NTR1                      FIND RECORD LENGHT IN LENTAB                 
*                                                                               
         L     R1,ALENTAB                                                       
MWRITE4  CLC   0(2,R1),RECTYPE                                                  
         BE    MWRITE5                                                          
         LA    R1,8(R1)                                                         
         CLI   0(R1),X'FF'                                                      
         BNE   MWRITE4                                                          
         DC    H'0'                UNKNOWN TYPE                                 
*                                                                               
MWRITE5  MVC   HALF,2(R1)                                                       
         AP    4(4,R1),=P'1'                                                    
****     LH    R3,HALF         DON'T SET LENGTH FROM TABLE                      
****     LA    R3,4(R3)                                                         
****     STH   R3,OUTREC-4                                                      
         CLI   SVQOPT7,C'Y'                                                     
         BE    WRIT1                                                            
         CLI   QOPT7,C'Y'                                                       
         BNE   WRIT2                                                            
WRIT1    MVI   RCSUBPRG,10                                                      
         MVC   P1+1(125),OUTREC                                                 
         MVC   P2+1(125),OUTREC+125                                             
         MVC   P3+1(125),OUTREC+250                                             
         MVC   P4+1(125),OUTREC+375                                             
         MVC   P5+1(125),OUTREC+500                                             
         MVC   P6+1(125),OUTREC+625                                             
         MVC   P7+1(125),OUTREC+750                                             
         MVC   P8+1(125),OUTREC+875                                             
         MVI   P3,0       SO LINE WILL ALWAYS PRINT                             
         MVI   P4,0       SO LINE WILL ALWAYS PRINT                             
         MVI   P5,0       SO LINE WILL ALWAYS PRINT                             
         MVI   P6,0       SO LINE WILL ALWAYS PRINT                             
         MVI   P7,0       SO LINE WILL ALWAYS PRINT                             
         MVI   P8,0       SO LINE WILL ALWAYS PRINT                             
*                                                                               
WRIT1A   MVI   SPACING,2                                                        
         BAS   RE,MYRPT                                                         
         MVI   RCSUBPRG,10                                                      
**VB     GOTO1 HEXOUT,DMCB,OUTREC-4,P1+10,54,=C'N'                              
         GOTO1 HEXOUT,DMCB,OUTREC,P1+18,50,=C'N'                                
         GOTO1 HEXOUT,(R1),OUTREC+50,P2+18,50,=C'N'                             
         GOTO1 (RF),(R1),OUTREC+100,P3+18,50,=C'N'                              
         GOTO1 (RF),(R1),OUTREC+150,P4+18,50,=C'N'                              
         GOTO1 (RF),(R1),OUTREC+200,P5+18,50,=C'N'                              
         GOTO1 (RF),(R1),OUTREC+250,P6+18,50,=C'N'                              
         GOTO1 (RF),(R1),OUTREC+300,P7+18,50,=C'N'                              
         GOTO1 (RF),(R1),OUTREC+350,P8+18,50,=C'N'                              
         GOTO1 (RF),(R1),OUTREC+400,P9+18,50,=C'N'                              
         GOTO1 (RF),(R1),OUTREC+450,P10+18,50,=C'N'                             
         GOTO1 (RF),(R1),OUTREC+500,P11+18,50,=C'N'                             
         GOTO1 (RF),(R1),OUTREC+550,P12+18,50,=C'N'                             
         GOTO1 (RF),(R1),OUTREC+600,P13+18,50,=C'N'                             
***      GOTO1 (RF),(R1),OUTREC+650,P14+18,50,=C'N'                             
         GOTO1 (RF),(R1),OUTREC+700,P14+18,50,=C'N'                             
WRIT1C   MVC   P1+1(7),=C'001-050'                                              
         MVC   P2+1(7),=C'051-100'                                              
         MVC   P3+1(7),=C'101-150'                                              
         MVC   P4+1(7),=C'151-200'                                              
         MVC   P5+1(7),=C'201-250'                                              
         MVC   P6+1(7),=C'251-300'                                              
         MVC   P7+1(7),=C'301-350'                                              
         MVC   P8+1(7),=C'351-400'                                              
         MVC   P9+1(7),=C'401-450'                                              
         MVC   P10+1(7),=C'451-500'                                             
         MVC   P11+1(7),=C'501-550'                                             
         MVC   P12+1(7),=C'551-600'                                             
         MVC   P13+1(7),=C'600-650'                                             
****     MVC   P14+1(7),=C'651-700'                                             
         MVC   P14+1(7),=C'701-750'                                             
WRIT1E   MVI   SPACING,2                                                        
         BAS   RE,MYRPT                                                         
WRIT2    DS    0H                                                               
         CLI   SVQOPT6,C'Y'     SEE IF TEST RUN                                 
         BE    WRIT3            THEN NO TAPE                                    
         CLI   QOPT6,C'Y'       SEE IF TEST RUN                                 
         BE    WRIT3            THEN NO TAPE                                    
         LA    R1,PCHTAPE                                                       
*                                                                               
**VB2B   LA    R0,OUTREC-4                                                      
WRIT2B   LA    R0,OUTREC                                                        
         PRINT GEN                                                              
         PUT   (1),(0)                                                          
         PRINT NOGEN                                                            
WRIT3    AP    TOTCNT,=P'1'                                                     
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
MQOPEN   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   DMCB+8,X'A0'        SUPPRESS LENGTH FOR MESSAGE & HDR            
*                                                                               
* IF WE'RE RUNNING A TEST, SEND TO TEST MQ BROKER                               
         CLI   TESTMQ,C'T'         IS THIS A MQ TEST RUN                        
         BNE   *+8                  NO                                          
         OI    DMCB+8,X'01'         YES -PUT TO TEST MQ BROKER                  
*                                                                               
         GOTO1 AMQRPT,DMCB,(0,=C'OPEN'),(0,=C'MEDIACOMSFTP****'),,0             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DCHO                                                                   
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                         CREATE COLUMN HEADINGS RECORD                         
COLREC   CSECT                                                                  
         NMOD1 0,COLREC                                                         
         L     RC,PPFILEC                                                       
*****                                                                           
*****    NOTE - DO NOT USE REGISTERS R7, R8 AND R9                              
*****           IN THIS CSECT                                                   
*****           THEY ARE USED FOR THE WHOLE PROGRAM                             
*****                                                                           
**VB     MVC   OUTREC-4(2),=H'4'    STARTING RECORD LENGTH                      
**OLD    LA    RE,OUTREC           CLEAR OUTREC                                 
**OLD    LH    RF,=H'1000'                                                      
**OLD    XCEF                                                                   
*                                                                               
         LA    RE,10                                                            
         LA    RF,OUTREC                                                        
CLROUT   MVC   0(100,RF),SPACES                                                 
         LA    RF,100(RF)                                                       
         BCT   RE,CLROUT                                                        
         SH    RF,=H'2'             BACK-UP RF                                  
         MVC   0(2,RF),=X'0D0A'     CRLF AT RECORD END                          
*                                                                               
         L     RF,=A(COLHDS)                                                    
         MVC   OUTREC(250),0(RF)                                                
         MVC   OUTREC+250(250),250(RF)                                          
         MVC   OUTREC+500(250),500(RF)                                          
         MVC   OUTREC+750(COLHDLEN-750),750(RF)                                 
**vb     LH    RF,OUTREC-4                                                      
**vb     LA    RF,COLHDLEN(RF)                                                  
**vb     STH   RF,OUTREC-4                                                      
         MVC   RECTYPE,=C'CH'      COLUMN HEADINGS                              
         BAS   RE,MWRITE                                                        
         XMOD1 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*                         CREATE FILE RECORD                                    
BLDREC   CSECT                                                                  
         NMOD1 0,BLDREC                                                         
         L     RC,PPFILEC                                                       
*****                                                                           
*****    NOTE - DO NOT USE REGISTERS R7, R8 AND R9                              
*****           IN THIS CSECT                                                   
*****           THEY ARE USED FOR THE WHOLE PROGRAM                             
*****                                                                           
         CLI   COLSW,C'Y'       COLUMN HEADINGS SENT?                           
         BE    BLDR1                                                            
         GOTO1 =A(COLREC)                                                       
         MVI   COLSW,C'Y'        SO I WON'T REDO                                
*                                                                               
BLDR1    DS    0H                                                               
         MVC   RECTYPE,=C'IR'       BILLING REC                                 
*                                                                               
**VB     MVC   OUTREC-4(2),=H'4'    STARTING RECORD LENGTH                      
**OLD    LA    RE,OUTREC           CLEAR OUTREC                                 
**OLD    LH    RF,=H'1000'                                                      
**OLD    XCEF                                                                   
*                                                                               
         LA    RE,10                                                            
         LA    RF,OUTREC                                                        
CLROUT2  MVC   0(100,RF),SPACES                                                 
         LA    RF,100(RF)                                                       
         BCT   RE,CLROUT2                                                       
         SH    RF,=H'2'             BACK-UP RF                                  
         MVC   0(2,RF),=X'0D0A'     CRLF AT RECORD END                          
*                                                                               
         LA    R2,OUTREC                                                        
* CUSTOMER ID                                                                   
*****    MVC   0(09,R2),=C'"CHOICE",'                                           
*****    LA    R2,09(R2)                                                        
         MVC   0(20,R2),=C'"US521209792CHOICE",'                                
         LA    R2,20(R2)                                                        
*                                                                               
* REMIT TO INFO   - DATA FROM PRINTPAK??                                        
         MVC   0(14,R2),=C'"Havas Media",'                                      
         LA    R2,14(R2)                                                        
         MVC   0(22,R2),=C'"200 Hudson Street",,,'                              
         LA    R2,22(R2)                                                        
         MVC   0(11,R2),=C'"New York",'                                         
         LA    R2,11(R2)                                                        
         MVC   0(05,R2),=C'"NY",'                                               
         LA    R2,05(R2)                                                        
         MVC   0(08,R2),=C'"10013",'                                            
         LA    R2,08(R2)                                                        
         MVC   0(05,R2),=C'"US",'                                               
         LA    R2,05(R2)                                                        
         MVC   0(13,R2),=C'"13-3977932",'   REMIT TO CODE                       
         LA    R2,13(R2)                                                        
* BILL TO INFO                                                                  
         MVC   0(30,R2),=C'"Choice Hotels International",'                      
         LA    R2,30(R2)                                                        
         MVC   0(24,R2),=C'"10750 Columbia Pike",,,'                            
         LA    R2,24(R2)                                                        
         MVC   0(16,R2),=C'"Silver Spring",'                                    
         LA    R2,16(R2)                                                        
         MVC   0(05,R2),=C'"MD",'                                               
         LA    R2,05(R2)                                                        
         MVC   0(08,R2),=C'"20901",'                                            
         LA    R2,08(R2)                                                        
         MVC   0(05,R2),=C'"US",'                                               
         LA    R2,05(R2)                                                        
* SHIP TO INFO                                                                  
         MVC   0(08,R2),=C',,,,,,,,'     8 EMPTY FIELDS                         
         LA    R2,8(R2)                  NO SHIP TO PARTY NEEDED                
***                                                                             
***      MVC   0(30,R2),=C'"Choice Hotels International",'                      
***      LA    R2,30(R2)                                                        
***      MVC   0(24,R2),=C'"10750 Columbia Pike",,,'                            
***      LA    R2,24(R2)                                                        
***      MVC   0(16,R2),=C'"Silver Spring",'                                    
***      LA    R2,16(R2)                                                        
***      MVC   0(05,R2),=C'"MD",'                                               
***      LA    R2,05(R2)                                                        
***      MVC   0(08,R2),=C'"20901",'                                            
***      LA    R2,08(R2)                                                        
***      MVC   0(05,R2),=C'US",'                                                
***      LA    R2,05(R2)                                                        
*                                                                               
         GOTO1 =V(PPFMTINO),DMCB,PBILLDAT,(2,PBILKBNO),                X        
               (PBILKMED,B1PROF),B1XPROF                                        
*                                                                               
         MVC   DINVFULL,SPACES                                                  
         L     RF,DMCB                                                          
         LA    R1,DINVFULL                                                      
         CLI   0(RF),C' '       SKIP LEADING BLANK                              
         BH    BLDI1                                                            
         LA    RF,1(RF)                                                         
         LA    R3,9                                                             
         B     BLDI2                                                            
*                                                                               
BLDI1    LA    R3,10                                                            
BLDI2    MVC   0(1,R1),0(RF)                                                    
         B     BLDI5                                                            
*                                                                               
BLDI5    LA    R1,1(R1)                                                         
BLDI8    LA    RF,1(RF)                                                         
         BCT   R3,BLDI2                                                         
         MVI   0(R2),C'"'                                                       
         MVC   1(10,R2),DINVFULL                                                
         LA    R2,11(R2)                                                        
BLDI9    CLI   0(R2),C' '                                                       
         BH    BLDI17                                                           
         SH    R2,=H'1'                                                         
         B     BLDI9                                                            
*                                                                               
BLDI17   MVC   1(2,R2),=C'",'                                                   
         LA    R2,3(R2)                                                         
*                                                                               
         MVI   0(R2),C'"'                                                       
         GOTO1 DATCON,DMCB,(3,PBILINVD),(X'20',WORK)                            
         MVC   1(2,R2),WORK+2    MM                                             
         MVI   3(R2),C'/'                                                       
         MVC   4(2,R2),WORK+4    DD                                             
         MVI   6(R2),C'/'                                                       
         MVC   7(2,R2),=C'20'    CENTRY 2000                                    
         MVC   9(2,R2),WORK+0    YY                                             
         MVC   11(2,R2),=C'",'                                                  
         LA    R2,13(R2)                                                        
         MVC   0(03,R2),=C',,,'  3 EMPTY FIELDS                                 
         LA    R2,3(R2)          DISC. %, DISC. DAY INC.,NET DAY INC.           
         MVI   0(R2),C'"'                                                       
         MVC   1(33,R2),=C'Alicia_Jenkins@choicehotels.com",'                   
         LA    R2,34(R2)                                                        
         MVC   0(4,R2),=C',,,,'  4 EMPTY FIELDS                                 
         LA    R2,4(R2)          SHIP TERMS,ORIG INV#,PO REF.#,COMMENTS         
*                                                                               
*  END OF HEADER DATA                                                           
*                                                                               
*  DETAIL (INVOICE) DATA                                                        
*                                                                               
         MVC   0(04,R2),=C'"1",'  INVOICE LINE NUMBER - ALWAYS 1                
         LA    R2,4(R2)                                                         
         MVC   0(01,R2),=C','    1 EMPTY FIELD - SUPPLIER                       
         LA    R2,1(R2)                                                         
*                                 DESCRIPTION - MEDIA NAME                      
*                                                                               
         MVI   0(R2),C'"'                                                       
         LA    R2,1(R2)                                                         
         MVC   0(07,R2),=C'TRADE",'                                             
         LA    R2,7(R2)                                                         
         CLI   PBILKMED,C'T'                                                    
         BE    BLDI20                                                           
         SH    R2,=H'7'    RETURN TO START                                      
         MVC   0(08,R2),=C'MOBILE",'                                            
         LA    R2,8(R2)                                                         
         CLI   PBILKMED,C'B'                                                    
         BE    BLDI20                                                           
         SH    R2,=H'8'    RETURN TO START                                      
         MVC   0(12,R2),=C'DIG. AUDIO",'                                        
         LA    R2,12(R2)                                                        
         CLI   PBILKMED,C'D'                                                    
         BE    BLDI20                                                           
         SH    R2,=H'12'    RETURN TO START                                     
         MVC   0(08,R2),=C'SOCIAL",'                                            
         LA    R2,8(R2)                                                         
         CLI   PBILKMED,C'L'                                                    
         BE    BLDI20                                                           
         SH    R2,=H'8'    RETURN TO START                                      
         MVC   0(08,R2),=C'SEARCH",'                                            
         LA    R2,8(R2)                                                         
         CLI   PBILKMED,C'S'                                                    
         BE    BLDI20                                                           
         SH    R2,=H'8'    RETURN TO START                                      
         MVC   0(09,R2),=C'DIGITAL",'                                           
         LA    R2,09(R2)                                                        
         CLI   PBILKMED,C'I'                                                    
         BE    BLDI20                                                           
         SH    R2,=H'9'    RETURN TO START                                      
         MVC   0(09,R2),=C'OUTDOOR",'                                           
         LA    R2,09(R2)                                                        
         CLI   PBILKMED,C'O'      SEE IF OUTDOOR                                
         BE    BLDI20                                                           
         SH    R2,=H'9'    RETURN TO START                                      
         MVC   0(10,R2),=C'MAGAZINE",'                                          
         LA    R2,10(R2)                                                        
         CLI   PBILKMED,C'M'                                                    
         BE    BLDI20                                                           
         SH    R2,=H'10'    RETURN TO START                                     
         MVC   0(11,R2),=C'NEWSPAPER",'                                         
         LA    R2,11(R2)                                                        
         CLI   PBILKMED,C'N'                                                    
         BE    BLDI20                                                           
         SH    R2,=H'11'    RETURN TO START                                     
         MVC   0(12,R2),=C'NAT. VIDEO",'                                        
         LA    R2,12(R2)                                                        
         CLI   PBILKMED,C'V'                                                    
         BE    BLDI20                                                           
         SH    R2,=H'12'    RETURN TO START                                     
         MVC   0(12,R2),=C'LOC. VIDEO",'                                        
         LA    R2,12(R2)                                                        
         CLI   PBILKMED,C'W'                                                    
         BE    BLDI20                                                           
         DC    H'0'         UNKNOWN MEDIA                                       
*                                                                               
BLDI20   DS    0H                                                               
*                                                                               
         ZAP   DUB,PBILLRCV       AMT DUE                                       
         CVB   RF,DUB                                                           
         ST    RF,MYFULL                                                        
*                                                                               
         MVC   0(4,R2),=C'"1",'  LINE ITEM QUANITY                              
         C     RF,=F'0'                                                         
         BNL   BLDR50            SEE IF NEGATIVE                                
         MVC   0(5,R2),=C'"-1",'                                                
         LA    R2,5(R2)                                                         
         B     BLDR52                                                           
*                                                                               
BLDR50   LA    R2,4(R2)                                                         
BLDR52   DS    0H                                                               
         MVI   0(R2),C'"'                                                       
         LA    R2,1(R2)                                                         
         EDIT  (B4,MYFULL),(14,0(R2)),2,ALIGN=LEFT                              
         AR    R2,R0        ADD LENGTH OF OUTPUT                                
         MVC   0(2,R2),=C'",'                                                   
         LA    R2,2(R2)                                                         
         MVC   0(5,R2),=C'"EA",'                                                
         LA    R2,5(R2)                                                         
         MVI   0(R2),C','    EMPTY FIELD                                        
         LA    R2,1(R2)                                                         
*                            LINE ITEM SUBTOTAL                                 
* END OF DETAIL DATA                                                            
*                                                                               
* SUMMARY DATA                                                                  
*                                                                               
         MVI   0(R2),C','    EMPTY FIELD                                        
         LA    R2,1(R2)      FREIGHT                                            
*                                                                               
         CLI   PBILOTH,X'09'   SEE IF ELEMMENT                                  
         BNE   NOTAX                                                            
         CLI   PBILOLEN,X'20'  CHECK ELEM LENGHT                                
         BL    NOTAX                                                            
         OC    PBILLTAX,PBILLTAX                                                
         BZ    NOTAX                                                            
*                                                                               
         MVI   0(R2),C'"'                                                       
         LA    R2,1(R2)                                                         
         EDIT  (B4,PBILLTAX),(14,0(R2)),2,ALIGN=LEFT                            
         AR    R2,R0        ADD LENGTH OF OUTPUT                                
         MVC   0(2,R2),=C'",'                                                   
         LA    R2,2(R2)                                                         
         B     NOTAXX                                                           
*                                                                               
NOTAX    MVI   0(R2),C','                                                       
         LA    R2,1(R2)                                                         
*                                                                               
NOTAXX   DS    0H            MYFULL SHOULD STILL HAVE AMOUNT DUE                
         MVI   0(R2),C'"'                                                       
         LA    R2,1(R2)                                                         
         EDIT  (B4,MYFULL),(14,0(R2)),2,ALIGN=LEFT,FLOAT=-                      
         AR    R2,R0        ADD LENGTH OF OUTPUT                                
         MVC   0(2,R2),=C'",'                                                   
         LA    R2,2(R2)                                                         
*                                                                               
         CP    PBILLRCV,=P'0'                                                   
         BNL   NOCM                                                             
         MVC   0(05,R2),=C'"CR",'                                               
         LA    R2,5(R2)                                                         
         B     NOCMX                                                            
*                                                                               
NOCM     MVI   0(R2),C','    EMPTY CM INDICATOR                                 
         LA    R2,1(R2)                                                         
NOCMX    MVI   0(R2),C','    EMPTY ATTACHMENT FILE NAME                         
***NO    LA    R2,1(R2)                                                         
***NO    MVC   0(2,R2),=X'0D0A'      CRLF                                       
*                                                                               
*   END OF RECORD                                                               
*                                                                               
BLDREND  DS    0H                                                               
**VB     LA    RE,OUTREC-4                                                      
**VB     LR    RF,R2                                                            
**VB     SR    RF,RE      DIFFERENCE SHOULD BE RECORD LENGTH                    
**VB     STH   RF,OUTREC-4                                                      
         BAS   RE,MWRITE                                                        
         B     BLDRX                                                            
*                                                                               
*        CODE BELOW SAVED - MAY USE FOR DESCRIPTION?                            
*                                                                               
         MVI   0(R2),C'"'                                                       
         LA    R2,1(R2)                                                         
         MVC   0(1,R2),PBILKMED                                                 
         MVI   1(R2),C' '                                                       
         LA    R2,2(R2)                                                         
*****                                                                           
*****    CODE BELOW TO SHOW MEDIA NAME                                          
*****                                                                           
*****    MVC   0(06,R2),=C'TRADE '                                              
*****    LA    R2,6(R2)                                                         
*****    CLI   PBILKMED,C'T'                                                    
*****    BE    BLDC1                                                            
*****    SH    R2,=H'6'    RETURN TO START                                      
*****    MVC   0(07,R2),=C'SEARCH '                                             
*****    LA    R2,7(R2)                                                         
*****    CLI   PBILKMED,C'S'                                                    
*****    BE    BLDC1                                                            
*****    SH    R2,=H'7'    RETURN TO START                                      
*****    MVC   0(09,R2),=C'MAGAZINE '                                           
*****    LA    R2,9(R2)                                                         
*****    CLI   PBILKMED,C'M'                                                    
*****    BE    BLDC1                                                            
*****    SH    R2,=H'9'    RETURN TO START                                      
*****    MVC   0(10,R2),=C'NEWSPAPER '                                          
*****    LA    R2,10(R2)                                                        
*****    CLI   PBILKMED,C'N'                                                    
*****    BE    BLDC1                                                            
*****    SH    R2,=H'10'    RETURN TO START                                     
*****    MVC   0(12,R2),=C'INTERACTIVE '                                        
*****    LA    R2,12(R2)                                                        
*****    CLI   PBILKMED,C'I'                                                    
*****    BE    BLDC1                                                            
*****    DC    H'0'         UNKNOWN MEDIA                                       
*                                                                               
BLDC1    BAS   RE,GETCOM      RETURNED IN WORK                                  
         MVC   0(12,R2),WORK                                                    
         LA    R2,12(R2)                                                        
BLDC2    CLI   0(R2),C' '     SCAN BACKWORDS FOR END                            
         BH    BLDC5          FLOAT MOS AFTER IT                                
         BCT   R2,BLDC2                                                         
*                                                                               
BLDC5    MVI   1(R2),C' '                                                       
         LA    R2,2(R2)                                                         
         GOTO1 DATCON,DMCB,(3,PBILKMOS),(6,0(R2))  MOS                          
         MVI   6(R2),C' '                                                       
         LA    R2,7(R2)           PAST MMM/YY                                   
         MVC   0(L'PRDU1,R2),PRDU1                                              
         LA    R2,L'PRDU1(R2)                                                   
BLDC8    CLI   0(R2),C' '                                                       
         BH    BLDC9                                                            
         BCT   R2,BLDC8                                                         
*                                                                               
BLDC9    MVI   1(R2),C' '                                                       
         LA    R2,2(R2)                                                         
         MVC   0(L'ESTU1,R2),ESTU1                                              
         LA    R2,L'ESTU1(R2)                                                   
BLDC10   CLI   0(R2),C' '                                                       
         BH    BLDC11                                                           
         BCT   R2,BLDC10                                                        
*                                                                               
BLDC11   MVI   1(R2),C' '                                                       
         LA    R2,2(R2)                                                         
         MVC   0(L'ESTU2,R2),ESTU2                                              
         LA    R2,L'ESTU2(R2)                                                   
BLDC12   CLI   0(R2),C' '                                                       
         BH    BLDC13                                                           
         BCT   R2,BLDC12                                                        
*                                                                               
*                                                                               
BLDC13   MVC   1(2,R2),=C'",'                                                   
         LA    R2,3(R2)                                                         
*                                                                               
         EJECT                                                                  
*                                                                               
*        GETCITY EXTRACTS CITY FROM BPRDLIN2                                    
*        THE FORMAT SHOULD BE CITY, ST  ZIP                                     
*                                                                               
GETCITY  DS    0H                                                               
         XC    PRDCITY,PRDCITY                                                  
         LA    R1,PRDCITY                                                       
         LA    R3,BPRDLIN2                                                      
         LA    R4,L'BPRDLIN2                                                    
GETC5    CLI   0(R3),C','     FIND FIRST ,                                      
         BE    GETCX                                                            
         MVC   0(1,R1),0(R3)                                                    
         LA    R1,1(R1)                                                         
         LA    R3,1(R3)                                                         
         BCT   R4,GETC5                                                         
*                                                                               
GETCX    OC    PRDCITY,SPACES                                                   
         BR    RE              RETURN                                           
                                                                                
         EJECT                                                                  
*                                                                               
*        GETST EXTRACTS STATE CODE FROM BPRDLIN2                                
*        THE FORMAT SHOULD BE CITY, ST  ZIP                                     
*                                                                               
GETST    DS    0H                                                               
         XC    PRDST,PRDST                                                      
         LA    R1,PRDST                                                         
         LA    R3,BPRDLIN2                                                      
         LA    R4,L'BPRDLIN2                                                    
GETS5    CLI   0(R3),C','     FIND FIRST ,                                      
         BE    GETS10                                                           
         LA    R3,1(R3)                                                         
         BCT   R4,GETS5                                                         
         B     GETSX           MEANS NO / FOUND                                 
*                                                                               
GETS10   LA    R3,1(R3)        BUMP PAST IT                                     
         CLI   0(R3),C' '     BUMP PAST A SPACE                                 
         BNE   *+8                                                              
         LA    R3,1(R3)                                                         
*                                                                               
GETS15   CLI   0(R3),C' '     FIND NEXT SPACE                                   
         BE    GETS20                                                           
         LA    R4,2            STATE CODE MUST BE 2 CHARS                       
         MVC   0(1,R1),0(R3)                                                    
         LA    R1,1(R1)                                                         
         LA    R3,1(R3)                                                         
         BCT   R4,GETS15                                                        
         B     GETSX                                                            
*                                                                               
GETS20   DS    0H                                                               
*                                                                               
GETSX    OC    PRDST,SPACES                                                     
         BR    RE              RETURN                                           
                                                                                
         EJECT                                                                  
*                                                                               
*        GETZIP EXTRACTS ZIP CODE FROM BPRDLIN2                                 
*        THE FORMAT SHOULD BE CITY, ST  ZIP                                     
*                                                                               
GETZIP   DS    0H                                                               
         XC    PRDZIP,PRDZIP                                                    
         LA    R1,PRDZIP                                                        
         LA    R3,BPRDLIN2                                                      
         LA    R4,L'BPRDLIN2                                                    
GETZ5    CLI   0(R3),C','     FIND FIRST ,                                      
         BE    GETZ10                                                           
         LA    R3,1(R3)                                                         
         BCT   R4,GETZ5                                                         
         B     GETZX           MEANS NO / FOUND                                 
*                                                                               
GETZ10   LA    R3,1(R3)        BUMP PAST IT                                     
         SH    R4,=H'1'       DECREMENT R4                                      
         CH    R4,=H'0'       DON'T GO NEGATIVE                                 
         BH    *+6                                                              
         DC    H'0'           BAD DATA                                          
*                                                                               
         CLI   0(R3),C' '     BUMP PAST A SPACE BEFORE THE STATE                
         BNE   GETZ15                                                           
         LA    R3,1(R3)                                                         
         SH    R4,=H'1'       DECREMENT R4                                      
         CH    R4,=H'0'       DON'T GO NEGATIVE                                 
         BH    *+6                                                              
         DC    H'0'           BAD DATA                                          
*                                                                               
GETZ15   CLI   0(R3),C' '     FIND NEXT SPACE                                   
         BE    GETZ20                                                           
         LA    R3,1(R3)                                                         
         BCT   R4,GETZ15                                                        
         B     GETZX                                                            
*                                                                               
GETZ20   DS    0H                                                               
         LA    R3,1(R3)        BUMP PAST IT                                     
         SH    R4,=H'1'       DECREMENT R4                                      
         BH    *+6                                                              
         DC    H'0'           BAD DATA                                          
         CLI   0(R3),C' '     BUMP PAST A SPACE                                 
         BNE   GETZ25         TO ALLOW FOR 2 SPACES BEFORE ZIP                  
         LA    R3,1(R3)                                                         
         SH    R4,=H'1'       DECREMENT R4                                      
         BH    *+6                                                              
         DC    H'0'           BAD DATA                                          
*                                                                               
GETZ25   MVC   0(1,R1),0(R3)                                                    
         LA    R1,1(R1)                                                         
         LA    R3,1(R3)                                                         
         BCT   R4,GETZ25                                                        
         B     GETZX                                                            
*                                                                               
*                                                                               
GETZX    OC    PRDZIP,SPACES                                                    
         BR    RE              RETURN                                           
         EJECT                                                                  
GETCOM   DS    0H              BUILD COMMENT LINE                               
*                              CLT PRD PRDNAME EST ESTNAME                      
         MVC   WORK(12),SPACES                                                  
         MVC   WORK(3),PBILKCLT                                                 
         MVC   WORK+4(3),PBILKPRD                                               
         MVC   HALF,PBILKEST                                                    
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK+8(3),DUB+6(2)                                               
         BR    RE                                                               
*****                                                                           
*****    CODE BELOW FLOATS PRODUCT NAME AFTER CODE                              
*****    AND ESTIMATE NAME AFTER CODE                                           
*****    MVC   WORK+7(20),PPRDNAME                                              
*****    OC    WORK+7(20),SPACES                                                
*****    LA    R1,WORK+30                                                       
***CM5   CLI   0(R1),C' '                                                       
*****    BH    GETCM10                                                          
*****    BCT   R1,GETCM5                                                        
*****                                                                           
***CM10  MVC   HALF,PBILKEST                                                    
****     LH    R0,HALF                                                          
****     CVD   R0,DUB                                                           
****     OI    DUB+7,X'0F'                                                      
****     UNPK  2(3,R1),DUB+6(2)                                                 
****     MVC   6(20,R1),PESTNAME                                                
****     OC    6(20,R1),SPACES                                                  
         BR    RE             RETURN                                            
         EJECT                                                                  
*                                                                               
*                                                                               
BLDRX    XMOD1                                                                  
         LTORG                                                                  
*                           PPBYOUT WORKAREA                                    
         DS    0D                                                               
         DC    CL8'PPBYOWRK'                                                    
PPBYOWRK DS    600C                                                             
         DC    X'00'                                                            
*                                                                               
         EJECT                                                                  
GETMARK  NTR1                                                                   
         L     R6,ELADDR                                                        
         USING PBILELEM,R6                                                      
         MVC   WORK(30),SPACES                                                  
         B     GETMKX                   NO MARKET FOR MINDSHARE                 
*                                                                               
***      MVC   WORK(9),=CL9'CORPORATE'                                          
***      CLC   PBPRD,=C'CON'                                                    
***      BE    GETMKX                                                           
***      CLC   PBPRD,=C'SAM'                                                    
***      BE    GETMKX                                                           
***      CLC   PBPRD,=C'SYO'                                                    
***      BE    GETMKX                                                           
***                                                                             
***      CLC   PBPRD,=C'BIZ'                                                    
***      BE    GETMK5                                                           
***      CLC   PBPRD,=C'PCS'                                                    
***      BE    GETMK5                                                           
***      B     GETMKX          ANY OTHERS GET CORPORATE                         
***                                                                             
***MK5   MVC   WORK(9),=CL9'NATIONAL'                                           
***      CLI   QMEDIA,C'M'                                                      
***      BE    GETMKX                                                           
***      CLI   QMEDIA,C'T'                                                      
***      BE    GETMKX                                                           
***      CLI   QMEDIA,C'S'                                                      
***      BE    GETMKX                                                           
***      CLI   QMEDIA,C'I'                                                      
***      BE    GETMKX                                                           
***      IF    QMEDIA,EQ,C'O',AND,PBPRD,EQ,=C'BIZ',GETMKX                       
***                                                                             
***                                                                             
***      ALL NEWSPAPER PRODUCTS USE THE DISTRICT NAME                           
***      OTHER OUTDOOR PRODUCTS USE THE REGION NAME                             
***                                                                             
***      NOTE: REQUEST BY DIV/REG FOR OUTDOOR ND NEWSPAPERS                     
***            AND DIV/REG/DST FOR NEWSPAPERS                                   
***                                                                             
***      MVC   WORK(20),PDSTNAME                                                
***      OC    WORK(20),SPACES     JUST IN CASE                                 
***      CLI   QMEDIA,C'N'                                                      
***      BE    GETMK20                                                          
***      MVC   WORK(20),PREGNAME    OUTDOOR USES REGIONS                        
***      OC    WORK(20),SPACES     JUST IN CASE                                 
***      B     GETMK20                                                          
***                                                                             
***MK20  CLC   WORK(06),=C'MASTER'   IF MASTER REGION/DISTRICT                  
***      BNE   GETMKX                (UNASSIGNED) MUST CHANGE NAME              
***      MVC   WORK(20),=CL20'UNASSIGNED'                                       
***                                                                             
GETMKX   XIT1                                                                   
         DROP  R6                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*                                                                               
TITLES   CSECT                                                                  
         DS    0C                                                               
         DC    CL30'         COLUMN HEADERS '                                   
         DC    CL30'        INVOICE RECORDS'                                    
*                                                                               
         EJECT                                                                  
*             TABLE OF RECORD TYPES,LENGTHS AND COUNTS                          
LENTAB   CSECT                                                                  
         DC    CL2'CH',AL2(0),PL4'0'                                            
         DC    CL2'IR',AL2(0),PL4'0'                                            
         DC    XL4'00',PL4'0'      EXTRA LINE                                   
         DC    X'FFFF'                                                          
         EJECT                                                                  
PPCHWRKD DSECT                                                                  
NETOPT   DS    CL1                                                              
SVQOPT1  DS    CL1          SO I'LL KNOW AT RUNLAST WHICH FILE TYPE             
SVQOPT6  DS    CL1          SO I'LL KNOW AT RUNLAST IF TEST RUN                 
SVQOPT7  DS    CL1          SO I'LL KNOW AT RUNLAST IF PDUMPING                 
*                                                                               
MYCD     DS    PL6                                                              
*                                                                               
EHDRSW   DS    CL1        GETS SET TO Y WHEN EST FILE HEADER WRITTEN            
IHDRSW   DS    CL1        GETS SET TO Y WHEN INVOICE FILE HDR WRITTEN           
ERRSW    DS    CL1                                                              
COLSW    DS    CL1          Y IF COLUMN HEADERS SENT                            
ELADDR   DS    A            ADDRESS OF BILLING ELEMENT                          
RECTYPE  DS    CL2          USED BY MWRITE                                      
*                                                                               
REQUSER  DS    CL10         SET FROM VENTAB                                     
*                                                                               
SVPRDIFC DS    CL4          SAVED FROM PRD INTERFACE CODE ELEMENT               
TOTCNT   DS    PL4'0'                                                           
MYDUB    DS    PL8                                                              
MYDUB2   DS    PL8                                                              
NETDUB   DS    PL8                                                              
CDDUB    DS    PL8                                                              
SDUB     DS    PL8          USED FOR SHARES IN PROCNET                          
SNETDUB  DS    PL8          USED FOR SHARES IN PROCNET                          
SVNUPRD2 DS    XL1          SAVED NUPRD2                                        
*                                                                               
WRKBDATE DS    CL10                                                             
MYDUMP   DS    XL2                                                              
         DS    0H           ALIGN                                               
WK       DS    XL100                                                            
*                                                                               
ALLOWSW  DS    XL1                                                              
DYNDDN   DS    CL8                                                              
DYNDSN   DS    CL20                                                             
         DS    0F          ALIGNMENT FOR WK                                     
*                                                                               
MYFULL   DS    F                                                                
MYGST    DS    PL6                                                              
MYHST    DS    PL6                                                              
MYPST    DS    PL6                                                              
WPRD     DS    XL1                                                              
*                                                                               
WRKDATE  DS    XL8                                                              
*                                                                               
PRDCITY  DS    CL30        EXTRACTED FROM PPRDLIN2                              
PRDST    DS    CL30                                                             
PRDZIP   DS    CL30                                                             
*                                                                               
INSCOM1  DS    CL47        1ST INSERTION COMMENT                                
INSCOM2  DS    CL47        2ND INSERTION COMMENT                                
*                                                                               
APRDBILL DS    CL20        SAVED FROM PRD AAA                                   
APRDBIL2 DS    CL20        SAVED FROM PRD AAA                                   
APRDLIN1 DS    CL30        SAVED FROM PRD AAA                                   
APRDLIN2 DS    CL30        SAVED FROM PRD AAA                                   
APRDATTN DS    CL24        SAVED FROM PRD AAA                                   
*                                                                               
BPRDBILL DS    CL20        SAVED FROM BRAND                                     
BPRDBIL2 DS    CL20        SAVED FROM BRAND                                     
BPRDLIN1 DS    CL30        SAVED FROM BRAND                                     
BPRDLIN2 DS    CL30        SAVED FROM BRAND                                     
BPRDATTN DS    CL24        SAVED FROM BRAND                                     
*                                                                               
MYBEST   DS    H           FROM QEST                                            
MYBESTE  DS    H           FROM QESTEND                                         
*                                                                               
AAAPRD   DS    CL1         =Y IF WE HAVE PRD=AAA                                
PRDFORMU DS    CL5         BILLING FORMULA FOR REGULAR PROD                     
ESTFORMU DS    CL5         BILLING FORMULA FOR REGULAR EST                      
AAAFORMU DS    CL5         BILLING FORMULA FOR AAA     PROD                     
AAAESTFR DS    CL5         BILLING FORMULA FOR AAAEST                           
BILLFORM DS    CL5         DEFAULT VALUE FOR FORMULA                            
*                                                                               
B1PROF   DS    CL16                                                             
B1XPROF  DS    CL16                                                             
*                                                                               
DINVFULL DS    CL10                                                             
DINVNO   DS    CL6      SHORT FORMAT                                            
*                                                                               
LINVFULL DS    CL10     SAVED                                                   
LBQDATE  DS    CL6      SAVED                                                   
*                                                                               
INVTOTD  DS    PL5      TOTAL $ FOR INV NUMBER                                  
LASTBILL DS    CL1      Y= LAST BILL                                            
FRSTBILL DS    CL1      Y= FIRST BILL                                           
INVRCNT  DS    PL5      COUNT OF INVOICE FILE RECORDS                           
*                                                                               
       ++INCLUDE PPBVALD                                                        
*                                                                               
WORK2    DS    CL64                                                             
ALENTAB  DS    A                                                                
ATITLES  DS    A                                                                
VGETUSER DS    A                                                                
VGETCOST DS    A                                                                
VDDUCOM  DS    A                                                                
AFMTINO  DS    A                                                                
AMQRPT   DS    A                                                                
APERVAL  DS    A                                                                
APPBVAL  DS    A                                                                
ACONIO1  DS    A                                                                
ADMASTC  DS    A                                                                
ANXTINV  DS    A                                                                
BUFFIO   DS    A                                                                
BUFFBUFF DS    A                                                                
AREGTAB  DS    A                                                                
ANXTREG  DS    A                                                                
AREGTABX DS    A                                                                
*                                                                               
MYUSER   DS    CL1       SET FROM AGYTAB AT FBUYREQ                             
SVSTART  DS    CL6                                                              
SVEND    DS    CL6                                                              
*                                                                               
CTODAY   DS    CL8                 YYYYMMDD                                     
TIMEOFD  DS    CL8                 HH.MM.SS                                     
*                                                                               
MTODAYB  DS    XL3                 YMD - TODAY BINARY                           
ELEM     DS    CL200                                                            
*                                                                               
MQMAPNM  DS    CL14                SFTPDISK.PROD.                               
*                                                                               
DSNAME   DS    CL35  DSN -  BIL.SYS.AGID.DYYYMMDD.THHMMSS                       
*                                                                               
         DS    CL22                                                             
TESTMQ   DS    CL1                                                              
CHSFTP   DS    CL1                                                              
*                                                                               
ESTU1    DS    CL54      USER FIELDS                                            
ESTU2    DS    CL38                                                             
PRDU1    DS    CL54                                                             
*                                                                               
MYBILLCD DS    PL8                                                              
MYBILLGR DS    PL8                                                              
*                                                                               
CINVGRS  DS    PL8        CURRENT INVOICE TOTALS                                
CINVBIL  DS    PL8                                                              
CINVCD   DS    PL8                                                              
CINVRCV  DS    PL8                                                              
CINVSW   DS    CL1                                                              
*                                                                               
ADDDEL   DS    CL1                 X'01' IF ADDED + DELETED IN PERIOD           
BILLONLY DS    CL1                 SET IN CKEST Y= BILL ONLY ESTIMATE           
*                                  (NO CHANGES)                                 
SAVMED   DS    CL2               USED IN MWRITE TO SAVE 'REAL' MEDIA            
CKESTREC DS    CL1                                                              
LBILLKEY DS    CL12           KEY OF LAST BILL READ                             
LESTOUT  DS    CL12           KEY OF LAST ESTIMATE OUTPUT                       
*                                                                               
ETOTSW   DS    CL1                                                              
ESTCD    DS    PL8                 ESTIMATE TOTALS FOR PRINTING                 
ESTAMTD  DS    PL8                                                              
ESTCOMM  DS    PL8                                                              
*                                                                               
GTTOTCD  DS    PL8                 REPORT TOTALS                                
GTTOTAMT DS    PL8                                                              
GTTOTCOM DS    PL8                                                              
*                                                                               
TODAY1   DS    CL6                                                              
TODAYY   DS    CL8         YYYYMMDD                                             
NOW      DS    CL6         TIME HHMMSS                                          
ELCODE1  DS    CL1                                                              
CHOPENSW DS    CL1                                                              
SAVCGR   DS    PL8                                                              
*                                                                               
TAPESW   DS    CL1         STARTS AS 0 CHANGED TO N OR Y AT FBUYCLI             
ETAPESW  DS    CL1         STARTS AS 0 CHANGED TO N OR Y AT FBUYCLI             
*                          MIX NOT ALLOWED                                      
*                                                                               
CIRCDAT  DS    CL3                                                              
TRCODE   DS    CL1                                                              
PPGKEY   DS    CL32                                                             
PPGAREC  DS    CL4                                                              
ZEROS    DS    CL30                                                             
LASTEST  DS    CL9          USED TO CHECK CHANGE OF EST                         
LASTEVY  DS    CL4          VALIDITY YEAR                                       
LASTEGL  DS    CL10         GL ACCOUNT                                          
LASTEIO  DS    CL12         IO NUMBER                                           
LASTECC  DS    CL10         COST CENTER                                         
*                           WHEN READING BUFFALO RECS                           
APPBYOWK DS    A                                                                
*                                                                               
BADESTS  DS    CL240            ROOM FOR 40 BAD PRD/ESTS                        
*                                PRD(3)/EST(2)/+ ONE BYTE                       
*                                TO BE USED FOR ERRORS                          
         DS    F                                                                
INVTAB   DS    CL252        ROOM FOR 18 MOS X14                                 
*                           ENTRIES ARE MOS (MY)                                
*                                       BACTP  (ACTUAL AMT DUE)                 
*                                       COMMISSION (BACTP-BNETP)                
INVLEN   EQU   14                                                               
*                                                                               
BUFREC   DS    0CL146                                                           
BUFKEY   DS    0CL22                                                            
BUFTYPE  DS    CL1                 E=ESTIMATE                                   
*                                                                               
BUFMED   DS    CL1                 MEDIA                                        
BUFPRD   DS    CL3                 PRODUCT                                      
BUFYR    DS    CL1                 LAST DIGIT OF YEAR                           
BUFEST   DS    CL3                 EST                                          
BUFMTH   DS    CL6                 MONTH  YYYYMM                                
BUFPUB   DS    CL6                 PUB  (EMPTY FOR EST DETAIL)                  
         DS    CL1                 SPARE                                        
*                                                                               
BUFCOM   DS    0CL100               COMMENT                                     
BUFCOMEN DS    CL20                ESTIMATE NAME                                
BUFCOME1 DS    CL4                 VALIDITY YEAR                                
BUFCOME2 DS    CL10                GL ACCOUNT                                   
BUFCOME3 DS    CL12                INTERNAL ORDER                               
BUFCOME4 DS    CL10                COST CENTER                                  
BUFCOMPN DS    CL20                PUB NAME                                     
BUFCOMPZ DS    CL20                PUB ZONE NAME                                
         DS    CL4                 SPARE                                        
*                                                                               
*                                                                               
BUFCD    DS    PL8                 CASH DISCOUNT                                
BUFAMTD  DS    PL8                 AMOUNT DUE (CALC. WITH BILL. FORM.)          
BUFCOMM  DS    PL8                 COMMISSION (DUE-NET)                         
         ORG                                                                    
*                                                                               
*        UCOM FIELDS AND CONTROL BLOCK                                          
UCOMBLK  DS    CL(UCOMDLNQ)     DDUCOM CONTROL BLOCK                            
UCTTLS   DS    CL80             LEN=20*4                                        
UCOMDATA DS    CL128            LEN=32*4                                        
*                                                                               
UCALL    EQU   *-UCTTLS                                                         
USAVKEY  DS    XL32             TO SAVE CURRENT READ SEQUENCE                   
UCOMQ    EQU   *-UCOMBLK                                                        
*                                                                               
         DS    F                                                                
OUTREC   DS    CL1000                                                           
*                                                                               
*                                                                               
         DS    CL50             SPARE                                           
*                                                                               
         BUFF  LINES=4000,ROWS=1,COLUMNS=3,FLAVOR=PACKED,COMMENT=100,KEX        
               YLIST=(22,A)                                                     
*                                                                               
COLHDS   CSECT                                                                  
*                                                                               
*        HEADER COLUMN HEADINGS                                                 
*                                                                               
         DC    C'"CustomerID",'                                                 
         DC    C'"Remit To Name",'                                              
         DC    C'"Remit To Address 1",'                                         
         DC    C'"Remit To Address 2",'                                         
         DC    C'"Remit To Address 3",'                                         
         DC    C'"Remit To City",'                                              
         DC    C'"Remit To State",'                                             
         DC    C'"Remit To Postal Code",'                                       
         DC    C'"Remit To Country",'                                           
         DC    C'"Remit To Code",'                                              
**IN     DC    C'"GST HST Registration ID",'                                    
**IN     DC    C'"QST Registration ID",'                                        
*                                                                               
         DC    C'"Bill To Name",'                                               
         DC    C'"Bill To Address 1",'                                          
         DC    C'"Bill To Address 2",'                                          
         DC    C'"Bill To Address 3",'                                          
         DC    C'"Bill To City",'                                               
         DC    C'"Bill To State",'                                              
         DC    C'"Bill To Postal Code",'                                        
         DC    C'"Bill To Country",'                                            
*                                                                               
         DC    C'"Ship To Name",'                                               
         DC    C'"Ship To Address 1",'                                          
         DC    C'"Ship To Address 2",'                                          
         DC    C'"Ship To Address 3",'                                          
         DC    C'"Ship To City",'                                               
         DC    C'"Ship To State",'                                              
         DC    C'"Ship To Postal Code",'                                        
         DC    C'"Ship To Country",'                                            
*                                                                               
**IN     DC    C'"Invoice Number (or Credit Memo Number)",'                     
         DC    C'"Invoice Number",'                                             
*                                                                               
         DC    C'"Invoice Date",'                                               
         DC    C'"Discount Percent",'                                           
         DC    C'"Discount Day Increment",'                                     
         DC    C'"Net Day Increment",'                                          
         DC    C'"Requestor Email",'                                            
         DC    C'"Ship Terms",'                                                 
         DC    C'"Original Invoice Number",'                                    
         DC    C'"PO Ref Number",'                                              
         DC    C'"Comments",'                                                   
*                                                                               
*        LINE COLUMN HEADINGS                                                   
*                                                                               
         DC    C'"Invoice Line Number",'                                        
         DC    C'"Supplier Part Number",'                                       
         DC    C'"Part Description",'                                           
         DC    C'"Line Item Quantity",'                                         
         DC    C'"Unit Price",'                                                 
         DC    C'"UOM",'                                                        
         DC    C'"Line Item Subtotal",'                                         
         DC    C'"Freight",'                                                    
* LINEH DR                                                                      
*  END OF LINEH DR                                                              
*                                                                               
*        SUMMARY COLUMNS HEADINGS                                               
*                                                                               
         DC    C'"Total Taxes on Invoice",'                                     
         DC    C'"Invoice Grand Total",'                                        
         DC    C'"CM Indicator",'                                               
         DC    C'"Attachment File Name",'                                       
***NO    DC    X'0D0A'       CRLF                                               
COLHDX   EQU   *                                                                
COLHDLEN EQU   *-COLHDS                                                         
*                                                                               
MQMSGD   DSECT                                                                  
MQHID    DS    CL6                 HUB RECORD ID                                
MQSYS    DS    CL3                 SYSTEM                                       
MQAGYID  DS    CL4                 AGENCY 1D 4-CHAR                             
MQQUAL   DS    CL16                QUALIFIER                                    
MQDATE   DS    CL6                 YYMMDD OF DSN                                
MQTIME   DS    CL6                 HHMMSS OF DSN                                
MQDATA1  DS    CL32                NOT USED                                     
MQDATA2  DS    CL32                NOT USED                                     
MQFILE   DS    CL64                DSN  (MINUS SFTPDISK.PROD.)                  
MQMSGLNQ EQU   *-MQMSGD                                                         
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE PPMODEQU                                                       
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPREPWORK2                                                     
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE DDBUFFALOD                                                     
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
*                                                                               
         EJECT                                                                  
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDUCOMD                                                        
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'019PPREPCH02 07/18/18'                                      
         END                                                                    
