*          DATA SET SPREPCH02  AT LEVEL 014 AS OF 08/17/15                      
*PHASE SPCH02A                                                                  
*INCLUDE GETUSER                                                                
*INCLUDE DDUCOM                                                                 
*INCLUDE SPFMTINO                                                               
*INCLUDE SPBVAL                                                                 
*INCLUDE PERVAL                                                                 
         TITLE 'SPCH02 - CHOICE HOTELS INTERFACE'                               
*                                                                               
*        CHANGE LOG                                                             
*                                                                               
*  DEC/2014  BPLA   ADD CRLF (X'0D0A') AT END OF EACH LINE                      
*                                                                               
*  SEP/2014  BPLA   CHANGE TO CUSTOMER ID                                       
*                                                                               
*  MAR/2014  BPLA   ADDRESS CHANGE                                              
*                                                                               
*  OCT/2013  BPLA   USE MCVMQRPT FROM VMASTC                                    
*                                                                               
*  MAY/2013  BPLA   CHANGES FOR USING THE HUB                                   
*                                                                               
*        QOPT6 Y= TEST RUN - NO TAPE, AND CONTINUE IF ERRORS                    
*                            ARE FOUND                                          
*        QOPT7 N= NO MQ NOTIFICATION                                            
*              T= TEST MQ NOTIFIACTION                                          
*              Y= PDUMP RECORDS - FOR TESTING                                   
*              P= PROD MQ NOTIFICATION                                          
*                                                                               
*        QSTART(6) = PERIOR START                                               
*        QEND(6) = PERIOD END - MAY BE BLANK                                    
*                                                                               
*        NOTE: PROGRAM USES THE BRAND'S ADDRESS DATA                            
*              NOT THE ONES FROM PRODUCT AAA (LIKE THE SE PROGRAM)              
*              IT ALSO READS BILLS  - NOT INSERTIONS                            
*                                                                               
SPCH02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPCH02,RR=R9                                                   
*                                                                               
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         L     RA,0(R1)                                                         
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
         USING SPWORKD,RA,R9                                                    
         LA    R8,SPACEND                                                       
         USING SPCHWRKD,R8                                                      
         LA    R7,SPCH02+4095                                                   
         LA    R7,1(R7)                                                         
         USING SPCH02+4096,R7     **NOTE USE OF R7 AS BASE REG*                 
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    INITIAL                                                          
         CLI   MODE,REQFRST                                                     
         BE    FIRSTB                                                           
         CLI   MODE,CLTFRST                                                     
         BE    FCLI                                                             
         CLI   MODE,ESTFRST                                                     
         BE    FEST                                                             
         CLI   MODE,PRDFRST                                                     
         BE    FPRD                                                             
         CLI   MODE,REQLAST                                                     
         BE    PUTBUFF                                                          
         CLI   MODE,RUNLAST                                                     
         BE    TOTALS                                                           
         CLI   MODE,PROCBILL                                                    
         BE    PROCESS                                                          
*                                                                               
         CLI   MODE,CLTLAST       END OF CLIENT                                 
         BE    LCLI                                                             
                                                                                
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
         L     R0,=V(SPFMTINO)                                                  
         A     R0,RELO                                                          
         ST    R0,AFMTINO                                                       
         L     R0,=V(PERVAL)                                                    
         A     R0,RELO                                                          
         ST    R0,APERVAL                                                       
         L     R0,=V(SPBVAL)                                                    
         A     R0,RELO                                                          
         ST    R0,ASPBVAL                                                       
*                                                                               
*                                                                               
         L     RF,VMASTC            USE MASTC'S AGYID                           
         USING MASTD,RF                                                         
         MVC   AMQRPT,MCVMQRPT                                                  
         DROP  RF                                                               
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
         L     R0,=A(SCHTAPE)                                                   
         A     R0,RELO                                                          
         ST    R0,ASCHTAPE                                                      
*                                                                               
         L     R2,ALENTAB          ZAP ACCUMS                                   
         LA    R3,2                                                             
INIT7L   ZAP   4(4,R2),=P'0'                                                    
         LA    R2,8(R2)                                                         
         BCT   R3,INIT7L                                                        
*                                                                               
INIT70   DS    0H                                                               
*                                                                               
         L     RF,VMASTC                                                        
         USING MASTD,RF                                                         
         MVI   NETPAKSW,C'Y'                                                    
         CLI   MCNETPAK,C'Y'                                                    
         BE    *+8                                                              
         MVI   NETPAKSW,C'N'                                                    
         DROP  RF                                                               
**                                                                              
INITX    B     EXIT                                                             
         EJECT                                                                  
*                       REQUEST FIRST                                           
FIRSTB   DS    0H                                                               
*                                                                               
*                                                                               
REQF20   DS    0H                                                               
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
         CLI   QOPT6,C'Y'        SEE IF TEST REQUEST                            
         BE    FIRSTB0                                                          
         CLI   TAPESW,C'N'       SEE IF A PRIOR REQUEST WAS TEST                
         BE    MIXERR                                                           
         MVI   TAPESW,C'Y'       SET TAPE BEING PRODUCED                        
REQF13   CLI   CHOPENSW,C'Y'    IS BK FILE ALREADY OPEN?                        
         BE    REQF13M          IF SO, SKIP CODE BELOW                          
*                                                                               
         MVC   DSNAME,SPACES                                                    
         MVC   DSNAME+0(4),=C'BIL.'                                             
         MVC   DSNAME+4(3),=C'SPT'                                              
         CLI   NETPAKSW,C'N'                                                    
         BE    *+10                                                             
         MVC   DSNAME+4(3),=C'NET'                                              
         MVI   DSNAME+7,C'.'                                                    
         L     RF,VMASTC             USE MASTC'S AGYID                          
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
         CLI   QOPT7,C'T'                                                       
         BNE   *+8                                                              
         MVI   TESTMQ,C'T'                                                      
*                                                                               
         CLI   TESTMQ,C'T'  PUTTING TO TEST BROKER?                             
         BNE   *+10                                                             
REQF13H  MVC   MQMAPNM+9(4),=C'TEST'                                            
*                                                                               
REQF13K  MVI   BYTE,X'45'         X'04' = BIG NAMES                             
         MVC   DUB,=X'000005000001'                                             
         CLI   CHSFTP,C'Y'        SEE IF DOING CH SFTP FILE                     
         BNE   REQF13M                                                          
         GOTO1 DYNALLOC,DMCB,(X'80',=C'SCHTAPE '),(BYTE,DUB),          X        
               (X'80',MQMAPNM)                                                  
*                                                                               
         OPEN  (SCHTAPE,(OUTPUT))                                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVI   CHOPENSW,C'Y'     SET BK FILE OPEN                               
         B     REQF13M                                                          
*                                                                               
*                                                                               
REQF13M  DS    0H                                                               
*                                                                               
REQF75   B     FIRSTB0X                                                         
*                                                                               
FIRSTB0  DS    0H                                                               
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
         CLC   0(2,R1),QAGY                                                     
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
*                                  SAVE DATES FOR ACTIVITY CHECKING             
         CLC   QEND,SPACES                                                      
         BNE   *+10                                                             
         MVC   QEND,QSTART     SET QEND TO QSTART IF NOT ENTERED                
*                                                                               
         GOTO1 DATCON,DMCB,(0,QSTART),(3,SVQSTART)                              
*                                                                               
         GOTO1 DATCON,DMCB,(0,QEND),(3,SVQEND)                                  
FIRSTB3  MVC   SVSTART(12),QSTART    SAVE EBCDIC DATES FOR RUNLAST              
*                                                                               
FIRSTB3C DS    0H                                                               
         GOTO1 DATCON,DMCB,TODAY,(3,TODAYB)                                     
         ZIC   R1,TODAYB                                                        
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
         M     R0,=F'10'                                                        
         STC   R1,DECADE           1980,1990,2000,2010, ETC.                    
*                                  HEX 50,5A,64,6E...                           
         MVC   YEARDIG,TODAY+1     GET YEAR WITHIN DECADE                       
         NI    YEARDIG,X'FF'-X'F0' ISOLATE YEAR DIGIT                           
*                            SO ALL INSERTIONS WILL BE PROCESSED                
*                            I MUST CLEAR THESE START AND END DATES             
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
         XC    B1PROF,B1PROF        FIRST READ B1 AND B1X PROFILES              
         XC    B1PROFC,B1PROFC      B1 FOR SUBMEDIA C- CABLE                    
         XC    B1PROFO,B1PROFO      B1 FOR SUBMEDIA O- CINEMA (SPRINT)          
         XC    B1PROFS,B1PROFS      B1 FOR SUBMEDIA S- SYNDICATION              
         XC    B1XPROF,B1XPROF      B1X PROFILE                                 
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'SOB1'                                                 
         MVC   WORK+4(2),AGY                                                    
         MVC   WORK+6(1),MED                                                    
         MVC   WORK+7(3),CLT                                                    
         L     RF,ADCLT                                                         
         USING CLTHDR,RF                                                        
*      NOW ALWAYS PASS OFFICE DATA                                              
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),COFFICE                                               
         DROP  RF                                                               
*                                                                               
FBC1     DS    0H                                                               
         GOTO1 GETPROF,DMCB,WORK,B1PROF,DATAMGR                                 
         MVI   WORK+6,C'C'                                                      
         GOTO1 GETPROF,DMCB,WORK,B1PROFC,DATAMGR                                
         MVI   WORK+6,C'O'                                                      
         GOTO1 GETPROF,DMCB,WORK,B1PROFO,DATAMGR                                
         MVI   WORK+6,C'S'                                                      
         GOTO1 GETPROF,DMCB,WORK,B1PROFS,DATAMGR                                
         MVC   WORK+6(1),MED      RESTORE                                       
         MVC   WORK(4),=C'SB1X'                                                 
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
         BAS   RE,FNDAAA      FIND PRD AAA AND STORE ITS ADDRESS                
*                                                                               
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
*                                                                               
FPRD     DS    0H                  FIRST BUY FOR PRODUCT                        
         L     R2,ADPRD                                                         
         USING PRDHDR,R2                                                        
*                                                                               
FPRD5    DS    0H                SAVE BRAND'S ADDRESSES                         
         MVC   BPRDBILL,PADDR1                                                  
         MVC   BPRDLIN1,PADDR2                                                  
         MVC   BPRDLIN2,PADDR3                                                  
         MVC   BPRDATTN,PADDR4                                                  
*                                                                               
         XC    PRDU1,PRDU1                                                      
         CLI   MYUSER,C'Y'        SEE IF USING USER FIELDS                      
         BNE   FPRDSAVE                                                         
         CLC   PKEYPRD,=C'AAA'    NOT FOR PRD=AAA                               
         BE    FPRDSAVE                                                         
*                                                                               
         GOTO1 VGETUSER,DMCB,(C'S',ADCLT),(C'P',ADPRD),(C':',PRDU1),0           
         CLI   DMCB,X'FF'                                                       
         BE    FPRDERR                                                          
****     CLI   PRDU1+21,C' '    MUST FIND DATA                                  
****     BNH   FPRDERR                                                          
         B     FPRDSAVE                                                         
*                                                                               
FPRDERR  DS    0H                                                               
         B     FPRDSAVE         P USER NOT NEEDED                               
*****    MVC   P1,SPACES                                                        
*****    MVC   P1(36),=C'*** MISSING PRODUCT USER FIELD 1 ***'                  
*****    MVC   P1+40(3),PKEYPRD                                                 
*****    MVI   SPACING,2                                                        
*****    GOTO1 REPORT                                                           
*****    CLI   QOPT6,C'Y'            SEE IF TEST RUN                            
*****    BE    FPRDSAVE              CONTINUE - ELSE DIE                        
*****    MVC   P1(23),=C'*** REQUEST STOPPED ***'                               
*****    GOTO1 REPORT                                                           
*****    DC    H'0'              MUST DIE                                       
*                                                                               
FPRDSAVE DS    0H                                                               
*                                                                               
         CLC   PKEYPRD,=C'AAA'                                                  
         BNE   FPRDFORM                                                         
*                                                                               
         MVI   AAAPRD,C'Y'                                                      
         MVC   AAAFORMU(5),PBILLBAS         MOVE FORMULA FOR AAA                
         B     FPRDX                                                            
*                                                                               
FPRDFORM DS    0H                                                               
         MVC   PRDFORMU(5),PBILLBAS                                             
*                                                                               
         DROP  R2                                                               
*                                                                               
FPRDX    B     EXIT                                                             
         EJECT                                                                  
*                                                                               
FEST     DS    0H                  ESTIMATE FIRST                               
         CLI   QOPT1,C'E'          ESTIMATE FILE?                               
         BE    FESTX               NOTHING TO DO                                
         L     R2,ADEST                                                         
         USING ESTHDR,R2                                                        
*                                                                               
FEST0    DS    0H                                                               
*                                                                               
         XC    ESTU1,ESTU1       CLEAR EST USER FIELDS                          
         XC    ESTU2,ESTU2                                                      
         GOTO1 VGETUSER,DMCB,(C'S',ADCLT),(C'E',ADEST),(C':',ESTU1),(C'+        
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
         CLOSE (SCHTAPE,)                                                       
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
RUNLBK2B L     RF,VMASTC                                                        
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
*****    CLC   KEY+BKEYINV-BKEY(2),LOWINV                                       
*****    BL    EXIT                                                             
*****    CLC   KEY+BKEYINV-BKEY(2),HIINV                                        
*****    BH    EXIT                                                             
*                                                                               
         CLI   KEY+BKEYEST-BKEY,0  SKIP EST 0 BILLS (BILLING BUG)               
         BE    EXIT                                                             
*                                                                               
PRB1     DS    0H                                                               
*                                                                               
PRB1D    DS    0H                                                               
*                                                                               
PRB1F    DS    0H                  IF NOT WESTERN/APL                           
*                                                                               
PRB1H    DS    0H                                                               
*                                                                               
*****    CLC   KEY+BKEYYSRV-BKEY(2),STARTMOS  MONTH-OF-SERVICE FILTERS          
*****    BL    EXIT                                                             
*****    CLC   KEY+BKEYYSRV-BKEY(2),ENDMOS                                      
*****    BH    EXIT                                                             
*                                                                               
         CLI   QOPT4,C'M'          TEST MOS FILTERING                           
         BNE   PRB2D                                                            
         CLC   KEY+BKEYYSRV-BKEY(2),SVQSTART                                    
         BL    EXIT                                                             
         CLC   KEY+BKEYYSRV-BKEY(2),SVQEND                                      
         BH    EXIT                                                             
         B     PRB3                                                             
*                                                                               
PRB2D    DS    0H                  RUN DATE FILTERING                           
         ZIC   R3,KEY+BKEYMBIL-BKEY                                             
         SRL   R3,4                YEAR DIGIT OF BILL                           
         ZIC   RE,DECADE                                                        
         CLM   R3,1,YEARDIG        COMPARE TO YEAR OF TODAY                     
         BNH   *+8                 IF NOT HIGH, OK                              
         SH    RE,=H'10'           ELSE BACK UP TO PREV DECADE                  
         AR    RE,R3                                                            
         STC   RE,FULL             CALCULATED YEAR OF BILL                      
*                                                                               
         MVC   FULL+1(1),KEY+BKEYMBIL-BKEY                                      
         NI    FULL+1,X'FF'-X'F0'  ISOLATE MONTH                                
*                                                                               
         CLC   FULL(2),SVQSTART                                                 
         BL    EXIT                                                             
         CLC   FULL(2),SVQEND                                                   
         BH    EXIT                                                             
*                                                                               
PRB3     DS    0H                                                               
         GOTO1 GETBILL                                                          
         L     R2,ADBILL                                                        
         USING BILLREC,R2                                                       
*                        MUST CHECK DATE AGAIN - CHECK ABOVE                    
*                        PASSES DECADE OLD BILLS                                
         CLC   BDATE,SVSTART      YYMMDD - START                                
         BL    EXIT                                                             
         CLC   BDATE,SVSTART+6    YYMMDD - END                                  
         BH    EXIT                                                             
*                                                                               
*                                                                               
         B     PB6                SKIP THESE CHECKS FOR THE IN                  
*                                                                               
         CLI   QOPT5+1,C'*'        TEST NETPAK SUB MED FILT                     
         BE    PRB4                NO                                           
         CLI   QOPT5+1,C' '                                                     
         BNH   PRB4                NO                                           
         MVC   BYTE,BLMED                                                       
         CLI   BYTE,C' '           IF NO SUB MEDIA                              
         BH    *+8                                                              
         MVI   BYTE,C'N'           DEFAULT TO N                                 
         CLC   BYTE,QOPT5+1        TEST RIGHT SUB-MED                           
         BNE   EXIT                                                             
*                                                                               
PRB4     DS    0H                                                               
         TM    BILSTAT,BSTCMONQ    TEST COMMISSION ONLY BILL                    
         BO    *+16                YES                                          
         CLI   QOPT5,C'C'          NO, TEST TO SKIP OTHERS                      
         BE    EXIT                                                             
         B     *+12                                                             
         CLI   QOPT5,C'N'          EXCLUDE COMMISSION-ONLY BILLS?               
         BE    EXIT                YES                                          
*                                                                               
         CLI   QOPT5,C'A'          AOR BILLS ONLY?                              
         BNE   *+12                                                             
         TM    BILSTAT,BSTTAORQ                                                 
         BZ    EXIT                                                             
*                                                                               
         CLI   QOPT5,C'B'          AOR AND AOR/CLIENT BILLS                     
         BNE   *+12                                                             
         TM    BILSTAT,BSTTAORQ+BSTCAORQ                                        
         BZ    EXIT                                                             
*                                                                               
         CLI   QOPT5,C'X'          NON-AOR BILLS ONLY?                          
         BNE   *+12                                                             
         TM    BILSTAT,BSTTAORQ                                                 
         BNZ   EXIT                                                             
*                                                                               
         CLI   QOPT5,C'S'          SOON BILLS ONLY?                             
         BNE   *+12                                                             
         TM    BILSTAT3,BSTSOONQ   WAS REC GENERATED BY SOON ?                  
         BNO   EXIT                                                             
*                                                                               
         CLC   =C'*SOON',QUESTOR   IF WAS AUTOREQUESTED BY SOON                 
         BNE   PB6                                                              
         TM    BILSTAT3,BSTSOONQ   WAS REC GENERATED BY SOON ?                  
         BZ    EXIT                                                             
         CLC   BILLUID,RCORIGID    PROCESS ONLY FOR REQUESTING USER ID          
         BNE   EXIT                                                             
*                                                                               
PB6      DS    0H                  SET BILL AMOUNTS                             
*                                                                               
         CP    BACTP,=P'0'         SKIP ZERO AMOUNT BILLS                       
         BE    EXIT                                                             
*                                                                               
PRB09D5  DS    0H                                                               
*                                                                               
PRB09DX  GOTO1 =V(SPBVAL),DMCB,(C'B',BILLREC),SPBVALD                           
*****                                                                           
*****    SET EFFECTIVE VALUES INTO BILLREC                                      
*****                                                                           
         MVC   BGRSP,SPBVGRSP                                                   
         MVC   BNETP,SPBVNETP                                                   
*                                 SET MYGST AND MYPST AND MYHST                 
*****    ZAP   MYGST,=P'0'                                                      
*****    ZAP   MYPST,=P'0'                                                      
         L     R0,SPBVGST                                                       
         CVD   R0,DUB                                                           
         ZAP   MYGST,DUB                                                        
         L     R0,SPBVPST                                                       
         CVD   R0,DUB                                                           
         ZAP   MYPST,DUB                                                        
         L     R0,SPBVHST                                                       
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
*                                                                               
FNDAAA   NTR1                                                                   
         MVC   PPGKEY,KEY                                                       
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD  AGY/MED                                         
         MVC   KEY+2(2),BCLT                                                    
         MVC   KEY+4(3),=C'AAA'                                                 
         L     RF,ADPRD                                                         
         CLC   0(8,RF),KEY                                                      
         BE    FNDP10                                                           
         GOTO1 HIGH                                                             
         CLC   KEY(8),KEYSAVE                                                   
         BE    FNDP08                                                           
         B     FNDAX      PRODUCT AAA NOT ON FILE                               
*                                                                               
FNDP08   GOTO1 GETPRD                                                           
*                                                                               
FNDP10   L     RE,ADPRD                                                         
         USING PRDHDR,RE                                                        
         MVC   APRDBILL(120),PADDR1    SAVE AAA 4 ADDRESS LINES                 
         DROP  RE                                                               
*                                                                               
FNDAX    MVC   KEY,PPGKEY                                                       
         GOTO1 HIGH                                                             
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*        AGENCY TABLE                                                           
*                                                                               
*        AGENCY CODE/VENDOR/COMPANY/MYUSER VALUE                                
*                                   Y=USE USER FIELDS                           
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
**VBAPE  DCB   DDNAME=SCHTAPE,DSORG=PS,RECFM=VB,LRECL=01200,                    
**VB           BLKSIZE=06004,MACRF=PM                                           
SCHTAPE  DCB   DDNAME=SCHTAPE,DSORG=PS,RECFM=FB,LRECL=01000,           X        
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
*                                                                               
         CLI   MODE,RUNLAST                                                     
         BNE   MYRPT5                                                           
         MVC   QSTART(12),SVSTART   RESTORE FOR HEADLINES                       
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
**FB**   LH    R3,HALF                                                          
**FB**   LA    R3,4(R3)                                                         
**FB**   STH   R3,OUTREC-4                                                      
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
         GOTO1 (RF),(R1),OUTREC+50,P2+18,50,=C'N'                               
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
         GOTO1 (RF),(R1),OUTREC+650,P14+18,50,=C'N'                             
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
         MVC   P14+1(7),=C'651-700'                                             
WRIT1E   MVI   SPACING,2                                                        
         BAS   RE,MYRPT                                                         
WRIT2    DS    0H                                                               
         CLI   SVQOPT6,C'Y'     SEE IF TEST RUN                                 
         BE    WRIT3            THEN NO TAPE                                    
         CLI   QOPT6,C'Y'       SEE IF TEST RUN                                 
         BE    WRIT3            THEN NO TAPE                                    
*******  LA    R1,SCHTAPE                                                       
         L     R1,ASCHTAPE                                                      
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
**VB     LH    RF,OUTREC-4                                                      
**VB     LA    RF,COLHDLEN(RF)                                                  
**VB     STH   RF,OUTREC-4                                                      
         MVC   RECTYPE,=C'CH'      COLUMN HEADINGS                              
         BAS   RE,MWRITE                                                        
         XMOD1 1                                                                
         LTORG                                                                  
         EJECT                                                                  
         DROP  R2                                                               
*                         CREATE FILE RECORD                                    
BLDREC   CSECT                                                                  
         NMOD1 0,BLDREC                                                         
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
*                                                                               
*                                                                               
         LA    R2,OUTREC                                                        
* CUSTOMER ID                                                                   
*******  MVC   0(09,R2),=C'"CHOICE",'                                           
*******  LA    R2,09(R2)                                                        
         MVC   0(20,R2),=C'"US521209792CHOICE",'                                
         LA    R2,20(R2)                                                        
*                                                                               
* REMIT TO INFO   - DATA FROM SPOT/NETPAK?                                      
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
         MVC   0(13,R2),=C'"13-3977932",'                                       
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
*                                                                               
         L     R3,ADBILL                                                        
         USING BILLREC,R3                                                       
*                                                                               
         GOTO1 AFMTINO,DMCB,BDATE,(2,BKEYINV),(QMED,B1PROF),B1XPROF             
         DROP  R3                                                               
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
         L     R3,ADBILL                                                        
         USING BILLREC,R3                                                       
*                                                                               
         MVI   0(R2),C'"'                                                       
         GOTO1 DATCON,DMCB,(0,BQDATE),(X'20',WORK)                              
         DROP  R3                                                               
         MVC   1(2,R2),WORK+2    MM                                             
         MVI   3(R2),C'/'                                                       
         MVC   4(2,R2),WORK+4    DD                                             
         MVI   6(R2),C'/'                                                       
         MVC   7(2,R2),=C'20'    CENTRY 2000                                    
         MVC   9(2,R2),WORK+0    YY                                             
         MVC   11(2,R2),=C'",'                                                  
         LA    R2,13(R2)                                                        
*                                                                               
BLDI17X  MVC   0(03,R2),=C',,,'  3 EMPTY FIELDS                                 
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
*                                                                               
         MVC   0(01,R2),=C','    1 EMPTY FIELD - SUPPLIER                       
         LA    R2,1(R2)                                                         
*                                 DESCRIPTION - MEDIA NAME                      
*                                                                               
         MVI   0(R2),C'"'                                                       
         LA    R2,1(R2)                                                         
*                                                                               
         CLI   NETPAKSW,C'Y'         SEE IF NETPAK                              
         BE    BLDI19                                                           
*                                                                               
         MVC   0(07,R2),=C'RADIO",'                                             
         LA    R2,7(R2)                                                         
         CLI   MED,C'R'                                                         
         BE    BLDI20                                                           
         SH    R2,=H'7'    RETURN TO START                                      
*                                                                               
         MVC   0(09,R2),=C'NETWORK",'                                           
         LA    R2,9(R2)                                                         
         CLI   MED,C'N'                                                         
         BE    BLDI20                                                           
         SH    R2,=H'9'    RETURN TO START                                      
         MVC   0(12,R2),=C'TELEVISION",'                                        
         LA    R2,12(R2)                                                        
         CLI   MED,C'T'                                                         
         BE    BLDI20                                                           
         SH    R2,=H'12'   RETURN TO START                                      
         MVC   0(15,R2),=C'RADIO NETWORK",'                                     
         LA    R2,15(R2)                                                        
         CLI   MED,C'X'                                                         
         BE    BLDI20                                                           
         B     BLDIERR     UNKNOWN SPOT MEDIA                                   
*                                                                               
BLDI19   L     R3,ADBILL          HERE FOR NETPAK                               
         USING BILLREC,R3                                                       
         MVC   0(15,R2),SPACES   CLEAR                                          
         MVC   0(07,R2),=C'CABLE",'                                             
         LA    R2,07(R2)                                                        
         CLI   BLMED,C'C'                                                       
         BE    BLDI20                                                           
         SH    R2,=H'07'    RETURN TO START                                     
         MVC   0(12,R2),=C'NETWORK TV",'                                        
         LA    R2,12(R2)                                                        
         CLI   BLMED,C'N'                                                       
         BE    BLDI20                                                           
         CLI   BLMED,C' '   NO SUB MEDIA                                        
         BE    BLDI20                                                           
         SH    R2,=H'12'    RETURN TO START                                     
         MVC   0(13,R2),=C'SYNDICATION",'                                       
         LA    R2,13(R2)                                                        
         CLI   BLMED,C'S'                                                       
         BE    BLDI20                                                           
         SH    R2,=H'13'    RETURN TO START                                     
         MVC   0(15,R2),=C'NETWORK-OTHER",'                                     
         LA    R2,15(R2)                                                        
         CLI   BLMED,C'O'                                                       
         BE    BLDI20                                                           
         SH    R2,=H'15'    RETURN TO START                                     
         MVC   0(15,R2),=C'NETWORK-RADIO",'                                     
         LA    R2,15(R2)                                                        
         CLI   BLMED,C'D'                                                       
         BE    BLDI20                                                           
         SH    R2,=H'15'                                                        
         MVC   0(15,R2),=C'NETWORK-OTHER",'    FOR ANY OTHER SUB-MEDIA          
         LA    R2,15(R2)                                                        
         B     BLDI20                                                           
         DROP  R3                                                               
*                                                                               
BLDIERR  DC    H'0'         UNKNOWN SPOT MEDIA                                  
*                                                                               
BLDI20   DS    0H                                                               
*                                                                               
         L     R3,ADBILL                                                        
         USING BILLREC,R3                                                       
         ZAP   DUB,BACTP          AMT DUE                                       
         CVB   RF,DUB                                                           
         ST    RF,MYFULL                                                        
         DROP  R3                                                               
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
         L     R3,ADBILL                                                        
         USING BILLREC,R3                                                       
         OC    BTAXAMT,BTAXAMT                                                  
         BZ    NOTAX                                                            
*                                                                               
         MVI   0(R2),C'"'                                                       
         LA    R2,1(R2)                                                         
         EDIT  (B4,BTAXAMT),(14,0(R2)),2,ALIGN=LEFT                             
         AR    R2,R0        ADD LENGTH OF OUTPUT                                
         MVC   0(2,R2),=C'",'                                                   
         LA    R2,2(R2)                                                         
         B     NOTAXX                                                           
         DROP  R3                                                               
*                                                                               
NOTAX    MVI   0(R2),C','                                                       
         LA    R2,1(R2)                                                         
*                                                                               
NOTAXX   DS    0H            MYFULL SHOULD STILL HACE AMOUNT DUE                
         MVI   0(R2),C'"'                                                       
         LA    R2,1(R2)                                                         
         EDIT  (B4,MYFULL),(14,0(R2)),2,ALIGN=LEFT,FLOAT=-                      
         AR    R2,R0        ADD LENGTH OF OUTPUT                                
         MVC   0(2,R2),=C'",'                                                   
         LA    R2,2(R2)                                                         
*                                                                               
         L     R3,ADBILL                                                        
         USING BILLREC,R3                                                       
         CP    BACTP,=P'0'                                                      
         BNL   NOCM                                                             
         DROP  R3                                                               
         MVC   0(05,R2),=C'"CR",'                                               
         LA    R2,5(R2)                                                         
         B     NOCMX                                                            
*                                                                               
NOCM     MVI   0(R2),C','    EMPTY CM INDICATOR                                 
         LA    R2,1(R2)                                                         
NOCMX    MVI   0(R2),C','    EMPTY ATTACHMENT FILE NAME                         
***NO    LA    R2,1(R2)                                                         
***NO    MVC   0(2,R2),=X'0D0A'     CRLF                                        
*                                                                               
*   END OF RECORD                                                               
*************************                                                       
** BELOW IS OLD SPOT/NET CODE                                                   
*************************                                                       
*                                                                               
*                                                                               
BLDREND  DS    0H                                                               
**VB     LA    RE,OUTREC-4                                                      
**VB     LR    RF,R2                                                            
**VB     SR    RF,RE      DIFFERENCE SHOULD BE RECORD LENGTH                    
**VB     STH   RF,OUTREC-4                                                      
         BAS   RE,MWRITE                                                        
         B     BLDRX                                                            
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
MYGETCOM DS    0H              BUILD COMMENT LINE                               
*                              CLT PRD PRDNAME EST ESTNAME                      
         MVC   WORK(12),SPACES                                                  
         MVC   WORK(3),CLT                                                      
         L     R3,ADPRD                                                         
         USING PRDHDR,R3                                                        
         MVC   WORK+4(3),PKEYPRD                                                
                                                                                
****     MVC   WORK+7(20),PNAME                                                 
         DROP  R3                                                               
****     OC    WORK+7(20),SPACES                                                
****     LA    R1,WORK+30                                                       
****M5   CLI   0(R1),C' '                                                       
****     BH    GETCM10                                                          
****     BCT   R1,GETCM5                                                        
*                                                                               
GETCM10  L     R3,ADEST                                                         
         USING ESTHDR,R3                                                        
         ZIC   R0,EKEYEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK+8(3),DUB+6(2)                                               
         BR    RE                                                               
****                                                                            
****     CODE BELOW FOR ESTIMATE NAMES                                          
****                                                                            
****     UNPK  2(3,R1),DUB+6(2)                                                 
****     MVC   6(20,R1),EDESC                                                   
****     OC    6(20,R1),SPACES                                                  
****     BR    RE             RETURN                                            
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
BLDRX    XMOD1                                                                  
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
SPCHWRKD DSECT                                                                  
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
NETPAKSW DS    CL1         Y=NETPAK REQUEST                                     
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
PRDCITY  DS    CL30        EXTRACTED FROM PADDR2                                
PRDST    DS    CL30                                                             
PRDZIP   DS    CL30                                                             
*                                                                               
INSCOM1  DS    CL47        1ST INSERTION COMMENT                                
INSCOM2  DS    CL47        2ND INSERTION COMMENT                                
*                                                                               
APRDBILL DS    CL30        SAVED FROM PRD AAA                                   
APRDLIN1 DS    CL30        SAVED FROM PRD AAA                                   
APRDLIN2 DS    CL30        SAVED FROM PRD AAA                                   
APRDATTN DS    CL30        SAVED FROM PRD AAA                                   
*                                                                               
BPRDBILL DS    CL30        SAVED FROM BRAND                                     
BPRDLIN1 DS    CL30        SAVED FROM BRAND                                     
BPRDLIN2 DS    CL30        SAVED FROM BRAND                                     
BPRDATTN DS    CL30        SAVED FROM BRAND                                     
*                                                                               
YEARDIG  DS    XL1                                                              
DECADE   DS    XL1                                                              
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
SVQSTART DS    XL3                                                              
SVQEND   DS    XL3                                                              
*                                                                               
B1PROF   DS    CL16                                                             
B1PROFC  DS    CL16                                                             
B1PROFO  DS    CL16                                                             
B1PROFS  DS    CL16                                                             
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
       ++INCLUDE SPBVALD                                                        
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
ASPBVAL  DS    A                                                                
ACONIO1  DS    A                                                                
ANXTINV  DS    A                                                                
AREGTAB  DS    A                                                                
ANXTREG  DS    A                                                                
AREGTABX DS    A                                                                
ASCHTAPE DS    A                                                                
*                                                                               
MYUSER   DS    CL1       SET FROM AGYTAB AT FBUYREQ                             
*                                                                               
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
         EJECT                                                                  
NETBLK   CSECT                                                                  
         DS    1200C                                                            
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
NETBLKD  DSECT                                                                  
*                                                                               
       ++INCLUDE NETBLOCKD                                                      
       ++INCLUDE NENETRATED                                                     
       ++INCLUDE SPGENSTAB                                                      
         PRINT OFF                                                              
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE NETBILLRD                                                      
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE SPGENMKT                                                       
       ++INCLUDE SPGENBUY                                                       
       ++INCLUDE SPGENBILL                                                      
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENAGY                                                       
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPMEDBLOCK                                                     
       ++INCLUDE DDBUFFALOD                                                     
       ++INCLUDE DDREPMASTD                                                     
*                                                                               
         PRINT ON                                                               
       ++INCLUDE DDUCOMD                                                        
QOPT6    EQU   QGRP                                                             
QOPT7    EQU   QGRP+1                                                           
       ++INCLUDE DDPERVALD                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'014SPREPCH02 08/17/15'                                      
         END                                                                    
