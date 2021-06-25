*          DATA SET PPREPSE02  AT LEVEL 064 AS OF 07/18/18                      
*PHASE PPSE02A                                                                  
*INCLUDE GETUSER                                                                
*INCLUDE DDUCOM                                                                 
*INCLUDE PPFMTINO                                                               
*INCLUDE PPBVAL                                                                 
*INCLUDE PERVAL                                                                 
         TITLE 'PPSE02 - SPRINT INTERFACE'                                      
*                                                                               
*        CHANGE LOG                                                             
*********************************************************************           
* USER    JIRA       DATE                  CHANGE LOG                           
* ---- ----------  -------- -----------------------------------------           
* SMUR SPEC-17729  5/15/18 SUPPORT FOR MEDIA (D)IGITAL AUDIO (18.3)             
*                                                                               
*   BPLA  07/15  SUPPORT FOR MEDIA B,V,W                                        
*                                                                               
*   BPLA  05/14  SUPPORT FOR MEDIA L                                            
*                                                                               
*   BPLA  08/10  REPORT FIRST 2 INSERTION COMMENTS                              
*                IN InstallationAddr1 and InstallationCity                      
*                                                                               
*   BPLA  06/10  REPORT 2nd ESTIMATE USER DATA IN CustomerOrderNumber           
*                                                                               
*   BPLA  06/10  CHANGE TO MINSHARE'S REMITTANCE ADDRESS                        
*                                                                               
*   BPLA  08/07  CHANGES FOR SPRINT'S CHANGE TO SPRINT NEXTEL                   
*                                                                               
*   BPLA  06/06  REWORK FOR MINDSHARE                                           
*                                                                               
*        QOPT6 Y= TEST RUN - NO TAPE, AND CONTINUE IF ERRORS                    
*                            ARE FOUND                                          
*        QOPT7 P=PDUMP RECORDS                                                  
*                                                                               
*        QSTART(6) = PERIOR START                                               
*        QEND(6) = PERIOD END - MAY BE BLANK                                    
*                                                                               
*        NOTE: REQUEST BY DIV/REG FOR OUTDOOR AND NEWSPAPERS                    
*              REQ PROGRAM SHOULD SET                                           
*                                                                               
PPSE02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PPSE02,RR=R9                                                   
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
         USING PPSEWRKD,R8                                                      
         MVC   ACONIO1,ACONIO                                                   
         LA    R7,PPSE02+4095                                                   
         LA    R7,1(R7)                                                         
         USING PPSE02+4096,R7     **NOTE USE OF R7 AS BASE REG*                 
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    INITIAL                                                          
         CLI   MODE,REQFRST                                                     
         BE    FIRSTB                                                           
         CLI   MODE,FBUYCLI                                                     
         BE    FCLI                                                             
         CLI   MODE,FBUYEST                                                     
         BE    FEST                                                             
         CLI   MODE,FBUYPRO                                                     
         BE    FPRD                                                             
         CLI   MODE,REQLAST                                                     
         BE    PUTBUFF                                                          
         CLI   MODE,RUNLAST                                                     
         BE    TOTALS                                                           
         CLI   MODE,PROCBUY                                                     
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
         MVI   ZEROS,C'0'                                                       
         MVC   ZEROS+1(L'ZEROS-1),ZEROS                                         
*                                                                               
         MVI   DMINBTS,X'08'                                                    
         MVI   DMOUTBTS,X'FD'                                                   
         MVI   TAPESW,0                                                         
         MVI   ETAPESW,0                                                        
         MVI   ALLOWSW,0     DYNAMIC ALLOCATION INV. FILE NOT DONE              
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
         MVC   DYNDDN,=CL8'PSETAPE'                                             
         MVC   DYNDSN,=CL20'PRTTAPE.PP0SEXX1'                                   
         MVC   DYNDSN+13(2),QAGENCY                                             
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
         CLI   QOPT6,C'Y'        SEE IF TEST REQUEST                            
         BE    FIRSTB0                                                          
         CLI   TAPESW,C'N'       SEE IF A PRIOR REQUEST WAS TEST                
         BE    MIXERR                                                           
         MVI   TAPESW,C'Y'       SET TAPE BEING PRODUCED                        
*                                                                               
         TM    ALLOWSW,X'01'     DYNAMIC ALLOCATION DONE ALREADY?               
         BO    REQF75                                                           
*                                                                               
         GOTO1 DYNALLOC,DMCB,(0,DYNDDN),(0,DYNDSN)                              
         OI    ALLOWSW,X'01'    SO I WON'T DO AGAIN                             
         PRINT GEN                                                              
         OPEN  (PSETAPE,OUTPUT)                                                 
         PRINT NOGEN                                                            
         B     REQF75                                                           
*                                                                               
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
*                                  SAVE DATES FOR ACTIVITY CHECKING             
         GOTO1 DATCON,DMCB,(0,QSTART),(3,SVQSTART)                              
         MVC   SVQEND,=3X'FF'                                                   
*                                                                               
*                                                                               
FIRSTB2X CLC   QEND,SPACES                                                      
         BE    FIRSTB3                                                          
         GOTO1 DATCON,DMCB,(0,QEND),(3,SVQEND)                                  
FIRSTB3  MVC   SVSTART(12),QSTART    SAVE EBCDIC DATES FOR BUY CHK              
*                                                                               
         CLC   QEND,SPACES                                                      
         BNE   FIRSTB3C                                                         
*NOP*    MVC   SVEND(6),=C'999999'                                              
         MVC   SVEND(6),=6X'FF'    FOR 21ST CENTURY                             
*                                                                               
FIRSTB3C DS    0H                                                               
*                            SO ALL INSERTIONS WILL BE PROCESSED                
*                            I MUST CLEAR THESE START AND END DATES             
         MVC   QSTART,SPACES                                                    
         MVC   QEND,SPACES                                                      
         MVC   BQSTART,=X'000000'                                               
         MVC   BQEND,=X'FFFFFF'                                                 
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
FPRD5    DS    0H                                                               
         B     FPRDX               DON'T NEED TO DO ANYTHING ELSE?              
*                                                                               
         XC    PRDU1,PRDU1                                                      
         CLI   MYUSER,C'Y'        SEE IF USING USER FIELDS                      
         BNE   FPRDSAVE                                                         
         CLC   PPRDKPRD,=C'AAA'    NOT FOR PRD=AAA                              
         BE    FPRDSAVE                                                         
*                                                                               
         GOTO1 VGETUSER,DMCB,(C'P',PCLTREC),(C'P',PPRDREC),PRDU1,0              
         CLI   DMCB,X'FF'                                                       
         BE    FPRDERR                                                          
         CLI   PRDU1+21,C' '    MUST FIND DATA                                  
         BNH   FPRDERR                                                          
         B     FPRDSAVE                                                         
*                                                                               
FPRDERR  DS    0H                                                               
         MVC   P1,SPACES                                                        
         MVC   P1(36),=C'*** MISSING PRODUCT USER FIELD 1 ***'                  
         MVC   P1+40(3),PPRDKPRD                                                
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         CLI   QOPT6,C'Y'            SEE IF TEST RUN                            
         BE    FPRDSAVE              CONTINUE - ELSE DIE                        
         MVC   P1(23),=C'*** REQUEST STOPPED ***'                               
         GOTO1 REPORT                                                           
         DC    H'0'              MUST DIE                                       
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
         GOTO1 VGETUSER,DMCB,(C'P',PCLTREC),(C'E',PESTREC),ESTU1,ESTU2          
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
         CLOSE (PSETAPE,)                                                       
*                                                                               
TOT5     B     EXIT                                                             
*                                                                               
         EJECT                                                                  
PROCESS  DS    0H       PROCESS BUYREC                                          
*                                                                               
*        CALL PPBYOUT  - NEEDED FOR SPACE DESCRIPTION                           
*                                                                               
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE1,X'26'    CHECK FOR BILLING ELEM WITHIN PERIOD            
PROCB5   BAS   RE,NEXTEL                                                        
         BNE   EXIT                                                             
         USING PBILELEM,R2                                                      
         OC    PBLDATE,PBLDATE  CHECK FOR DATE                                  
         BZ    PROCB5           NONE- SKIP                                      
         CLC   PBLDATE,SVQSTART    SEE IF BILLED IN PERIOD                      
         BL    PROCB5                                                           
         CLC   PBLDATE,SVQEND                                                   
         BH    PROCB5                                                           
         ST    R2,ELADDR      SAVE ADDRESS OF BILLING ELEMENT                   
         GOTO1 =A(BLDREC)                                                       
         B     PROCB5         LOOK FOR MORE                                     
*                                                                               
         DROP  R2                                                               
         DS    XL2200          SO ROUTINES BELOW CAN BE ACCESSED                
*                              BY OTHER CODE                                    
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
         CLI   SVQOPT7,C'P'                                                     
         BE    WRIT1                                                            
         CLI   QOPT7,C'P'                                                       
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
         MVC   P9+1(125),OUTREC+1000                                            
         MVI   P3,0       SO LINE WILL ALWAYS PRINT                             
         MVI   P4,0       SO LINE WILL ALWAYS PRINT                             
         MVI   P5,0       SO LINE WILL ALWAYS PRINT                             
         MVI   P6,0       SO LINE WILL ALWAYS PRINT                             
         MVI   P7,0       SO LINE WILL ALWAYS PRINT                             
         MVI   P8,0       SO LINE WILL ALWAYS PRINT                             
         MVI   P9,0       SO LINE WILL ALWAYS PRINT                             
*                                                                               
WRIT1A   MVI   SPACING,2                                                        
         BAS   RE,MYRPT                                                         
         MVI   RCSUBPRG,10                                                      
         GOTO1 HEXOUT,DMCB,OUTREC-4,P1+10,54,=C'N'                              
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
         LA    R1,PSETAPE                                                       
*                                                                               
WRIT2B   LA    R0,OUTREC-4                                                      
         PRINT GEN                                                              
         PUT   (1),(0)                                                          
         PRINT NOGEN                                                            
WRIT3    AP    TOTCNT,=P'1'                                                     
         XIT1                                                                   
*                                                                               
MYRPT    NTR1                                                                   
*                                                                               
         CLI   QOPT6,C'Y'              SEE IF TEST RUN                          
         BE    MYPRT5                                                           
         CLI   SVQOPT6,C'Y'            SEE IF TEST RUN                          
         BNE   *+10                                                             
*                                                                               
MYPRT5   MVC   HEAD4+54(12),=C'**TEST RUN**'                                    
*                                                                               
         MVC   QSTART(12),SVSTART      RESTORE FOR HEADLINES                    
*NOP*    CLC   SVEND,=C'999999'                                                 
         CLC   SVEND,=6X'FF'       FOR 21ST CENTURY                             
         BNE   MYRPT5                                                           
         MVC   QEND,SPACES                                                      
MYRPT5   GOTO1 REPORT                                                           
         MVC   QSTART(12),SPACES                                                
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
*                                                                               
FNDAAA   NTR1                                                                   
         MVC   PPGKEY,KEY                                                       
         XC    KEY,KEY                                                          
         MVC   KEY(7),PCLTKAGY     AGY/MED/CODE/CLT                             
         MVI   KEY+3,X'06'                                                      
         MVC   KEY+7(3),=C'AAA'                                                 
         CLC   PPRDKAGY(7),KEY                                                  
         BE    FNDP10                                                           
         GOTO1 HIGH                                                             
         CLC   KEY(10),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'       PRODUCT NOT ON FILE                                   
*                                                                               
         LA    RE,PPRDREC                                                       
         ST    RE,AREC                                                          
         GOTO1 GETPRT                                                           
*                                                                               
FNDP10   MVC   APRDBILL,PPRDBILL                                                
         MVC   APRDBIL2,PPRDBIL2                                                
         MVC   APRDLIN1,PPRDLIN1                                                
         MVC   APRDLIN1,SPACES    NOT USED FOR SPRINT NEXTEL                    
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
*                                   E=ESTIMATE USER FIELDS                      
*                                                                               
AGYTAB   DC    C'H7',C'E'        MINDSHARE                                      
         DC    C'SJ',C'E'        SJR                                            
         DC    X'FFFF'                                                          
*                                                                               
AGYTABL  EQU   3                                                                
         EJECT                                                                  
*                                                                               
         PRINT GEN                                                              
PSETAPE  DCB   DDNAME=PSETAPE,DSORG=PS,RECFM=VB,LRECL=01100,           X        
               BLKSIZE=05504,MACRF=PM                                           
         PRINT NOGEN                                                            
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
         MVC   OUTREC-4(2),=H'4'    STARTING RECORD LENGTH                      
         LA    RE,OUTREC           CLEAR OUTREC                                 
         LH    RF,=H'1100'                                                      
         XCEF                                                                   
*                                                                               
         L     RF,=A(COLHDS)                                                    
         MVC   OUTREC(250),0(RF)                                                
         MVC   OUTREC+250(250),250(RF)                                          
         MVC   OUTREC+500(250),500(RF)                                          
         MVC   OUTREC+750(250),750(RF)                                          
         MVC   OUTREC+1000(COLHDLEN-1000),1000(RF)                              
         LH    RF,OUTREC-4                                                      
         LA    RF,COLHDLEN(RF)                                                  
         STH   RF,OUTREC-4                                                      
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
         MVC   RECTYPE,=C'IR'       INSERTION BILLING ELEMENT REC               
*                                                                               
         MVC   OUTREC-4(2),=H'4'    STARTING RECORD LENGTH                      
         LA    RE,OUTREC           CLEAR OUTREC                                 
         LH    RF,=H'1100'                                                      
         XCEF                                                                   
*                                                                               
         LA    R2,OUTREC                                                        
         MVC   0(8,R2),=C'"SPRNT",'                                             
         LA    R2,8(R2)                                                         
*                                                                               
         MVI   0(R2),C'"'                                                       
         MVC   1(20,R2),APRDBILL    PRD BILL NAME                               
         LA    R2,20(R2)                                                        
BLDR2    CLI   0(R2),C' '         SCAN BACKWARD FOR NON-SPACE                   
         BH    BLDR4                                                            
         BCT   R2,BLDR2                                                         
BLDR4    DS    0H                                                               
         MVC   1(2,R2),=C'",'                                                   
         LA    R2,3(R2)                                                         
*                                                                               
         MVI   0(R2),C'"'                                                       
         MVC   1(20,R2),APRDBIL2    2ND BILL NAME                               
         LA    R2,20(R2)                                                        
BLDR5    CLI   0(R2),C' '         SCAN BACKWARD FOR NON-SPACE                   
         BH    BLDR5A                                                           
         BCT   R2,BLDR5                                                         
BLDR5A   DS    0H                                                               
         MVC   1(2,R2),=C'",'                                                   
         LA    R2,3(R2)                                                         
*                                                                               
         MVI   0(R2),C'"'                                                       
         MVC   1(30,R2),APRDLIN1  ADDRESS LINE1                                 
         LA    R2,30(R2)                                                        
BLDR6    CLI   0(R2),C' '         SCAN BACKWARD FOR NON-SPACE                   
         BH    BLDR6A                                                           
         BCT   R2,BLDR6                                                         
BLDR6A   DS    0H                                                               
         MVC   1(2,R2),=C'",'                                                   
*                                                                               
         LA    R2,3(R2)                                                         
*                                                                               
         MVI   0(R2),C'"'                                                       
         LA    R2,1(R2)                                                         
         BAS   RE,GETCITY           RETURNED IN PRDCITY (CITY)                  
         MVC   0(30,R2),PRDCITY                                                 
         LA    R2,30(R2)                                                        
BLDR7    CLI   0(R2),C' '         SCAN BACKWARD FOR NON-SPACE                   
         BH    BLDR7A                                                           
         BCT   R2,BLDR7                                                         
BLDR7A   DS    0H                                                               
         MVC   1(2,R2),=C'",'                                                   
         LA    R2,3(R2)                                                         
*                                                                               
         MVI   0(R2),C'"'                                                       
         LA    R2,1(R2)                                                         
         BAS   RE,GETST             RETURNED IN PRDST (STATE)                   
         MVC   0(2,R2),PRDST                                                    
         LA    R2,2(R2)                                                         
BLDR7B   CLI   0(R2),C' '         SCAN BACKWARD FOR NON-SPACE                   
         BH    BLDR7C                                                           
         BCT   R2,BLDR7B                                                        
BLDR7C   DS    0H                                                               
         MVC   1(2,R2),=C'",'                                                   
         LA    R2,3(R2)                                                         
*                                                                               
         MVI   0(R2),C'"'                                                       
         LA    R2,1(R2)                                                         
         BAS   RE,GETZIP            RETURNED IN PRDZIP (ZIP)                    
         MVC   0(30,R2),PRDZIP                                                  
         LA    R2,30(R2)                                                        
BLDR7D   CLI   0(R2),C' '         SCAN BACKWARD FOR NON-SPACE                   
         BH    BLDR7E                                                           
         BCT   R2,BLDR7D                                                        
BLDR7E   DS    0H                                                               
         MVC   1(2,R2),=C'",'                                                   
         LA    R2,3(R2)                                                         
*                                                                               
***TST                                                                          
***      MVC   0(7,R2),=C'"CITY",'                                              
***      MVC   7(5,R2),=C'"ST",'                                                
***      MVC   12(6,R2),=C'"ZIP",'                                              
***      LA    R2,18(R2)      PAST CITY/STATE/ZIP                               
***TST                                                                          
         MVC   0(21,R2),=C'"Mindshare USA, LLC",'                               
         LA    R2,21(R2)                                                        
         MVC   0(33,R2),=C'"16368 Collections Center Drive",'                   
         LA    R2,33(R2)                                                        
         MVI   0(R2),C','     EMPTY FIELD                                       
         LA    R2,1(R2)                                                         
         MVC   0(10,R2),=C'"Chicago",'                                          
         LA    R2,10(R2)                                                        
         MVC   0(5,R2),=C'"IL",'                                                
         LA    R2,5(R2)                                                         
         MVC   0(08,R2),=C'"60693",'                                            
         LA    R2,08(R2)                                                        
*                                                                               
         MVI   0(R2),C'"'                                                       
         MVC   1(20,R2),APRDBILL    PRD BILL NAME                               
         LA    R2,20(R2)                                                        
BLDR12   CLI   0(R2),C' '         SCAN BACKWARD FOR NON-SPACE                   
         BH    BLDR14                                                           
         BCT   R2,BLDR12                                                        
BLDR14   DS    0H                                                               
         MVC   1(2,R2),=C'",'                                                   
         LA    R2,3(R2)                                                         
*                                                                               
         MVI   0(R2),C'"'                                                       
         MVC   1(30,R2),APRDLIN1    ADDRESS LINE 1                              
         LA    R2,30(R2)                                                        
BLDR16   CLI   0(R2),C' '         SCAN BACKWARD FOR NON-SPACE                   
         BH    BLDR18                                                           
         BCT   R2,BLDR16                                                        
BLDR18   DS    0H                                                               
         MVC   1(2,R2),=C'",'                                                   
         LA    R2,3(R2)                                                         
*                                                                               
         MVC   0(3,R2),=C'"",'     EMPTY ADDRLINE 2                             
         LA    R2,3(R2)                                                         
*                                                                               
         MVI   0(R2),C'"'                                                       
         LA    R2,1(R2)                                                         
         MVC   0(30,R2),PRDCITY                                                 
         LA    R2,30(R2)                                                        
BLDR16A  CLI   0(R2),C' '         SCAN BACKWARD FOR NON-SPACE                   
         BH    BLDR18A                                                          
         BCT   R2,BLDR16A                                                       
BLDR18A  DS    0H                                                               
         MVC   1(2,R2),=C'",'                                                   
         LA    R2,3(R2)                                                         
*                                                                               
         MVI   0(R2),C'"'                                                       
         LA    R2,1(R2)                                                         
         MVC   0(2,R2),PRDST                                                    
         LA    R2,2(R2)                                                         
BLDR16B  CLI   0(R2),C' '         SCAN BACKWARD FOR NON-SPACE                   
         BH    BLDR18B                                                          
         BCT   R2,BLDR16B                                                       
BLDR18B  DS    0H                                                               
         MVC   1(2,R2),=C'",'                                                   
         LA    R2,3(R2)                                                         
*                                                                               
         MVI   0(R2),C'"'                                                       
         LA    R2,1(R2)                                                         
         MVC   0(30,R2),PRDZIP                                                  
         LA    R2,30(R2)                                                        
BLDR16C  CLI   0(R2),C' '         SCAN BACKWARD FOR NON-SPACE                   
         BH    BLDR18C                                                          
         BCT   R2,BLDR16C                                                       
BLDR18C  DS    0H                                                               
         MVC   1(2,R2),=C'",'                                                   
         LA    R2,3(R2)                                                         
*                                                                               
         MVI   0(R2),C'"'                                                       
         MVC   1(24,R2),APRDATTN    ATTENTION OF                                
         LA    R2,24(R2)                                                        
BLDR26   CLI   0(R2),C' '         SCAN BACKWARD FOR NON-SPACE                   
         BH    BLDR28                                                           
         BCT   R2,BLDR26                                                        
BLDR28   DS    0H                                                               
         MVC   1(2,R2),=C'",'                                                   
         LA    R2,3(R2)                                                         
         MVC   0(3,R2),=C'"",'  EMPTY FIELD                                     
         LA    R2,3(R2)                                                         
         L     R6,ELADDR        ADDRESS OF BILLING ELEMENT                      
         USING PBILELEM,R6                                                      
*                                                                               
         GOTO1 DATCON,DMCB,(3,PBLDATE),(0,WORK)                                 
         GOTO1 AFMTINO,DMCB,WORK,(2,PBINVNO),(QMEDIA,B1PROF),B1XPROF            
         MVC   DINVFULL,SPACES                                                  
         L     RF,DMCB                                                          
         LA    R1,DINVFULL                                                      
         LA    R3,10                                                            
BLDI2    CLI   0(RF),C'-'       SKIP -                                          
         BE    BLDI8                                                            
         MVC   0(1,R1),0(RF)                                                    
         B     BLDI5                                                            
*                                                                               
BLDI5    LA    R1,1(R1)                                                         
BLDI8    LA    RF,1(RF)                                                         
         BCT   R3,BLDI2                                                         
*                                                                               
         MVI   0(R2),C'"'                                                       
*                                                                               
****     MVC   1(2,R2),=C'CM'                                                   
**       CLI   QMEDIA,C'M'        MAGAZINES?                                    
**       BE    BLDI9                                                            
**       MVC   1(2,R2),=C'NP'                                                   
**       CLI   QMEDIA,C'N'        NEWSPAPERS?                                   
**       BE    BLDI9                                                            
**       CLI   QMEDIA,C'S'        OR SUPPLEMENTS?                               
**       BE    BLDI9                                                            
**       MVC   1(2,R2),=C'OH'                                                   
**       CLI   QMEDIA,C'O'        OUTDOOR?                                      
**       BE    BLDI9                                                            
**       MVC   1(2,R2),=C'TM'                                                   
**       CLI   QMEDIA,C'T'        TRADE?                                        
**       BE    BLDI9                                                            
**       MVC   1(2,R2),=C'IN'                                                   
**       CLI   QMEDIA,C'I'        INTERACTIVE?                                  
**       BE    BLDI9                                                            
**       DC    H'0'         UNKNOWN MEDIA                                       
**                                                                              
**DI9    MVC   3(2,R2),WORK+2      SHOULD STILL BE BILLING MONTH                
*                                                                               
*        MINDSHARE JUST WANTS THEIR # - MINUS THE DASHES                        
*                                                                               
         MVC   1(10,R2),DINVFULL                                                
         LA    R2,10(R2)                                                        
BLDI10   CLI   0(R2),C' '                                                       
         BH    BLDI15                                                           
         BCT   R2,BLDI10                                                        
*                                                                               
BLDI15   DS    0H                                                               
**                                                                              
**       NO CR FOR MINDSHARE INVOICES                                           
**                         NEED TO ADD "CR"  IF NEGATIVE INVOICE                
**       MVC   FULL,PBGROSS                                                     
**       L     R0,FULL                                                          
**       MVC   FULL,PBAGYCOM                                                    
**       S     R0,FULL                                                          
**       MVC   FULL,PBCSHDSC                                                    
**       S     R0,FULL                                                          
**       C     R0,=F'0'     CHECK NET-CD                                        
**       BNL   BLDI17                                                           
**       MVC   1(2,R2),=C'CR'                                                   
**       LA    R2,2(R2)                                                         
*                                                                               
BLDI17   MVC   1(2,R2),=C'",'                                                   
         LA    R2,3(R2)                                                         
*                                                                               
         MVI   0(R2),C'"'                                                       
         GOTO1 DATCON,DMCB,(3,PBLDATE),(X'20',WORK)                             
         MVC   1(2,R2),WORK+2    MM                                             
         MVC   3(2,R2),WORK+4    DD                                             
         MVC   5(2,R2),WORK+0    YY                                             
         MVC   7(2,R2),=C'",'                                                   
         LA    R2,9(R2)                                                         
*                                                                               
         ST    R2,FULL                                                          
*                                                                               
         LA    R2,PESTREC+33                                                    
         USING PESTUDEF,R2                                                      
         MVI   ELCODE1,X'08'      LOOK FOR USER ELEMENT                         
         BAS   RE,NEXTEL                                                        
         BNE   BLDR40             MISSING EST UDEF 1                            
         MVC   WORK(32),PEUSER1                                                 
         DROP  R2                                                               
*                                                                               
         L     R2,FULL                                                          
         MVI   0(R2),C'"'                                                       
         MVC   1(32,R2),WORK       FIRST ESTIMATE UDEF                          
         LA    R2,32(R2)                                                        
BLDR36   CLI   0(R2),C' '         SCAN BACKWARD FOR NON-SPACE                   
         BH    BLDR38                                                           
         BCT   R2,BLDR36                                                        
BLDR38   DS    0H                                                               
         MVC   1(2,R2),=C'",'                                                   
         LA    R2,3(R2)                                                         
BLDR39   MVC   0(4,R2),=C'"1",'                                                 
         LA    R2,4(R2)                                                         
         MVC   0(4,R2),=C'"1",'                                                 
         LA    R2,4(R2)                                                         
         B     BLDR45                                                           
*                                                                               
BLDR40   L     R2,FULL          RESTORE R2                                      
         MVI   0(R2),C','       EMPTY UDEF FIELD                                
         LA    R2,1(R2)                                                         
         B     BLDR39                                                           
*                                                                               
BLDR45   DS    0H                                                               
         MVC   WORK(20),SPACES                                                  
         MVC   WORK(17),PBDSPACE                                                
         CLI   PBUYKMED,C'M'        MAGAZINES                                   
         BE    BLDR45X                                                          
         CLI   PBUYKMED,C'S'        SUPPLEMENTS                                 
         BE    BLDR45X                                                          
         CLI   PBUYKMED,C'T'        TRADE                                       
         BE    BLDR45X                                                          
         CLI   PBUYKMED,C'I'        INTERNET                                    
         BE    BLDR45X                                                          
         CLI   PBUYKMED,C'L'        SOCIAL MEDIA                                
         BE    BLDR45X                                                          
         CLI   PBUYKMED,C'B'        MOBILE MEDIA                                
         BE    BLDR45X                                                          
         CLI   PBUYKMED,C'V'        NAT. VIDEO                                  
         BE    BLDR45X                                                          
         CLI   PBUYKMED,C'W'        LOC. VIDEO                                  
         BE    BLDR45X                                                          
         CLI   PBUYKMED,C'D'        DIG. AUDIO                                  
         BE    BLDR45X                                                          
*                                                                               
*        FOR OTHER MEDIA USE PPBYOUT                                            
*                                                                               
         XC    GROSS(70),GROSS     USE FOR PVALUES                              
*                                                                               
         MVC   INSCOM1,SPACES                                                   
         MVC   INSCOM2,SPACES                                                   
*                                                                               
         GOTO1 GETINS,DMCB,PBUYREC,GROSS,PBUYKPRD                               
*                                                                               
         L     R3,=A(PPBYOWRK)                                                  
         USING PPBYOUTD,R3                                                      
         LA    RF,PBUYREC                                                       
         ST    RF,PBYOINPT                                                      
         MVC   PBYODTCN,DATCON                                                  
         LA    R0,GROSS                                                         
         ST    R0,PBYOVALS                                                      
         MVI   PBYOCTL,X'28'                                                    
         L     RE,ADAGY                                                         
         CLI   PAGYPROF+1-PAGYREC(RE),C'L'    LINE NUMBER CONTROL               
         BE    *+8                                                              
         OI    PBYOCTL,X'02'       SUPPRESS LINE NUMBER                         
*                                                                               
*                                                                               
         DS    0H                                                               
         GOTO1 PPBYOUT,DMCB,PPBYOUTD                                            
         MVC   WORK(20),PBYOSPC                                                 
*                                                                               
         CLI   QMEDIA,C'O'           SEE IF OUTDOOR                             
         BNE   BLDR45X                                                          
*                                                                               
         MVC   INSCOM1,PBYOCOMS                                                 
         MVC   INSCOM2,PBYOCOMS+47                                              
*                                                                               
         DROP  R3                                                               
*                                                                               
BLDR45X  DS    0H                                                               
         MVI   0(R2),C'"'                                                       
         MVC   1(20,R2),WORK      SPACE DSECRIPTION                             
         LA    R2,20(R2)                                                        
BLDR46   CLI   0(R2),C' '         SCAN BACKWARD FOR NON-SPACE                   
         BH    BLDR48                                                           
         BCT   R2,BLDR46                                                        
BLDR48   DS    0H                                                               
         MVC   1(2,R2),=C'",'                                                   
         LA    R2,3(R2)                                                         
*                                                                               
         MVI   0(R2),C'"'                                                       
         GOTO1 DATCON,DMCB,(3,PBUYKDAT),(X'20',WORK)                            
         MVC   1(2,R2),WORK+2    MM                                             
         MVC   3(2,R2),WORK+4    DD                                             
         MVC   5(2,R2),WORK+0    YY                                             
         MVC   7(2,R2),=C'",'                                                   
         LA    R2,9(R2)                                                         
*                                                                               
         MVC   FULL,PBGROSS                                                     
         L     RF,FULL                                                          
         MVC   MYFULL,PBAGYCOM                                                  
         S     RF,MYFULL                                                        
         MVC   MYFULL,PBCSHDSC                                                  
         S     RF,MYFULL         RF NOW NET-CD                                  
*                                                                               
         MVC   0(4,R2),=C'"1",'                                                 
         C     RF,=F'0'                                                         
         BNL   BLDR50                                                           
         MVC   0(5,R2),=C'"-1",'                                                
         LA    R2,5(R2)                                                         
         B     BLDR52                                                           
*                                                                               
BLDR50   LA    R2,4(R2)                                                         
BLDR52   DS    0H                                                               
         MVC   0(5,R2),=C'"EA",'                                                
         LA    R2,5(R2)                                                         
*                                                                               
         MVI   0(R2),C'"'                                                       
         LA    R2,1(R2)                                                         
         ST    RF,MYFULL                                                        
         EDIT  (B4,MYFULL),(10,0(R2)),2,ALIGN=LEFT                              
         AR    R2,R0        ADD LENGTH OF OUTPUT                                
         MVC   0(2,R2),=C'",'                                                   
         LA    R2,2(R2)                                                         
         MVC   0(2,R2),=C',,'     2 EMPTY FIELDS                                
         LA    R2,2(R2)                                                         
         BAS   RE,GETCOM      RETURNED IN WORK                                  
         MVI   0(R2),C'"'                                                       
         LA    R2,1(R2)                                                         
         MVC   0(29,R2),WORK                                                    
         LA    R2,29(R2)                                                        
BLDC2    CLI   0(R2),C' '                                                       
         BH    BLDC5                                                            
         BCT   R2,BLDC2                                                         
*                                                                               
BLDC5    MVC   1(2,R2),=C'",'                                                   
         LA    R2,3(R2)                                                         
         MVC   0(17,R2),=C',,,,,,,,,,,,,,,,,,' 17 EMPTY FIELDS                  
         LA    R2,17(R2)                                                        
*                                                                               
         LA    RF,1(RC)             SO I CAN ADDRESS PUB FIELDS                 
         LA    RF,4095(RF)                                                      
         USING PPFILED+4096,RF                                                  
*                                                                               
         MVI   0(R2),C'"'                                                       
         MVC   1(20,R2),PUBNAME                                                 
         LA    R2,20(R2)                                                        
BLDR66   CLI   0(R2),C' '         SCAN BACKWARD FOR NON-SPACE                   
         BH    BLDR67                                                           
         BCT   R2,BLDR66                                                        
*                                                                               
         DROP  RF                                                               
*                                                                               
BLDR67   DS    0H                                                               
         MVC   1(2,R2),=C'",'                                                   
         LA    R2,3(R2)                                                         
*                                                                               
         CLC   INSCOM1,SPACES                                                   
         BH    BLDR67A                                                          
         MVI   0(R2),C','         EMPTY FIELD                                   
         LA    R2,1(R2)                                                         
         B     BLDR67X                                                          
*                                                                               
BLDR67A  MVI   0(R2),C'"'           1ST INSERTION COMMENT                       
*                                 InstallationAddr1                             
         MVC   1(47,R2),INSCOM1                                                 
         LA    R2,47(R2)                                                        
BLDR67B  CLI   0(R2),C' '         SCAN BACKWARD FOR NON-SPACE                   
         BH    BLDR67C                                                          
         BCT   R2,BLDR67B                                                       
*                                                                               
BLDR67C  MVC   1(2,R2),=C'",'                                                   
         LA    R2,3(R2)                                                         
*                                                                               
BLDR67X  MVI   0(R2),C','         EMPTY FIELD                                   
         LA    R2,1(R2)                                                         
*                                                                               
BLDR68   CLC   INSCOM2,SPACES      ANYTHING THERE?                              
         BH    BLDR68A                                                          
         MVI   0(R2),C','         EMPTY FIELD                                   
         LA    R2,1(R2)                                                         
         B     BLDR68X                                                          
*                                                                               
BLDR68A  MVI   0(R2),C'"'                                                       
         MVC   1(47,R2),INSCOM2      2ND INSERTION COMMENT                      
*                                 InstallationCity                              
         LA    R2,47(R2)                                                        
BLDR68B  CLI   0(R2),C' '         SCAN BACKWARD FOR NON-SPACE                   
         BH    BLDR68C                                                          
         BCT   R2,BLDR68B                                                       
*                                                                               
BLDR68C  MVC   1(2,R2),=C'",'                                                   
         LA    R2,3(R2)                                                         
*                                                                               
BLDR68X  DS    0H                                                               
         B     BLDR75                                                           
*                                 EMPTY FOR MINDSHARE                           
*                                                                               
***      CLC   PBPRD,=C'CON'                                                    
***      BE    BLDR69                                                           
***      CLC   PBPRD,=C'SAM'                                                    
***      BE    BLDR69                                                           
***      CLC   PBPRD,=C'SYO'                                                    
***      BNE   BLDR70                                                           
***R69   MVC   0(12,R2),=C'"CORPORATE",'                                        
***      LA    R2,12(R2)                                                        
***      B     BLDR75                                                           
***                                                                             
***R70   DS    0H              IF PRD IS NOT CON,SAM,SYO                        
***      BAS   RE,GETMARK      SETS MARKET IN VARIOUS WAYS                      
***      MVI   0(R2),C'"'                                                       
***      MVC   1(30,R2),WORK                                                    
***      LA    R2,29(R2)                                                        
***R70C  CLI   0(R2),C' '     SCAN BACKWARD FOR NON-SPACE                       
***      BH    BLDR70E                                                          
***      BCT   R2,BLDR70C                                                       
***                                                                             
***R70E  MVC   1(2,R2),=C'",'                                                   
***      LA    R2,3(R2)                                                         
*                                                                               
BLDR75   DS    0H                                                               
         CLC   ESTU2+21(6),SPACES     SEE IF DATA PRESENT                       
         BNH   BLDR75E                                                          
         MVC   0(2,R2),=C',,'         2 MORE EMPTY FIELDS                       
         LA    R2,2(R2)                                                         
*                                                                               
         MVI   0(R2),C'"'                                                       
         MVC   1(16,R2),ESTU2+21      CustomerOrderNumber                       
         LA    R2,15(R2)              SCAN BACKWARDS FOR NON-SPACE              
BLDR75A  CLI   0(R2),C' '                                                       
         BH    BLDR75C                                                          
         BCT   R2,BLDR75A                                                       
BLDR75C  MVC   1(2,R2),=C'",'                                                   
         LA    R2,3(R2)                                                         
         MVC   0(4,R2),=C',,,,'       4 MORE EMPTY FIELDS                       
         LA    R2,4(R2)                                                         
         B     BLDR76                                                           
*                                                                               
BLDR75E  DS    0H                     HERE IF NO ESTU2 DATA                     
         MVC   0(7,R2),=C',,,,,,,'    7 EMPTY FIELDS                            
         LA    R2,7(R2)                                                         
*                                                                               
BLDR76   MVI   0(R2),C'"'                                                       
         MVC   1(3,R2),PBPRD                                                    
         MVC   4(2,R2),=C'",'                                                   
         LA    R2,6(R2)                                                         
*                                                                               
         MVC   HALF,PBUYKEST                                                    
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         MVI   0(R2),C'"'                                                       
         UNPK  1(3,R2),DUB+6(2)                                                 
         MVC   4(2,R2),=C'",'                                                   
         LA    R2,6(R2)                                                         
*                                                                               
         GOTO1 DATCON,DMCB,(3,PBDBDATE),(9,WORK)                                
         MVI   0(R2),C'"'                                                       
         MVC   1(6,R2),WORK     MMM/YY                                          
         MVC   7(2,R2),=C'",'                                                   
         LA    R2,9(R2)                                                         
         MVC   0(5,R2),=C',,,,,'    5 MORE EMPTY FIELDS                         
         LA    R2,5(R2)                                                         
         DROP  R6                                                               
*                                                                               
BLDREND  DS    0H                                                               
         LA    RE,OUTREC-4                                                      
         LR    RF,R2                                                            
         SR    RF,RE      DIFFERENCE SHOULD BE RECORD LENGTH                    
         STH   RF,OUTREC-4                                                      
         BAS   RE,MWRITE                                                        
         B     BLDRX                                                            
         EJECT                                                                  
*                                                                               
*        GETCITY EXTRACTS CITY FROM APRDLIN2                                    
*        THE FORMAT SHOULD BE CITY/ST/ZIP                                       
*                                                                               
GETCITY  DS    0H                                                               
         XC    PRDCITY,PRDCITY                                                  
         LA    R1,PRDCITY                                                       
         LA    R3,APRDLIN2                                                      
         LA    R4,L'APRDLIN2                                                    
GETC5    CLI   0(R3),C'/'     FIND FIRST /                                      
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
*        GETST EXTRACTS STATE CODE FROM APRDLIN2                                
*        THE FORMAT SHOULD BE CITY/ST/ZIP                                       
*                                                                               
GETST    DS    0H                                                               
         XC    PRDST,PRDST                                                      
         LA    R1,PRDST                                                         
         LA    R3,APRDLIN2                                                      
         LA    R4,L'APRDLIN2                                                    
GETS5    CLI   0(R3),C'/'     FIND FIRST /                                      
         BE    GETS10                                                           
         LA    R3,1(R3)                                                         
         BCT   R4,GETS5                                                         
         B     GETSX           MEANS NO / FOUND                                 
*                                                                               
GETS10   LA    R3,1(R3)        BUMP PAST IT                                     
GETS15   CLI   0(R3),C'/'     FIND NEXT /                                       
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
*        GETZIP EXTRACTS ZIP CODE FROM APRDLIN2                                 
*        THE FORMAT SHOULD BE CITY/ST/ZIP                                       
*                                                                               
GETZIP   DS    0H                                                               
         XC    PRDZIP,PRDZIP                                                    
         LA    R1,PRDZIP                                                        
         LA    R3,APRDLIN2                                                      
         LA    R4,L'APRDLIN2                                                    
GETZ5    CLI   0(R3),C'/'     FIND FIRST /                                      
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
GETZ15   CLI   0(R3),C'/'     FIND NEXT /                                       
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
         MVC   WORK(29),SPACES                                                  
         MVC   WORK(3),PBUYKCLT                                                 
         MVC   WORK+3(3),PBUYKPRD                                               
         MVC   HALF,PBUYKEST                                                    
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK+6(3),DUB+6(2)                                               
         MVC   WORK+9(20),PESTNAME                                              
         BR    RE             RETURN                                            
         EJECT                                                                  
*                                                                               
*        ROUTINE TO INCREMENT RECORD LENGTH                                     
*        AND SET R2 TO BEGINNING OF NEXT FIELD                                  
*                                                                               
ADDLEN   LH    RF,OUTREC-4                                                      
         AR    RF,R1                                                            
         AH    RF,=H'1'      FOR THE NEXT COMMA                                 
         STH   RF,OUTREC-4                                                      
         LA    R2,OUTREC-4                                                      
         AR    R2,RF         POINT R2 TO NEXT FIELD                             
         MVI   0(R2),C','    FIELD DELIMITER                                    
         LA    R2,1(R2)      PAST COMMA                                         
         BR    RE            RETURN                                             
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
PPSEWRKD DSECT                                                                  
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
MYAMTD   DS    F           CALCULATED AMT DUE FOR INSERTION                     
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
SVSTART  DS    CL6                                                              
SVEND    DS    CL6                                                              
SVQSTART DS    XL3                                                              
SVQEND   DS    XL3                                                              
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
APERVAL  DS    A                                                                
APPBVAL  DS    A                                                                
ACONIO1  DS    A                                                                
ANXTINV  DS    A                                                                
BUFFIO   DS    A                                                                
BUFFBUFF DS    A                                                                
AREGTAB  DS    A                                                                
ANXTREG  DS    A                                                                
AREGTABX DS    A                                                                
*                                                                               
MYUSER   DS    CL1       SET FROM AGYTAB AT FBUYREQ                             
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
OUTREC   DS    CL1100                                                           
*                                                                               
*                                                                               
         DS    CL50             SPARE                                           
*                                                                               
         BUFF  LINES=4000,ROWS=1,COLUMNS=3,FLAVOR=PACKED,COMMENT=100,KEX        
               YLIST=(22,A)                                                     
*                                                                               
COLHDS   CSECT                                                                  
         DC    C'"SprintID",'                                                   
         DC    C'"BillToName",'                                                 
         DC    C'"BillToAddr1",'                                                
         DC    C'"BillToAddr2",'                                                
         DC    C'"BillToCity",'                                                 
         DC    C'"BillToState",'                                                
         DC    C'"BillToPostalCode",'                                           
*                                                                               
         DC    C'"RemitToName",'                                                
         DC    C'"RemitToAddr1",'                                               
         DC    C'"RemitToAddr2",'                                               
         DC    C'"RemitToCity",'                                                
         DC    C'"RemitToState",'                                               
         DC    C'"RemitToPostalCode",'                                          
*                                                                               
         DC    C'"ShipToServiceLocation",'                                      
         DC    C'"ShippingAddr1",'                                              
         DC    C'"ShippingAddr2",'                                              
         DC    C'"ShippingCity",'                                               
         DC    C'"ShippingState",'                                              
         DC    C'"ShippingPostalCode",'                                         
*                                                                               
         DC    C'"SprintContactName",'                                          
         DC    C'"SprintContactPhone",'                                         
*                                                                               
         DC    C'"InvNumber",'                                                  
         DC    C'"InvDate",'                                                    
         DC    C'"PONumber",'                                                   
         DC    C'"POLineNumber",'                                               
         DC    C'"POScheduleNumber",'                                           
*                                                                               
         DC    C'"ItemID",'                                                     
         DC    C'"PartDescription",'                                            
         DC    C'"LineItemQty",'                                                
         DC    C'"UOM",'                                                        
         DC    C'"UnitPrice",'                                                  
         DC    C'"TotalTaxesonInv",'                                            
         DC    C'"Freight",'                                                    
         DC    C'"Comments",'                                                   
*                                                                               
         DC    C'"GeneralLedgerBU",'                                            
         DC    C'"CostCenter",'                                                 
         DC    C'"GLAccount",'                                                  
         DC    C'"FunctionCode",'                                               
         DC    C'"RegularityID",'                                               
         DC    C'"Workorder",'                                                  
         DC    C'"ProjectID",'                                                  
         DC    C'"Location",'                                                   
         DC    C'"GeoCode",'                                                    
         DC    C'"ProductCode",'                                                
         DC    C'"MarketCode",'                                                 
         DC    C'"CommodityCode",'                                              
*                                                                               
         DC    C'"TradeDiscount",'                                              
         DC    C'"DiscountPercent",'                                            
         DC    C'"NetDays",'                                                    
         DC    C'"DiscountDueDays",'                                            
*                                                                               
         DC    C'"ShippingContact",'                                            
         DC    C'"InstallLocation",'                                            
         DC    C'"InstallationAddr1",'                                          
         DC    C'"InstallationAddr2",'                                          
         DC    C'"InstallationCity",'                                           
         DC    C'"InstallationState",'                                          
*                                                                               
         DC    C'"OrigInvNumber",'                                              
         DC    C'"CustomerOrderNumber",'                                        
         DC    C'"SupplierContact",'                                            
         DC    C'"SupplierEmail",'                                              
         DC    C'"SupplierPhone",'                                              
         DC    C'"SupplierAcctNumber",'                                         
*                                                                               
         DC    C'"CustomerUse1",'                                               
         DC    C'"CustomerUse2",'                                               
         DC    C'"CustomerUse3",'                                               
*                                                                               
         DC    C'"ProNumber",'                                                  
         DC    C'"AirwayBill",'                                                 
         DC    C'"ShipVia",'                                                    
         DC    C'"ServiceStartDate",'                                           
         DC    C'"ServiceEndDate",'                                             
COLHDX   EQU   *                                                                
COLHDLEN EQU   *-COLHDS                                                         
         PRINT OFF                                                              
       ++INCLUDE PPMODEQU                                                       
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPREPWORK2                                                     
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE DDBUFFALOD                                                     
*                                                                               
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDUCOMD                                                        
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'064PPREPSE02 07/18/18'                                      
         END                                                                    
