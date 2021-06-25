*          DATA SET SPREPP3AR  AT LEVEL 052 AS OF 05/01/02                      
*PHASE SPP302A                                                                  
*INCLUDE EDITOR                                                                 
         TITLE 'SPREPP302 - DAYPART SUMMARY CUMES'                              
         PRINT NOGEN                                                            
*                                                                               
*                                                                               
*        QOPT1      RATING SOURCE  A=ARB,N=ACN                                  
*        QOPT2      DPG PHASE - (A - D = DOWNLOAD)                              
*                                                                               
*                                                                               
SPP302   CSECT                                                                  
         NMOD1 0,SPP302,RR=R5                                                   
*                                                                               
         L     R8,0(R1)                                                         
         LA    R9,2048(R8)                                                      
         LA    R9,2048(R9)                                                      
         USING SPWORKD,R8,R9                                                    
         STM   R8,RB,SVRGS                                                      
         ST    R5,RELO                                                          
*                                                                               
         L     RC,=A(GLAREA)                                                    
         USING GLOBALD,RC                                                       
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    SP050                                                            
         CLI   MODE,REQFRST                                                     
         BE    SP100                                                            
         B     EXIT                                                             
*                                                                               
EXIT     XIT1                                                                   
         SPACE 3                                                                
* RUN FIRST                                                                     
*                                                                               
SP050    DS    0H                                                               
         XC    ADRIVER,ADRIVER                                                  
         XC    ASYSDRV,ASYSDRV                                                  
*                                                                               
         B     EXIT                                                             
         SPACE 3                                                                
* REQUEST FIRST                                                                 
*                                                                               
SP100    DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVI   DLHSW,0                                                          
*                                                                               
         LA    RE,GLOBALD          INIT DRIVER STORAGE TO ZEROS                 
         L     RF,=F'40000'                                                     
         XCEF                                                                   
         MVC   GLSIZE,=F'40000'    GLOBAL SIZE                                  
         MVC   GLOBALD(8),=C'*GLOBAL*'                                          
*                                                                               
         MVI   GLTRACE,C'N'                                                     
         NI    GLDOWNLD,X'3F'                                                   
         MVC   DPGFILE+6(1),QOPT2                                               
         CLI   QOPT2,C' '          TEST DOWNLOADING                             
         BE    SP102                                                            
         CLI   QOPT2,C'D'                                                       
         BH    SP102               A-D = DOWNLOADING                            
         OI    GLDOWNLD,X'C0'                                                   
*                                                                               
SP102    DS    0H                                                               
         GOTO1 LOADER,DMCB,DPGFILE,0    LOAD DPG PROGRAM                        
         ICM   R6,15,DMCB+4                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    R6,GLAPROG                                                       
*                                                                               
         OC    ADRIVER,ADRIVER                                                  
         BNZ   SP110                                                            
         GOTO1 LOADER,DMCB,DRIVER       LOAD DRIVER                             
         ICM   R6,15,DMCB+4                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    R6,ADRIVER                                                       
*                                                                               
SP110    OC    ASYSDRV,ASYSDRV                                                  
         BNZ   SP120                                                            
         GOTO1 LOADER,DMCB,SPDRIVER     LOAD SYSTEM DRIVER                      
         ICM   R6,15,DMCB+4                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    R6,ASYSDRV                                                       
*                                                                               
SP120    MVC   GLASYSDR,ASYSDRV    SYSTEM DRIVER ADDR                           
*                                                                               
         LA    RE,SPWORKD                                                       
         ST    RE,GLAWORKD         SPOT WORK ADDR                               
         LA    RE,DRHOOK                                                        
         ST    RE,GLAHOOK          OUR DRIVER HOOK                              
         MVI   GLTWORKD,GLTSPOT    SPOT WORK                                    
         MVI   GLDETHED,C'Y'       I GET HEADS AT DETAIL TIME                   
         LA    RE,PROGPROF                                                      
         ST    RE,GLAPPROF                                                      
         L     RE,GLAPLIST                                                      
         MVC   ASPDRWKC,0(RE)      A(SPOT DRIVER WORK AREA)                     
*                                                                               
         MVC   SRCE,QOPT1                                                       
         L     RE,ADCLT                                                         
         USING CLTHDR,RE                                                        
         MVI   CPROF+3,C'0'                                                     
         CLI   SRCE,C'A'          A=ARB, N=NSI                                  
         BNE   *+8                                                              
         MVI   CPROF+3,C'1'                                                     
         B     SPRZ                                                             
         SPACE 2                                                                
*                                                                               
SPRZ     DS    0H                                                               
         OPEN  (IN1,(INPUT))                                                    
         SPACE 2                                                                
         XC    MYRECS,MYRECS                                                    
         MVI   GLMODE,GLINIT       INITIALIZE DRIVER                            
         GOTO1 ADRIVER,DMCB,(RC)                                                
*                                                                               
*                                  DRIVER INPUT PHASE                           
*                                  ------------------                           
         MVI   GLMODE,GLINPUT      SET UP FOR DRIVER INPUT                      
*                                                                               
SP140    DS    0H                                                               
         LA    R6,INREC                                                         
         USING DMCDSECT,R6                                                      
         GET   IN1,(R6)                                                         
         CLC   DMCID,=C'DMC'                                                    
         BNE   SP140P                                                           
         MVC   CALL,=C'    '                                                    
         MVC   DPTCD,=C'UUU'                                                    
         MVC   MKTTYP,DMCMTYP                                                   
         MVC   MKTCL,DMCMCLAS                                                   
         MVC   TIMEZONE,DMCTZ+1                                                 
         MVC   MKTNAM,DMCMNAME                                                  
         MVC   MKTNUMC,DMCMKT                                                   
         XC    DOUTS,DOUTS                                                      
         PACK  DUB,DMCHH                                                        
         CVB   R1,DUB                                                           
         SR    R0,R0                                                            
         AH    R1,=H'500'                                                       
         D     R0,=F'1000'                                                      
         STH   R1,DOUTS                                                         
         PACK  DUB,DMCV18O                                                      
         CVB   R1,DUB                                                           
         SR    R0,R0                                                            
         AH    R1,=H'500'                                                       
         D     R0,=F'1000'                                                      
         STH   R1,DOUTS+2                                                       
         PACK  DUB,DMCM18O                                                      
         CVB   R1,DUB                                                           
         SR    R0,R0                                                            
         AH    R1,=H'500'                                                       
         D     R0,=F'1000'                                                      
         STH   R1,DOUTS+4                                                       
         PACK  DUB,DMCM1834                                                     
         CVB   R1,DUB                                                           
         SR    R0,R0                                                            
         AH    R1,=H'500'                                                       
         D     R0,=F'1000'                                                      
         STH   R1,DOUTS+6                                                       
         PACK  DUB,DMCW18O                                                      
         CVB   R1,DUB                                                           
         SR    R0,R0                                                            
         AH    R1,=H'500'                                                       
         D     R0,=F'1000'                                                      
         STH   R1,DOUTS+8                                                       
         PACK  DUB,DMCW1834                                                     
         CVB   R1,DUB                                                           
         SR    R0,R0                                                            
         AH    R1,=H'500'                                                       
         D     R0,=F'1000'                                                      
         STH   R1,DOUTS+10                                                      
         PACK  DUB,DMCW1849                                                     
         CVB   R1,DUB                                                           
         SR    R0,R0                                                            
         AH    R1,=H'500'                                                       
         D     R0,=F'1000'                                                      
         STH   R1,DOUTS+12                                                      
         PACK  DUB,DMCW2554                                                     
         CVB   R1,DUB                                                           
         SR    R0,R0                                                            
         AH    R1,=H'500'                                                       
         D     R0,=F'1000'                                                      
         STH   R1,DOUTS+14                                                      
         GOTO1 ADRIVER,DMCB,(RC)                                                
         B     SP140                                                            
SP140P   CLC   DMCID,=C'DPM'                                                    
         BNE   SP140S                                                           
         MVC   CALL,=C'ALL '                                                    
         XC    DOUTS,DOUTS                                                      
         USING DPMDSECT,R6                                                      
         MVC   DPTCD,DPMDPTCD                                                   
         PACK  DUB,DPMHH                                                        
         CVB   R1,DUB                                                           
         STH   R1,DOUTS                                                         
         PACK  DUB,DPMV18O                                                      
         CVB   R1,DUB                                                           
         STH   R1,DOUTS+2                                                       
         PACK  DUB,DPMM18O                                                      
         CVB   R1,DUB                                                           
         STH   R1,DOUTS+4                                                       
*        PACK  DUB,DPMM1834                                                     
*        CVB   R1,DUB                                                           
*        STH   R1,DOUTS+6                                                       
         PACK  DUB,DPMW18O                                                      
         CVB   R1,DUB                                                           
         STH   R1,DOUTS+8                                                       
*        PACK  DUB,DPMW1834                                                     
*        CVB   R1,DUB                                                           
*        STH   R1,DOUTS+10                                                      
         PACK  DUB,DPMW1849                                                     
         CVB   R1,DUB                                                           
         STH   R1,DOUTS+12                                                      
         PACK  DUB,DPMW2554                                                     
         CVB   R1,DUB                                                           
         STH   R1,DOUTS+14                                                      
         GOTO1 ADRIVER,DMCB,(RC)                                                
         B     SP140                                                            
*                                                                               
         USING DMCDSECT,R6                                                      
SP140S   CLC   DMCID,=C'DPS'                                                    
         BNE   SP140                                                            
         USING DPSDSECT,R6                                                      
         CLI   DPSTYPE,C'0'                                                     
         BNE   SP140                                                            
         XC    DOUTS,DOUTS                                                      
         MVC   CALL,DPSCALL                                                     
         MVC   DPTCD,DPSDPTCD                                                   
         PACK  DUB,DPSHH                                                        
         CVB   R1,DUB                                                           
         STH   R1,DOUTS                                                         
         PACK  DUB,DPSV18O                                                      
         CVB   R1,DUB                                                           
         STH   R1,DOUTS+2                                                       
         PACK  DUB,DPSM18O                                                      
         CVB   R1,DUB                                                           
         STH   R1,DOUTS+4                                                       
*        PACK  DUB,DPSM1834                                                     
*        CVB   R1,DUB                                                           
*        STH   R1,DOUTS+6                                                       
         PACK  DUB,DPSW18O                                                      
         CVB   R1,DUB                                                           
         STH   R1,DOUTS+8                                                       
*        PACK  DUB,DPSW1834                                                     
*        CVB   R1,DUB                                                           
*        STH   R1,DOUTS+10                                                      
         PACK  DUB,DPSW1849                                                     
         CVB   R1,DUB                                                           
         STH   R1,DOUTS+12                                                      
         PACK  DUB,DPSW2554                                                     
         CVB   R1,DUB                                                           
         STH   R1,DOUTS+14                                                      
         GOTO1 ADRIVER,DMCB,(RC)                                                
         B     SP140                                                            
*                                                                               
         USING DPPSECT,R6                                                       
         CLC   DMCCDE,=C'01'       MKT INFO RECORD                              
         BE    SP160                                                            
         CLC   DPRCDE,=C'02'       MKT INFO RECORD                              
         BE    SP170                                                            
         CLC   DPRCDE,=C'21'       DAYPART DEMO RECORD                          
         BNE   SP140                                                            
         CLI   DPSCALL,C' '        ONLY DO MARKET LEVELS                        
         BNE   SP140                                                            
*        CLI   MKTTYP,C'5'         NON DMA MARKETS                              
*        BE    SP140                                                            
*                                                                               
         LA    RE,HMDPTTAB         CONVERT DAYPARTS TO HARRIS                   
HMDPT    CLI   0(RE),X'FF'         BYPASS IF NOT FOUND                          
         BE    SP140                                                            
         CLC   DPDPTCD,0(RE)                                                    
         BE    *+12                                                             
         LA    RE,4(RE)                                                         
         B     HMDPT                                                            
         MVC   DPDPTCD,2(RE)                                                    
*                                                                               
         MVC   DOUTS(2),=PL2'0'    CLEAR DEMO AREA                              
         MVC   DOUTS+2(2*20-2),DOUTS                                            
         MVC   DOUTS(2*15),DPCUME1                                              
         CLI   MKTTYP,C'1'        NON-METERED MARKETS                           
         BH    SP140A                                                           
         MVC   DOUTS+2*18(4),DPCAVG                                             
         B     *+10                                                             
SP140A   MVC   DOUTS+2*15(6),DPCMETA                                            
*                                                                               
         LA    R1,DOUTS            ADJUST FIELDS TO SAME PRECISION              
         LA    R0,15                                                            
SP140B   ZAP   DUB,0(2,R1)                                                      
         CVB   RE,DUB                                                           
         MH    RE,=H'10'                                                        
         CH    RE,=H'1000'                                                      
         BL    *+8                                                              
         LA    RE,990                                                           
         STH   RE,0(R1)                                                         
         LA    R1,2(R1)                                                         
         BCT   R0,SP140B                                                        
*                                                                               
         L     RE,MYRECS                                                        
         LA    RE,1(RE)                                                         
         ST    RE,MYRECS                                                        
         B     *+12                LIMIT TO N RECORDS                           
         C     RE,=F'1000'          *                                           
         BH    SP140                *                                           
         GOTO1 ADRIVER,DMCB,(RC)                                                
         B     SP140                                                            
*                                                                               
SP140D   DS    0H                                                               
         SPACE 2                                                                
*                                  DRIVER OUTPUT PHASE                          
*                                  -------------------                          
         MVI   GLMODE,GLOUTPUT                                                  
         GOTO1 ADRIVER,DMCB,(RC)                                                
*                                                                               
SP150    DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
         USING MIDSECT,R6                                                       
SP160    MVC   MKTTYP,M1MTYP                                                    
         MVC   MKTCL,M1MCL                                                      
         MVC   TIMEZONE,M1TZ                                                    
         B     SP140                                                            
         SPACE 2                                                                
         USING M2DSECT,R6                                                       
SP170    MVC   MKTNAM,M2MKTNAM                                                  
         B     SP140                                                            
         EJECT                                                                  
         USING DPDSECT,R6                                                       
DRHOOK   NTR1                                                                   
*                                                                               
*        DRIVER HOOK ROUTINE                                                    
*                                                                               
         CLI   GLHOOK,GLROUT       TEST TO EXECUTE USER ROUTINE                 
         BNE   DH005                                                            
         CLI   GLMODE,GLINPUT      YES - INPUT PHASE                            
         BE    DH100                                                            
         CLI   GLMODE,GLOUTPUT           OUTPUT PHASE                           
         BE    DH200                                                            
         DC    H'0'                                                             
*                                                                               
DH005    CLI   GLHOOK,GLRESOLV     TEST TO RESOLVE LABELS                       
         BE    DH010                                                            
         CLI   GLHOOK,GLRESLIT     TEST TO RESOLVE LITERAL                      
         BE    DH300                                                            
         CLI   GLHOOK,GLHEAD       HEADLINES                                    
         BE    DH400                                                            
         CLI   GLHOOK,GLPUTSRT     RECORD INPUT TEST                            
         BE    DH450                                                            
         B     EXIT                                                             
         SPACE 2                                                                
*                                                                               
* RESOLVE ROUTINE LABELS                                                        
*                                                                               
DH010    LA    R1,ROUTLIST                                                      
         LA    R2,GLLABEL                                                       
*                                                                               
DH020    CLI   0(R1),X'FF'                                                      
         BE    EXIT                                                             
         CLC   0(8,R1),0(R2)                                                    
         BE    DH030                                                            
         LA    R1,12(R1)                                                        
         B     DH020                                                            
*                                                                               
DH030    MVC   GLAROUT,8(R1)                                                    
         B     EXIT                                                             
         SPACE 2                                                                
ROUTLIST DS    0F                                                               
         DC    C'MKTNUM  ',A(MKTNUM)                                            
         DC    C'OPMKTNAM',A(OPMKTNAM)                                          
         DC    C'OPCALL  ',A(OPCALL)                                            
         DC    C'OPDPT   ',A(OPDPT)                                             
         DC    C'DEMIP   ',A(DEMIP)                                             
         DC    X'FF'                                                            
         SPACE 3                                                                
*                                                                               
* EXECUTE INPUT PHASE ROUTINES                                                  
*                                                                               
DH100    L     RF,GLAROUT                                                       
         L     R3,GLAIFLD                                                       
         BASR  RE,RF                                                            
         DC    H'0'                                                             
         SPACE 3                                                                
*                                                                               
* EXECUTE OUTPUT PHASE ROUTINES                                                 
*                                                                               
DH200    L     RF,GLAROUT                                                       
         L     R2,GLAIFLD                                                       
         L     R3,GLAOFLD                                                       
         BASR  RE,RF                                                            
         DC    H'0'                                                             
         SPACE 3                                                                
*                                                                               
* INPUT ROUTINES                                                                
*                                                                               
MKTNUM   DS    0H                                                               
*        MVC   0(2,R3),DPMNO                                                    
         PACK  0(2,R3),MKTNUMC                                                  
         B     EXIT                                                             
*                                                                               
OPMKTNAM DS    0H                                                               
         MVC   0(26,R3),MKTNAM                                                  
         B     EXIT                                                             
*                                                                               
OPCALL   DS    0H                                                               
         MVC   0(4,R3),CALL                                                     
         B     EXIT                                                             
*                                                                               
OPDPT    DS    0H                                                               
         MVC   0(3,R3),DPTCD                                                    
         B     EXIT                                                             
*                                                                               
DEMIP    DS    0H                                                               
         ZIC   RF,GLARGS           ARG IS DEMO NUM                              
         SLL   RF,1                X 2                                          
         LA    R1,DOUTS-2(RF)                                                   
         MVC   0(2,R3),0(R1)       MOVE IN DEMO VALUE                           
         B     EXIT                                                             
         SPACE 2                                                                
* OUTPUT ROUTINES                                                               
*                                                                               
DEMNAM   DS    0H                                                               
         ZIC   RF,GLARGS           DEMO NMUMBER                                 
         MH    RF,=H'5'                                                         
         LA    RF,DNAMES-5(RF)                                                  
         MVC   0(5,R3),0(RF)                                                    
         B     EXIT                                                             
         SPACE 2                                                                
DLHTTL   DS    0H                  DOWNLOAD HEADER                              
         MVC   0(8,R3),=C'MKTMAST '                                             
         B     EXIT                                                             
*                                                                               
DLHSRC   DS    0H                                                               
         MVC   0(4,R3),=C'ARB '    SOURCE                                       
         CLI   SRCE,C'A'                                                        
         BE    *+10                                                             
         MVC   0(4,R3),=C'ACN '                                                 
         B     EXIT                                                             
*                                                                               
DLHYR    DS    0H                                                               
         PACK  DUB,QBOOK1(2)       YEAR                                         
         CVB   R1,DUB                                                           
         CLC   QBOOK1+2(2),=C'11'   IF NOVEMBER                                 
         BNE   *+8                                                              
         LA    R1,1(R1)            BUMP YEAR                                    
         STC   R1,0(R3)                                                         
         B     EXIT                                                             
*                                                                               
DLHNMKT  DS    0H                  NUMBER OF MARKETS                            
         L     RF,MKTCNT                                                        
         LA    RF,1(RF)            + ONE FOR US                                 
         STCM  RF,3,0(R3)                                                       
         B     EXIT                                                             
*                                                                               
DLHNDEM  DS    0H                  NUMBER OF DEMOS                              
         MVI   0(R3),NDEMS                                                      
         B     EXIT                                                             
*                                                                               
MKTABBO  DS    0H                                                               
         MVC   0(30,R3),SPACES                                                  
         MVC   2(8,R3),0(R2)                                                    
         B     EXIT                                                             
         SPACE 2                                                                
*                                                                               
TOTROUT  DS    0H                                                               
         DC    H'0'                                                             
         B     EXIT                                                             
         SPACE 2                                                                
* RESOLVE LITERALS                                                              
*                                                                               
DH300    DS    0H                                                               
         DC    H'0'                                                             
         SPACE 2                                                                
* HEADLINE ROUTINE                                                              
*                                                                               
DH400    DS    0H                                                               
         MVC   HEAD3(11),=C'*** ARB ***'                                        
         MVC   HEAD4(11),=C'-----------'                                        
         CLI   SRCE,C'A'                                                        
         BE    *+10                                                             
         MVC   HEAD3+4(3),=C'ACN'                                               
*                                                                               
         MVC   WORK(4),QBOOK1                                                   
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,WORK,(6,DUB)                                         
         MVC   HEAD6(5),=C'BOOK='                                               
         MVC   HEAD6+6(6),DUB                                                   
*                                                                               
         B     EXIT                                                             
         SPACE 2                                                                
* DETAIL LINE ROUTINE                                                           
*                                                                               
DH450    DS    0H                                                               
         TM    GLDOWNLD,X'80'      IF DOWNLOADING                               
         BZ    EXIT                                                             
         CLI   GLRECNO,1           FOR RECORD 1                                 
         B     EXIT                                                             
         CLI   DLHSW,0             TEST HAVE DONE HEADS                         
         BNE   EXIT                                                             
         MVI   DLHSW,1                                                          
         MVI   GLHOOK,GLDONT                                                    
         B     EXIT                                                             
*                                                                               
         SPACE 2                                                                
DIV      DIV   (R0),(RF)       RETURNS VIA RE                                   
         SPACE 2                                                                
*                                                                               
BMEOF    DS    0H                                                               
*                                                                               
         CLOSE (IN1,REWIND)                                                     
         B     SP140D                                                           
         SPACE 2                                                                
IN1      DCB   DDNAME=IN1,                                             X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=00900,                                            X        
               BLKSIZE=03600,                                          X        
               MACRF=GM,                                               X        
               EODAD=BMEOF                                                      
         EJECT                                                                  
INREC    DS    900C                                                             
         LTORG                                                                  
         EJECT                                                                  
         DS    0D                                                               
ADRIVER  DS    A                                                                
ASYSDRV  DS    A                                                                
ASPDRWKC DS    A                                                                
MKTPARS  DS    6F                                                               
MYRECS   DC    F'0'                                                             
AMKTTAB  EQU   MKTPARS+4                                                        
MKTCNT   EQU   MKTPARS+8                                                        
*                                                                               
DPGFILE  DC    CL8'SPP305  '                                                    
DRIVER   DC    CL8'T00A3A  '                                                    
SPDRIVER DC    CL8'SPDRIVER'                                                    
*                                                                               
HMDPTTAB DC    C'10',C'EM'         HARRIS MEDIA DAYPARTS                        
         DC    C'12',C'DA'                                                      
         DC    C'14',C'EF'                                                      
         DC    C'15',C'EN'                                                      
         DC    C'18',C'PA'                                                      
         DC    C'21',C'LN'                                                      
         DC    C'22',C'LF'                                                      
         DC    C'35',C'PR'                                                      
         DC    C'38',C'CU'                                                      
         DC    C'40',C'CH'                                                      
         DC    C'41',C'SP'                                                      
         DC    X'FF'                                                            
*                                                                               
SRCE     DS    C                                                                
XFF      DC    20X'FF'                                                          
X        DS    XL256                                                            
*                                                                               
SVKEY    DS    CL32                                                             
ELCODE   DS    XL1                                                              
DLHSW    DS    X                                                                
QTRNO    DS    CL1                                                              
MKTTYP   DS    CL1                                                              
MKTCL    DS    CL1                                                              
MKTNAM   DS    CL26                                                             
TIMEZONE DS    CL1                                                              
MKTNUMC  DS    CL3                                                              
DPTCD    DS    CL3                                                              
CALL     DS    CL4                                                              
*                                                                               
         SPACE 2                                                                
         DS    0F                                                               
SVRGS    DS    0XL16                                                            
SVR8     DS    F                                                                
SVR9     DS    F                                                                
SVRA     DS    F                                                                
SVRB     DS    F                                                                
         SPACE 2                                                                
*        DEMO LIST                                                              
*                                                                               
DEMLST   DS    0X                                                               
         DC    X'00',C'L',AL1(001)   HOMES                                      
         DC    X'00',C'L',AL1(065)   WW                                         
         DC    X'00',C'L',AL1(045)   W18+                                       
         DC    X'00',C'L',AL1(041)   W1834                                      
         DC    X'00',C'L',AL1(042)   W1849                                      
         DC    X'00',C'L',AL1(047)   W2549                                      
         DC    X'00',C'L',AL1(048)   W2554                                      
         DC    X'00',C'L',AL1(049)   W2564                                      
         DC    X'00',C'L',AL1(028)   W1224                                      
         DC    X'00',C'L',AL1(029)   W1234                                      
         DC    X'00',C'L',AL1(095)   M18+                                       
         DC    X'00',C'L',AL1(091)   M1834                                      
         DC    X'00',C'L',AL1(092)   M1849                                      
         DC    X'00',C'L',AL1(097)   M2549                                      
         DC    X'00',C'L',AL1(098)   M2554                                      
         DC    X'00',C'L',AL1(099)   M2564                                      
         DC    X'00',C'L',AL1(127)   V02+                                       
         DC    X'00',C'L',AL1(122)   V0211                                      
         DC    X'00',C'L',AL1(123)   V0611                                      
         DC    X'00',C'L',AL1(125)   V1217                                      
         DC    X'00',C'L',AL1(128)   V1224                                      
         DC    X'00',C'L',AL1(129)   V1234                                      
NDEMS    EQU   20                                                               
         DC    X'FFFFFF'                                                        
*                                                                               
         SPACE 2                                                                
DNAMES   DS    0X                                                               
         DC    CL5'HOMES'                                                       
         DC    CL5'WWORK'                                                       
         DC    CL5'W18+ '                                                       
         DC    CL5'W1834'                                                       
         DC    CL5'W1849'                                                       
         DC    CL5'W2549'                                                       
         DC    CL5'W2554'                                                       
         DC    CL5'W2564'                                                       
         DC    CL5'W1224'                                                       
         DC    CL5'W1234'                                                       
         DC    CL5'M18+ '                                                       
         DC    CL5'M1834'                                                       
         DC    CL5'M1849'                                                       
         DC    CL5'M2549'                                                       
         DC    CL5'M2554'                                                       
         DC    CL5'M2564'                                                       
         DC    CL5'V02+ '                                                       
         DC    CL5'V0211'                                                       
         DC    CL5'V0611'                                                       
         DC    CL5'V1217'                                                       
         DC    CL5'V1224'                                                       
         DC    CL5'V1234'                                                       
*                                                                               
         EJECT                                                                  
RELO     DC    F'0'                                                             
         DS    0D                                                               
DOUTS    DS    CL(NDEMS*2)                                                      
         SPACE 2                                                                
DMCDSECT DSECT                                                                  
DMCID    DS    CL3                                                              
         DS    CL2                                                              
         DS    CL2                                                              
DMCMKT   DS    CL3                                                              
         DS    CL3                                                              
         DS    CL1                                                              
DMCTZ    DS    CL2                                                              
         DS    CL3                                                              
DMCMNAME DS    CL36                                                             
DMCMTYP  DS    C                                                                
DMCMCLAS DS    C                                                                
         DS    CL234                                                            
DMCHH    DS    CL8                                                              
         DS    CL8    V2+                                                       
         DS    CL8     12-24                                                    
         DS    CL8     12-34                                                    
         DS    CL8     18-34                                                    
         DS    CL8     18-49                                                    
DMCV18O  DS    CL8     18+                                                      
         DS    CL8     35+                                                      
         DS    CL8     50+                                                      
         DS    CL8     21-49                                                    
         DS    CL8     25-54                                                    
         DS    CL8     35-64                                                    
         DS    CL8    W12-24                                                    
DMCW1834 DS    CL8     18-34                                                    
DMCW1849 DS    CL8     18-49                                                    
DMCW18O  DS    CL8     18+                                                      
         DS    CL8     25-49                                                    
DMCW2554 DS    CL8     25-54                                                    
         DS    CL8     25-64                                                    
         DS    CL8     35+                                                      
         DS    CL8     WW18+                                                    
         DS    CL8    W21-49                                                    
         DS    CL8                                                              
DMCM1834 DS    CL8    M18-34                                                    
         DS    CL8     18-49                                                    
DMCM18O  DS    CL8     18+                                                      
         DS    CL8     25-49                                                    
         DS    CL8     25-54                                                    
         DS    CL8     25-64                                                    
         DS    CL8     35+                                                      
         DS    CL8     21-49                                                    
         DS    CL8                                                              
         DS    CL8    V12-17                                                    
         DS    CL8     2-11                                                     
         DS    CL8     6-11                                                     
         DS    CL8                                                              
*                                                                               
DPMDSECT DSECT                                                                  
         DS    CL17                                                             
DPMDPTCD DS    CL3                                                              
         DS    CL24                                                             
         DS    CL3                                                              
         DS    CL3                                                              
         DS    CL525                                                            
DPMHH    DS    CL5                                                              
         DS    CL10                                                             
DPMV18O  DS    CL5     18+                                                      
         DS    CL5                                                              
         DS    CL5                                                              
DPMW1849 DS    CL5     18-49                                                    
DPMW18O  DS    CL5     18+                                                      
         DS    CL5                                                              
DPMW2554 DS    CL5     25-54                                                    
         DS    CL5                                                              
         DS    CL5                                                              
         DS    CL5                                                              
         DS    CL5     18-49                                                    
DPMM18O  DS    CL5     18+                                                      
*                                                                               
*                                                                               
DPSDSECT DSECT                                                                  
         DS    CL20                                                             
DPSDPTCD DS    CL3                                                              
DPSCALL  DS    CL5                                                              
         DS    C                                                                
         DS    C                                                                
DPSTYPE  DS    C                                                                
         ORG   DPSDSECT                                                         
         DS    CL429                                                            
DPSHH    DS    CL5                                                              
         DS    CL10                                                             
DPSV18O  DS    CL5     18+                                                      
         DS    CL5                                                              
         DS    CL5                                                              
DPSW1849 DS    CL5     18-49                                                    
DPSW18O  DS    CL5     18+                                                      
         DS    CL5                                                              
DPSW2554 DS    CL5     25-54                                                    
         DS    CL5                                                              
         DS    CL5                                                              
         DS    CL5                                                              
         DS    CL5     18-49                                                    
DPSM18O  DS    CL5     18+                                                      
*                                                                               
* MARKET INFORMATION RECORD 1                                                   
MIDSECT  DSECT                                                                  
M1MNO    DS    CL2       P         MARKET NUMBER                                
         DS    CL6                 SPARE                                        
MIRCODE  DS    CL2       C         RECORD CODE - 01                             
MIWKS    DS    CL1       P         NUMBER OF WEEKS MEASURED                     
M1YR     DS    CL2       C         REPORT YEAR                                  
M1MO     DS    CL2       C         REPORT MONTH                                 
M1TZ     DS    CL1       C         TIME ZONE                                    
M1MTYP   DS    CL1       C         MARKET TYPE                                  
M1MCL    DS    CL1       C         MARKET CLASS                                 
M1TSCL   DS    CL1       C         TARGET SUB-CLASS                             
M1ROUND  DS    CL1       C         ROUNDING CONTROL                             
M1DMA    DS    CL1       C         DMA INDICATOR                                
         DS    CL1       C         SPARE                                        
M1TRPT   DS    CL1       C         D=DAYPART                                    
         DS    CL12      C                                                      
M1INTAB  DS    CL48      P         NET-IN-TAB COUNTS (4)                        
M1QHTRND DS    CL16      C         QUARTER HOUR TRENDS (4)                      
M1ADJDMA DS    CL6       P         ADJACENT DMAS (3)                            
         DS    CL19      C         SPARE                                        
M1STAT   DS    CL250               REPORTABLE STATIONS (20)                     
*                                                                               
INTABD   DSECT                     DSECT FOR NET-IN-TAB-COUNTS                  
ITMETA   DS    CL3       P         METRO A                                      
ITMETB   DS    CL3       P         METRO B                                      
ITDMA    DS    CL3       P         DMA                                          
ITNTA    DS    CL3       P         NSI AREA                                     
*                                                                               
DPTD     DSECT                     DSECT FOR QUARTER-HOUR TRENDS                
DPTMO    DS    CL2       C         REPORT PERIOD                                
DPTYR    DS    CL2       C         REPORT YEAR                                  
*                                                                               
M1STATD  DSECT                     DSECT FOR REPORTABLE STATIONS                
M1STATST DS    0C                                                               
M1SCDE   DS    CL3       P         STATION CODE                                 
M1STATUS DS    CL1       C         REPORTABILITY STATUS                         
M1AFFL   DS    CL5       C         NETWORK AFFILIATION                          
M1STYP   DS    CL1       C         STATION TYPE                                 
M1STATEN DS    0C                                                               
M1STATLN EQU   M1STATEN-M1STATST                                                
M2DSECT  DSECT                                                                  
M2MNO    DS    CL2       P         MARKET NUMBER                                
         DS    CL6                 SPARE                                        
M2RCODE  DS    CL2       C         RECORD CODE = 02                             
M2MKTNAM DS    CL26                MARKET NAME                                  
         DS    CL16                SPARE                                        
*                                                                               
* TV HOUSEHOLDS UNIVERSE ESTIMATES (0)                                          
M2MA     DS    CL4       P         METRO A                                      
M2MB     DS    CL4       P         METRO B                                      
M2DMA    DS    CL4       P         DMA                                          
M2NA     DS    CL4       P         NSI AREA                                     
M2ADJ1   DS    CL4       P         ADJ DMA 1                                    
M2ADJ2   DS    CL4       P         ADJ DMA 2                                    
M2ADJ3   DS    CL4       P         ADJ DMA 3                                    
         DS    CL100               SPARE                                        
*                                                                               
* NSI AREA UNIVERSE ESTIMATES (00) OR (000)                                     
         DS    CL120                                                            
*                                                                               
* DMA UNIVERSE ESTIMATES (00) OR (000)                                          
         DS    CL124                                                            
         EJECT                                                                  
         EJECT                                                                  
DPDSECT  DSECT                                                                  
DPMNO    DS    CL2       P         MARKET NUMBER                                
DPDSEQ   DS    CL1       C         DAY SEQUENCE                                 
DPDCDE   DS    CL1       C         DAY CODE (ALWAYS ZERO)                       
DPDPTCD  DS    CL2       C         DDAYPART CODE                                
DPSSEQ   DS    CL2       C         REPORT SEQ                                   
DPRCDE   DS    CL2       C         RECORD CODE = 21                             
DPSCDE   DS    CL3       P         STATION CODE                                 
DPSCALL  DS    CL4       C         STATION CALL LETTERS                         
DPPPI    DS    CL1       C         PARENT PLUS INDICATOR                        
DPSTYP   DS    CL1       C         STATION TYPE                                 
DPCHNL   DS    CL2       C         CHANNEL                                      
DPAFFL   DS    CL7       C         AFFILIATES                                   
DPWCUME  DS    CL9       P         WEEKLY CUMES                                 
DPDMATND DS    CL12      P         TREND DMA HUTS                               
DPPDIST  DS    CL12      P         PERCENT DISTIBUTION                          
DPRTGADJ DS    CL6       P         ADJACENT DMA RATINGS                         
DPHUTTND DS    CL12      P         TREND DMA HUT                                
         DS    CL10      C         SPARE                                        
DPMAHH   DS    CL19      P         METRO A HH DEMOS                             
DPMBHH   DS    CL19      P         METRO B HH DEMOS                             
DPDMAHH  DS    CL19      P         DMA HH DEMOS                                 
DPRW1    DS    CL3       P         WEEK 1 RATING                                
DPRW2    DS    CL3       P         WEEK 2 RATING                                
DPRW3    DS    CL3       P         WEEK 3 RATING                                
DPRW4    DS    CL3       P         WEEK 4 RATING                                
         EJECT                                                                  
*                                                                               
DPREACH  DS    CL5                 REACH                                        
DPOCABLE DS    CL3                 CABLE VIEWING (XX.XXX)                       
DPCABLE  DS    CL5                 CABLE VIEWING HOMES                          
         DS    CL11                SPARE                                        
*                                                                               
* STATION TOTAL PROJECTIONS IN UNITS                                            
DPSDEMOS DS    CL120                                                            
*                                                                               
* DMA PROJECTIONS IN UNITS                                                      
DPDDEMOS DS    CL104                                                            
DPCUME1  DS    15CL2               PERSON CUMES                                 
         DS    4CL2                SPARE                                        
         DS    C                   SPARE                                        
         DS    2CL2                SPARE                                        
DPCMETA  DS    CL2                 METRO A CUME                                 
DPCMETB  DS    CL2                 METRO B CUME                                 
DPCDMA   DS    CL2                 DMA CUME                                     
DPCIMP   DS    CL5                 STA TOT CUME(UNITS)                          
         ORG   DPCMETA                                                          
DPCAVG   DS    CL2                 AVERAGE WEEK CUME                            
DPC4W    DS    CL2                 4 WEEK CUME                                  
         SPACE 3                                                                
*                                                                               
SPP302   CSECT                                                                  
         DS    0D                                                               
         DC    CL8'DRGLOBAL'                                                    
GLAREA   DS    40000X              DRIVER GLOBAL AREA                           
         SPACE 2                                                                
         PRINT OFF                                                              
       ++INCLUDE DRGLOBAL                                                       
       ++INCLUDE SPDRVWRKD                                                      
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE DDCOMFACS                                                      
DBLKD    DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         SPACE 2                                                                
       ++INCLUDE DEDEMFILE                                                      
         SPACE 2                                                                
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'052SPREPP3AR 05/01/02'                                      
         END                                                                    
