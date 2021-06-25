*          DATA SET SPREPP302  AT LEVEL 087 AS OF 05/01/02                      
*PHASE SPP302A                                                                  
*INCLUDE EDITOR                                                                 
*INCLUDE LOGIO                                                                  
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
         CLI   QOPT2,C'U'          UNIVERSE LIST                                
         BE    *+8                                                              
         CLI   QOPT2,C'M'          MARKET LIST                                  
         BE    *+12                                                             
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
         L     RF,=A(IN1)                                                       
         A     RF,RELO                                                          
         PRINT GEN                                                              
         OPEN  ((RF),(INPUT))                                                   
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
         L     R6,=A(INREC)                                                     
         A     R6,RELO                                                          
         USING DPDSECT,R6                                                       
         L     RF,=A(IN1)                                                       
         A     RF,RELO                                                          
         GET   (RF),(R6)                                                        
         PRINT NOGEN                                                            
         ZIC   R1,DPMNO                                                         
         SRL   R1,4                                                             
         STC   R1,BYTE                                                          
         OI    BYTE,X'F0'                                                       
         CLI   QOPT3,C' '                                                       
         BE    *+14                                                             
         CLC   QOPT3(1),BYTE                                                    
         BNE   SP140                                                            
         CLC   DPRCDE,=C'01'       MKT INFO RECORD                              
         BE    SP160                                                            
         CLC   DPRCDE,=C'02'       MKT INFO RECORD                              
         BE    SP170                                                            
         CLC   DPRCDE,=C'21'       DAYPART DEMO RECORD                          
         BNE   SP140                                                            
*        CLI   DPSCALL,C' '        ONLY DO MARKET LEVELS                        
*        BNE   SP140                                                            
*        CLI   MKTTYP,C'5'         NON DMA MARKETS                              
*        BE    SP140                                                            
*                                                                               
         MVC   CALL,=C'     '                                                   
         MVC   CALL,DPSCALL                                                     
         L     R7,=A(DPTABLE3)     MAP NEW DAYPART CODE TO OLD CODE             
         USING DPTAB2D,R7                                                       
GETODPT  CLI   0(R7),0                                                          
         BNE   GETODPT2                                                         
         DC    H'0'                                                             
         DC    CL20'NEW DAYPART UNKNOWN'                                        
GETODPT2 CLC   DPDPTCD,DP2NEW      FIND NEW DAYPART CODE                        
         BE    *+12                                                             
GETODPT3 LA    R7,DPTAB2LN(R7)                                                  
         B     GETODPT                                                          
         MVC   DUB,DP2OLD          DUB=OLD 2-CHAR DAYPART CODE                  
         DROP  R7                                                               
*                                                                               
         LA    RE,HMDPTTAB         CONVERT DAYPARTS TO HARRIS                   
HMDPT    CLI   0(RE),X'FF'         BYPASS IF NOT FOUND                          
         BE    SP140                                                            
         CLC   DUB(2),0(RE)                                                     
         BE    *+12                                                             
         LA    RE,5(RE)                                                         
         B     HMDPT                                                            
         MVC   P3DPT,2(RE)                                                      
         MVC   CAPVAR,=C'CUM'                                                   
*                                                                               
         MVC   DOUTS(2),=PL2'0'    CLEAR DEMO AREA                              
         MVC   DOUTS+2(2*20-2),DOUTS                                            
         LA    R1,DOUTS                                                         
         USING DO2,R1                                                           
         LA    RF,DPCUME1                                                       
         USING CUMDEM,RF                                                        
         CLI   MKTTYP,C'7'                                                      
         BL    *+10                                                             
         MVC   DO2META,CUMETA                                                   
         MVC   DO2HOME,CUHOMES                                                  
         CLI   MKTTYP,C'1'                                                      
         BE    *+8                                                              
         CLI   MKTTYP,C'3'                                                      
         BNE   CUDIARY                                                          
         MVC   DO2META,=PL2'0'                                                  
         MVC   DO2HOME,CUMETA                                                   
CUDIARY  MVC   DO2WWORK,CUWWORK                                                 
         MVC   DO2V2P,CUV2P                                                     
         MVC   DO2V211,CUV211                                                   
         MVC   DO2V1217,CUV1217                                                 
         MVC   DO2V1224,CUV1224                                                 
         MVC   DO2M1834,CUM1834                                                 
         MVC   DO2M1849,CUM1849                                                 
         MVC   DO2M18P,CUM18P                                                   
         MVC   DO2M2549,CUM2549                                                 
         MVC   DO2M2554,CUM2554                                                 
         MVC   DO2W1834,CUW1834                                                 
         MVC   DO2W1849,CUW1849                                                 
         MVC   DO2W18P,CUW18P                                                   
         MVC   DO2W2549,CUW2549                                                 
         MVC   DO2W2554,CUW2554                                                 
         DROP  R1                                                               
         DROP  RF                                                               
*        MVC   DOUTS(2*15),DPCUME1                                              
*        CLI   MKTTYP,C'1'        NON-METERED MARKETS                           
*        BH    SP140A                                                           
*        MVC   DOUTS+2*18(4),DPCAVG                                             
*        B     *+10                                                             
*P140A   MVC   DOUTS+2*15(6),DPCMETA                                            
*                                                                               
         LA    R1,DOUTS            ADJUST FIELDS TO SAME PRECISION              
         LA    RF,DOUTS4                                                        
         LA    R0,17                                                            
SP140B   ZAP   DUB,0(2,R1)                                                      
         CVB   RE,DUB                                                           
         CH    R0,=H'15'                                                        
         BH    *+8                                                              
         MH    RE,=H'10'                                                        
         CH    RE,=H'1000'                                                      
         BL    *+8                                                              
         LA    RE,999                                                           
         ST    RE,0(RF)                                                         
         LA    R1,2(R1)                                                         
         LA    RF,4(RF)                                                         
         BCT   R0,SP140B                                                        
*                                                                               
         L     RE,MYRECS                                                        
         LA    RE,1(RE)                                                         
         ST    RE,MYRECS                                                        
         B     *+12                LIMIT TO N RECORDS                           
         C     RE,=F'1000'          *                                           
         BH    SP140                *                                           
         CLI   QOPT2,C'U'                                                       
         BE    SP140                                                            
         GOTO1 ADRIVER,DMCB,(RC)                                                
         XC    DOUTS4,DOUTS4                                                    
         B     SP140               BYPASS RATINGS FOR NOW                       
*                                                                               
         LA    R2,DPMAHH           HOMES ARE SPECIAL                            
         USING HOMESD,R2                                                        
         ZAP   DUB,HMSIMP                                                       
         CVB   RF,DUB                                                           
         ST    RF,DOUTS4                                                        
         LA    R2,DPDMAHH                                                       
         ZAP   DUB,HMSIMP                                                       
         CVB   RF,DUB                                                           
         ST    RF,DOUTS4+4                                                      
         DROP  R2                                                               
*                                                                               
         LA    RF,DOUTS4+8                                                      
         LA    R2,DPDDEMOS                                                      
         BAS   R7,BINCNV                                                        
         LA    R1,DOUTS4                                                        
         LA    R2,UNIV                                                          
         LA    R0,17                                                            
CALRTG   ICM   RF,15,0(R1)                                                      
         BZ    CALRTG2                                                          
         OC    0(4,R2),0(R2)                                                    
         BZ    CALRTG2                                                          
         SR    RE,RE                                                            
         M     RE,=F'1000'                                                      
         D     RE,0(R2)                                                         
         ST    RF,0(R1)                                                         
CALRTG2  LA    R2,4(R2)                                                         
         LA    R1,4(R1)                                                         
         BCT   R0,CALRTG                                                        
         MVC   CAPVAR,=C'RTG'                                                   
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
         MVI   MKTTYPA,C'D'                                                     
         CLI   MKTTYP,C'1'                                                      
         BE    *+8                                                              
         CLI   MKTTYP,C'3'                                                      
         BNE   *+8                                                              
         MVI   MKTTYPA,C'M'                                                     
         LA    RE,TZTAB                                                         
SP160B   CLI   0(RE),X'FF'                                                      
         BE    SP140                                                            
         CLC   TIMEZONE,0(RE)                                                   
         BE    *+12                                                             
         LA    RE,2(RE)                                                         
         B     SP160B                                                           
         MVC   TIMEZONE,1(RE)                                                   
         B     SP140                                                            
         SPACE 2                                                                
         USING M2DSECT,R6                                                       
SP170    MVC   MKTNAM,M2MKTNAM                                                  
         CLI   QOPT2,C'M'          MARKET NUMBER/NAME                           
         BE    SP180                                                            
         MVC   CALL,=C'UUUU'                                                    
         MVC   CAPVAR,=C'UNV'                                                   
         MVC   P3DPT,=C'TOT'                                                    
         XC    UNIV,UNIV                                                        
         LA    R2,M2DMAU                                                        
         USING DMADEM,R2                                                        
         LA    RF,UNIV+8                                                        
         BAS   R7,BINCNV                                                        
         ZAP   DUB,M2MA                                                         
         CVB   RF,DUB                                                           
         ST    RF,UNIV                                                          
         ZAP   DUB,M2DMA                                                        
         CVB   RF,DUB                                                           
         ST    RF,UNIV+4                                                        
         LA    RF,UNIVB+8                                                       
         BAS   R7,BINCNVU                                                       
         ZAP   DUB,M2MA                                                         
         CVB   RF,DUB                                                           
         ST    RF,UNIVB                                                         
         ZAP   DUB,M2DMA                                                        
         CVB   RF,DUB                                                           
         ST    RF,UNIVB+4                                                       
         MVC   DOUTS4,UNIVB                                                     
         CLI   QOPT2,C'U'                                                       
         BNE   SP140                                                            
SP180    GOTO1 ADRIVER,DMCB,(RC)                                                
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
         DC    C'TZ      ',A(OPTZ)                                              
         DC    C'MKTTYP  ',A(OPMKTTYP)                                          
         DC    C'OPCALL  ',A(OPCALL)                                            
         DC    C'OPDPT   ',A(OPDPT)                                             
         DC    C'CAPTION ',A(OPCAP)                                             
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
         MVC   0(2,R3),DPMNO                                                    
         B     EXIT                                                             
*                                                                               
OPMKTNAM DS    0H                                                               
         MVC   0(26,R3),MKTNAM                                                  
         B     EXIT                                                             
*                                                                               
OPCALL   DS    0H                                                               
         MVC   0(5,R3),CALL                                                     
         B     EXIT                                                             
*                                                                               
OPDPT    DS    0H                                                               
         MVC   0(3,R3),P3DPT                                                    
         B     EXIT                                                             
*                                                                               
OPCAP    DS    0H                                                               
         MVC   0(3,R3),CAPVAR                                                   
         B     EXIT                                                             
*                                                                               
OPTZ     DS    0H                                                               
         MVC   0(1,R3),TIMEZONE                                                 
         B     EXIT                                                             
*                                                                               
OPMKTTYP DS    0H                                                               
         MVC   0(1,R3),MKTTYPA                                                  
         B     EXIT                                                             
*                                                                               
DEMIP    DS    0H                                                               
         ZIC   RF,GLARGS           ARG IS DEMO NUM                              
         SLL   RF,2                X 4                                          
         LA    R1,DOUTS4-4(RF)                                                  
         MVC   0(4,R3),0(R1)       MOVE IN DEMO VALUE                           
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
BINCNV   ZAP   DUB,DMWWORK                                                      
         CVB   R1,DUB                                                           
         ST    R1,0(RF)            WWORK                                        
         LA    RF,4(RF)                                                         
         ZAP   DUB,DMK25                                                        
         AP    DUB,DMK611                                                       
         AP    DUB,DMM1217                                                      
         AP    DUB,DMW1217                                                      
         AP    DUB,DMM1820                                                      
         AP    DUB,DMM2124                                                      
         AP    DUB,DMM2534                                                      
         AP    DUB,DMM3549                                                      
         AP    DUB,DMM5054                                                      
         AP    DUB,DMM5564                                                      
         AP    DUB,DMM65P                                                       
         AP    DUB,DMW1820                                                      
         AP    DUB,DMW2124                                                      
         AP    DUB,DMW2534                                                      
         AP    DUB,DMW3549                                                      
         AP    DUB,DMW5054                                                      
         AP    DUB,DMW5564                                                      
         AP    DUB,DMW65P                                                       
         CVB   R1,DUB                                                           
         ST    R1,0(RF)                                                         
         LA    RF,4(RF)                                                         
         ZAP   DUB,DMK25                                                        
         AP    DUB,DMK611                                                       
         CVB   R1,DUB                                                           
         ST    R1,0(RF)                                                         
         LA    RF,4(RF)                                                         
         ZAP   DUB,DMM1217                                                      
         AP    DUB,DMW1217                                                      
         CVB   R1,DUB                                                           
         ST    R1,0(RF)                                                         
         LA    RF,4(RF)                                                         
         AP    DUB,DMM1820                                                      
         AP    DUB,DMW1820                                                      
         AP    DUB,DMM2124                                                      
         AP    DUB,DMW2124                                                      
         CVB   R1,DUB                                                           
         ST    R1,0(RF)                                                         
         LA    RF,4(RF)                                                         
         ZAP   DUB,DMM1820                                                      
         AP    DUB,DMM2124                                                      
         AP    DUB,DMM2534                                                      
         CVB   R1,DUB                                                           
         ST    R1,0(RF)                                                         
         LA    RF,4(RF)                                                         
         AP    DUB,DMM3549                                                      
         CVB   R1,DUB                                                           
         ST    R1,0(RF)                                                         
         LA    RF,4(RF)                                                         
         ZAP   DUB,DMM1820                                                      
         AP    DUB,DMM2124                                                      
         AP    DUB,DMM2534                                                      
         AP    DUB,DMM3549                                                      
         AP    DUB,DMM5054                                                      
         AP    DUB,DMM5564                                                      
         AP    DUB,DMM65P                                                       
         CVB   R1,DUB                                                           
         ST    R1,0(RF)                                                         
         LA    RF,4(RF)                                                         
         ZAP   DUB,DMM2534                                                      
         AP    DUB,DMM3549                                                      
         CVB   R1,DUB                                                           
         ST    R1,0(RF)                                                         
         LA    RF,4(RF)                                                         
         AP    DUB,DMM5054                                                      
         CVB   R1,DUB                                                           
         ST    R1,0(RF)                                                         
         LA    RF,4(RF)                                                         
         ZAP   DUB,DMW1820                                                      
         AP    DUB,DMW2124                                                      
         AP    DUB,DMW2534                                                      
         CVB   R1,DUB                                                           
         ST    R1,0(RF)                                                         
         LA    RF,4(RF)                                                         
         AP    DUB,DMW3549                                                      
         CVB   R1,DUB                                                           
         ST    R1,0(RF)                                                         
         LA    RF,4(RF)                                                         
         ZAP   DUB,DMW1820                                                      
         AP    DUB,DMW2124                                                      
         AP    DUB,DMW2534                                                      
         AP    DUB,DMW3549                                                      
         AP    DUB,DMW5054                                                      
         AP    DUB,DMW5564                                                      
         AP    DUB,DMW65P                                                       
         CVB   R1,DUB                                                           
         ST    R1,0(RF)                                                         
         LA    RF,4(RF)                                                         
         ZAP   DUB,DMW2534                                                      
         AP    DUB,DMW3549                                                      
         CVB   R1,DUB                                                           
         ST    R1,0(RF)                                                         
         LA    RF,4(RF)                                                         
         AP    DUB,DMW5054                                                      
         CVB   R1,DUB                                                           
         ST    R1,0(RF)                                                         
         LA    RF,4(RF)                                                         
         BR    R7                                                               
*                                                                               
BINCNVU  ZAP   DUB,DMK25                                                        
         CVB   R1,DUB                                                           
         ST    R1,0(RF)            WWORK                                        
         LA    RF,4(RF)                                                         
         ZAP   DUB,DMK611                                                       
         CVB   R1,DUB                                                           
         ST    R1,0(RF)                                                         
         LA    RF,4(RF)                                                         
         ZAP   DUB,DMM1217                                                      
         CVB   R1,DUB                                                           
         ST    R1,0(RF)                                                         
         LA    RF,4(RF)                                                         
         ZAP   DUB,DMW1217                                                      
         CVB   R1,DUB                                                           
         ST    R1,0(RF)                                                         
         LA    RF,4(RF)                                                         
         ZAP   DUB,DMM1820                                                      
         CVB   R1,DUB                                                           
         ST    R1,0(RF)                                                         
         LA    RF,4(RF)                                                         
         ZAP   DUB,DMM2124                                                      
         CVB   R1,DUB                                                           
         ST    R1,0(RF)                                                         
         LA    RF,4(RF)                                                         
         ZAP   DUB,DMM2534                                                      
         CVB   R1,DUB                                                           
         ST    R1,0(RF)                                                         
         LA    RF,4(RF)                                                         
         ZAP   DUB,DMM3549                                                      
         CVB   R1,DUB                                                           
         ST    R1,0(RF)                                                         
         LA    RF,4(RF)                                                         
         ZAP   DUB,DMM5054                                                      
         CVB   R1,DUB                                                           
         ST    R1,0(RF)                                                         
         LA    RF,4(RF)                                                         
         ZAP   DUB,DMM5564                                                      
         CVB   R1,DUB                                                           
         ST    R1,0(RF)                                                         
         LA    RF,4(RF)                                                         
         ZAP   DUB,DMM65P                                                       
         CVB   R1,DUB                                                           
         ST    R1,0(RF)                                                         
         LA    RF,4(RF)                                                         
         ZAP   DUB,DMW1820                                                      
         CVB   R1,DUB                                                           
         ST    R1,0(RF)                                                         
         LA    RF,4(RF)                                                         
         ZAP   DUB,DMW2124                                                      
         CVB   R1,DUB                                                           
         ST    R1,0(RF)                                                         
         LA    RF,4(RF)                                                         
         ZAP   DUB,DMW2534                                                      
         CVB   R1,DUB                                                           
         ST    R1,0(RF)                                                         
         LA    RF,4(RF)                                                         
         ZAP   DUB,DMW3549                                                      
         CVB   R1,DUB                                                           
         ST    R1,0(RF)                                                         
         LA    RF,4(RF)                                                         
         ZAP   DUB,DMW5054                                                      
         CVB   R1,DUB                                                           
         ST    R1,0(RF)                                                         
         LA    RF,4(RF)                                                         
         ZAP   DUB,DMW5564                                                      
         CVB   R1,DUB                                                           
         ST    R1,0(RF)                                                         
         LA    RF,4(RF)                                                         
         ZAP   DUB,DMW65P                                                       
         CVB   R1,DUB                                                           
         ST    R1,0(RF)                                                         
         LA    RF,4(RF)                                                         
         ZAP   DUB,DMWWORK                                                      
         CVB   R1,DUB                                                           
         ST    R1,0(RF)                                                         
         LA    RF,4(RF)                                                         
         BR    R7                                                               
*                                                                               
         SPACE 2                                                                
DIV      DIV   (R0),(RF)       RETURNS VIA RE                                   
         SPACE 2                                                                
*                                                                               
BMEOF    DS    0H                                                               
*                                                                               
         CLOSE (IN1,REWIND)                                                     
*          DATA SET DEDEMCNV   AT LEVEL 104 AS OF 03/10/95                      
*ETMSG   GOTO1 =V(LOGIO),DMCB,1,=C'*SPP302* ANY MORE INPUT TAPES? Y/N'          
*        GOTO1 (RF),(R1),0,(3,DUB)                                              
*        CLI   DUB,C'Y'                                                         
*        BE    NEXTTAPE                                                         
*        CLI   DUB,C'N'                                                         
*        BNE   GETMSG                                                           
         ZIC   R1,NTAPES                                                        
         LA    R1,1(R1)                                                         
         STC   R1,NTAPES                                                        
         CLI   NTAPES,4                                                         
         BNE   *+8                                                              
         B     SP140D                                                           
NEXTTAPE L     RF,=A(IN1)                                                       
         A     RF,RELO                                                          
         PRINT GEN                                                              
         OPEN  ((RF),(INPUT))                                                   
         B     SP140                                                            
         LTORG                                                                  
         EJECT                                                                  
NTAPES   DC    X'00'                                                            
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
HMDPTTAB DC    C'69',C'TOT' TOTAL        ' HARRIS MEDIA DAYPARTS                
         DC    C'04',C'EAM' EARLY MORNING'                                      
         DC    C'07',C'AM ' MORNING      '                                      
         DC    C'10',C'DAY' AFTERNOON    '                                      
         DC    C'14',C'EFR' EARLY FRINGE '                                      
         DC    C'24',C'PAC' PRIME ACCESS '                                      
         DC    C'56',C'PRI' PRIME        '                                      
         DC    C'60',C'LNW' LATE NEWS    '                                      
         DC    C'40',C'LFR' MF LTE FRINGE'                                      
         DC    C'00',C'NT ' NIGHT TIME   '                                      
         DC    C'82',C'SAA' SAT. AM      '                                      
         DC    C'84',C'SAP' SAT. PM      '                                      
         DC    C'00',C'SUA' SUN. AM      '                                      
         DC    C'92',C'SUP' SUN. PM      '                                      
         DC    X'FF'                                                            
*                                                                               
TZTAB    DC    C'1',C'E'           EASTERN                                      
         DC    C'2',C'C'           CENTRAL                                      
         DC    C'3',C'M'           MOUNTAIN                                     
         DC    C'4',C'P'           PACIFIC                                      
         DC    C'5',C'Y'           YUKON                                        
         DC    C'6',C'H'           HAWAIIAN                                     
*                                                                               
SRCE     DS    C                                                                
XFF      DC    20X'FF'                                                          
X        DS    XL256                                                            
UNIV     DS    CL80                                                             
UNIVB    DS    CL88                                                             
*                                                                               
SVKEY    DS    CL32                                                             
ELCODE   DS    XL1                                                              
DLHSW    DS    X                                                                
QTRNO    DS    CL1                                                              
MKTTYP   DS    CL1                                                              
MKTCL    DS    CL1                                                              
MKTNAM   DS    CL26                                                             
TIMEZONE DS    CL1                                                              
MKTTYPA  DS    CL1                                                              
CAPVAR   DS    CL3                                                              
P3DPT    DS    CL3                                                              
CALL     DS    CL5                                                              
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
NDEMS    EQU   24                                                               
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
DOUTS4   DS    CL(NDEMS*4)                                                      
         SPACE 2                                                                
*          DATA SET DEDN9310   AT LEVEL 019 AS OF 11/17/93                      
* PRIOR 96/02                                                                   
DPTABLE2 DC    C'10001000',C'02'                                                
         DC    C'10020500',C'04'                                                
         DC    C'10022000',C'06'                                                
         DC    C'10023000',C'07'                                                
         DC    C'10026800',C'08'                                                
         DC    C'10027000',C'10'                                                
         DC    C'10030000',C'12'                                                
         DC    C'10030900',C'14'                                                
         DC    C'10032000',C'16'                                                
         DC    C'10033400',C'18'                                                
         DC    C'10035000',C'20'                                                
         DC    C'10036000',C'22'                                                
         DC    C'10037000',C'24'                                                
         DC    C'10039000',C'26'                                                
         DC    C'10039675',C'27'                                                
         DC    C'10040000',C'28'                                                
         DC    C'10040800',C'30'                                                
         DC    C'10041000',C'32'                                                
         DC    C'10041450',C'34'                                                
         DC    C'10042500',C'36'                                                
         DC    C'10043000',C'38'                                                
         DC    C'10045000',C'40'                                                
         DC    C'10045300',C'39'                                                
         DC    C'10045050',C'41'                                                
         DC    C'10020100',C'42'                                                
         DC    C'10020400',C'44'                                                
                                                                                
         DC    C'50009000',C'50'                                                
         DC    C'50003000',C'52'                                                
         DC    C'50004000',C'53'                                                
         DC    C'50004900',C'54'                                                
         DC    C'50004950',C'55'                                                
         DC    C'50005000',C'56'                                                
         DC    C'50005500',C'57'                                                
         DC    C'50006000',C'58'                                                
         DC    C'50007000',C'60'                                                
         DC    C'50009200',C'62'                                                
         DC    C'50002000',C'64'                                                
         DC    C'50001000',C'66'                                                
         DC    C'50000250',C'69'                                                
         DC    C'50000500',C'68'                                                
                                                                                
         DC    C'20021000',C'82'                                                
         DC    C'20022000',C'84'                                                
                                                                                
         DC    C'30021000',C'92'                                                
         DC    X'00'                                                            
*          DATA SET DEDN9611   AT LEVEL 022 AS OF 01/13/97                      
* 96/02 AND SUBSEQUENT                                                          
DPTABLE3 DC    C'10001000',C'02'                                                
         DC    C'10020500',C'04'                                                
         DC    C'10022000',C'06'                                                
         DC    C'10023000',C'07'                                                
         DC    C'10026800',C'08'                                                
         DC    C'10027000',C'10'                                                
         DC    C'10030000',C'12'                                                
         DC    C'10030900',C'14'                                                
         DC    C'10032000',C'16'                                                
         DC    C'10033400',C'18'                                                
         DC    C'10035000',C'20'                                                
         DC    C'10036000',C'22'                                                
         DC    C'10037000',C'24'                                                
         DC    C'10039000',C'26'                                                
         DC    C'10039675',C'27'                                                
         DC    C'10040000',C'28'                                                
         DC    C'10040800',C'30'                                                
         DC    C'10041000',C'32'                                                
         DC    C'10041450',C'34'                                                
         DC    C'10042500',C'36'                                                
         DC    C'10043000',C'38'                                                
         DC    C'10045000',C'40'                                                
         DC    C'10045300',C'39'                                                
         DC    C'10045050',C'41'                                                
         DC    C'10020100',C'42'                                                
         DC    C'10020400',C'44'                                                
         DC    C'10042375',C'45'                                                
         DC    C'10042675',C'46'                                                
         DC    C'10043500',C'47'                                                
                                                                                
         DC    C'50009000',C'50'                                                
         DC    C'50003000',C'52'                                                
         DC    C'50004000',C'53'                                                
         DC    C'50004900',C'54'                                                
         DC    C'50004950',C'55'                                                
         DC    C'50005000',C'56'                                                
         DC    C'50005500',C'57'                                                
         DC    C'50006000',C'58'                                                
         DC    C'50007000',C'60'                                                
         DC    C'50009200',C'62'                                                
         DC    C'50002000',C'64'                                                
         DC    C'50001000',C'66'                                                
         DC    C'50000250',C'69'                                                
         DC    C'50000500',C'68'                                                
         DC    C'50004975',C'69'                                                
         DC    C'50004985',C'70'                                                
         DC    C'50004990',C'71'                                                
                                                                                
         DC    C'20021000',C'82'                                                
         DC    C'20022000',C'84'                                                
                                                                                
         DC    C'30021000',C'92'                                                
         DC    X'00'                                                            
         SPACE 2                                                                
IN1      DCB   DDNAME=IN1,                                             X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=00460,                                            X        
               BLKSIZE=04600,                                          X        
               MACRF=GM,                                               X        
               EODAD=BMEOF                                                      
         SPACE                                                                  
INREC    DS    460C                                                             
         EJECT                                                                  
DPTAB2D  DSECT                                                                  
DP2NEW   DS    CL(L'DPDPTCD)       NEW DAYPART CODE                             
DP2OLD   DS    CL(2)               OLD DAYPART CODE                             
DPTAB2LN EQU   *-DPTAB2D                                                        
         EJECT                                                                  
HOMESD   DSECT                                                                  
HMSRTG   DS    PL3                                                              
HMSHUT   DS    PL3                                                              
HMSSHR   DS    PL3                                                              
HMSIMP   DS    PL5                                                              
*                                                                               
DO2      DSECT                                                                  
DO2META  DS    PL2                                                              
DO2HOME  DS    PL2                                                              
DO2WWORK DS    PL2                                                              
DO2V2P   DS    PL2                                                              
DO2V211  DS    PL2                                                              
DO2V1217 DS    PL2                                                              
DO2V1224 DS    PL2                                                              
DO2M1834 DS    PL2                                                              
DO2M1849 DS    PL2                                                              
DO2M18P  DS    PL2                                                              
DO2M2549 DS    PL2                                                              
DO2M2554 DS    PL2                                                              
DO2W1834 DS    PL2                                                              
DO2W1849 DS    PL2                                                              
DO2W18P  DS    PL2                                                              
DO2W2549 DS    PL2                                                              
DO2W2554 DS    PL2                                                              
DMADEM   DSECT                                                                  
DMK25    DS    PL4                                                              
DMK611   DS    PL4                                                              
         DS    PL4                                                              
DMM1217  DS    PL4                                                              
DMW1217  DS    PL4                                                              
         DS    PL4                                                              
         DS    PL4                                                              
         DS    PL4                                                              
DMM1820  DS    PL4                                                              
DMM2124  DS    PL4                                                              
DMM2534  DS    PL4                                                              
DMM3549  DS    PL4                                                              
DMM5054  DS    PL4                                                              
DMM5564  DS    PL4                                                              
DMM65P   DS    PL4                                                              
         DS    PL4                                                              
         DS    PL4                                                              
DMW1820  DS    PL4                                                              
DMW2124  DS    PL4                                                              
DMW2534  DS    PL4                                                              
DMW3549  DS    PL4                                                              
DMW5054  DS    PL4                                                              
DMW5564  DS    PL4                                                              
DMW65P   DS    PL4                                                              
         DS    PL4                                                              
DMWWORK  DS    PL4                                                              
         SPACE 2                                                                
CUMDEM   DSECT                                                                  
CUV211   DS    PL2                                                              
CUV1217  DS    PL2                                                              
CUW18P   DS    PL2                                                              
CUW1834  DS    PL2                                                              
CUW1849  DS    PL2                                                              
CUW2549  DS    PL2                                                              
CUW2554  DS    PL2                                                              
CUWWORK  DS    PL2                                                              
CUM18P   DS    PL2                                                              
CUM1834  DS    PL2                                                              
CUM1849  DS    PL2                                                              
CUM2549  DS    PL2                                                              
CUM2554  DS    PL2                                                              
CUV1224  DS    PL2                                                              
CUV2P    DS    PL2                                                              
CUV18P   DS    PL2                                                              
         DS    PL2    439                                                       
         DS    PL2    441                                                       
         DS    PL2    443                                                       
         DS    PL1    445                                                       
         DS    PL2    446                                                       
         DS    PL2    448                                                       
CUMETA   DS    PL2                                                              
CUMETB   DS    PL2                                                              
CUHOMES  DS    PL2                                                              
         EJECT                                                                  
*          DATA SET DEDN9310   AT LEVEL 019 AS OF 11/17/93                      
*======================== NIELSEN-TAPE DSECTS ========================*         
*                                                                               
*------------------- MARKET INFORMATION RECORD 1 ---------------------*         
*                                                                               
MIDSECT  DSECT                                                                  
M1MNO    DS    CL2       P         MARKET NUMBER                                
         DS    CL6                 SPARE                                        
M1SRSQCD DS    CL2                 STATION REPORT SEQUENCE CODE                 
M1RCODE  DS    CL2       C         RECORD CODE - 01                             
M1WKS    DS    CL1       P         NUMBER OF WEEKS MEASURED                     
M1YR     DS    CL2       C         REPORT YEAR                                  
M1MO     DS    CL2       C         REPORT MONTH                                 
M1TZ     DS    CL1       C         TIME ZONE                                    
M1MTYP   DS    CL1       C         MARKET TYPE                                  
M1MCL    DS    CL1       C         MARKET CLASS                                 
M1TSCL   DS    CL1       C         TARGET SUB-CLASS                             
M1ROUND  DS    CL1       C         ROUNDING CONTROL                             
M1DMA    DS    CL1       C         DMA INDICATOR                                
M1CNTST  DS    CL1       C         CONTEST INDICATOR                            
M1TRPT   DS    CL1       C         D=DAYPART                                    
         DS    CL10                (FOR FUTURE USE)                             
M1INTAB  DS    CL48      P         NET-IN-TAB COUNTS (4)                        
M1QHTRND DS    CL16      C         QUARTER HOUR TRENDS (4)                      
M1ADJDMA DS    CL6       P         ADJACENT DMAS (3)                            
M1STRDTE DS    CL6       C         PERIOD START DATE (MMDDYY)                   
M1ENDDTE DS    CL6       C         PERIOD END DATE                              
         DS    CL7       C         SPARE                                        
M1STAT   DS    CL250               REPORTABLE STATIONS (20)                     
         DS    CL86                (FOR FUTURE USE)                             
M1RECLQ  EQU   *-MIDSECT                                                        
         EJECT                                                                  
*------------------- MARKET INFORMATION RECORD 2 ---------------------*         
*                                                                               
M2DSECT  DSECT                                                                  
M2MNO    DS    CL2       P         MARKET NUMBER                                
         DS    CL6                 SPARE                                        
M2SRSQCD DS    CL2                 STATION REPORT SEQUENCE CODE                 
M2RCODE  DS    CL2       C         RECORD CODE = 02                             
M2MKTNAM DS    CL26      C         MARKET NAME                                  
         DS    CL14                SPARE                                        
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
M2DMAU   DS    CL160                                                            
*                                                                               
M2RECLQ  EQU   *-M2DSECT                                                        
         EJECT                                                                  
*-------------------------- DAYPART RECORD ---------------------------*         
*                                                                               
DPDSECT  DSECT                                                                  
DPMNO    DS    CL2       P         MARKET NUMBER                                
DPDSEQ   DS    CL1       C         DAY SEQUENCE                                 
DPDCDE   DS    CL1       C         DAY CODE (ALWAYS ZERO)                       
DPDSQCD  DS    CL4       C         DAYPART SEQUENCE CODE                        
DPSSEQ   DS    CL2       C         REPORT SEQ                                   
DPRCDE   DS    CL2       C         RECORD CODE = 21                             
DPSCDE   DS    CL3       P         STATION CODE                                 
DPSCALL  DS    CL4       C         STATION CALL LETTERS                         
DPPPI    DS    CL1       C         PARENT PLUS INDICATOR                        
DPSTYP   DS    CL1       C         STATION TYPE                                 
DPCHNL   DS    CL2       C         CHANNEL                                      
DPAFFL   DS    CL7       C         AFFILIATES                                   
DPDPTCD  DS    CL8       C         DDAYPART CODE                                
DPDMATND DS    CL12      P         TREND DMA HUTS                               
DPPDIST  DS    CL12      P         PERCENT DISTIBUTION                          
DPRTGADJ DS    CL6       P         ADJACENT DMA RATINGS (3)                     
DPHUTTND DS    CL12      P         TREND DMA HUT                                
         DS    CL9       C         SPARE                                        
DPMAHH   DS    CL19      P         METRO A HH DEMOS                             
DPMBHH   DS    CL19      P         METRO B HH DEMOS                             
DPDMAHH  DS    CL19      P         DMA HH DEMOS                                 
DPRW1    DS    CL3       P         WEEK 1 RATING                                
DPRW2    DS    CL3       P         WEEK 2 RATING                                
DPRW3    DS    CL3       P         WEEK 3 RATING                                
DPRW4    DS    CL3       P         WEEK 4 RATING                                
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
**PAN#1  DC    CL21'087SPREPP302 05/01/02'                                      
         END                                                                    
