*          DATA SET SPREPP202  AT LEVEL 006 AS OF 05/01/02                      
*PHASE SPP202A                                                                  
*INCLUDE EDITOR                                                                 
         TITLE 'SPREPP202 - MASTER MARKET LIST '                                
         PRINT NOGEN                                                            
*                                                                               
*                                                                               
*        QOPT1      RATING SOURCE  A=ARB,N=ACN                                  
*        QOPT2      DPG PHASE - (A - D = DOWNLOAD)                              
*                                                                               
*                                                                               
SPP202   CSECT                                                                  
         NMOD1 0,SPP202,RR=R5                                                   
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
         PACK  DUB,QBOOK1+1(2)     SET QUARTER NUMBER                           
         CVB   R1,DUB              = BOOK MONTH/3 + 1                           
         SR    R0,R0                                                            
         D     R0,=F'3'                                                         
         LA    R1,1(R1)                                                         
         STC   R1,QTRNO                                                         
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
         SPACE 2                                                                
*                                  GET MARKET NAMES                             
*                                  ----------------                             
         BAS   RE,BLDMTAB                                                       
*                                                                               
*                                  GET UNIVERSES                                
*                                  -------------                                
         L     R6,AMKTTAB                                                       
         USING MKTTABD,R6                                                       
         SPACE 2                                                                
SP123    DS    0H                                                               
         CLI   0(R6),X'FF'         EOL                                          
         BE    SP123D                                                           
         L     R4,ADBLOCK                                                       
         USING DBLOCK,R4                                                        
         XC    0(256,R4),0(R4)                                                  
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBSELMED,C'T'                                                    
         MVC   DBSELSRC,SRCE       RATING SOURCE                                
         PACK  DUB,QBOOK1(2)       CONVERT BOOK                                 
         CVB   RE,DUB                                                           
         STC   RE,DBSELBK                                                       
         PACK  DUB,QBOOK1+2(2)                                                  
         CVB   RE,DUB                                                           
         STC   RE,DBSELBK+1                                                     
         MVC   DBCOMFCS,ACOMFACS                                                
         L     RE,ADBUY                                                         
         ST    RE,DBAREC                                                        
         MVC   DBSELRMK,MKNUM                                                   
         MVI   DBSELDAY,X'40'                                                   
         MVC   DBSELTIM(2),=H'1700'                                             
         MVC   DBSELTIM+2(2),=H'1715'                                           
         MVI   DBFUNCT,DBGETTOT                                                 
         GOTO1 DEMAND,DMCB,ADBLOCK,SVUNIV                                       
         LA    R6,MKLEN(R6)                                                     
         B     SP123                                                            
*                                                                               
SP123D   DS    0H                                                               
*                                  TOTAL AND SORT TO GET RANK                   
*                                  --------------------------                   
*                                                                               
         MVC   DMCB+4(4),MKTCNT                                                 
         MVC   DMCB(4),AMKTTAB                                                  
         MVI   DMCB,C'D'                                                        
         GOTO1 XSORT,DMCB,,,MKLEN,4,MKUNVS-MKNUM                                
*                                                                               
         L     R6,AMKTTAB                                                       
         USING MKTTABD,R6                                                       
*                                                                               
         XC    DOUTS,DOUTS                                                      
         LA    RF,1                                                             
*                                                                               
SP124    DS    0H                                                               
         CLI   0(R6),X'FF'         EOL                                          
         BE    SP125                                                            
         STH   RF,MKRNK                                                         
         LA    RF,1(RF)                                                         
*                                                                               
         LA    R5,NDEMS                                                         
         LA    R2,MKUNVS                                                        
         LA    R3,DOUTS                                                         
*                                                                               
SP124D   DS    0H                                                               
         L     R0,0(R2)                                                         
         A     R0,0(R3)                                                         
         ST    R0,0(R3)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R5,SP124D                                                        
*                                                                               
         LA    R6,MKLEN(R6)                                                     
         B     SP124                                                            
*                                                                               
SP125    DS    0H                                                               
         XC    MKNUM,MKNUM                                                      
         XC    MKRNK,MKRNK                                                      
         MVC   MKNAM,=CL30'**U.S. TOTALS**'                                     
         MVC   MKUNVS,DOUTS                                                     
         MVI   MKNUM+MKLEN,X'FF'   SET EOL                                      
*                                                                               
         SPACE 2                                                                
         MVI   GLMODE,GLINIT       INITIALIZE DRIVER                            
         GOTO1 ADRIVER,DMCB,(RC)                                                
*                                                                               
*                                  DRIVER INPUT PHASE                           
*                                  ------------------                           
         L     R6,AMKTTAB                                                       
         MVI   GLMODE,GLINPUT      SET UP FOR DRIVER INPUT                      
*                                                                               
SP140    DS    0H                                                               
         CLI   0(R6),X'FF'         EOL                                          
         BE    SP140D                                                           
         GOTO1 ADRIVER,DMCB,(RC)                                                
         LA    R6,MKLEN(R6)        NEXT MKT                                     
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
         DC    C'MKTNAM  ',A(MKTNAM)                                            
         DC    C'MKTNUM  ',A(MKTNUM)                                            
         DC    C'MKTRNK  ',A(MKTRNK)                                            
         DC    C'MKTTZI  ',A(MKTTZI)                                            
         DC    C'MKTCLSI ',A(MKTCLSI)                                           
         DC    C'MKTREGI ',A(MKTREGI)                                           
         DC    C'MKTABBI ',A(MKTABBI)                                           
         DC    C'MKTABBO ',A(MKTABBO)                                           
         DC    C'DEMIP   ',A(DEMIP)                                             
         DC    C'DEMNAM  ',A(DEMNAM)                                            
         DC    C'DLHTTL  ',A(DLHTTL)                                            
         DC    C'DLHSRC  ',A(DLHSRC)                                            
         DC    C'DLHYR   ',A(DLHYR)                                             
         DC    C'DLHNMKT ',A(DLHNMKT)                                           
         DC    C'DLHNDEM ',A(DLHNDEM)                                           
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
MKTNAM   DS    0H                                                               
         MVC   0(30,R3),MKNAM                                                   
         B     EXIT                                                             
*                                                                               
MKTRNK   DS    0H                                                               
         MVC   0(2,R3),MKRNK                                                    
         B     EXIT                                                             
*                                                                               
MKTNUM   DS    0H                                                               
         MVC   0(2,R3),MKNUM                                                    
         B     EXIT                                                             
*                                                                               
MKTTZI   DS    0H                                                               
         MVC   0(1,R3),MKTZ                                                     
         B     EXIT                                                             
*                                                                               
MKTCLSI  DS    0H                                                               
         MVC   0(1,R3),MKCLS                                                    
         B     EXIT                                                             
*                                                                               
MKTREGI  DS    0H                                                               
         MVC   0(2,R3),MKREG                                                    
         B     EXIT                                                             
*                                                                               
MKTABBI  DS    0H                                                               
         MVC   0(8,R3),MKABBR                                                   
         OC    0(8,R3),SPACES                                                   
         B     EXIT                                                             
*                                                                               
DEMIP    DS    0H                                                               
         ZIC   RF,GLARGS           ARG IS DEMO NUM                              
         SLL   RF,2                X 4                                          
         LA    R1,MKUNVS-4(RF)                                                  
         L     R1,0(R1)                                                         
         CLI   SRCE,C'A'          FOR ARB, ALREADY IN 100'S                     
         BE    DEMIP4                                                           
         M     R0,=F'1'                                                         
         L     RF,=F'100'                                                       
         BAS   RE,DIV                                                           
*                                                                               
DEMIP4   DS    0H                                                               
         ST    R1,0(R3)                                                         
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
         BNE   EXIT                                                             
         CLI   DLHSW,0             TEST HAVE DONE HEADS                         
         BNE   EXIT                                                             
         MVI   DLHSW,1                                                          
         MVI   GLHOOK,GLDONT                                                    
         B     EXIT                                                             
*                                                                               
         SPACE 2                                                                
DIV      DIV   (R0),(RF)       RETURNS VIA RE                                   
         SPACE 2                                                                
* SAVE UNIVERSES IN RANK TABLE                                                  
         SPACE 2                                                                
SVUNIV   NTR1                                                                   
         L     R4,ADBLOCK                                                       
         L     R7,ACOMFACS                                                      
         USING COMFACSD,R7                                                      
         GOTO1 CDEMOUT,DMCB,(C'L',DEMLST),ADBLOCK,DOUTS                         
         DROP  R7                                                               
         USING MKTTABD,R6                                                       
         MVC   MKUNVS,DOUTS                                                     
         B     EXIT                                                             
         SPACE 3                                                                
*        BUILD MARKET DATA TABLE                                                
         SPACE 2                                                                
BLDMTAB  NTR1                                                                   
         MVC   MKTSAV+40(8),=C'ADILIST '                                        
         CLI   SRCE,C'A'                                                        
         BE    *+10                                                             
         MVC   MKTSAV+40(8),=C'DMALIST '                                        
*                                                                               
         OPEN  (MKTSAV,(INPUT))                                                 
*                                  SET BINSRCH PARS                             
         SR    R0,R0                                                            
         L     R1,=A(MKTTAB)                                                    
         SR    R2,R2                                                            
         LA    R3,MKLEN                                                         
         LA    R4,MKKLEN                                                        
         LH    R5,=Y(MKTMAX)                                                    
         STM   R0,R5,MKTPARS                                                    
*                                                                               
         LA    R6,X                                                             
         XC    X,X                                                              
         USING MKTTABD,R6                                                       
         L     R5,ADBUY                                                         
         USING MSRECD,R5           MARKET SAVE RECORD                           
*                                                                               
BMK4     DS    0H                                                               
         GET   MKTSAV,(R5)                                                      
         CLI   0(R5),C'*'          SKIP COMMENTS                                
         BE    BMK4                                                             
         LA    RF,MSRNUM                                                        
         PACK  DUB,0(3,RF)                                                      
         CVB   R0,DUB                                                           
         STCM  R0,3,MKNUM                                                       
         MVC   MKNAM,MSRNAM                                                     
         MVC   MKTZ,MSRTZ                                                       
         MVC   MKCLS,MSRCLS                                                     
         MVC   MKREG,MSRREG                                                     
         MVC   MKABBR,MSRABBR                                                   
*                                                                               
         GOTO1 BINSRCH,MKTPARS,(1,X)                                            
         OC    1(3,R1),1(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     BMK4                                                             
*                                                                               
BMEOF    DS    0H                                                               
         L     R6,AMKTTAB                                                       
         LA    RF,MKLEN            SET EOL                                      
         MH    RF,MKTCNT+2                                                      
         AR    RF,R6                                                            
         MVI   0(RF),X'FF'                                                      
*                                                                               
         CLOSE MKTSAV                                                           
         B     EXIT                                                             
         SPACE 2                                                                
MSRECD   DSECT                                                                  
MSRNAM   DS    CL30                                                             
MSRNUM   DS    CL3                                                              
MSRTZ    DS    CL1                                                              
MSRCLS   DS    CL1                                                              
MSRREG   DS    CL2                                                              
MSRABBR  DS    CL8                                                              
         DS    CL32                SPARE                                        
         SPACE 2                                                                
SPP202   CSECT                                                                  
MKTSAV   DCB   DDNAME=MKTSAV,                                          X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               MACRF=GM,                                               X        
               EODAD=BMEOF                                                      
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         DS    0D                                                               
ADRIVER  DS    A                                                                
ASYSDRV  DS    A                                                                
ASPDRWKC DS    A                                                                
MKTPARS  DS    6F                                                               
AMKTTAB  EQU   MKTPARS+4                                                        
MKTCNT   EQU   MKTPARS+8                                                        
*                                                                               
DPGFILE  DC    CL8'SPP205  '                                                    
DRIVER   DC    CL8'T00A3A  '                                                    
SPDRIVER DC    CL8'SPDRIVER'                                                    
SRCE     DS    C                                                                
XFF      DC    20X'FF'                                                          
X        DS    XL256                                                            
*                                                                               
SVKEY    DS    CL32                                                             
ELCODE   DS    XL1                                                              
DLHSW    DS    X                                                                
QTRNO    DS    CL1                                                              
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
NDEMS    EQU   (*-DEMLST)/3                                                     
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
DOUTS    DS    CL(NDEMS*4)                                                      
         SPACE 2                                                                
MKTTABD  DSECT                                                                  
MKNUM    DS    XL2                                                              
MKKLEN   EQU   *-MKTTABD                                                        
MKRNK    DS    XL2                                                              
MKNAM    DS    CL30                                                             
MKTZ     DS    CL1                                                              
MKCLS    DS    CL1                                                              
MKREG    DS    CL2                                                              
MKABBR   DS    CL8                                                              
MKUNVS   DS    XL(NDEMS*4)                                                      
MKLEN    EQU   *-MKTTABD                                                        
MKTMAX   EQU   250                                                              
         SPACE 3                                                                
*                                                                               
SPP202   CSECT                                                                  
         DS    0D                                                               
MKTTAB   DS    0X                                                               
         ORG   *+MKTMAX*MKLEN                                                   
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
**PAN#1  DC    CL21'006SPREPP202 05/01/02'                                      
         END                                                                    
