*          DATA SET SPREPP7S   AT LEVEL 110 AS OF 05/01/02                      
*PHASE SPP702A                                                                  
         TITLE 'SPREPP702 - NETWORK FILE CREATION'                              
         PRINT NOGEN                                                            
*                                                                               
*                                                                               
*        QOPT1   RATING SOURCE- A=ARB,N=NSI                                     
*        QOPT2   DPG PHASE TO USE-  (A THRU D = DOWNLOADING)                    
*        QOPT3   MKT BY MKT DCF'S - A=ALL,L=LIST,N=NO                           
*        QOPT4   Y=UPDATE NPD NUMBER FILE                                       
*        QOPT5   DRIVER TEST VERSION                                            
*        QOPT6   Y=PRINT MKT HH REPORT                                          
*                                                                               
SPP702   CSECT                                                                  
         NMOD1 0,SPP702,RR=R5                                                   
*                                                                               
         L     R9,0(R1)                                                         
         LA    RA,2048(R9)                                                      
         LA    RA,2048(RA)                                                      
         USING SPWORKD,R9,RA                                                    
         L     R8,AGENSUBS                                                      
         USING GENSUBS,R8                                                       
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
AGENSUBS DC    A(GENSUBS)                                                       
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
         MVI   NPGMS,0                                                          
         MVI   DLHSW,0                                                          
         MVI   DLMSW,0                                                          
         MVC   UPDTOPT,QOPT4       SET NPTSAV UPDATE SW                         
         BAS   RE,BLDNPT           BUILD NPT TABLE                              
         CLI   UPDTOPT,C'Y'          TEST UPDATING NPTSAV                       
         BNE   SP101                                                            
         OPEN  (NPTSAVOP,(OUTPUT))                                              
*                                                                               
SP101    DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         CLI   UPDTOPT,C'Y'        IF UPDATING NPTSAV, SKIP DRIVER              
         BE    SP120F                                                           
*                                                                               
         LA    RE,GLOBALD          INIT DRIVER STORAGE TO ZEROS                 
         L     RF,=F'40000'                                                     
         XCEF                                                                   
         MVC   GLSIZE,=F'40000'    GLOBAL SIZE                                  
         MVC   GLOBALD(8),=C'*GLOBAL*'                                          
*                                                                               
         MVI   GLTRACE,C'N'                                                     
         NI    GLDOWNLD,X'3F'                                                   
         MVC   DPGFILE+6(1),QOPT2  SET DPG PHASE                                
         CLI   QOPT2,C' '          TEST DOWNLOADING                             
         BE    SP102                                                            
         CLI   QOPT2,C'D'          A THRU D = DOWNLOAD                          
         BH    SP102                                                            
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
         MVC   DRIVER+6(1),QOPT5   SET TEST OPTION                              
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
         MVI   GLMODE,GLINIT       INITIALIZE DRIVER                            
         GOTO1 ADRIVER,DMCB,(RC)                                                
*                                                                               
SP120F   DS    0H                                                               
         MVC   SRCE,QOPT1          RATING SOURCE                                
         L     RE,ADCLT                                                         
         USING CLTHDR,RE                                                        
         MVI   CPROF+3,C'0'                                                     
         CLI   SRCE,C'A'           A=ARB, N=NSI                                 
         BNE   *+8                                                              
         MVI   CPROF+3,C'1'                                                     
*                                                                               
         BAS   RE,BLDMTAB          BUILD MARKET TABLE                           
         L     R1,MKTCNT           CALC NUMBER OF MKT SETS                      
         LA    R1,9(R1)                                                         
         SR    R0,R0                                                            
         D     R0,=A(MSETCNT)                                                   
         STC   R1,MSETMX           MAX SETS                                     
*                                                                               
         BAS   RE,GETPGMS          GET PROGRAM DATA FROM TAPE                   
*                                  TABLE, AND PASS TO DRIVER                    
*                                                                               
SP138    DS    0H                                                               
         CLI   UPDTOPT,C'Y'        TEST UPDATING NPTSAVE                        
         BE    SP140               YES, SKIP                                    
*                                  DRIVER OUTPUT PHASE                          
         MVI   GLMODE,GLOUTPUT                                                  
         GOTO1 ADRIVER,DMCB,(RC)                                                
         B     SP150                                                            
*                                                                               
SP140    DS    0H                                                               
*                                  UPDATING NPTSAV-FLUSH NPTTAB                 
         MVI   FORCEHED,C'Y'                                                    
         MVC   P(19),=C'PROGRAM FILE UPDATE'                                    
         MVC   P+25(05),=C'BOOK='                                               
         MVC   P+30(4),QBOOK1                                                   
         MVC   P2+25(9),=9C'-'                                                  
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
*                                  SORT TABLE ON PROGRAM NAME                   
         MVC   DMCB(4),ANPTTAB                                                  
         MVC   DMCB+4(4),NPTCNT                                                 
         MVC   DMCB+8(4),NPTPARS+12 LENGTH                                      
         LA    R0,L'NPTNAM                                                      
         ST    R0,DMCB+12                                                       
         LA    R0,NPTNAM-NPTTABD   DISP                                         
         ST    R0,DMCB+16                                                       
         GOTO1 XSORT,DMCB                                                       
*                                                                               
         L     R2,ANPTTAB                                                       
         USING NPTTABD,R2                                                       
*                                                                               
SP142    DS    0H                                                               
         CLI   0(R2),X'FF'         TEST DONE                                    
         BE    SP146                                                            
         MVC   X,SPACES                                                         
         MVC   X(NPTLEN),NPTNUM                                                 
         PUT   NPTSAVOP,X                                                       
*                                                                               
         MVC   P(NPTLEN),X                                                      
         GOTO1 REPORT                                                           
*                                                                               
         LA    R2,NPTLEN(R2)                                                    
         B     SP142                                                            
*                                                                               
SP146    DS    0H                                                               
         CLOSE NPTSAVOP                                                         
         DROP  R2                                                               
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
         CLI   GLHOOK,GLPUTSRT     SORT INPUT TEST                              
         BE    DH450                                                            
         B     EXIT                                                             
         EJECT                                                                  
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
         DC    C'PGMNAM  ',A(PGMNAM)                                            
         DC    C'PGMNUM  ',A(PGMNUM)                                            
         DC    C'PGMDPT  ',A(PGMDPT)                                            
         DC    C'PGMWGT  ',A(PGMWGT)                                            
         DC    C'PGMNET  ',A(PGMNET)                                            
         DC    C'PGMCOD  ',A(PGMCOD)                                            
         DC    C'DEMIP   ',A(DEMIP)                                             
         DC    C'UNVIP   ',A(UNVIP)                                             
         DC    C'DLHTTL  ',A(DLHTTL)                                            
         DC    C'DLHSRC  ',A(DLHSRC)                                            
         DC    C'DLHYR   ',A(DLHYR)                                             
         DC    C'DLHQTR  ',A(DLHQTR)                                            
         DC    C'DLHNDEM ',A(DLHNDEM)                                           
         DC    C'DLHNMKT ',A(DLHNMKT)                                           
         DC    C'DLHNPGM ',A(DLHNPGM)                                           
         DC    C'DEMNAM  ',A(DEMNAM)                                            
         DC    C'MKTSET  ',A(MKTSET)                                            
         DC    C'MKTNAM  ',A(MKTNAM)                                            
         DC    C'MKTNUM  ',A(MKTNUM)                                            
         DC    C'MKTIN   ',A(MKTIN)                                             
         DC    C'DEMIPM  ',A(DEMIPM)                                            
         DC    C'DEMOPM  ',A(DEMOPM)                                            
         DC    C'MKTOROUT',A(MKTOROUT)                                          
         DC    C'MKTORIN ',A(MKTORIN)                                           
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
* EXECUTE INPUT PHASE ROUTINES                                                  
*                                                                               
DH100    DS    0H                                                               
         CLI   GLRECNO,3           FOR REC 3 (DCF'S)                            
         BNE   DH101                                                            
         CLI   DCFPASS,C'Y'        MUST BE DCF PASS                             
         BNE   EXIT                                                             
         B     DH102                                                            
*                                                                               
DH101    DS    0H                                                               
         CLI   DCFPASS,C'Y'        ELSE MUST NOT BE DCF PASS                    
         BE    EXIT                                                             
*                                                                               
DH102    DS    0H                                                               
         L     RF,GLAROUT                                                       
         L     R3,GLAIFLD                                                       
         BASR  RE,RF                                                            
         DC    H'0'                                                             
         SPACE 2                                                                
*                                                                               
* EXECUTE OUTPUT PHASE ROUTINES                                                 
*                                                                               
DH200    L     RF,GLAROUT                                                       
         L     R2,GLAIFLD                                                       
         L     R3,GLAOFLD                                                       
         BASR  RE,RF                                                            
         DC    H'0'                                                             
         SPACE 2                                                                
*                                                                               
* INPUT ROUTINES                                                                
*                                                                               
         USING PGMTABD,R6                                                       
         USING MKTTABD,R4                                                       
*                                                                               
PGMNAM   DS    0H                                                               
         MVC   0(25,R3),SPACES                                                  
         TM    GLDOWNLD,X'80'      IF DOWNLOADING                               
         BZ    PGMNAM3                                                          
         OC    MKNUM,MKNUM         DO PGM NAME ONLY                             
         BNZ   EXIT                FOR US TOTALS                                
*                                                                               
PGMNAM3  DS    0H                                                               
         CLI   DRPASS,1                                                         
         BNE   PGMNAM4                                                          
         MVC   0(25,R3),PGNAM                                                   
         B     EXIT                                                             
*                                                                               
PGMNAM4  DS    0H                                                               
         CLI   DRPASS,2                                                         
         BNE   PGMNAM6                                                          
         MVC   0(4,R3),PGNET                                                    
         MVI   4(R3),C'/'                                                       
         MVC   5(3,R3),PGDPT                                                    
         MVC   9(4,R3),=C'AVG.'                                                 
         B     EXIT                                                             
*                                                                               
PGMNAM6  DS    0H                                                               
         CLI   DRPASS,3                                                         
         BNE   EXIT                                                             
         MVC   0(8,R3),=C'ALL NETS'                                             
         MVI   8(R3),C'/'                                                       
         MVC   9(3,R3),PGDPT                                                    
         MVC   13(4,R3),=C'AVG.'                                                
         B     EXIT                                                             
*                                                                               
PGMNUM   DS    0H                                                               
         MVC   0(4,R3),=4C'0'                                                   
         TM    GLDOWNLD,X'80'      IF DOWNLOADING                               
         BZ    PGMNUM3                                                          
         OC    MKNUM,MKNUM         DO PGM NUMBER ONLY                           
         BNZ   EXIT                FOR US TOTALS                                
*                                                                               
PGMNUM3  DS    0H                                                               
         MVC   0(4,R3),PGNUM                                                    
         CLI   DRPASS,1            UNLESS PASS 1                                
         BE    *+10                                                             
         MVC   0(4,R3),=C'9999'     SET 'ALL' PGMS                              
         B     EXIT                                                             
*                                                                               
PGMDPT   DS    0H                                                               
         MVC   0(3,R3),PGDPT                                                    
         B     EXIT                                                             
*                                                                               
PGMNET   DS    0H                                                               
         MVC   0(4,R3),PGNET                                                    
         CLI   DRPASS,3            FOR PASS 3                                   
         BL    *+10                                                             
         MVC   0(4,R3),=C'9999'    'ALL' NETS                                   
         B     EXIT                                                             
*                                                                               
PGMCOD   DS    0H                                                               
         MVC   0(8,R3),PGCODE                                                   
         CLI   DRPASS,1                                                         
         BE    EXIT                                                             
         MVC   0(8,R3),=C'99999999'                                             
         B     EXIT                                                             
*                                                                               
PGMWGT   DS    0H                                                               
         MVC   0(4,R3),PGWGT                                                    
         B     EXIT                                                             
*                                                                               
DEMIP    DS    0H                  DEMO INPUT - DCFPASS ONLY                    
         ZIC   RF,GLARGS           ARG IS DEM NUM                               
         SLL   RF,2                X 4                                          
         L     R1,MKDEMS-4(RF)     FIRST DEMO (-4) + DISPL                      
         M     R0,PGWGT            X WEIGHT FACTOR                              
         LA    RF,100              UNITS TO HUNDREDS                            
         BAS   RE,DIV                                                           
         ST    R1,0(R3)                                                         
         B     EXIT                                                             
*                                                                               
UNVIP    DS    0H                  UNIV INPUT - DCFPASS ONLY                    
         ZIC   RF,GLARGS           ARG IS DEM NUM                               
         SLL   RF,2                X 4                                          
         L     R1,MKUNVS-4(RF)     FIRST DEMO (-4) + DISPL                      
         M     R0,PGWGT            X WEIGHT FACTOR                              
         LA    RF,100              UNITS TO HUNDREDS                            
         BAS   RE,DIV                                                           
         ST    R1,0(R3)                                                         
         B     EXIT                                                             
*                                                                               
DLHTTL   DS    0H                  DOWNLOAD HEADER                              
         MVC   0(6,R3),=C'NETPGM'                                               
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
         MVC   0(2,R3),QBOOK1      YEAR                                         
         B     EXIT                                                             
*                                                                               
DLHQTR   DS    0H                                                               
         MVI   0(R3),C'1'          FIRST QTR                                    
         CLC   QBOOK1+2(2),=C'02'  FEB                                          
         BE    EXIT                                                             
         MVI   0(R3),C'2'          2ND QTR                                      
         CLC   QBOOK1+2(2),=C'05'  MAY                                          
         BE    EXIT                                                             
         MVI   0(R3),C'3'         3RD QTR                                       
         CLC   QBOOK1+2(2),=C'07'  JUL                                          
         BE    EXIT                                                             
         MVI   0(R3),C'4'          4TH QTR                                      
         CLC   QBOOK1+2(2),=C'11'  NOV                                          
         BE    EXIT                                                             
         DC    H'0'                                                             
*                                                                               
DLHNDEM  DS    0H                                                               
         MVI   0(R3),NDEMS         NUMBER OF DEMOS                              
         B     EXIT                                                             
*                                                                               
DLHNMKT  DS    0H                                                               
         MVC   0(2,R3),MKTCNT+2    NUMBER OF MARKETS                            
         B     EXIT                                                             
*                                                                               
MKTSET   DS    0H                                                               
         MVC   0(1,R3),MSETNO      MARKET SET NUMBER                            
         B     EXIT                                                             
         SPACE 2                                                                
*                                                                               
MKTNUM   DS    0H                  MKT NUMBER                                   
         MVC   0(2,R3),MKNUM                                                    
         B     EXIT                                                             
         SPACE 2                                                                
*                                                                               
MKTNAM   DS    0H                  MKT NAME                                     
         MVC   0(30,R3),MKNAM                                                   
         B     EXIT                                                             
         SPACE 2                                                                
*                                                                               
MKTIN    DS    0H                  MKT NUMBER FOR MKT LIST                      
         BAS   R7,MSCALC           GET MKT FROM SET AND NUMBER                  
         LTR   R4,R4               TEST REAL MARKET                             
         BZ    EXIT                                                             
         USING MKTTABD,R4                                                       
         MVC   0(2,R3),MKNUM                                                    
         B     EXIT                                                             
         SPACE 2                                                                
*                                                                               
MKTORIN  DS    0H                  SET ORIGINATION                              
         BAS   R7,MSCALC           GET MKT FROM SET AND NUMBER                  
         LTR   R4,R4               TEST REAL MARKET                             
         BZ    EXIT                                                             
         CLI   MKORIG,C'N'         TEST ORIGINATING                             
         BE    EXIT                                                             
         MVI   0(R3),1             YES- SET COUNTER                             
         B     EXIT                                                             
*                                                                               
DEMIPM   DS    0H                  MARKET HH INPUT                              
         BAS   R7,MSCALC           GET MKT FROM SET AND NUMBER                  
         LTR   R4,R4               TEST REAL MARKET                             
         BZ    EXIT                                                             
         ICM   R1,15,MKDEMS           HH'S                                      
         M     R0,PGWGT                                                         
         LA    RF,100              UNITS TO HUNDREDS                            
         BAS   RE,DIV                                                           
         ST    R1,0(R3)                                                         
         B     EXIT                                                             
         SPACE 2                                                                
*                                                                               
MSCALC   DS    0H                  FIND MKT FROM SET AND NUMBER                 
         ZIC   R4,MSETNO           MKT SET NUMBER                               
         BCTR  R4,R0                                                            
         MH    R4,=Y(MSETCNT*MKLEN)  NUMBER PER SET X LENGTH OF ONE             
         A     R4,MKTPARS+4        POINT TO 1ST MKT IN SET                      
*                                                                               
         ZIC   RF,GLARGS           ARG IS MKT NO WITHIN SET                     
         BCTR  RF,R0                                                            
         MH    RF,=Y(MKLEN)                                                     
         AR    R4,RF               THIS MARKET                                  
*                                                                               
         C     R4,AMKTTABX         TEST PAST LAST MKT                           
         BL    *+12                                                             
         XC    0(4,R3),0(R3)       YES- ZERO VALUE                              
         SR    R4,R4               SET NO MKT                                   
*                                                                               
         BR    R7                                                               
*                                                                               
* OUTPUT ROUTINES                                                               
*                                                                               
DEMNAM   DS    0H                                                               
         ZIC   RF,GLARGS           DEM NUMBER                                   
         MH    RF,=H'5'                                                         
         LA    RF,DNAMES-5(RF)                                                  
         MVC   0(5,R3),0(RF)                                                    
         B     EXIT                                                             
         SPACE 2                                                                
*                                                                               
MKTOROUT DS    0H                  CHECK ORIGINATION SWITCH                     
         MVC   ORIGSW,0(R2)                                                     
         B     EXIT                                                             
         SPACE 2                                                                
*                                                                               
DEMOPM   DS    0H                  MKT HH OUTPUT                                
         MVI   GLHOOK,GLEDIT       LET DRIVER EDIT                              
         CLI   ORIGSW,0            IF ANY ORIGINATION                           
         BNE   EXIT                OK                                           
         CP    0(8,R2),=P'0'                                                    
         BNH   EXIT                                                             
         OI    7(R2),X'01'         ELSE COMPLIMENT NON-ZERO VALUE               
         B     EXIT                                                             
*                                                                               
DLHNPGM  DS    0H                  NUMBER PROGRAMS                              
         MVI   GLHOOK,GLEDIT                                                    
         MVC   0(2,R3),NPGMS                                                    
         B     EXIT                                                             
*                                                                               
* RESOLVE LITERALS                                                              
*                                                                               
DH300    DS    0H                                                               
         DC    H'0'                                                             
*                                                                               
         SPACE 2                                                                
* HEADLINES                                                                     
*                                                                               
DH400    DS    0H                                                               
         MVC   HEAD3(11),=C'*** NPD ***'                                        
*                                                                               
         MVC   HEAD6(5),=C'BOOK='                                               
         MVC   HEAD6+7(4),QBOOK1                                                
*                                                                               
         B     EXIT                                                             
*                                                                               
         SPACE 2                                                                
* SORT INPUT TEST                                                               
*                                                                               
DH450    DS    0H                                                               
         CLI   GLRECNO,3           RECORD 3                                     
         BNE   DH451                                                            
         CLI   DCFPASS,C'Y'        ONLY FOR DCF PASS                            
         BNE   DH459X                                                           
         B     DH451D                                                           
*                                                                               
DH451    DS    0H                                                               
         CLI   DCFPASS,C'Y'        OTHERS ONLY IF NOT DCF PASS                  
         BE    DH459X                                                           
*                                                                               
DH451D   DS    0H                                                               
         TM    GLDOWNLD,X'80'      IF DOWNLOADING                               
         BZ    DH454                                                            
         CLI   GLRECNO,1           FOR RECORD 1                                 
         BNE   DH452                                                            
         CLI   DLHSW,0             TEST HAVE DONE HEADS                         
         BNE   DH459X              YES - DONT DO AGAIN                          
         MVI   DLHSW,1                                                          
         B     EXIT                                                             
*                                                                               
DH452    DS    0H                                                               
         CLI   GLRECNO,2           FOR REC 2 (MKT LIST)                         
         BNE   DH454                                                            
         CLI   DLMSW,0             TEST HAVE ALREADY DONE                       
         BNE   DH459X              YES - NOT AGAIN                              
         CLC   MSETNO,MSETMX       ON LAST SET                                  
         BNE   EXIT                                                             
         MVI   DLMSW,1             SET NOT TO DO AGAIN                          
         B     EXIT                                                             
*                                                                               
DH454    DS    0H                                                               
DH456    DS    0H                                                               
         B     EXIT                                                             
*                                                                               
DH459X   DS    0H                                                               
         MVI   GLHOOK,GLDONT                                                    
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
*        BUILD PROGRAM TABLE                                                    
         SPACE 2                                                                
BLDNPT   NTR1                                                                   
*                                  SET BINSRCH PARS                             
         SR    R0,R0                                                            
         L     R1,=A(NPTTAB)                                                    
         SR    R2,R2                                                            
         LA    R3,NPTLEN                                                        
         LA    R4,NPTKLEN                                                       
         LH    R5,=Y(NPTMAX)                                                    
         STM   R0,R5,NPTPARS                                                    
*                                                                               
         OPEN  (NPTSAVIP,(INPUT))                                               
*                                                                               
         LA    R6,X                                                             
         XC    X,X                                                              
         USING NPTTABD,R6                                                       
         L     R5,ADBUY                                                         
         USING NPSRECD,R5           NPT SAVE RECORD                             
*                                                                               
BNT4     DS    0H                                                               
         GET   NPTSAVIP,(R5)                                                    
         MVC   NPTNUM(NPTLEN),NPSNUM                                            
         MVC   NPTCHGS,SPACES      CLEAR CHANGE CONTROLS                        
*                                                                               
         GOTO1 BINSRCH,NPTPARS,(1,X)                                            
         OC    1(3,R1),1(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         B     BNT4                GET NEXT                                     
*                                                                               
NPSEOF   DS    0H                                                               
         L     R6,ANPTTAB                                                       
         LA    RF,NPTLEN            SET EOL                                     
         MH    RF,NPTCNT+2                                                      
         AR    RF,R6                                                            
         MVI   0(RF),X'FF'                                                      
*                                                                               
         CLOSE NPTSAVIP                                                         
*                                                                               
BNTX     B     EXIT                                                             
*                                                                               
         SPACE 2                                                                
NPSRECD  DSECT                                                                  
NPSNUM   DS    CL4                                                              
NPSDPT   DS    CL3                                                              
NPSDPTF  DS    CL1                                                              
NPSNET   DS    CL4                                                              
NPSPGMCD DS    CL8                                                              
NPSNAM   DS    CL25                                                             
NPSCHGS  DS    CL5                                                              
NPSBOOK  DS    CL4                                                              
         DS    CL26                SPARE                                        
*                                                                               
SPP702   CSECT                                                                  
MKTSAV   DCB   DDNAME=MKTSAV,                                          X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               MACRF=GM,                                               X        
               EODAD=BMEOF                                                      
         SPACE 2                                                                
NPTSAVIP DCB   DDNAME=NPTSVIP,                                         X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=80,                                               X        
               BLKSIZE=3200,                                           X        
               MACRF=GM,                                               X        
               EODAD=NPSEOF                                                     
         SPACE 2                                                                
NPTSAVOP DCB   DDNAME=NPTSVOP,                                         X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=80,                                               X        
               BLKSIZE=3200,                                           X        
               MACRF=PM                                                         
         SPACE 2                                                                
NPMFIL   DCB   DDNAME=NPMFIL,                                          X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               MACRF=GM,                                               X        
               EODAD=NPMEOF                                                     
         EJECT                                                                  
*        GETPGMS - GET PROGRAM DATA FROM TAPE,                                  
*                  TABLE, AND PASS TO DRIVER                                    
         SPACE 2                                                                
GETPGMS  NTR1                                                                   
         OPEN  (NPMFIL,(INPUT))                                                 
         XC    LASTPGM,LASTPGM                                                  
*                                                                               
GP4      DS    0H                                                               
         GET   NPMFIL,NPMREC                                                    
         CLC   NPMPGMNO,=C'9999'   EOF FILLER                                   
         BE    GP4                                                              
         CLI   NPMRTYP,C'1'        USE ONLY REC 1'S                             
         BE    GP6                                                              
         CLI   NPMRTYP,C'3'        AND 3'S                                      
         BNE   GP4                                                              
*                                                                               
GP6      DS    0H                                                               
         CLC   NPMPGMNO,LASTPGM     TEST PROGRAM BREAK                          
         BE    GP40                                                             
*                                                                               
         OC    LASTPGM,LASTPGM     TEST FIRST TIME                              
         BZ    GP20                YES - DONT FINISH PREV PGM                   
*                                                                               
         CLC   PGMWRK,SPACES       TEST HAVE SET PGM DATA                       
         BNE   *+6                                                              
         DC    H'0'                NO                                           
*                                                                               
         BAS   RE,PUTPGM          PASS PGM DATA TO DRIVER                       
         BAS   RE,CLRDEMS          CLEAR DEMOS FROM MKTTAB                      
*                                                                               
GP20     DS    0H                  START A NEW PROGRAM                          
         CLI   NPMPGMNO,X'FF'        TEST EOF                                   
         BE    GPX                                                              
         MVC   PGMWRK,SPACES                                                    
         MVC   LASTPGM,NPMPGMNO                                                 
*                                                                               
GP40     DS    0H                  CONTINUE SAME PROGRAM                        
         CLI   NPMRTYP,C'3'        FOR US TOTAL REC                             
         BNE   GP40F                                                            
         BAS   RE,SETUSDMS         JUST SET US TOTALS                           
         B     GP4                 AND GET NEXT REC                             
*                                                                               
GP40F    DS    0H                                                               
         CLC   PGMWRK,SPACES       TEST HAVE SET PGM DATA                       
         BNE   GP41                YES                                          
         CLI   NPMTZ,C'E'          DO FOR FIRST EASTERN MKT                     
         BNE   GP41                                                             
         CLI   NPMSTATN,C'*'      MUST BE ORIG MARKET                           
         BE    GP41                                                             
         BAS   RE,SVPGM            SET PROGRAM DATA                             
*                                                                               
GP41     DS    0H                                                               
         PACK  DUB,NPMMKT                                                       
         CVB   R0,DUB                                                           
         SH    R0,=H'400'          CONVERT TO NORMAL DMAS                       
*                                                                               
         CH    R0,=H'124'          CHANGE 124 (ATLANTA NSI)                     
         BNE   *+8                                                              
         LH    R0,=H'168'          TO 168 (DMA/SMA CONFUSION?)                  
*                                                                               
         XC    WORK,WORK                                                        
         LA    R6,WORK                                                          
         USING MKTTABD,R6                                                       
         STH   R0,MKNUM                                                         
*                                                                               
         GOTO1 BINSRCH,MKTPARS,WORK                                             
         L     R6,0(R1)                                                         
         CLI   0(R1),1             TEST FOUND                                   
         BNE   GP42                                                             
*                                                                               
         MVC   P(26),=C'** MARKET NNN NOT FOUND **'                             
         MVC   P+10(3),NPMMKT                                                   
         MVC   P+27(26),NPMMNAM    MKT NAME                                     
         GOTO1 REPORT                                                           
         B     GP4                 NEXT RECORD                                  
*                                                                               
GP42     DS    0H                                                               
         MVI   SDEMSW,C'A'         FIRST DO AUDIENCES                           
         BAS   RE,SETDEMS          SET DEMOS IN MARKET TABLE                    
         MVI   SDEMSW,C'U'         THEN UNIVERSES                               
         BAS   RE,SETDEMS          SET DEMOS IN MARKET TABLE                    
         B     GP4                 GET NEXT TAPE RECORD                         
*                                                                               
NPMEOF   DS    0H                  END OF TAPE                                  
         CLOSE NPMFIL                                                           
         MVI   NPMPGMNO,X'FF'        SET EOF                                    
         B     GP6                                                              
*                                                                               
GPX      DS    0H                                                               
         XIT1                                                                   
         SPACE 2                                                                
CLRDEMS  DS    0H                  CLEAR DEMOS FROM MKT TABLE                   
         L     RF,MKTPARS+4                                                     
         L     R0,MKTCNT                                                        
*                                                                               
CLD4     DS    0H                                                               
         LA    R1,MKDEMS-MKTTABD(RF)   AUDS                                     
         XC    0(L'MKDEMS,R1),0(R1)                                             
         LA    R1,MKUNVS-MKTTABD(RF)   AND UNIVS                                
         XC    0(L'MKUNVS,R1),0(R1)                                             
         MVI   MKORIG-MKNUM(RF),C'Y'   ALSO CLEAR ORIG INDICATOR                
         LA    RF,MKLEN(RF)                                                     
         BCT   R0,CLD4                                                          
*                                                                               
         BR    RE                                                               
         SPACE 2                                                                
SETDEMS  NTR1                       SET DEMOS IN MKT TAB                        
         MVI   MKORIG,C'Y'         SET ORIGINATING MKT SWITCH                   
         CLI   NPMSTATN,C'*'       STATION **** = SPILL                         
         BNE   *+8                                                              
         MVI   MKORIG,C'N'                                                      
*                                                                               
         XC    X,X                 CONVERT DEMOS ON TAPE TO OUR                 
         LA    R3,X                BASE DEMOS                                   
         LA    R4,DFORMS           DEMO FORMULA LIST                            
*                                                                               
SDM3     DS    0H                                                               
         SR    R2,R2               CLEAR CUME REG                               
*                                                                               
SDM4     DS    0H                                                               
         CLI   0(R4),C'='          END OF A FORMULA                             
         BE    SDM5                                                             
*                                                                               
         PACK  DUB,1(2,R4)         DEMO NUMBER                                  
         CVB   RE,DUB                                                           
         MH    RE,=Y(NPMDEML)                                                   
         LA    R0,NPMDEMS-NPMDEML(RE)    AUDIENCES                              
         CLI   SDEMSW,C'A'                                                      
         BE    *+8                                                              
         LA    R0,NPMUNIVS-NPMDEML(RE)    OR UNIVERSES                          
         LR    RE,R0                                                            
*                                                                               
         PACK  DUB,0(NPMDEML,RE)                                                
         CVB   R0,DUB                                                           
         CLI   0(R4),C'+'          + IS ADD                                     
         BE    *+6                 ELSE SUBTRACT                                
         LCR   R0,R0                                                            
         AR    R2,R0                                                            
*                                                                               
         LA    R4,3(R4)            NEXT FORMULA ELEM                            
         B     SDM4                                                             
*                                                                               
SDM5     DS    0H                  END OF A FORMULA                             
         ST    R2,0(R3)            SET RESULT VALUE                             
         CLI   1(R4),X'FF'         TEST EOL                                     
         BE    SDM5C                                                            
         LA    R3,4(R3)            NEXT DEMO                                    
         LA    R4,1(R4)            BUMP PAST =                                  
         B     SDM3                                                             
*                                                                               
SDM5C    DS    0H                                                               
         LA    R7,MKDEMS           SET MARKET DEMOS                             
         CLI   SDEMSW,C'A'                                                      
         BE    *+8                                                              
         LA    R7,MKUNVS           OR UNIVERSES                                 
         LA    R5,X                                                             
         LA    R3,NDEMS                                                         
*                                                                               
SDM6     DS    0H                                                               
         L     R0,0(R7)                                                         
         A     R0,0(R5)                                                         
         ST    R0,0(R7)                                                         
*                                                                               
         LA    R7,4(R7)                                                         
         LA    R5,4(R5)                                                         
         BCT   R3,SDM6                                                          
*                                                                               
         CLI   SDEMSW,C'U'         FOR UNIV PASS                                
         BNE   SDMX                                                             
         L     R6,MKTPARS+4        DO TO US TOTALS                              
         LA    R7,MKUNVS                                                        
         LA    R5,X                                                             
         LA    R3,NDEMS                                                         
*                                                                               
SDM8     DS    0H                                                               
         L     R0,0(R7)                                                         
         A     R0,0(R5)                                                         
         ST    R0,0(R7)                                                         
*                                                                               
         LA    R7,4(R7)                                                         
         LA    R5,4(R5)                                                         
         BCT   R3,SDM8                                                          
*                                                                               
SDMX     DS    0H                                                               
         XIT1                                                                   
         SPACE 2                                                                
SETUSDMS NTR1                       SET US TOTAL AUDS                           
         XC    X,X                 CONVERT DEMOS ON TAPE TO OUR                 
         LA    R3,X                BASE DEMOS                                   
         LA    R4,DFORMS           DEMO FORMULA LIST                            
*                                                                               
         MVC   WORK(9),NPMUDEMS-1  ADJUST FOR MIS-ALIGN OF HH                   
         MVC   NPMUDEMS(9),WORK                                                 
*                                                                               
SUDM3    DS    0H                                                               
         SR    R2,R2               CLEAR CUME REG                               
*                                                                               
SUDM4    DS    0H                                                               
         CLI   0(R4),C'='          END OF A FORMULA                             
         BE    SUDM5                                                            
*                                                                               
         PACK  DUB,1(2,R4)         DEMO NUMBER                                  
         CVB   RE,DUB                                                           
         MH    RE,=Y(NPMUDEML)                                                  
         LA    R0,NPMUDEMS-NPMUDEML(RE)                                         
         LR    RE,R0                                                            
*                                                                               
         PACK  DUB,0(NPMUDEML,RE)                                               
         CVB   R0,DUB                                                           
         CLI   0(R4),C'+'          + IS ADD                                     
         BE    *+6                 ELSE SUBTRACT                                
         LCR   R0,R0                                                            
         AR    R2,R0                                                            
*                                                                               
         LA    R4,3(R4)            NEXT FORMULA ELEM                            
         B     SUDM4                                                            
*                                                                               
SUDM5    DS    0H                  END OF A FORMULA                             
         ST    R2,0(R3)            SET RESULT VALUE                             
         CLI   1(R4),X'FF'         TEST EOL                                     
         BE    SUDM5C                                                           
         LA    R3,4(R3)            NEXT DEMO                                    
         LA    R4,1(R4)            BUMP PAST =                                  
         B     SUDM3                                                            
*                                                                               
SUDM5C   DS    0H                                                               
         L     R6,MKTPARS+4        US TOTAL MKT                                 
         LA    R7,MKDEMS           SET MARKET DEMOS                             
         LA    R5,X                                                             
         LA    R3,NDEMS                                                         
*                                                                               
SUDM6    DS    0H                                                               
         L     R0,0(R7)                                                         
         A     R0,0(R5)                                                         
         ST    R0,0(R7)                                                         
*                                                                               
         LA    R7,4(R7)                                                         
         LA    R5,4(R5)                                                         
         BCT   R3,SUDM6                                                         
*                                                                               
         XIT1                                                                   
         DROP  R6                                                               
         SPACE 2                                                                
PUTPGM   NTR1                                                                   
         CLI   UPDTOPT,C'Y'        SKIP IF UPDATING PGM FILE                    
         BE    PUTPX                                                            
         MVI   DCFPASS,C'N'        NOT THE DCF PASS                             
         LA    R6,PGMWRK                                                        
         MVI   MSETNO,1            START WITH FIRST MKT SET                     
*                                                                               
PUTP4    DS    0H                                                               
         MVI   DRPASS,1            START WITH PASS 1                            
*                                                                               
PUTP6    DS    0H                                                               
         MVI   GLMODE,GLINPUT      SET UP FOR DRIVER INPUT                      
         GOTO1 ADRIVER,DMCB,(RC)                                                
*                                                                               
         CLI   DRPASS,3                                                         
         BE    PUTP8                                                            
         ZIC   R1,DRPASS           BUMP DRIVER PASS                             
         LA    R1,1(R1)                                                         
         STC   R1,DRPASS                                                        
         B     PUTP6                                                            
*                                                                               
PUTP8    DS    0H                                                               
         CLC   MSETNO,MSETMX       TEST DONE WITH MKT SETS                      
         BNL   PUTP10                                                           
         ZIC   RF,MSETNO           BUMP TO NEXT SET                             
         LA    RF,1(RF)                                                         
         STC   RF,MSETNO                                                        
         B     PUTP4                                                            
*                                                                               
PUTP10   DS    0H                                                               
         MVI   DCFPASS,C'Y'        SET DCF PASS                                 
         L     R4,AMKTTAB                                                       
         USING MKTTABD,R4                                                       
*                                                                               
PUTP14   DS    0H                                                               
         CLI   0(R4),X'FF'         END OF MARKETS                               
         BE    PUTP20                                                           
         CLI   QOPT3,C'L'          TEST DOING LIST OF MKTS                      
         BNE   PUTP15                                                           
         LA    RF,DCFMKTS                                                       
         LA    RE,NDCFMKTS                                                      
*                                                                               
PUTP14D  DS    0H                                                               
         CLC   MKNUM,0(RF)                                                      
         BE    PUTP15              MKT IS IN LIST - DO IT                       
         LA    RF,2(RF)                                                         
         BCT   RE,PUTP14D                                                       
         B     PUTP18                                                           
*                                                                               
PUTP15   DS    0H                                                               
         MVI   DRPASS,1            START WITH PASS 1                            
*                                                                               
PUTP16   DS    0H                                                               
         MVI   GLMODE,GLINPUT      SET UP FOR DRIVER INPUT                      
         GOTO1 ADRIVER,DMCB,(RC)                                                
*                                                                               
         CLI   DRPASS,3                                                         
         BE    PUTP18                                                           
         ZIC   R1,DRPASS           BUMP DRIVER PASS                             
         LA    R1,1(R1)                                                         
         STC   R1,DRPASS                                                        
         B     PUTP16                                                           
*                                                                               
PUTP18   DS    0H                                                               
         CLI   QOPT3,C'N'          TEST DOING MKT DCF'S                         
         BE    PUTP20              NO - DONE                                    
         LA    R4,MKLEN(R4)        NEXT MARKET                                  
         B     PUTP14                                                           
*                                                                               
PUTP20   DS    0H                                                               
*                                                                               
PUTPX    DS    0H                                                               
         XIT1                                                                   
         DROP  R4                                                               
         SPACE 2                                                                
NPMREC   DS    0C                                                               
NPMMKT   DS    CL3      1                                                       
NPMMNAM  DS    CL26     4                                                       
NPMTZ    DS    CL1     30                                                       
         DS    CL1     31       SPARE                                           
NPMROUND DS    CL1     32                                                       
         DS    CL5     33       SPARE                                           
NPMPTYP  DS    CL1     38                                                       
NPMCYCLE DS    CL1     39                                                       
NPMYEAR  DS    CL1     40                                                       
NPMPGMNO DS    CL4     41                                                       
NPMPGNAM DS    CL25    45                                                       
         DS    CL20    70       SPARE                                           
NPMSTIM  DS    CL4     90                                                       
NPMETIM  DS    CL4     94                                                       
NPMAMPM  DS    CL2     98                                                       
NPMDAYS  DS    CL9    100                                                       
NPMDAYC  DS    CL1    109                                                       
         DS    CL2    110       SPARE                                           
NPMQHS   DS    CL3    112                                                       
NPMRTYP  DS    CL1    115                                                       
         DS    CL6    116       SPARE                                           
NPMUNIVS DS    18CL7  122       UNIVS                                           
NPMDEMS  DS    18CL7  248       18 DEMOS                                        
NPMDEML  EQU   7                                                                
NPMNDEMS EQU   18                                                               
NPMSTATN DS    CL4    374                                                       
         DS    CL5    378       SPARE                                           
         DS    CL5    383       SPARE                                           
NPMORGHH DS    CL7    388                                                       
         DS    CL33   395       SPARE                                           
*                                                                               
         ORG   NPMREC+178          US AUDS (REALLY AT +177)                     
NPMUDEMS DS    18CL9                                                            
NPMUDEML EQU   9                                                                
         ORG                                                                    
*                                                                               
NPMRECL  EQU   *-NPMREC                                                         
         EJECT                                                                  
*        GENSUBS- GENERAL SUBROUTINES AND DATA AREAS                            
         SPACE 2                                                                
GENSUBS  DS    0D                                                               
DIV      DIV   (R0),(RF)       RETURNS VIA RE                                   
*                                                                               
EXIT     XIT1                                                                   
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
         MVC   MKNAM,=CL30'**U.S. TOTALS**'                                     
         GOTO1 BINSRCH,MKTPARS,(1,X)                                            
*                                                                               
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
         MVI   MKORIG,C'Y'                                                      
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
         ST    RF,AMKTTABX                                                      
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
         SPACE 3                                                                
*                                                                               
*        DEMO LIST                                                              
*                                                                               
SPP702   CSECT                                                                  
         EJECT                                                                  
*        SVPGM- SAVE PROGRAM DATA                                               
         SPACE 2                                                                
SVPGM    NTR1                                                                   
         LA    R6,PGMWRK                                                        
         MVC   PGMWRK,SPACES                                                    
         USING PGMTABD,R6                                                       
*                                                                               
         MVC   PGSTIM,NPMSTIM                                                   
         MVC   PGETIM,NPMETIM                                                   
         OC    PGSTIM,=4C'0'                                                    
         OC    PGETIM,=4C'0'                                                    
*                                                                               
         CLC   NPMAMPM,=C'NN'      NOON - NO CHANGE                             
         BE    SVP17D                                                           
*                                                                               
         PACK  DUB(4),PGSTIM                                                    
         PACK  DUB+4(4),PGETIM                                                  
*                                                                               
         CLC   NPMAMPM,=C'MD'      AM/PM = MIDNIGHT                             
         BNE   SVP14                                                            
         AP    DUB(4),=P'1200'     BUMP BOTH                                    
         AP    DUB+4(4),=P'1200'                                                
         B     SVP17                                                            
*                                                                               
SVP14    DS    0H                                                               
         CLC   NPMAMPM,=C'PM'      AM/PM REFERS TO END TIME                     
         BNE   SVP15                                                            
         CLC   PGETIM(2),=C'12'                                                 
         BE    *+10                                                             
         AP    DUB+4(4),=P'1200'   BUMP END                                     
         CLC   PGSTIM(2),=C'12'                                                 
         BE    SVP17                                                            
         CLC   PGSTIM,PGETIM       IF START TIME NOT LATER THAN END             
         BH    SVP17                                                            
         AP    DUB(4),=P'1200'     BUMP START                                   
         B     SVP17                                                            
*                                                                               
SVP15    DS    0H                  ELSE IS AM                                   
         CLC   PGETIM(2),=C'12'                                                 
         BNE   *+16                                                             
         SP    DUB+4(4),=P'1200'                                                
         MVC   PGETIM(2),=C'00'                                                 
         CLC   PGSTIM(2),=C'12'                                                 
         BNE   *+16                                                             
         SP    DUB(4),=P'1200'                                                  
         MVC   PGSTIM(2),=C'00'                                                 
         CLC   PGSTIM,PGETIM       IF START TIME LATER THAN END                 
         BNH   *+10                                                             
         AP    DUB(4),=P'1200'     START IS PM                                  
*                                                                               
SVP17    DS    0H                 SET NEW TIMES                                 
         OI    DUB+3,X'0F'                                                      
         UNPK  PGSTIM,DUB(4)                                                    
         OI    DUB+7,X'0F'                                                      
         UNPK  PGETIM,DUB+4(4)                                                  
*                                 GET DURATION OF SHOW                          
SVP17D   DS    0H                                                               
         PACK  DUB(4),PGETIM+2(2)                                               
         PACK  DUB+4(4),PGSTIM+2(2)                                             
         SP    DUB(4),DUB+4(4)                                                  
         BNM   *+10                                                             
         AP    DUB(4),=P'60'                                                    
         ZAP   FULL,DUB(4)         SAVE MINUTES PART                            
*                                                                               
         PACK  DUB(4),PGETIM(2)                                                 
         PACK  DUB+4(4),PGSTIM(2)                                               
         CLC   PGSTIM(2),PGETIM    TEST START HOUR VS END                       
         BNH   *+10                IF HIGH TIMES SPAN MIDNIGHT                  
         AP    DUB(4),=P'24'                                                    
         SP    DUB(4),DUB+4(4)                                                  
         CLC   PGETIM+2(2),PGSTIM+2  IF END MINUTE AFTER START                  
         BNL   *+10                                                             
         SP    DUB(4),=P'1'        BORROW ONE HOUR                              
         MP    DUB(4),=P'60'                                                    
         AP    FULL,DUB(4)         ADD HOURS PART TO GET TOTAL MINS             
         ZAP   DUB,FULL            SET IN QTR HRS                               
         CVB   R1,DUB                                                           
         SR    R0,R0                                                            
         D     R0,=F'15'                                                        
         LTR   R1,R1                                                            
         BP    *+8                                                              
         LA    R1,1                MUST BE AT LEAST ONE                         
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PGDUR,DUB                                                        
*                                                                               
         MVC   PGPGMTP,NPMPTYP     PROGRAM TYPE                                 
*                                                                               
         MVC   PGNAM,NPMPGNAM                                                   
         MVC   PGNET,=C'NBC '                                                   
         CLI   NPMPGMNO,C'1'       1000-1999 = NBC                              
         BE    SVP18                                                            
         MVC   PGNET,=C'ABC '                                                   
         CLI   NPMPGMNO,C'2'       2000-2999 = ABC                              
         BE    SVP18                                                            
         MVC   PGNET,=C'CBS '                                                   
         CLI   NPMPGMNO,C'3'       3000-3999 = CBS                              
         BE    SVP18                                                            
         MVC   PGNET,=C'UNK'       UNKNOWN                                      
*                                                                               
SVP18    DS    0H                                                               
         MVC   PGNUM,NPMPGMNO                                                   
*                                                                               
         MVC   PGDAYC,NPMDAYC      DAYS CODE                                    
         MVI   PGDAYB,X'40'        SET TO ONE DAY (NO MATTER WHICH)             
         CLI   PGDAYC,C'9'         TEST VARIOUS                                 
         BNE   *+8                                                              
         MVI   PGDAYB,X'7C'        TO M-F (5 DAY WEIGHT)                        
*                                  CALCULATE WEIGHT=DURATION X DAYS             
         ZIC   R0,PGDAYB           GET NUMBER OF DAYS                           
         SR    R1,R1                                                            
         SR    RF,RF                                                            
*                                                                               
SVP19    DS    0H                                                               
         LTR   R0,R0                                                            
         BZ    SVP19D                                                           
         SRDL  R0,1                                                             
         LTR   R1,R1                                                            
         BNM   *+8                                                              
         LA    RF,1(RF)            BUMP COUNT                                   
         B     SVP19                                                            
*                                                                               
SVP19D   DS    0H                                                               
         PACK  DUB,PGDUR           DURATION                                     
         CVB   R0,DUB                                                           
         MR    RE,R0                                                            
         ST    RF,PGWGT            SET IN WEIGHT FIELD                          
*                                                                               
         LA    R3,X                                                             
         MVC   X,SPACES                                                         
         USING NPTTABD,R3                                                       
         MVC   NPTNUM,PGNUM                                                     
*                                                                               
         CLI   UPDTOPT,C'Y'       IF UPDATING PGM FILE                          
         BNE   SVP21                                                            
         BAS   RE,SETDPT           GET DAYPART                                  
         MVC   PGCODE(4),PGNUM     SET CODE = NUMBER                            
         B     SVP21D                                                           
*                                                                               
SVP21    DS    0H                  ELSE GET FROM TABLE                          
         GOTO1 BINSRCH,NPTPARS,NPTNUM                                           
         L     R3,0(R1)                                                         
         CLI   0(R1),1             TEST FOUND                                   
         BNE   SVP21C                                                           
*                                  **NO-OP ABEND ON PGM NOT FOUND               
         MVC   PGDPT,=C'XXX'                                                    
         MVC   PGCODE(4),PGNUM                                                  
         B     SVP21D                                                           
*                                                                               
SVP21C   DS    0H                                                               
         MVC   PGDPT,NPTDPT        SET DAYPART                                  
         MVC   PGCODE,NPTPGMCD     AND CODE                                     
*                                                                               
SVP21D   DS    0H                                                               
*                                                                               
         CLI   UPDTOPT,C'Y'        IF UPDATING, TRACE PGM DATA                  
         BNE   SVP50                                                            
*                                                                               
         MVC   P(4),PGNET                                                       
         MVC   P+5(3),PGDPT                                                     
         MVC   P+9(4),PGNUM                                                     
         MVC   P+14(8),PGCODE                                                   
         MVC   P+23(25),PGNAM                                                   
*                                                                               
         MVC   P+49(1),PGDAYC                                                   
         MVC   P+51(1),PGPGMTP                                                  
*                                                                               
         MVC   P+53(4),PGSTIM                                                   
         MVC   P+58(4),PGETIM                                                   
         MVC   P+63(3),PGDUR                                                    
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         LA    R3,X                                                             
         MVC   X,SPACES                                                         
         USING NPTTABD,R3                                                       
         MVC   NPTNUM,PGNUM                                                     
*                                                                               
         GOTO1 BINSRCH,NPTPARS,(1,NPTNUM)                                       
         L     R3,0(R1)                                                         
         CLI   0(R1),1             TEST NEW                                     
         BE    SVP42                                                            
*                                  NOT NEW, TEST CHANGES                        
         CLC   NPTNAM,PGNAM                                                     
         BE    *+8                                                              
         MVI   NPTCHGS,C'N'        TITLE                                        
         CLC   NPTNET,PGNET                                                     
         BE    *+8                                                              
         MVI   NPTCHGS+1,C'S'      NETWORK                                      
         CLC   NPTDPT,PGDPT                                                     
         BE    *+8                                                              
         MVI   NPTCHGS+2,C'D'      DAYPART                                      
*                                                                               
         MVC   NPTNAM,PGNAM                                                     
         CLI   NPTDPTF,C'*'        TEST DAYPART 'FROZEN'                        
         BE    *+10                YES- CANT RESET                              
         MVC   NPTDPT,PGDPT                                                     
         MVC   NPTNET,PGNET                                                     
         MVC   NPTBOOK,QBOOK1      SET LATEST BOOK                              
         B     SVP44                                                            
*                                                                               
SVP42    DS    0H                  NEW PROGRAM                                  
         LA    R3,0(R3)                                                         
         LTR   R3,R3                                                            
         BNZ   *+6                                                              
         DC    H'0'                TOO MANY PROGRAMS                            
*                                                                               
         MVC   NPTNAM,PGNAM                                                     
         MVC   NPTDPT,PGDPT                                                     
         MVC   NPTNET,PGNET                                                     
         MVC   NPTBOOK,QBOOK1      SET LATEST BOOK                              
         MVC   NPTPGMCD(4),NPTNUM                                               
         MVC   NPTCHGS(5),=C'(NEW)'                                             
*                                                                               
         L     R6,ANPTTAB                                                       
         LA    RF,NPTLEN            SET EOL                                     
         MH    RF,NPTCNT+2                                                      
         AR    RF,R6                                                            
         MVI   0(RF),X'FF'                                                      
*                                                                               
SVP44    DS    0H                                                               
SVP50    DS    0H                                                               
SVPX     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*        SETDPT SET DAYPART CODE                                                
*        -ROUTINE MAKES A TENTATIVE ASSIGNMENT BASED                            
*         ON PGM TYPE OR TIME OF DAY. THE DAYPART GOES                          
*         ON THE SAVED PROGRAM FILE, WHERE IT CAN BE                            
*         CHANGED.                                                              
*                                                                               
*        NB- NO PGMS ASSIGNED TO LATE NEWS.                                     
*                                                                               
         SPACE 2                                                                
SETDPT   NTR1                                                                   
         MVC   PGDPT,=C'XXX'                                                    
*                                                                               
         CLI   PGPGMTP,C'3'         CHILD PROGS                                 
         BNE   *+14                                                             
         MVC   PGDPT,=C'CH '       = CHILD                                      
         B     SETD50                                                           
*                                                                               
         CLI   PGPGMTP,C'4'         SPORTS                                      
         BNE   *+14                                                             
         MVC   PGDPT,=C'SP '       = SPORTS                                     
         B     SETD50                                                           
*                                  NOT KID,SPORTS,                              
*                                  BASE ON TIME ALONE                           
*                                                                               
         CLC   PGSTIM,=C'0555'     555A                                         
         BL    SETD10                                                           
         CLC   PGSTIM,=C'0955'     TO 955A                                      
         BH    SETD10                                                           
         MVC   PGDPT,=C'EM '       = EARLY MORNING                              
         B     SETD50                                                           
*                                                                               
SETD10   DS    0H                                                               
         CLC   PGSTIM,=C'0955'     955A                                         
         BL    SETD12                                                           
         CLC   PGSTIM,=C'1555'     TO 355P                                      
         BH    SETD12                                                           
         MVC   PGDPT,=C'DA '       = DAY                                        
         B     SETD50                                                           
*                                                                               
SETD12   DS    0H                                                               
         CLC   PGSTIM,=C'1555'     355P                                         
         BL    SETD13                                                           
         CLC   PGSTIM,=C'1855'     TO 655P                                      
         BH    SETD13                                                           
         MVC   PGDPT,=C'EF '       = EARLY FRINGE                               
         B     SETD50                                                           
*                                                                               
SETD13   DS    0H                                                               
         CLC   PGSTIM,=C'1855'     655P                                         
         BL    SETD14                                                           
         CLC   PGSTIM,=C'1925'     TO 725P                                      
         BH    SETD14                                                           
         MVC   PGDPT,=C'EN '       = EARLY NEWS                                 
         B     SETD50                                                           
*                                                                               
SETD14   DS    0H                                                               
         CLC   PGSTIM,=C'1925'     725P                                         
         BL    SETD16                                                           
         CLC   PGSTIM,=C'1955'     TO 755P                                      
         BH    SETD16                                                           
         MVC   PGDPT,=C'PA '       = PRIME ACCESS                               
         B     SETD50                                                           
*                                                                               
SETD16   DS    0H                                                               
         CLC   PGSTIM,=C'1955'     755P                                         
         BL    SETD18                                                           
         CLC   PGSTIM,=C'2255'     TO 1055P                                     
         BH    SETD18                                                           
         MVC   PGDPT,=C'PR '       = PRIME                                      
         B     SETD50                                                           
*                                                                               
SETD18   DS    0H                  ELSE IS LATE FRINGE                          
         MVC   PGDPT,=C'LF '                                                    
*                                                                               
SETD50   DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
DEMLST   DS    0X                                                               
         DC    X'00',C'T',AL1(001)   HOMES                                      
         DC    X'00',C'T',AL1(065)   WW                                         
         DC    X'00',C'T',AL1(045)   W18+                                       
         DC    X'00',C'T',AL1(041)   W1834                                      
         DC    X'00',C'T',AL1(042)   W1849                                      
         DC    X'00',C'T',AL1(047)   W2549                                      
         DC    X'00',C'T',AL1(048)   W2554                                      
         DC    X'00',C'T',AL1(049)   W2564                                      
         DC    X'00',C'T',AL1(028)   W1224                                      
         DC    X'00',C'T',AL1(029)   W1234                                      
         DC    X'00',C'T',AL1(095)   M18+                                       
         DC    X'00',C'T',AL1(091)   M1834                                      
         DC    X'00',C'T',AL1(092)   M1849                                      
         DC    X'00',C'T',AL1(097)   M2549                                      
         DC    X'00',C'T',AL1(098)   M2554                                      
         DC    X'00',C'T',AL1(099)   M2564                                      
         DC    X'00',C'T',AL1(127)   V02+                                       
         DC    X'00',C'T',AL1(122)   V0211                                      
         DC    X'00',C'T',AL1(123)   V0611                                      
         DC    X'00',C'T',AL1(125)   V1217                                      
         DC    X'00',C'T',AL1(128)   V1224                                      
         DC    X'00',C'T',AL1(129)   V1234                                      
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
         SPACE 3                                                                
*        DEMO FORMULAS                                                          
*                                                                               
DFORMS   DS    0X                                                               
         DC    C'+01='                          HOMES                           
         DC    C'+09='                          WWORK                           
         DC    C'+03+04+05+06+07+08='           W18+                            
         DC    C'+03+04='                       W1834                           
         DC    C'+03+04+05='                    W1849                           
         DC    C'+04+05='                       W2549                           
         DC    C'+04+05+06='                    W2554                           
         DC    C'+04+05+06+07='                 W2564                           
         DC    C'+02+03='                       W1224                           
         DC    C'+02+03+04='                    W1234                           
         DC    C'+11+12+13+14+15+16='           M18+                            
         DC    C'+11+12='                       M1834                           
         DC    C'+11+12+13='                    M1849                           
         DC    C'+12+13='                       M2549                           
         DC    C'+12+13+14='                    M2554                           
         DC    C'+12+13+14+15='                 M2564                           
         DC    C'+17+18+02+03+04+05+06+07+08'                                   
         DC    C'+10+11+12+13+14+15+16='        V02+                            
         DC    C'+17+18='                       V0211                           
         DC    C'+18='                          V0611                           
         DC    C'+02+10='                       V1217                           
         DC    C'+02+03+10+11='                 V1224                           
         DC    C'+02+03+04+10+11+12='           V1234                           
         DC    3X'FF'                                                           
         SPACE 3                                                                
*        MARKET LIST - DO DCF'S FOR THESE MKTS                                  
         SPACE 3                                                                
DCFMKTS  DS    0C                                                               
         DC    AL2(000)            US TOTALS                                    
         DC    AL2(101)            NEW YORK                                     
         DC    AL2(104)            PHILADELPHIA                                 
         DC    AL2(105)            DETROIT                                      
         DC    AL2(106)            BOSTON                                       
         DC    AL2(108)            PITTSBURGH                                   
         DC    AL2(110)            CLEVELAND                                    
         DC    AL2(111)            WASHINGTON                                   
         DC    AL2(112)            BALTIMORE                                    
         DC    AL2(115)            CINCINNATTI                                  
         DC    AL2(121)            PROVIDENCE                                   
         DC    AL2(128)            MIAMI                                        
         DC    AL2(168)            ATLANTA                                      
         DC    AL2(202)            CHICAGO                                      
         DC    AL2(223)            DALLAS                                       
         DC    AL2(403)            LA                                           
         DC    AL2(407)            SAN FRANCISCO                                
*                                                                               
NDCFMKTS EQU  (*-DCFMKTS)/2                                                     
         DC    X'FFFF'                                                          
         SPACE 3                                                                
RELO     DC    F'0'                                                             
W        DS    XL200                                                            
X        DS    XL200                                                            
PGMWRK   DS    XL200                                                            
SRCE     DS    C                                                                
UPDTOPT  DS    C                                                                
TYPRET   DS    C                                                                
DRPASS   DS    X                                                                
DCFPASS  DS    C                                                                
SDEMSW   DS    C                                                                
NPGMS    DS    H                                                                
         DS    0D                                                               
ADRIVER  DS    A                                                                
ASYSDRV  DS    A                                                                
ASPDRWKC DS    A                                                                
MKTPARS  DS    6F                                                               
AMKTTAB  EQU   MKTPARS+4                                                        
MKTCNT   EQU   MKTPARS+8                                                        
NPTPARS  DS    6F                                                               
ANPTTAB  EQU   NPTPARS+4                                                        
NPTCNT   EQU   NPTPARS+8                                                        
AMKTTABX DS    A                                                                
*                                                                               
DPGFILE  DC    CL8'SPP705  '                                                    
DRIVER   DC    CL8'T00A3A  '                                                    
SPDRIVER DC    CL8'SPDRIVER'                                                    
XFF      DC    20X'FF'                                                          
*                                                                               
SVKEY    DS    CL32                                                             
ELCODE   DS    XL1                                                              
ORIGSW   DS    CL1                                                              
DLHSW    DS    XL1                                                              
DLMSW    DS    XL1                                                              
MSETNO   DS    XL1                 DL MKT SET NUMBER                            
MSETMX   DS    XL1                 MKT SET MAX                                  
MSETCNT  EQU   10                  MARKETS PER DL RECORD SET                    
*                                                                               
LASTPGM  DS    CL4                                                              
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
NPTTABD  DSECT                DSECT FOR MASTER PROGRAM TABLE                    
NPTNUM   DS    CL4                                                              
NPTKLEN  EQU   *-NPTTABD                                                        
NPTDPT   DS    CL3                                                              
NPTDPTF  DS    CL1                                                              
NPTNET   DS    CL4                                                              
NPTPGMCD DS    CL8                                                              
NPTNAM   DS    CL25                                                             
NPTCHGS  DS    CL5                                                              
NPTBOOK  DS    CL4                                                              
NPTLEN   EQU   *-NPTTABD                                                        
NPTMAX   EQU   1600                                                             
         SPACE 2                                                                
PGMTABD  DSECT                DSECT FOR PROGRAM DATA                            
PGNAM    DS    CL25                                                             
PGNUM    DS    CL4                                                              
PGMKLEN  EQU   *-PGMTABD                                                        
PGDPT    DS    CL3                                                              
PGNET    DS    CL4                                                              
PGDAYC   DS    CL1                                                              
PGDAYB   DS    XL1                                                              
PGSTIM   DS    CL4                                                              
PGETIM   DS    CL4                                                              
PGWGT    DS    F                                                                
PGDUR    DS    CL3                                                              
PGPGMTP  DS    CL2                                                              
PGCODE   DS    CL8                                                              
PGMLEN   EQU   *-PGMTABD                                                        
*                                                                               
         SPACE 3                                                                
MKTTABD  DSECT                DSECT FOR MARKET DATA                             
MKNUM    DS    XL2                                                              
MKKLEN   EQU   *-MKTTABD                                                        
MKRNK    DS    XL2                                                              
MKNAM    DS    CL30                                                             
MKTZ     DS    CL1                                                              
MKCLS    DS    CL1                                                              
MKREG    DS    CL2                                                              
MKABBR   DS    CL8                                                              
MKORIG   DS    CL1                                                              
         DS    0F                                                               
MKUNVS   DS    XL(NDEMS*4)                                                      
MKDEMS   DS    XL(NDEMS*4)                                                      
MKLEN    EQU   *-MKTTABD                                                        
MKTMAX   EQU   250                                                              
*                                                                               
SPP702   CSECT                                                                  
         DS    0D                                                               
NPTTAB   DS    0X                                                               
         ORG   *+(NPTMAX*NPTLEN)                                                
         DS    0D                                                               
MKTTAB   DS    0X                                                               
         ORG   *+(MKTMAX*MKLEN)                                                 
         DS    0D                                                               
         DC    CL8'DRGLOBAL'                                                    
GLAREA   DS    40000X              DRIVER GLOBAL AREA                           
         EJECT                                                                  
       ++INCLUDE DRGLOBAL                                                       
         SPACE 2                                                                
       ++INCLUDE SPDRVWRKD                                                      
         SPACE 2                                                                
         PRINT OFF                                                              
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE DDCOMFACS                                                      
DBLKD    DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         SPACE 2                                                                
       ++INCLUDE DEDEMFILE                                                      
         SPACE 2                                                                
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'110SPREPP7S  05/01/02'                                      
         END                                                                    
