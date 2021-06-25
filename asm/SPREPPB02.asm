*          DATA SET SPREPPB02  AT LEVEL 013 AS OF 05/01/02                      
*PHASE SPPB02A                                                                  
         TITLE 'SPREPPB02 - STATION LISTING'                                    
         PRINT NOGEN                                                            
*                                                                               
*     QOPT2    N = DONT READ STATION LIST DATA SET                              
*                  IE - START FROM SCRATCH                                      
*                                                                               
SPPB02   CSECT                                                                  
         NMOD1 0,SPPB02,RR=R5                                                   
*                                                                               
         L     R9,0(R1)                                                         
         LA    RA,2048(R9)                                                      
         LA    RA,2048(RA)                                                      
         USING SPWORKD,R9,RA                                                    
         STM   R9,RB,SVRGS                                                      
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
         L     RF,=A(HLHOOK)                                                    
         ST    RF,HEADHOOK                                                      
*                                                                               
         B     EXIT                                                             
         SPACE 3                                                                
* REQUEST FIRST                                                                 
*                                                                               
SP100    DS    0H                                                               
*                                  BUILD STATION TABLE                          
*                                  -------------------                          
*                                                                               
         BAS   RE,BLDSTAB          FIRST READ EXISTING DATA SET                 
*                                                                               
SP110    DS    0H                                                               
         MVI   SRCE,C'A'           ARB FIRST                                    
         BAS   RE,BLDMTAB          GET MARKETS                                  
*                                                                               
         L     R4,ADBLOCK                                                       
         USING DBLOCK,R4                                                        
         XC    0(256,R4),0(R4)                                                  
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBSELMED,C'T'                                                    
         MVC   DBSELAGY,QAGY                                                    
         MVI   DBSELSRC,C'A'       ARB FIRST                                    
         PACK  DUB,QBOOK1(2)       CONVERT BOOK                                 
         CVB   RE,DUB                                                           
         STC   RE,DBSELBK                                                       
         PACK  DUB,QBOOK1+2(2)                                                  
         CVB   RE,DUB                                                           
         STC   RE,DBSELBK+1                                                     
         MVI   DBSELDAY,X'40'                                                   
         MVC   DBSELTIM(2),=H'1700'                                             
         MVC   DBSELTIM+2(2),=H'1715'                                           
         MVC   DBCOMFCS,ACOMFACS                                                
         L     RE,ADBUY                                                         
         ST    RE,DBAREC                                                        
*                                                                               
         L     R6,AMKTTAB                                                       
         USING MKTTABD,R6                                                       
*                                                                               
SP112    DS    0H                                                               
         CLI   0(R6),X'FF'         EOL                                          
         BE    SP120                                                            
         MVC   DBSELRMK,MKNUM                                                   
         MVI   DBFUNCT,DBGETMS                                                  
         GOTO1 DEMAND,DMCB,ADBLOCK,SVSTN                                        
         CLI   DBERROR,0                                                        
         BE    SP113                                                            
         CLI   DBERROR,X'80'                                                    
         BE    SP113                                                            
         B     SP113               NO-OP ABEND ON ERROR                         
         DC    H'0'                                                             
         SPACE 2                                                                
*                                                                               
SP113    DS    0H                                                               
         LA    R6,MKLEN(R6)                                                     
         B     SP112                                                            
*                                                                               
SP120    DS    0H                                                               
         MVI   SRCE,C'N'           NIELSEN                                      
         BAS   RE,BLDMTAB          GET MARKETS                                  
*                                                                               
         MVI   DBSELSRC,C'N'       NOW DO NIELSEN                               
         L     R6,AMKTTAB                                                       
         USING MKTTABD,R6                                                       
*                                                                               
SP122    DS    0H                                                               
         CLI   0(R6),X'FF'         EOL                                          
         BE    SP130                                                            
         MVC   DBSELRMK,MKNUM                                                   
         MVI   DBFUNCT,DBGETMS                                                  
         GOTO1 DEMAND,DMCB,ADBLOCK,SVSTN                                        
         CLI   DBERROR,0                                                        
         BE    SP123                                                            
         CLI   DBERROR,X'80'                                                    
         BE    SP123                                                            
         B     SP123               NO-OP ABEND ON ERROR                         
         DC    H'0'                                                             
         SPACE 2                                                                
*                                                                               
SP123    DS    0H                                                               
         LA    R6,MKLEN(R6)                                                     
         B     SP122                                                            
*                                                                               
SP130    DS    0H                                                               
*                                  GET STATION DATA                             
*                                  ----------------                             
         L     R6,ASTNTAB                                                       
         LA    RF,STNLEN                                                        
         MH    RF,STNCNT+2                                                      
         AR    RF,R6                                                            
         MVI   0(RF),X'FF'         SET EOL                                      
*                                                                               
         USING STNTABD,R6                                                       
         L     R4,ADBLOCK                                                       
         MVC   DBFILE,=C'DPT'                                                   
         MVI   DBFUNCT,DBGETDEM                                                 
         MVI   DBSELMED,C'D'       DAYPART DEMOS                                
*                                                                               
*                                                                               
SP133    DS    0H                                                               
         CLI   0(R6),X'FF'         EOL                                          
         BE    SP134F                                                           
         CLI   STNDMA,C' '         MUST BE A NEILSON STATION                    
         BNH   SP133R                                                           
         MVC   DBSELSTA,STNID                                                   
         PACK  DUB,STNDMA                                                       
         CVB   R0,DUB                                                           
         STCM  R0,3,DBSELRMK                                                    
         XC    DBSELMK,DBSELMK                                                  
         MVI   DBSELDAY,X'7F'         MON-SUN                                   
         MVC   DBSELTIM(2),=H'1800'                                             
         MVC   DBSELTIM+2(2),=H'1815'                                           
*                                                                               
SP133M   DS    0H                                                               
         GOTO1 DEMAND,DMCB,ADBLOCK,SVSDAT                                       
         CLI   DBERROR,0                                                        
         BE    SP133N                                                           
         CLI   DBERROR,X'80'                                                    
         BE    SP133N                                                           
         B     SP133N              NO-OP                                        
         DC    H'0'                                                             
*                                                                               
SP133N   DS   0H                                                                
*                                                                               
SP133R   DS    0H                                                               
         LA    R6,STNLEN(R6)                                                    
         B     SP133                                                            
*                                                                               
SP134F   DS    0H                                                               
         GOTO1 REPORT                                                           
*                                                                               
         XC    TOTCS,TOTCS                                                      
         MVC   TOTCNT,STNCNT                                                    
         OPEN  (STASAVO,(OUTPUT))                                               
*                                                                               
         L     R6,ASTNTAB                                                       
*                                                                               
SP135    DS    0H                                                               
         CLI   0(R6),X'FF'                                                      
         BE    SP138                                                            
*                                                                               
         CLC   STNADI(6),SPACES    TEST NEITHER ADI NOR DMA                     
         BNE   SP135B              SKIP                                         
         L     RF,NETHCNT                                                       
         LA    RF,1(RF)                                                         
         ST    RF,NETHCNT                                                       
         B     SP137                                                            
*                                                                               
SP135B   DS    0H                                                               
         CLI   STNADI,C' '                                                      
         BNH   SP136                                                            
         L     RF,ACNT             ALL ARB                                      
         LA    RF,1(RF)                                                         
         ST    RF,ACNT                                                          
*                                                                               
         CLI   STNDMA,C' '                                                      
         BH    SP136B                                                           
         L     RF,ANOTN            ARB NOT NSI                                  
         LA    RF,1(RF)                                                         
         ST    RF,ANOTN                                                         
*                                                                               
SP136    DS    0H                                                               
         CLI   STNDMA,C' '                                                      
         BNH   SP137                                                            
*                                                                               
SP136B   DS    0H                                                               
         L     RF,NCNT             ALL NSI                                      
         LA    RF,1(RF)                                                         
         ST    RF,NCNT                                                          
*                                                                               
         CLI   STNADI,C' '                                                      
         BH    SP136D                                                           
         L     RF,NNOTA            NSI NOT ARB                                  
         LA    RF,1(RF)                                                         
         ST    RF,NNOTA                                                         
         B     SP137                                                            
*                                                                               
SP136D   DS    0H                                                               
         L     RF,BOTHCNT          BOTH NSI AND ARB                             
         LA    RF,1(RF)                                                         
         ST    RF,BOTHCNT                                                       
*                                                                               
SP137    DS    0H                                                               
SP137C   DS    0H                                                               
         MVC   P(5),STNID                                                       
         MVC   P+9(3),STNADI                                                    
         MVC   P+14(3),STNDMA                                                   
         MVC   P+19(3),STNAFF                                                   
         MVC   P+25(1),STNTZ                                                    
         MVC   P+29(1),STNMCLS                                                  
         MVC   P+33(4),STNLTST                                                  
         MVC   P+39(3),STNSTAT                                                  
*                                                                               
         GOTO1 REPORT                                                           
*                                  WRITE TO DATASET                             
         MVC   W,SPACES                                                         
         MVC   W(STNLEN),STNID                                                  
         PUT   STASAVO,W                                                        
*                                                                               
SP137G   DS    0H                                                               
         LA    R6,STNLEN(R6)                                                    
         B     SP135                                                            
*                                                                               
SP138    DS    0H                                                               
         CLOSE STASAVO                                                          
         GOTO1 REPORT                                                           
*                                                                               
         LA    R3,TOTCS                                                         
         LA    R5,TOTWDS                                                        
         LA    R4,7                                                             
*                                                                               
SP139    DS    0H                                                               
         MVC   P(10),0(R5)                                                      
         EDIT  (B4,0(R3)),(5,P+11),ZERO=NOBLANK                                 
         GOTO1 REPORT                                                           
*                                                                               
         LA    R5,10(R5)                                                        
         LA    R3,4(R3)                                                         
         BCT   R4,SP139                                                         
*                                                                               
         B     EXIT                                                             
         SPACE 2                                                                
* BUILD STATION LIST                                                            
         DS    0D                                                               
         USING *,RF                                                             
SVSTN    NTR1  BASE=SVRB                                                        
         LM    R9,RB,SVRGS                                                      
         DROP  RF                                                               
         L     R4,ADBLOCK                                                       
         L     R5,DBAREC                                                        
         USING MLKEY,R5                                                         
*                                                                               
         CLI   MLSTAT,C'0'         SKIP MARKETS                                 
         BNL   EXIT                                                             
         OC    MLKMKT,MLKMKT       SKIP SPILL                                   
         BNZ   EXIT                                                             
*                                                                               
         LA    R6,X                                                             
         USING STNTABD,R6                                                       
         MVI   STNID,C' '          CLEAR ENTRY                                  
         MVC   STNID+1(STNLEN-1),STNID                                          
         MVC   STNID,MLSTAT                                                     
         LA    RF,STNDMA                                                        
         CLI   DBSELSRC,C'N'                                                    
         BE    SVSTN3                                                           
         LA    RF,STNADI                                                        
         CLC   MLRMKT,=H'800'      SKIP 'ADIS' 800+                             
         BNL   EXIT                                                             
*                                                                               
SVSTN3   DS    0H                                                               
         EDIT  (B2,MLRMKT),(3,0(RF)),FILL=0                                     
         MVC   STNLTST,QBOOK1      SET LATEST BOOK                              
         MVI   STNSTAT+2,C'*'      SET NEW                                      
*                                                                               
         GOTO1 BINSRCH,STNPARS,(1,X)                                            
         OC    1(3,R1),1(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   0(R1),1             TEST ALREADY THERE                           
         BE    SVSTN4              NO- JUST ADD                                 
*                                                                               
         L     R6,0(R1)            ELSE SET MARKET                              
         EDIT  (B2,MLRMKT),(3,FULL),FILL=0                                      
         LA    RF,STNADI                                                        
         LA    RE,STNSTAT                                                       
         CLI   DBSELSRC,C'A'                                                    
         BE    *+12                                                             
         LA    RF,STNDMA                                                        
         LA    RE,STNSTAT+1                                                     
         CLC   FULL(3),0(RF)       TEST SAME AS ON FILE                         
         BE    SVSTN3F                                                          
         CLI   0(RF),C' '                                                       
         BNH   *+10                                                             
         MVC   0(1,RE),DBSELSRC    SET CHANGED                                  
         MVC   0(3,RF),FULL                                                     
*                                                                               
SVSTN3F  DS    0H                                                               
         CLC   STNLTST,QBOOK1                                                   
         BNL   *+10                                                             
         MVC   STNLTST,QBOOK1      SET LATEST BOOK                              
*                                                                               
SVSTN4   DS    0H                                                               
*                                                                               
         B     EXIT                                                             
         SPACE 2                                                                
* SAVE STATION DATA                                                             
         DS    0D                                                               
         USING *,RF                                                             
SVSDAT   NTR1  BASE=SVRB                                                        
         LM    R9,RA,SVRGS                                                      
         DROP  RF                                                               
         L     R4,ADBLOCK                                                       
         L     R7,ACOMFACS                                                      
         USING COMFACSD,R7                                                      
*                                                                               
         L     R2,DBAREC                                                        
         LA    R2,DRFRSTEL-DRKEY(R2)                                            
*                                                                               
SVP3     DS    0H                                                               
         CLI   0(R2),0                                                          
         BE    SVP5                                                             
         CLI   0(R2),X'03'                                                      
         BE    SVP4                                                             
*                                                                               
SVP3B    DS    0H                                                               
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     SVP3                                                             
*                                                                               
SVP4     DS    0H                                                               
         USING DIELEM,R2                                                        
         MVC   STNMCLS,DIMCLASS                                                 
         MVC   STNTZ,DITZ          TIME ZONE                                    
         MVC   STNAFF,DIAFFL       AFFILIATION                                  
         CLI   DIDMAIND,C'Y'       TEST TRUE DMA                                
         BE    *+10                                                             
         MVC   STNDMA,SPACES       NO, CLEAR DMA                                
*                                                                               
SVP5     DS    0H                                                               
         B     EXIT                                                             
         SPACE 2                                                                
* HEADLINE HOOK                                                                 
         DS    0D                                                               
         USING *,RF                                                             
HLHOOK   NTR1  BASE=SVRB                                                        
         LM    R9,RA,SVRGS                                                      
         DROP  RF                                                               
*                                                                               
         MVC   WORK(4),QBOOK1                                                   
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,WORK,(6,DUB)                                         
         MVC   HEAD1(5),=C'BOOK='                                               
         MVC   HEAD1+6(6),DUB                                                   
*                                                                               
         MVC   HEAD6(43),=C'STATION  ADI  DMA  AFF  TZ  CLS  BOOK  STATX        
               '                                                                
         MVC   HEAD7(43),=C'-------  ---  ---  ---  --  ---  ----  ----X        
               '                                                                
         B     EXIT                                                             
         EJECT                                                                  
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
*                                                                               
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
SPPB02   CSECT                                                                  
MKTSAV   DCB   DDNAME=MKTSAV,                                          X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               MACRF=GM,                                               X        
               EODAD=BMEOF                                                      
         EJECT                                                                  
*        BUILD STATION TABLE                                                    
         SPACE 2                                                                
BLDSTAB  NTR1                                                                   
*                                  SET BINSRCH PARS                             
         SR    R0,R0                                                            
         L     R1,=A(STNTAB)                                                    
         SR    R2,R2                                                            
         LA    R3,STNLEN                                                        
         LA    R4,STNKLEN                                                       
         LH    R5,=Y(STNMAX)                                                    
         STM   R0,R5,STNPARS                                                    
*                                                                               
         CLI   QOPT2,C'N'          TEST TO READ DSN                             
         BE    BSTX                                                             
         OPEN  (STASAV,(INPUT))                                                 
*                                                                               
         LA    R6,X                                                             
         XC    X,X                                                              
         USING STNTABD,R6                                                       
         L     R5,ADBUY                                                         
         USING STSRECD,R5           STATION SAVE RECORD                         
*                                                                               
BST4     DS    0H                                                               
         GET   STASAV,(R5)                                                      
         CLI   0(R5),C'*'          SKIP COMMENTS                                
         BE    BST4                                                             
         MVC   STNID(STNLEN),STSID                                              
         MVC   STNSTAT,SPACES                                                   
*                                                                               
         GOTO1 BINSRCH,STNPARS,(1,X)                                            
         OC    1(3,R1),1(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     BST4                                                             
*                                                                               
BSTEOF   DS    0H                                                               
         L     R6,ASTNTAB                                                       
         LA    RF,STNLEN            SET EOL                                     
         MH    RF,STNCNT+2                                                      
         AR    RF,R6                                                            
         MVI   0(RF),X'FF'                                                      
*                                                                               
         CLOSE STASAV                                                           
*                                                                               
BSTX     B     EXIT                                                             
         SPACE 2                                                                
STSRECD  DSECT                                                                  
STSID    DS    CL5                                                              
STSADI   DS    CL3                                                              
STSDMA   DS    CL3                                                              
STSAFF   DS    CL3                                                              
STSTZ    DS    CL1                                                              
STSMCLS  DS    CL1                                                              
STSLTST  DS    CL4                                                              
STSSTAT  DS    CL3                                                              
*                                                                               
SPPB02   CSECT                                                                  
STASAV   DCB   DDNAME=STASVIP,                                         X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               MACRF=GM,                                               X        
               EODAD=BSTEOF                                                     
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
         DS    0D                                                               
ADRIVER  DS    A                                                                
ASYSDRV  DS    A                                                                
ASPDRWKC DS    A                                                                
MKTPARS  DS    6F                                                               
AMKTTAB  EQU   MKTPARS+4                                                        
MKTCNT   EQU   MKTPARS+8                                                        
STNPARS  DS    6F                                                               
ASTNTAB  EQU   STNPARS+4                                                        
STNCNT   EQU   STNPARS+8                                                        
*                                                                               
         DS    0F                                                               
TOTCS    DS    0XL28                                                            
ACNT     DS    F                                                                
NCNT     DS    F                                                                
ANOTN    DS    F                                                                
NNOTA    DS    F                                                                
BOTHCNT  DS    F                                                                
NETHCNT  DS    F                                                                
TOTCNT   DS    F                                                                
*                                                                               
TOTWDS   DS    0X                                                               
         DC    CL10'ARB TOTAL'                                                  
         DC    CL10'NSI TOTAL'                                                  
         DC    CL10'ARB ONLY '                                                  
         DC    CL10'NSI ONLY '                                                  
         DC    CL10'BOTH     '                                                  
         DC    CL10'NEITHER  '                                                  
         DC    CL10'TOTAL    '                                                  
*                                                                               
DPGFILE  DC    CL8'SPPA05  '                                                    
DRIVER   DC    CL8'T00A3A  '                                                    
SPDRIVER DC    CL8'SPDRIVER'                                                    
XFF      DC    20X'FF'                                                          
*                                                                               
SVKEY    DS    CL32                                                             
ELCODE   DS    XL1                                                              
X        DS    XL64                                                             
W        DS    CL132                                                            
SRCE     DS    C                                                                
*                                                                               
         SPACE 2                                                                
         DS    0F                                                               
SVRGS    DS    0XL12                                                            
SVR9     DS    F                                                                
SVRA     DS    F                                                                
SVRB     DS    F                                                                
         SPACE 2                                                                
         EJECT                                                                  
RELO     DC    F'0'                                                             
         SPACE 2                                                                
*                                                                               
STASAVO  DCB   DDNAME=STASVOP,                                         X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               MACRF=PM                                                         
         SPACE 3                                                                
*                                                                               
STNTABD  DSECT                                                                  
STNID    DS    CL5                                                              
STNKLEN  EQU   *-STNID                                                          
STNADI   DS    CL3                                                              
STNDMA   DS    CL3                                                              
STNAFF   DS    CL3                                                              
STNTZ    DS    CL1                                                              
STNMCLS  DS    CL1                                                              
STNLTST  DS    CL4                                                              
STNSTAT  DS    CL3                                                              
STNLEN   EQU   *-STNTABD                                                        
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
MKSPIL   DS    C                                                                
MKLEN    EQU   *-MKTTABD                                                        
*                                                                               
MKTMAX   EQU   250                                                              
         SPACE 2                                                                
*                                                                               
         CSECT                                                                  
         DS    0D                                                               
STNTAB   DS    0X                                                               
STNMAX   EQU   2500                                                             
         ORG   *+(STNMAX*STNLEN)                                                
         DS    0D                                                               
MKTTAB   DS    CL(MKTMAX*MKLEN)                                                 
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
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013SPREPPB02 05/01/02'                                      
         END                                                                    
