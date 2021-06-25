*          DATA SET SPREPDN03  AT LEVEL 030 AS OF 10/08/12                      
*PHASE SPDN03A                                                                  
         TITLE 'SPREPDN03    NETWK RPT CONTROLLER'                              
*                                                                               
* CHANGE LOG                                                                    
*                                                                               
* AKAT 10/12       2-BYTE BUYLINE FIX                                           
*                                                                               
* BPLA  9/99       OPTIONS FIXED QOPT3 TO QOPT7                                 
*                  QOPT2 TO QOPT6                                               
*                                                                               
* BPLA 12/93       ADD PST                                                      
*                                                                               
* BPLA 9/8/93      ADD ACTIVITY SWITCH                                          
*                                                                               
* BPLA 11/19/92 PROFILE OPTIONS FOR EXCLUDING TAX AND GST FROM GROSS            
*                  AND NET.   ALSO GST+PST COLUMN OPTION                        
*********                                                                       
***      PROFILE OPTIONS                                                        
***                                                                             
*        +0    Y=INCLUDE TAX IN GROSS (DEFAULT)                                 
*              N=DON'T                                                          
*        +1    Y=INCLUDE GST/PST IN GROSS (DEFAULT)                             
*              N=DON'T                                                          
*        +2    Y=INCLUDE TAX IN NET (DEFAULT)                                   
*              N=DON'T                                                          
*        +3    Y=INCLUDE GST+PST IN NET (DEFAULT)                               
*              N=DON'T                                                          
*        +4    Y=GST+PST COLUMNS                                                
*              N=NO  (DEFAULT)                                                  
*                                                                               
*        QOPTIONS                                                               
*                                                                               
*        QOPT1,QOPT2,QOPT3   BUY LINE NUMBER (OPTIONAL)                         
*        QOPT4  U=INPAID                                                        
*        QOPT5  NOT USED                                                        
*        QOPT6  Y= DISPLAY SPILL MARKETS                                        
*        QOPT7  B= FORCE TO BROADCAST MONTHS                                    
*                                                                               
QOPT6    EQU    QGRP        COL 67                                              
QOPT7    EQU    QGRP+1      COL 68                                              
*                                                                               
SPDN03   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPDN03,RR=R9                                                   
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         LA    R9,SPACEND                                                       
         USING SPDNWRKD,R9                                                      
         MVC   PAGE,=H'1'                                                       
         MVI   RCSUBPRG,1                                                       
*                                                                               
         L     R0,=A(PRINTIT)                                                   
         A     R0,RELO                                                          
         ST    R0,APRINTIT                                                      
         MVI   ACTSW,0                                                          
*                                                                               
         XC    DEMDISP,DEMDISP                                                  
         XC    SVBNET,SVBNET                                                    
         XC    BUYDLN,BUYDLN                                                    
*                                                                               
         GOTO1 FCNXTCLT            READ CLIENT HEADER                           
*                                                                               
         OC    PROGPROF,PROGPROF   SEE IF I HAVE A PROFILE                      
         BNZ   *+10                                                             
         MVC   PROGPROF(5),=C'YYYYN'      SET DEFAULTS                          
*                                                                               
         XC    KEY,KEY             READ PRODUCT HEADER                          
         L     R6,ADCLT                                                         
         MVC   BCLT,2(R6)          NEED TO SET BCLT FOR MEDPRDRD                
         MVC   KEY(4),0(R6)                                                     
         MVC   KEY+4(3),QPRD                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    PRDFND                                                           
         MVC   P(19),=C'PRODUCT NOT ON FILE'                                    
         GOTO1 REPORT                                                           
         B     NETXX                                                            
*                                                                               
PRDFND   DS    0H                                                               
         L     R6,ADPRD                                                         
         ST    R6,AREC                                                          
         GOTO1 GET                                                              
         USING PRDHDRD,R6                                                       
         MVC   BPRD,PCODE+1                                                     
         MVC   PRD,KEY+4                                                        
         MVC   PRDNM(20),PNAME                                                  
*                                  READ ESTIMATE                                
**NEW 3/14/90                                                                   
EST0     XC    BEST,BEST                                                        
         XC    BESTEND,BESTEND      FOR RANGE                                   
         CLC   QEST(3),=C'ALL'     SEE IF DOING ALL ESTS                        
         BE    ESTFNDX                                                          
**NEW 3/14/90                                                                   
         CLC   QESTEND,=C'000'                                                  
         BNH   EST5                                                             
         PACK  DUB,QESTEND(3)                                                   
         CVB   R0,DUB                                                           
         STC   R0,BESTEND                                                       
*                                                                               
EST5     PACK  DUB,QEST(3)                                                      
         CVB   R0,DUB                                                           
         STC   R0,KEY+7                                                         
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    ESTFND                                                           
         MVC   P(20),=C'ESTIMATE NOT ON FILE'                                   
         GOTO1 REPORT                                                           
         B     NETXX                                                            
*                                                                               
ESTFND   DS    0H                                                               
         L     R6,ADEST                                                         
         ST    R6,AREC                                                          
         GOTO1 GET                                                              
         USING ESTHDRD,R6                                                       
         MVC   BEST,KEY+7                                                       
         MVC   EST,QEST                                                         
         MVC   ESTNM(20),EDESC                                                  
         MVC   ESTDATES,ESTART                                                  
ESTFNDX  DS    0H                                                               
*                                                                               
*                                  NEED TO READ EQUIVHDR                        
         MVC   DUB(3),QAGY                                                      
         MVC   DUB+3(2),BCLT                                                    
         GOTO1 EQVRD,DMCB,DUB,DPEQTAB,ADBUY,DATAMGR                             
         L     RE,ADBUY                                                         
         MVC   EQTAB,0(RE)                                                      
*                                  READ DAYPART HDR                             
         XC    DMCB(12),DMCB                                                    
         MVC   DMCB(3),QAGY                                                     
         MVC   DMCB+3(1),EDAYMENU                                               
         GOTO1 DPTRD,DMCB,,ADDPTTAB                                             
         DROP  R6                                                               
*                                                                               
**NEW 3/14/90                                                                   
         XC    BMKT(5),BMKT           CLEAR MKT/STA                             
         CLC   QSTA(4),=C'ALL '        SEE IF DOING ALL NETWORKS                
         BE    DATES                                                            
**NEW 3/14/90                                                                   
         CLI   QSTA+4,C' '                                                      
         BNE   *+10                                                             
         MVC   QSTA+4(1),QMED                                                   
         GOTO1 MSPACK,DMCB,=C'0000',QSTA,BMKT                                   
*                                                                               
DATES    CLC   QSTART(12),SPACES                                                
         BNE   DATES2                                                           
         MVC   QSTART(12),ESTDATES                                              
         B     DATES3                                                           
*                                                                               
DATES2   CLC   QSTART+4(8),SPACES                                               
         BNE   DATES3                                                           
         CLI   QOPT7,C'B'          FORCE TO BRDCAST MONTHS                      
         BNE   *+12                                                             
         CLI   QOPT4,C'U'          UNPAID AND REQUESTED BY BRD MONTH            
         BE    DATESX              WILL HANDLE DATES LATER                      
         MVC   QSTART+4(2),=C'01'                                               
         MVC   QEND(4),QSTART                                                   
         MVC   QEND+4(2),=C'24'                                                 
         CLC   QEND+2(2),=C'02'        FEB                                      
         BNE   VR50C                                                            
         MVC   QEND+4(2),=C'22'                                                 
         B     VR50E                                                            
*                                                                               
VR50C    CLC   QEND+2(2),=C'04'       30 DAY MTH                                
         BE    VR50E                                                            
         CLC   QEND+2(2),=C'06'       30 DAY MTH                                
         BE    VR50E                                                            
         CLC   QEND+2(2),=C'09'       30 DAY MTH                                
         BE    VR50E                                                            
         CLC   QEND+2(2),=C'11'       30 DAY MTH                                
         BE    VR50E                                                            
         MVC   QEND+4(2),=C'25'   SET DAY TO 25 FOR 31 DAY MTHS                 
VR50E    DS    0H                                                               
*                                                                               
DATES3   DS    0H                                                               
         GOTO1 DATCON,DMCB,(0,QSTART),(1,BQSTART)                               
         GOTO1 DATCON,DMCB,(0,QEND),(1,BQEND)                                   
         GOTO1 DATCON,DMCB,(0,QSTART),(2,BQSTARTP)                              
         GOTO1 DATCON,DMCB,(0,QEND),(2,BQENDP)                                  
*                                                                               
DATESX   XC    KEY,KEY                                                          
*                                                                               
         L     RE,MEDBUFF                                                       
         USING MEDBLOCK,RE                                                      
         MVC   MEDNUMPE,=F'1'                                                   
         MVI   RQDAYPT,C'Y'                                                     
         MVI   RQEQUIV,C'Y'                                                     
         MVI   MEDEXTDM,4                                                       
         MVC   MEDLCHNK,=F'168'                                                 
         MVI   RQGSTOPT,C'N'       SET TO N UNLESS UNPAID REPORT                
         DROP  RE                                                               
*                                                                               
         CLI   QOPT4,C'U'          UNPAID REPORT                                
         BNE   NET0                                                             
*KEVIN                                                                          
         CLI   QOPT7,C'B'          FORCE TO BRDCAST MONTHS                      
         BNE   KEVX                                                             
         CLC   QSTART+4(8),SPACES  REQUESTED FOR 1 MONTH                        
         BNE   KEVX                                                             
         MVC   QSTART+4(2),=C'01'                                               
         MVI   SPOTPROF+8,0        GET REAL BRD MONTH - NO PROFILES!            
         GOTO1 GETBROAD,DMCB,(1,QSTART),WORK,GETDAY,ADDAY                       
         CLI   DMCB,X'FF'                                                       
         BNE   *+6                                                              
         DC    H'0'                INVALID DATE IS PASSED INTO GETBRD           
*                                                                               
         MVC   QSTART,WORK                                                      
         MVC   QEND,WORK+6                                                      
         GOTO1 DATCON,DMCB,(0,QSTART),(1,BQSTART)                               
         GOTO1 DATCON,DMCB,(0,QEND),(1,BQEND)                                   
         GOTO1 DATCON,DMCB,(0,QSTART),(2,BQSTARTP)                              
         GOTO1 DATCON,DMCB,(0,QEND),(2,BQENDP)                                  
*KEVIN                                                                          
*                                                                               
KEVX     L     RE,MEDBUFF                                                       
         USING MEDBLOCK,RE                                                      
         MVI   RQGSTOPT,C'G'       SET TO INCLUDE GST IN AMOUNTS                
         MVI   MEDEXTAC,C'Y'                                                    
         MVI   MEDEXTAX,C'Y'                                                    
         MVC   MEDLCHNK,=F'200'                                                 
         MVI   MEDEXTDM,4          NO DEMOS NEEDED                              
         XC    RUNPDGR(20),RUNPDGR                                              
         B     NET0                                                             
*                                                                               
NET0     GOTO1 MEDDATE,DMCB,(RA)                                                
         GOTO1 MEDPRDRD,DMCB,(RA)                                               
*KEVIN2                                                                         
******** MVC   QSTART,MYQSTART                                                  
******** MVC   QEND,MYQEND                                                      
*KEVIN2                                                                         
*                                                                               
*                                  NOW READ FIRST OR ONLY BUY                   
         XC    KEY,KEY                                                          
         MVC   KEY(1),BAGYMD                                                    
         GOTO1 CLPACK,DMCB,QCLT,KEY+1                                           
         MVC   KEY+3(1),BPRD                                                    
         MVC   KEY+6(3),BSTA                                                    
         MVC   KEY+9(1),BEST                                                    
         CLI   QOPT1,C'0'                                                       
         BL    NET1                NO LINE NUMBER IN REQ                        
         PACK  DUB,QOPT1(3)                                                     
         CVB   R0,DUB                                                           
         STC   R0,KEY+11                                                        
         CLI   VGETBUY,2           2-BYTE BUYLINE?                              
         BNE   NET1                NO                                           
         STCM  R0,3,KEY+11         YES                                          
*                                                                               
NET1     DS    0H                                                               
         GOTO1 HIGH                                                             
         B     NET4                                                             
*                                                                               
NET2     GOTO1 SEQ                                                              
*                                                                               
**NEW 3/14/90      WAS (11) TO CHECK EST                                        
NET4     CLC   KEY(4),KEYSAVE      FIRST CHK A/M CLT PRD                        
         BNE   NETX                                                             
         OC    BMKT(5),BMKT        SEE IF NETWORK GIVEN                         
         BZ    NET4C                                                            
         CLC   KEY(9),KEYSAVE                                                   
         BNE   NETX                END OF MKT/STA                               
NET4C    OC    KEY+4(2),KEY+4      SEE IF BUY HAS MKT                           
         BNZ   NETX                DONE - I'M PAST THE NETWORK BUYS             
*                                  (THEY HAVE MKT = X'0000')                    
         CLI   BEST,0              SEE IF ESTIMATE GIVEN                        
         BE    NET6                CAN'T HAVE LINE SO READ BUY                  
         CLI   BESTEND,0           SEE IF DOING ONE EST                         
         BE    NET4E                                                            
         CLC   KEY+9(1),BEST       SEE IF EST WITHIN RANGE                      
         BL    NET2                                                             
         CLC   KEY+9(1),BESTEND                                                 
         BH    NET4E5              IF HIGH SEE IF DOING ALL NETWORKS            
         B     NET6                PROCESS                                      
*                                                                               
NET4E    CLC   KEY+9(1),BEST       MUST MATCH EST                               
         BE    NET4F               END OF EST                                   
NET4E5   OC    BMKT(5),BMKT        SEE IF DOING ALL NETWORKS                    
         BZ    NET2                KEEP LOOKING                                 
         B     NETX                DONE                                         
*                                                                               
NET4F    CLI   QOPT1,C'0'          SEE IF DOING ONE LINE NUMBER                 
         BL    NET6                                                             
         LA    RE,1                RE = 1 FOR 2 BYTE BUYLINE                    
         CLI   VGETBUY,2           2-BYTE BUYLINE?                              
         BE    *+6                 YES                                          
         XR    RE,RE               NO - CLEAR RE                                
         EX    RE,*+8              EXECUTE                                      
         B     *+10                                                             
         CLC   KEY+11(0),KEYSAVE+11                                             
         BE    NET6                LINES MATCH                                  
         MVC   P(31),=C'BUY-LINE DELETED OR NOT ON FILE'                        
         GOTO1 REPORT                                                           
         B     NETXX                                                            
*                                                                               
NET6     DS    0H                                                               
         L     R7,ADBUY                                                         
         ST    R7,AREC                                                          
         GOTO1 GET                                                              
         XC    SVPKGEL,SVPKGEL                                                  
         BAS   RE,PKGTEST                                                       
         B     PKGB                SLAVE RETURN                                 
         B     PROCESS             REG OR MASTER RETURN                         
*                                                                               
PKGB     CLI   QOPT1,C'0'          SEE IF DOING ONE LINE                        
         BNL   PROCESS             YES - PROCESS SLAVE                          
         B     NET2                NO - BYPASS SLAVES                           
         EJECT                                                                  
PROCESS  DS    0H                                                               
**NEW 3/14/90                                                                   
         CLI   BESTEND,0           FIRST CHECK FOR RANGE                        
         BNE   PROC2                                                            
*                                  SEE IF ONE EST                               
         CLI   BEST,0                                                           
         BNE   PROC10              I ALREADY READ IT                            
*                                                                               
PROC2    L     R6,ADPRD            MUST READ ESTIMATE HERE                      
         MVC   MYBUYKEY,KEY        SAVE KEY                                     
         XC    KEY,KEY                                                          
         MVC   KEY(7),0(R6)                                                     
         MVC   KEY+7(1),MYBUYKEY+9     EST FROM BUY                             
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    PROC5                                                            
         MVC   P(20),=C'ESTIMATE NOT ON FILE'                                   
         GOTO1 REPORT                                                           
         B     NETXX                                                            
*                                                                               
PROC5    L     R6,ADEST                                                         
         ST    R6,AREC                                                          
         GOTO1 GET                                                              
         USING ESTHDRD,R6                                                       
         MVC   ESTNM(20),EDESC                                                  
         ZIC   R0,MYBUYKEY+9                                                    
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  EST(3),DUB+6(2)                                                  
*                                                                               
         MVC   KEY,MYBUYKEY          RESTORE KEY                                
*                                                                               
**NEW 3/14/90                                                                   
PROC10   MVI   FORCEHED,C'Y'                                                    
         MVI   MODE,PROCBUY                                                     
         GOTO1 MSUNPK,DMCB,KEY+4,WORK,SVBNET                                    
         MVC   MYBUYKEY,KEY                                                     
         GOTO1 GO                                                               
         CLI   QOPT1,C'0'                                                       
         BNL   NETX                DONE                                         
         CLI   SVPKGEL,0                                                        
         BNE   NEXTPKG                                                          
*                                                                               
NEXTBUY  DS    0H                                                               
         MVC   KEY,MYBUYKEY                                                     
         GOTO1 HIGH                RESET FOR SEQ READ                           
         B     NET2                                                             
*                                                                               
NETX     DS    0H                                                               
         CLI   QOPT4,C'U'          SEE IF DOING UNPAID REPORT                   
         BNE   NETX30                                                           
         CLI   QOPT1,C'0'          SEE IF DOING ONE LINE                        
         BNL   NETX30                                                           
         OC    DEMDISP,DEMDISP                                                  
         BZ    NETX30              NO BUYS                                      
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,10                                                      
         CLI   PROGPROF+4,C'Y'      GST+PST COLUMNS                             
         BNE   *+8                                                              
         MVI   RCSUBPRG,12                                                      
*                                                                               
         LA    R3,P+19                                                          
         MVC   0(13,R3),=C'REPORT TOTALS'                                       
         EDIT  RUNPDGR,(10,14(R3)),2,FLOAT=-                                    
         EDIT  RUNPDNET,(10,24(R3)),2,FLOAT=-                                   
         EDIT  RUNPDTAX,(10,34(R3)),2,FLOAT=-,ZERO=BLANK                        
         CLI   PROGPROF+4,C'Y'                                                  
         BNE   NETX5                                                            
         EDIT  RUNPDGST,(10,44(R3)),2,FLOAT=-,ZERO=BLANK                        
         EDIT  RUNPDPST,(10,54(R3)),2,FLOAT=-,ZERO=BLANK                        
NETX5    GOTO1 APRINTIT,DMCB,(RA)                                               
*                                                                               
         CLC   PROGPROF(4),=C'YYYY'    SEE IF BOTH GROSS AND NET                
         BE    NETX10                  INCLUDED TAX AND GST+PST                 
*                                                                               
         LA    R3,P+07                                                          
         MVC   0(25,R3),=C'INCLUDING TAX AND GST/PST'                           
         L     R0,RUNPDGR                                                       
         CLI   PROGPROF+0,C'Y'                                                  
         BE    *+8                                                              
         A     R0,RUNPDTAX                                                      
         CLI   PROGPROF+1,C'Y'                                                  
         BE    *+12                                                             
         A     R0,RUNPDGST                                                      
         A     R0,RUNPDPST                                                      
         ST    R0,MYFULL                                                        
         EDIT  MYFULL,(10,26(R3)),2,FLOAT=-                                     
*                                                                               
         L     R0,RUNPDNET                                                      
         CLI   PROGPROF+2,C'Y'                                                  
         BE    *+8                                                              
         A     R0,RUNPDTAX                                                      
         CLI   PROGPROF+3,C'Y'                                                  
         BE    *+12                                                             
         A     R0,RUNPDGST                                                      
         A     R0,RUNPDPST                                                      
         ST    R0,MYFULL                                                        
         EDIT  MYFULL,(10,36(R3)),2,FLOAT=-                                     
         GOTO1 APRINTIT,DMCB,(RA)                                               
*                                                                               
NETX10   DS    0H                                                               
         MVI   RCSUBPRG,1                                                       
         B     NETXX                                                            
*                                                                               
NETX30   CLI   ACTSW,0                                                          
         BNE   NETXX                                                            
         MVC   P(23),=C'** NO DATA GENERATED **'                                
         MVI   FORCEHED,C'N'                                                    
         GOTO1 REPORT                                                           
*                                                                               
NETXX    XMOD1 1                                                                
         EJECT                                                                  
*          DATA SET SPREPRN03  AT LEVEL 051 AS OF 08/18/09                      
*=================================================================              
* TEST IF LINE IS A PACKAGE MASTER OR SLAVE                                     
* IF MASTER, SAVE PACKAGE ELEMENT                                               
* IF SLAVE, TELL CALLER TO PROCESS LATER                                        
*=================================================================              
                                                                                
PKGTEST  DS    0H                                                               
         L     R6,ADBUY                                                         
         USING BUYREC,R6                                                        
         LA    R2,BDELEM                                                        
         SR    R0,R0                                                            
*                                                                               
PKGT2    IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    4(RE)               TAKE RETURN 2                                
         CLI   QBYID,C'Y'          CANNOT HANDLE DIFF. ID'S                     
         BE    PKGT2               ON PKG MST/PKG SLAVE                         
         CLI   0(R2),5                                                          
         BNE   PKGT2                                                            
         TM    2(R2),X'01'         TEST MASTER OR SLAVE                         
         BZR   RE                  SLAVE - RETURN 1 - SKIP FOR NOW              
* LINE IS A MASTER                                                              
         MVC   SVPKGKEY,KEY        SAVE MASTER KEY                              
         LLC   RF,1(R2)                                                         
         BCTR  RF,0                SET FOR EX                                   
         EX    RF,*+8              SAVE                                         
         B     *+10                                                             
         MVC   SVPKGEL(0),0(R2)  *EXECUTED*                                     
         B     4(RE)               TAKE RETURN 2 - PKG DATA SAVED               
         EJECT                                                                  
         EJECT                                                                  
*=========================================================                      
* FETCH NEXT LINE IN PKG                                                        
*=========================================================                      
                                                                                
NEXTPKG  MVC   KEY,MYBUYKEY        MOVE LAST BUY KEY                            
         LA    R2,KEY+12           1-BYTE LINE NUM FOR POL BRND                 
         CLI   KEY+10,X'FF'        TEST FOR IT                                  
         BE    *+8                                                              
         LA    R2,KEY+11           LINE NUM FOR POL                             
*                                                                               
         LA    R5,SVPKGEL                                                       
         TM    2(R5),X'10'         TEST 2-BYTE LINE NUMS IN PKGEL               
         BO    NPKG10                                                           
*                                                                               
         LA    R5,3(R5)            POINT TO FIRST LINE NUMBER                   
         CLC   SVPKGKEY,KEY        TEST FIRST TIME (HAVE MASTER)                
         BE    NPKG8                                                            
*                                                                               
NPKG4    CLC   0(1,R2),0(R5)                                                    
         BE    NPKG6                                                            
         LA    R5,1(R5)                                                         
         CLI   0(R5),0                                                          
         BNE   NPKG4                                                            
         DC    H'0'                                                             
*                                                                               
NPKG6    LA    R5,1(R5)                                                         
*                                                                               
NPKG8    CLI   0(R5),0             TEST E-O-L                                   
         BE    NPKG22                                                           
*                                                                               
         MVC   0(1,R2),0(R5)       MOVE LINE TO KEY                             
         L     RE,ADBUY                                                         
         TM    15(RE),BUYRLN2      TEST 2-BYTE LINE IN BUYREC                   
         BZ    NPKG9                                                            
         MVI   KEY+11,0                                                         
         MVC   KEY+12(1),0(R5)                                                  
                                                                                
NPKG9    GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(12),KEYSAVE                                                  
         BE    NPKG20                                                           
         MVC   KEY(13),KEYSAVE     RESTORE LINE NOT FOUND AND TRY NEXT          
         B     NPKG6                                                            
                                                                                
* THIS CODE FOR 2-BYTE LINE NUMBERS *                                           
                                                                                
NPKG10   LA    R5,3(R5)            FIRST PACKAGE LINE NUMBER                    
         CLC   SVPKGKEY,KEY        TEST FIRST TIME (HAVE MASTER)                
         BE    NPKG16                                                           
*                                                                               
NPKG12   CLC   KEY+11(2),0(R5)                                                  
         BE    NPKG14                                                           
         LA    R5,2(R5)                                                         
         OC    0(2,R5),0(R5)                                                    
         BNE   NPKG12                                                           
         DC    H'0'                                                             
*                                                                               
NPKG14   LA    R5,2(R5)            NEXT LINE NUMBER                             
*                                                                               
NPKG16   OC    0(2,R5),0(R5)       TEST E-O-L                                   
         BZ    NPKG22                                                           
* SET FOR NEXT PKG LINE                                                         
         MVC   KEY+11(2),0(R5)                                                  
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(13),KEYSAVE                                                  
         BE    NPKG20                                                           
         MVC   KEY(13),KEYSAVE     RESTORE LINE NOT FOUND AND TRY NEXT          
         B     NPKG14                                                           
*                                                                               
NPKG20   GOTO1 GETBUY                                                           
         MVC   SVBUYKEY,KEY                                                     
         B     PROCESS                                                          
*                                                                               
* END OF PKG                                                                    
*                                                                               
NPKG22   MVC   KEY(13),SVPKGKEY    RESTORE MASTER LINE                          
         XC    SVPKGEL,SVPKGEL                                                  
         GOTO1 HIGH                                                             
         B     NET2                                                             
         LTORG                                                                  
*                                                                               
SVPKGEL  DC    XL256'00'                                                        
         EJECT                                                                  
PRINTIT  CSECT                                                                  
         NMOD1 0,PRINTIT                                                        
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
*                                                                               
*                                                                               
         LA    R9,SPACEND                                                       
         USING SPDNWRKD,R9                                                      
         CLI   RCSUBPRG,10          REPORT TOTALS                               
         BE    PRNT0                                                            
         CLI   RCSUBPRG,12          REPORT TOTALS WITH GST+PST COLUMN           
         BE    PRNT0                                                            
*                                                                               
         MVC   H5+55(4),SVBNET                                                  
         MVC   H5+60(7),=C'NETWORK'                                             
         MVC   H10(L'BUYDLN),BUYDLN                                             
*                                                                               
PRNT0    DS    0H                                                               
         CLI   LINE,99                                                          
         BE    PRNT1                                                            
         CLC   LINE,MAXLINES                                                    
         BL    *+8                                                              
         MVI   FORCEMID,C'Y'                                                    
*                                                                               
PRNT1    DS    0H                                                               
         CLI   QOPT4,C'U'                                                       
         BNE   PRNT10                                                           
         MVC   H7(32),=C'** UNPAID AMOUNTS INCLUDE TAX **'                      
         CLC   PROGPROF+0(4),=C'YNYN'                                           
         BE    PRNT10                                                           
         MVC   H7(36),=C'** UNPAID AMOUNTS INCLUDE GST/PST **'                  
         CLC   PROGPROF+0(4),=C'NYNY'                                           
         BE    PRNT10                                                           
         MVC   H7(44),=C'** UNPAID AMOUNTS INCLUDE TAX AND GST/PST **'          
         CLC   PROGPROF+0(4),=C'YYYY'                                           
         BE    PRNT10                                                           
*                                                                               
         LA    R6,H8                                                            
         MVC   H7(44),SPACES                                                    
         MVC   H7(31),=C'** UNPAID GROSS INCLUDES TAX **'                       
         CLC   PROGPROF+0(2),=C'YN'                                             
         BE    PRNT5                                                            
         MVC   H7(35),=C'** UNPAID GROSS INCLUDES GST/PST **'                   
         CLC   PROGPROF+0(2),=C'NY'                                             
         BE    PRNT5                                                            
         MVC   H7(43),=C'** UNPAID GROSS INCLUDES TAX AND GST/PST **'           
         CLC   PROGPROF+0(2),=C'YY'                                             
         BE    PRNT5                                                            
         MVC   H7(44),SPACES                                                    
         LA    R6,H7                                                            
*                                                                               
PRNT5    DS    0H                                                               
         MVC   0(29,R6),=C'** UNPAID NET INCLUDES TAX **'                       
         CLC   PROGPROF+2(2),=C'YN'                                             
         BE    PRNT10                                                           
         MVC   0(33,R6),=C'** UNPAID NET INCLUDES GST/PST **'                   
         CLC   PROGPROF+2(2),=C'NY'                                             
         BE    PRNT10                                                           
         MVC   0(41,R6),=C'** UNPAID NET INCLUDES TAX AND GST/PST **'           
         CLC   PROGPROF+2(2),=C'YY'                                             
         BE    PRNT10                                                           
         MVC   0(41,R6),SPACES                                                  
PRNT10   DS    0H                                                               
         GOTO1 REPORT                                                           
         XMOD1 1                                                                
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPMEDBLOCK                                                     
         EJECT                                                                  
       ++INCLUDE SPDNWRK                                                        
         EJECT                                                                  
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'030SPREPDN03 10/08/12'                                      
         END                                                                    
