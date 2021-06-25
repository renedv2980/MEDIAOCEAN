*          DATA SET NESFM02S   AT LEVEL 085 AS OF 05/01/02                      
*PHASE T31C02A,*                                                                
*INCLUDE BINSRCH2                                                               
***********************************************************************         
*                                                                               
*  TITLE: T31C02 - MAINTENANCE/LIST OF PRODUCT RECORDS                          
*                                                                               
*  COMMENTS: MAINTAINS PRODUCT RECORDS                                          
*                                                                               
*  CALLED FROM: NET SFM CONTROLLER (T31C40), WHICH CALLS                        
*               DDGENCON (T00A30) WHICH CALLS THIS.                             
*                                                                               
*  CALLS TO:    DATAMGR                                                         
*                                                                               
*  INPUTS: SCREENS NESFMC3 (T31CC3) -- MAINTENANCE                              
*                  NESFMC4 (T31CC4) -- LIST                                     
*                                                                               
*  OUTPUTS: UPDATED OR NEW PRODUCTS                                             
*                                                                               
*  LOCALS: REGISTER USAGE                                                       
*          R0 - WORK                                                            
*          R1 - WORK                                                            
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CURSOR              
*          R3 - WORK                                                            
*          R4 - WORK                                                            
*          R5 - WORK                                                            
*          R6 - USED FOR GETEL ELEMENT DSECT POINTER                            
*          R7 - WORK                                                            
*          R8 - SPOOLD                                                          
*          R9 - SYSD                                                            
*          RA - TWA                                                             
*          RB - FIRST BASE                                                      
*          RC - GEND                                                            
*          RD - SYSTEM                                                          
*          RE - SYSTEM                                                          
*          RF - SYSTEM                                                          
*                                                                               
*   HIST: (PLEASE INSERT BRIEF DESCRIPTION OF UPDATES)                          
*                        ******* READ THIS !!!!!! *******                       
*  06DEC/91  (SKU)  ---  WAITING FOR CHRIS O'CONNOR TO UPDATE TERMINAL          
*                        RECS. BEFORE WE CAN RUN THIS VERSION                   
*                   ---  PRODUCT DELETE UPDATES PRODUCT CODE LIST IN            
*                        CLIENT RECORD                                          
*                                                                               
*  20JAN/92  (SKU)  ---  NEW SECURITY ROUTINE TO CHECK CLIENT ACCESS.           
*                        TERMINAL RECS. ARE READY (SEE PREVIOUS)                
*                                                                               
*  14FEB/92  (SKU)  ---  LEFT ALIGN COMMISSION RATE                             
*                                                                               
*  24MAR/92  (SKU)  ---  FIX BUG TO DISPLAY BILL FORMULA                        
*                                                                               
*  25NOV/92  (SKU)  ---  FIX DELETED BUG                                        
*                                                                               
***********************************************************************         
         TITLE 'T31C02 NETWORK PRODUCT RECORD'                                  
T31C02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T31C02,RR=R2                                                   
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         LA    R7,2048(RB)                                                      
         LA    R7,2048(R7)                                                      
         USING T31C02,RB,R7                                                     
         ST    R2,RELO                                                          
         MVI   IOOPT,C'Y'          CONTROL MY OWN ADDREC/PUTREC                 
*                                                                               
         GOTO1 VTERMACC            CHECK TERMINAL ACCESS                        
*                                                                               
VALK     CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,RECDEL         CAN'T SPECIFY DELETE HERE                    
         BE    DELMSG                                                           
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BNE   PRCHK                                                            
         GOTO1 =A(LISTREC),DMCB,(1,DUB),(R9),(RA),(RC),RR=RELO                  
PRCHK    CLI   MODE,PRINTREP       PRINT RECORDS                                
         BNE   EXIT                                                             
         GOTO1 =A(LISTREC),DMCB,(1,DUB),(R9),(RA),(RC),RR=RELO                  
EXIT     XIT1                                                                   
*                                                                               
         EJECT                                                                  
***********************************************************************         
*     VALIDATE KEY ROUTINE                                                      
***********************************************************************         
VK       DS    0H                                                               
         XC    SVKEY,SVKEY                                                      
         LA    R6,SVKEY                                                         
         USING PKEY,R6                                                          
         MVI   NOPTFLG,0                                                        
         SPACE                                                                  
         LA    R2,PRDMEDH              * MEDIA                                  
         GOTO1 VALIMED                                                          
         MVC   PKEYAM,BAGYMD                                                    
         SPACE                                                                  
         CLI   ACTNUM,ACTLIST      IF IT'S LIST                                 
         BE    VKX                                                              
         CLI   ACTNUM,ACTREP       IF IT'S REPORT                               
         BE    VKX                                                              
         SPACE                                                                  
VK50     LA    R2,PRDCLTH             * CLIENT                                  
         GOTO1 VALIFLD                                                          
         BZ    VK60                                                             
         GOTO1 VALICLT                                                          
         MVC   PKEYCLT,BCLT                                                     
*                                                                               
         CLI   PRDUSR1H+5,0        IS THERE SOMETHING IN HERE ALREADY?          
         BNE   *+10                                                             
         XC    PRDUSR1,PRDUSR1                                                  
         XC    PRDDSC1,PRDDSC1                                                  
         OI    PRDUSR1H+1,X'20'    SET PROTECTED                                
         CLC   SVP1USER,SPACES                                                  
         BNH   VK55                                                             
         MVC   PRDDSC1,SVP1USER                                                 
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BE    *+10                                                             
         MVC   PRDUSR1,PUSER1                                                   
         NI    PRDUSR1H+1,X'FF'-X'20'                                           
*                                                                               
VK55     CLI   PRDUSR2H+5,0        IS THERE SOMETHING IN HERE ALREADY?          
         BNE   *+10                                                             
         XC    PRDUSR2,PRDUSR2                                                  
         XC    PRDDSC2,PRDDSC2                                                  
         OI    PRDUSR2H+1,X'20'    SET PROTECTED                                
         CLC   SVP2USER,SPACES                                                  
         BNH   VK56                                                             
         MVC   PRDDSC2,SVP2USER                                                 
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BE    *+10                                                             
         MVC   PRDUSR2,PUSER2                                                   
         NI    PRDUSR2H+1,X'FF'-X'20'                                           
*                                                                               
VK56     OI    PRDDSC1H+6,X'80'                                                 
         OI    PRDDSC2H+6,X'80'                                                 
         OI    PRDUSR1H+6,X'80'                                                 
         OI    PRDUSR2H+6,X'80'                                                 
*                                                                               
VK60     LA    R2,PRDPRDH                                                       
         GOTO1 VALIFLD                                                          
         BZ    VKX                                                              
         CLI   ACTNUM,ACTADD       IF ITS ADD                                   
         BNE   VK70                                                             
         MVI   ERROR,INVALID                                                    
         CLI   5(R2),2             CHECK IT HERE                                
         BL    TRAPERR                                                          
         CLI   5(R2),3                                                          
         BH    TRAPERR                                                          
         CLI   NFLD,C'A'           FIRST POSITION MUST BE ALPHA                 
         BL    TRAPERR                                                          
         CLI   NFLD,C'Z'                                                        
         BH    TRAPERR                                                          
         MVC   PKEYPRD,NFLD                                                     
         MVC   QPRD,NFLD           ALSO SET QPRD                                
         B     VKX                                                              
VK70     DS    0H                                                               
         CLC   =C'AAA',8(R2)       FOR SOME REASON VALIPRD DOES NOT             
         BNE   VK80                LIKE =C'AAA' IN THE CONTROLLER?????          
         MVC   PKEYPRD,8(R2)                                                    
         B     VKX                                                              
VK80     GOTO1 VALIPRD                                                          
         MVC   PKEYPRD,QPRD                                                     
         SPACE                                                                  
VKX      DS    0H                                                               
         LA    R2,PRDMEDH                                                       
         MVC   KEY,SVKEY                                                        
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY KEY                                                                   
***********************************************************************         
DK       DS    0H                                                               
         L     R6,AIO                                                           
         USING PKEY,R6                                                          
         MVI   PRDMED,C'N'                   * MEDIA                            
         OI    PRDMEDH+6,X'80'     XMIT                                         
         SPACE                                                                  
         GOTO1 CLUNPK,DMCB,PKEYCLT,PRDCLT    * CLIENT                           
         OI    PRDCLTH+6,X'80'     XMIT                                         
         SPACE                                                                  
         CLI   PKEYPRD,X'FF'                 * PRODUCT                          
         BNE   *+14                                                             
         MVC   PRDPRD(3),=C'POL'                                                
         B     *+10                                                             
         MVC   PRDPRD,PKEYPRD                                                   
         OI    PRDPRDH+6,X'80'     XMIT                                         
         DROP  R6                                                               
*                                                                               
         OC    T31CFFD+6(2),T31CFFD+6  TEST ANY SECURITY LIMIT                  
         BZ    DKX                                                              
         CLI   T31CFFD+6,C'*'          TEST OFFICE LOCKOUT                      
         BE    DK10                                                             
         CLC   T31CFFD+6(2),BCLT       ELSE SINGLE CLIENT ACCESS                
         BNE   LOCKOUT                                                          
         B     DKX                                                              
*                                                                               
DK10     DS    0H                                                               
         L     R6,AIO1                                                          
         USING CLTHDR,R6                                                        
         CLC   T31CFFD+7(1),COFFICE   MATCH OFFICE CODE                         
         BNE   LOCKOUT                                                          
         DROP  R6                                                               
*                                                                               
DKX      DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY RECORD                                                                
***********************************************************************         
DR       DS    0H                                                               
         CLI   ACTNUM,ACTSEL                                                    
         BNE   DR010                                                            
         MVC   PREVKEY,KEY                                                      
         MVI   PREVFLAG,C'Y'                                                    
*                                                                               
DR010    LA    R2,PRDNAMEH                 CLEAR SCREEN                         
         BAS   RE,CLRSCRN                                                       
         XC    PRDLDAT,PRDLDAT                                                  
         OI    PRDLDATH+6,X'80'    XMIT                                         
*                                                                               
         L     R6,AIO                                                           
         USING PKEY,R6                                                          
*                                           PRODUCT NAME                        
         MVC   PRDNAME,PNAME                                                    
         OI    PRDNAMEH+6,X'80'    XMIT                                         
*                                           PRODUCT/CLIENT CODE                 
         CLI   PACCT,X'FF'                                                      
         BNE   DR015                                                            
         UNPK  PRDACCT(5),PACCT+1(3)                                            
         OI    PRDACCTH+6,X'80'    XMIT                                         
         B     DR024                                                            
*                                                                               
DR015    MVC   PRDACCT(4),PACCT                                                 
         OI    PRDACCTH+6,X'80'    XMIT                                         
*                                           PRODUCT CLASS                       
         LA    R2,PRDCLASH                                                      
         CLI   PCLASS,0                                                         
         BE    DR024                                                            
         MVC   FULL(1),PCLASS                                                   
         MVI   FULL+1,C' '                                                      
         CLI   PCLASS,X'99'        TEST 2 CLASSES                               
         BH    DR020               NO                                           
         PACK  FULL(1),PCLASS      FIRST CLASS                                  
         NI    FULL,X'0F'                                                       
         OI    FULL,X'C0'          MAKE A - I                                   
         CLI   PCLASS+1,0                                                       
         BE    DR020                                                            
         MVC   FULL+1(1),PCLASS    2ND CLASS                                    
         NI    FULL+1,X'0F'                                                     
         OI    FULL+1,X'C0'        MAKE A - I                                   
DR020    MVC   PRDCLAS(2),FULL                                                  
         OI    PRDCLASH+6,X'80'    XMIT                                         
*                                                                               
DR024    CLI   PLOCK,X'40'         PRODUCT LOCKOUT                              
         BNH   DR030                                                            
         MVC   PRDLOCK,PLOCK                                                    
         OI    PRDLOCKH+6,X'80'    XMIT                                         
         GOTO1 DATCON,DMCB,(2,PLKDAT),(8,PRDLDAT)                               
         OI    PRDLDATH+6,X'80'    PROD LOCK ACTV DATE                          
*                                           PRODUCT ADDRESS 1                   
DR030    MVC   PRDBNAM(30),PADDR1                                               
         OI    PRDBNAMH+6,X'80'    XMIT                                         
*                                           PRODUCT ADDRESS 2                   
         MVC   PRDADD2(30),PADDR2                                               
         OI    PRDADD2H+6,X'80'    XMIT                                         
*                                           PRODUCT ADDRESS 3                   
         MVC   PRDADD3(30),PADDR3                                               
         OI    PRDADD3H+6,X'80'    XMIT                                         
*                                           PRODUCT ADDRESS 4                   
         MVC   PRDADD4(30),PADDR4                                               
         OI    PRDADD4H+6,X'80'    XMIT                                         
*                                           PRODUCT AGENCY FEE                  
         LA    R2,PRDOAFH                                                       
         CLC   PAGYFEE(2),=6X'00'                                               
         BE    DR050                                                            
         EDIT  (2,PAGYFEE),(5,PRDOAF),2                                         
         OI    6(R2),X'80'         XMIT                                         
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                            BILL BASIS                         
DR050    XC    PRDBBAS,PRDBBAS                                                  
         XC    PRDCPCT,PRDCPCT                                                  
         XC    PRDCBAS,PRDCBAS                                                  
         XC    PRDEDAT,PRDEDAT                                                  
         OI    PRDBBASH+6,X'80'    XMIT                                         
         OI    PRDCPCTH+6,X'80'    XMIT                                         
         OI    PRDCBASH+6,X'80'    XMIT                                         
         OI    PRDEDATH+6,X'80'    XMIT                                         
*                                                                               
         OC    PBILLBAS(5),PBILLBAS                                             
         BZ    DR095                                                            
*                                                                               
         LA    R2,PRDBBASH                                                      
         MVC   8(5,R2),=CL5'CNET'                                               
         TM    PBILLBAS,X'50'                                                   
         BO    DR060                                                            
         MVC   PRDBBAS,=CL5'NET'                                                
         TM    PBILLBAS,X'10'                                                   
         BO    DR060                                                            
         MVC   PRDBBAS,=C'CGROS'                                                
         TM    PBILLBAS,X'40'                                                   
         BO    DR060                                                            
         MVC   PRDBBAS,=C'GROSS'                                                
*                                                                               
DR060    OI    6(R2),X'80'         XMIT                                         
*                                                                               
         L     R5,PBILLCOM                                                      
         LTR   R5,R5                                                            
         BZ    DR095                                                            
*                                             COMMISION PERCENT                 
         LPR   RF,R5                                                            
         C     RF,=F'1000000'      +/-100.0000 WONT FIT                         
         BNE   DR070                                                            
         MVC   PRDCPCT+1(3),=C'100'                                             
         B     DR075                                                            
DR070    EDIT  (R5),(8,PRDCPCT),4,FLOAT=+,ALIGN=LEFT                            
DR075    LTR   R5,R5                                                            
         BNM   *+8                                                              
         MVI   PRDCPCT,C'-'        SET TO MINUS                                 
         OI    PRDCPCTH+6,X'80'    XMIT                                         
*                                  COMMISION BASIS                              
DR080    LA    R2,PRDCBASH                                                      
         MVC   8(5,R2),=C'GROSS'                                                
         TM    PBILLBAS,X'01'                                                   
         BZ    DR090                                                            
         MVC   8(5,R2),=C'NET  '                                                
DR090    OI    6(R2),X'80'         XMIT                                         
*                                  EFFECTIVE DATE                               
DR095    DS    0H                                                               
         CLC   PBILLDT,=6X'00'                                                  
         BE    DR100                                                            
         GOTO1 DATCON,DMCB,(3,PBILLDT),(9,PRDEDAT)                              
         LA    R2,PRDEDATH                                                      
         OI    6(R2),X'80'         XMIT                                         
*                                                                               
DR100    XC    PRDUSR1,PRDUSR1                                                  
         XC    PRDDSC1,PRDDSC1                                                  
         OI    PRDUSR1H+1,X'20'    SET PROTECTED                                
         CLC   SVP1USER,SPACES                                                  
         BNH   *+20                                                             
         MVC   PRDDSC1,SVP1USER                                                 
         MVC   PRDUSR1,PUSER1                                                   
         NI    PRDUSR1H+1,X'FF'-X'20'                                           
*                                                                               
         XC    PRDUSR2,PRDUSR2                                                  
         XC    PRDDSC2,PRDDSC2                                                  
         OI    PRDUSR2H+1,X'20'    SET PROTECTED                                
         CLC   SVP2USER,SPACES                                                  
         BNH   *+20                                                             
         MVC   PRDDSC2,SVP2USER                                                 
         MVC   PRDUSR2,PUSER2                                                   
         NI    PRDUSR2H+1,X'FF'-X'20'                                           
*                                                                               
         OI    PRDDSC1H+6,X'80'                                                 
         OI    PRDDSC2H+6,X'80'                                                 
         OI    PRDUSR1H+6,X'80'                                                 
         OI    PRDUSR2H+6,X'80'                                                 
*  OPTIONS                                                                      
         XC    PRDOPTS,PRDOPTS                                                  
         LA    R2,PRDOPTSH                                                      
         TM    POPT1,POPT1_NOBILL                                               
         BZ    *+10                                                             
         MVC   8(7,R2),=C'BILL=NO'                                              
         OI    6(R2),X'80'         XMIT                                         
*                                             END OF DISPLAY                    
DR120    DS    0H                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
DELMSG   DS    0H                                                               
         LA    R2,CONACTH                                                       
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(39),=C'ACTION DELETE INVALID - PLEASE RE-ENTER'          
         OI    CONHEADH+6,X'80'    XMIT                                         
         GOTO1 ERREX2                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE RECORD                                                               
***********************************************************************         
VR       L     R6,AIO                                                           
         USING PKEY,R6                                                          
         LA    R2,PRDNAMEH         PRODUCT NAME                                 
         GOTO1 ANY                                                              
         MVI   ERROR,INVALID                                                    
         CLC   8(6,R2),=C'DELETE'                                               
         BNE   VR010                                                            
         CLI   T31CFFD+1,C'*'      DDS ONLY CAN DELETE                          
         BNE   TRAPERR                                                          
         CLI   ACTNUM,ACTADD                                                    
         BE    TRAPERR                                                          
*  ALLOW DELETE OF PRODUCT HEADER IF NO ESTIMATES EXIST FOR THAT PROD           
         MVC   SAVEKEY,KEY                                                      
         XC    KEY,KEY                                                          
         MVC   KEY(7),SAVEKEY                                                   
         MVI   KEY+7,1                                                          
         GOTO1 HIGH                                                             
         CLC   KEY(7),KEYSAVE                                                   
         BNE   DEL10                                                            
         CLI   KEY+7,0                                                          
         BNE   ESTERR                                                           
DEL10    DS    0H                  DELETE PRODUCT HEADER                        
         MVC   KEY,SAVEKEY                                                      
         MVI   KEY+13,X'DD'                                                     
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'SPTDIR  ',KEY,KEY                      
         L     RE,AIO                                                           
         MVI   15(RE),X'C0'                                                     
         GOTO1 DATAMGR,DMCB,=C'PUTREC',=CL8'SPTFILE',KEY+14,AIO,MYDMWRK         
*                                                                               
*  CLEAR PRODUCT CODE FROM CLIENT CODE LIST IN CLIENT RECORD                    
*                                                                               
         MVC   AIO,AIO2                                                         
         XC    KEY,KEY                                                          
         L     R1,AIO1                                                          
         MVC   KEY+1(3),1(R1)                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATAMGR,DMCB,(X'80',=C'DMRDIR'),=C'SPTFILE ',KEY+14,AIO          
*                                                                               
         LA    R0,SVCLIST          CLEAR SVCLIST                                
         LA    R1,880                                                           
         LR    RE,R0                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         MVC   QPRD,PRDPRD         PRODUCT CODE                                 
         OC    QPRD,SPACES         BLANK PAD                                    
*                                                                               
         L     R6,AIO2                                                          
         USING CLTHDR,R6                                                        
         LA    R3,SVCLIST                                                       
         LA    R4,CLIST                                                         
DEL20    CLI   0(R4),0             DO NOT COPY NULL ENTRIES                     
         BE    DEL30                                                            
         CLC   QPRD,0(R4)          JUNK DELETED PRODUCT CODE                    
         BE    DEL30                                                            
         MVC   0(4,R3),0(R4)                                                    
         LA    R3,4(R3)                                                         
DEL30    LA    R4,4(R4)                                                         
         LA    RF,CLIST+880                                                     
         CR    R4,RF                                                            
         BL    DEL20               CHECK END-OF-LIST                            
*                                                                               
         LA    R4,SVCLIST          MOVE IN UPDATED LIST                         
         LA    RE,CLIST                                                         
         LA    RF,880                                                           
         MVCL  RE,R4                                                            
         DROP  R6                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'SPTFILE ',KEY+14,AIO                   
*                                                                               
         MVC   AIO,AIO1            RESTORE AIO                                  
*                                                                               
         MVC   PRDNAME(19),=C'* PRODUCT DELETED *'                              
         OI    6(R2),X'80'         XMIT                                         
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
VR010    L     R6,AIO                                                           
         USING PRODD,R6                                                         
         MVC   PNAME,PRDNAME                                                    
         LA    R2,PRDACCTH         CLT/PRD CODE                                 
         GOTO1 ANY                                                              
         MVI   ERROR,INVALID                                                    
*                                                                               
         MVC   AGYALPHA,14(RA)                                                  
         CLC   =C'GY',AGYALPHA                                                  
         BE    VR030                                                            
         CLC   =C'DR',AGYALPHA                                                  
         BE    VR030                                                            
         CLC   =C'GN',AGYALPHA                                                  
         BE    VR030                                                            
         CLC   =C'CE',AGYALPHA                                                  
         BE    VR030                                                            
         CLC   =C'FM',AGYALPHA                                                  
         BE    VR030                                                            
         CLC   =C'RE',AGYALPHA                                                  
         BE    VR030                                                            
         CLI   5(R2),4                                                          
         BNE   TRAPERR                                                          
         CLC   =C'BM ',PRDCLT       BRISTOL MYERS                               
         BNE   VR020                                                            
         CLC   =C'BO',AGYALPHA                                                  
         BNE   VR020                                                            
         TM    4(R2),X'08'         MUST BE 4 NUMERICS                           
         BZ    TRAPERR                                                          
*                                                                               
VR020    MVC   PACCT(4),8(R2)                                                   
         B     VR040                                                            
VR030    CLI   5(R2),5         FM,GY,DR,GN,CE MUST BE 5 NUMERICS                
         BNE   TRAPERR                                                          
         TM    4(R2),X'08'                                                      
         BZ    TRAPERR                                                          
         PACK  PACCT(4),8(5,R2)                                                 
         MVI   PACCT,X'FF'                                                      
         B     VR040                                                            
*                                                                               
VR040    DS    0H                                                               
         LA    R2,PRDCLASH         PROD CLASS(SINGLE OR DOUBLE)                 
         MVI   BYTE,0                                                           
         CLI   5(R2),0                                                          
         BE    VR050               NO CLASS                                     
         MVC   FULL(2),8(R2)                                                    
         MVC   BYTE,FULL                                                        
         CLI   FULL+1,C' '         IS 2ND CLASS GIVEN                           
         BNH   VR050               NO                                           
*                                                                               
         CLI   FULL,C'A'           BOTH MUST BE A-I                             
         BL    VR060                                                            
         CLI   FULL,C'I'                                                        
         BH    VR060                                                            
         CLI   FULL+1,C'A'                                                      
         BL    VR060                                                            
         CLI   FULL+1,C'I'                                                      
         BH    VR060                                                            
*                                                                               
         CLC   FULL(1),FULL+1      AND NOT THE SAME                             
         BE    VR060                                                            
*                                  HOLD AS 2 NIBBLES 1-9                        
         NI    FULL,X'0F'                                                       
         NI    FULL+1,X'0F'                                                     
         PACK  BYTE,FULL(1)        1ST CLASS                                    
         OC    BYTE,FULL+1         2ND CLASS                                    
*                                                                               
VR050    MVC   PCLASS,BYTE                                                      
*                                                                               
         LA    R2,PRDLOCKH         PRODUCT LOCKOUT                              
         CLI   5(R2),0                                                          
         BE    VR070                                                            
         CLI   8(R2),C'L'                                                       
         BE    VR053                                                            
         CLI   8(R2),C'U'                                                       
         BNE   VR060                                                            
VR053    MVC   PLOCK,8(R2)                                                      
         GOTO1 DATCON,DMCB,(5,0),(2,PLKDAT)     PROD LOCK ACTV DATE             
         B     VR070                                                            
*                                                                               
VR060    MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
         EJECT                                                                  
*                                                                               
VR070    LA    R2,PRDBNAMH                                                      
         GOTO1 ANY                                                              
         XC    PADDR1(120),PADDR1                                               
         MVC   PADDR1,PRDBNAM                                                   
         OC    PADDR1,SPACES                                                    
         LA    R2,PRDADD2H                                                      
         CLI   5(R2),0                                                          
         BE    *+16                                                             
         MVC   PADDR2,8(R2)                                                     
         OC    PADDR2,SPACES                                                    
         LA    R2,PRDADD3H                                                      
         CLI   5(R2),0                                                          
         BE    *+16                                                             
         MVC   PADDR3,8(R2)                                                     
         OC    PADDR3,SPACES                                                    
         LA    R2,PRDADD4H                                                      
         CLI   5(R2),0                                                          
         BE    VR080                                                            
         MVC   PADDR4,8(R2)                                                     
         OC    PADDR4,SPACES                                                    
*                                                                               
*                                                                               
VR080    LA    R2,PRDOAFH                                                       
         CLI   5(R2),0                                                          
         BNE   VR090                                                            
         XC    PAGYFEE,PAGYFEE                                                  
         B     VR100                                                            
*                                                                               
VR090    SR    R0,R0                                                            
         IC    R0,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,(2,PRDOAF),(R0)                                     
         CLI   DMCB,X'FF'                                                       
         BNE   *+8                                                              
         B     TRAPERR                                                          
         L     R0,DMCB+4                                                        
         CVD   R0,DUB                                                           
         CP    DUB,=P'999'        CAN'T EXCEED 9.99                             
         BH    TRAPERR                                                          
         ZAP   PAGYFEE,DUB                                                      
VR100    MVI   ERROR,INVALID                                                    
VR110    LA    R2,PRDBBASH                                                      
         CLI   5(R2),0                                                          
         BNE   VR120                                                            
         XC    PBILLBAS(5),PBILLBAS                                             
         B     VR140                                                            
         EJECT                                                                  
*                                                                               
VR120    MVI   ERROR,INVALID                                                    
         XC    PBILLBAS(5),PBILLBAS                                             
         GOTO1 ANY                      R2 $S AT ESTBBASH                       
         SR    R4,R4                                                            
         IC    R4,5(R2)                                                         
         BCTR  R4,0                                                             
         LA    R5,8(R2)                                                         
         CLI   8(R2),C'C'          CHK FOR COMMISSION ONLY                      
         BNE   VR130                                                            
         OI    PBILLBAS,X'40'                                                   
         BCTR  R4,0                                                             
         CLI   5(R2),1                                                          
         BE    TRAPERR             NO 'C' ALONE                                 
         LA    R5,1(R5)            BUMP PAST 'C'                                
VR130    EX    R4,GROSCOM                                                       
         BE    VR140                                                            
         EX    R4,NETCOM                                                        
         BNE   TRAPERR                                                          
*                                       CHECK PROFILE                           
         OI    PBILLBAS,X'10'                                                   
         B     VR140                                                            
*                                                                               
GROSCOM  CLC   0(0,R5),=C'GROSS'        ALLOW G-GROSS                           
NETCOM   CLC   0(0,R5),=C'NET  '        OR N-NET                                
*                                                                               
VR140    LA    R2,PRDCPCTH                                                      
         CLI   5(R2),0                  NOT REQUIRED                            
         BE    VR160                                                            
         SR    R0,R0                                                            
         IC    R0,5(R2)                                                         
         BCTR  R0,R0                                                            
         GOTO1 CASHVAL,DMCB,(4,PRDCPCT+1),(R0)                                  
         CLI   DMCB,X'FF'                                                       
         BNE   *+8                                                              
         B     TRAPERR                                                          
         L     R0,DMCB+4                                                        
         C     R0,=F'1000000'      100.0000 MAX                                 
         BH    TRAPERR                                                          
         C     R0,=F'0'                                                         
         BNH   TRAPERR                                                          
         CLI   PRDCPCT,C'-'                                                     
         BNE   VR150                    ERROR                                   
         LCR   R0,R0                    MAKE NEGATIVE                           
VR150    ST    R0,FULL                                                          
         MVC   PBILLCOM,FULL                                                    
         B     VR170                                                            
VR160    MVI   ERROR,INVALID            PRIOR (MSSNGERR)                        
         CLI   PRDCBASH+5,0             REQUIRED IF COM BASIS PRESENT           
         BNE   TRAPERR                                                          
         B     VR170                                                            
         EJECT                                                                  
*                                                                               
VR170    LA    R2,PRDCBASH                                                      
         CLI   5(R2),0                  NOT REQUIRED                            
         BE    VR190                                                            
         MVI   ERROR,INVALID                                                    
         SR    R4,R4                                                            
         IC    R4,5(R2)                                                         
         BCTR  R4,0                                                             
         LA    R5,8(R2)                                                         
         EX    R4,GROSCOM                                                       
         BE    VR180                                                            
         EX    R4,NETCOM                                                        
         BNE   TRAPERR                                                          
         OI    PBILLBAS,X'01'                                                   
VR180    B     VR200                                                            
*                                                                               
VR190    MVI   ERROR,INVALID            PRIOR (MSSNGERR)                        
         CLI   PRDCPCTH+5,0                                                     
         BNE   TRAPERR                                                          
*                                                                               
VR200    OC    PBILLBAS(5),PBILLBAS           WILL BE ZEROS IF                  
         BNZ   VR210                          GROSS ALONE                       
         CLI   PRDBBASH+5,0                                                     
         BE    VR230           NO FORMULA                                       
*                              MUST HAVE BEEN GROSS ALONE                       
*                              PUT X'80' SO BILLING WILL THINK IT'S             
*                              A FORMULA                                        
         B     VR220                                                            
VR210    OC    PBILLCOM,PBILLCOM                                                
         BNZ   VR230                                                            
         CLI   PBILLBAS,X'40'      WAS GROSS ALONE + COMMISSION ONLY            
         BNE   VR230                                                            
VR220    OI    PBILLBAS,X'80'                                                   
*                                                                               
VR230    LA    R2,PRDEDATH            EFFECTIVE DATE OF BILL FORMULA            
         OC    PBILLBAS(5),PBILLBAS           SEE IF FORMULA INPUT              
         BNZ   VR240                                                            
         XC    PBILLDT,PBILLDT                                                  
         CLI   5(R2),0                                                          
         BE    VR245                                                            
         MVI   ERROR,INVALID      NO FORMULA SO EFF DATE INVALID                
         B     TRAPERR                                                          
*                                                                               
VR240    GOTO1 ANY          REQUIRED IF FORMULA INPUT                           
         GOTO1 DATVAL,DMCB,(2,PRDEDAT),WORK                                     
         MVI   ERROR,INVALID       PRIOR (DATERR)                               
         OC    DMCB(4),DMCB                                                     
         BZ    TRAPERR                                                          
         GOTO1 DATCON,DMCB,(0,WORK),(3,WORK+10)                                 
         MVC   PBILLDT,WORK+10           YM                                     
         B     VR245                                                            
*                                                                               
VR245    XC    USERDATA,USERDATA                                                
         OC    SVP1USER,SVP1USER   ANY "PRODUCT 1" INFO?                        
         BZ    VR247                NO, DO "PRODUCT 2".                         
         LA    R2,PRDUSR1H                                                      
         ST    R2,AUSR             AUSR=A(INPUT FIELD).                         
         MVC   UTYPE,SVP1TYPE      TYPE.                                        
         MVC   LEN,SVP1LEN         LENGTH.                                      
         MVC   FLAG1,SVP1FLG1      1ST FLAG                                     
         MVC   FLAG2,SVP1FLG2      2ND FLAG                                     
         BAS   RE,EDTUSR                                                        
*                                                                               
VR247    MVC   PUSER1,USERDATA                                                  
         MVC   PRDUSR1,USERDATA    CLEAR OR RE-TRANSMIT FIELD.                  
         OI    PRDUSR1H+6,X'80'                                                 
*                                                                               
         XC    USERDATA,USERDATA                                                
         OC    SVP2USER,SVP2USER   ANY "PRODUCT 2" INFO?                        
         BZ    VR248                NO, MOVE ON.                                
         LA    R2,PRDUSR2H                                                      
         ST    R2,AUSR             A(INPUT FIELD).                              
         MVC   UTYPE,SVP2TYPE      TYPE.                                        
         MVC   LEN,SVP2LEN         LENGTH.                                      
         MVC   FLAG1,SVP2FLG1      1ST FLAG                                     
         MVC   FLAG2,SVP2FLG2      2ND FLAG                                     
         BAS   RE,EDTUSR                                                        
*                                                                               
VR248    MVC   PUSER2,USERDATA                                                  
         MVC   PRDUSR2,USERDATA    CLEAR OR RE-TRANSMIT FIELD.                  
         OI    PRDUSR2H+6,X'80'                                                 
*  OPTIONS                                                                      
         NI    POPT1,X'FF'-POPT1_NOBILL  TURN OFF FLAG IN CASE ERASED           
         LA    R2,PRDOPTSH           CALL SCANNER TO BREAK UP OPTIONS           
         CLI   5(R2),0                                                          
         BE    VR250                                                            
         GOTO1 SCANNER,DMCB,(R2),BLOCK                                          
         ZICM  R0,DMCB+4             # OF OPTIONS SCANNED                       
*                                                                               
         LA    R5,BLOCK                                                         
         ZICM  R1,0(R5)                                                         
*                                                                               
         MVI   ERROR,INVALID                                                    
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R5),=C'BILL'                                                
         BE    VR249                                                            
         B     TRAPERR                                                          
*                                                                               
*                                                                               
VR249    DS    0H                                                               
         ZICM  R1,1(R5),1            LEN OF SECOND HALF OF FIELD                
         BZ    TRAPERR                                                          
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   22(0,R5),=C'YES'                                                 
         BE    VR250                                                            
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   22(0,R5),=C'NO'                                                  
         BNE   TRAPERR                                                          
         OI    POPT1,POPT1_NOBILL                                               
*                                                                               
*                                                                               
VR250    DS    0H                                                               
         MVC   PPROF(30),=30C'0'          SET DEFAULT PROFILE                   
         CLI   ACTNUM,ACTADD       IS IT ADD                                    
         BE    VR255               YES/GO AND ADD PRD TO CLIENT HEADER          
         CLI   ACTNUM,ACTSEL       IS IT SELECT/CHANGE                          
         BE    VR252                                                            
         CLI   ACTNUM,ACTCHA       IS IT CHANGE                                 
         BNE   VRXIT                                                            
VR252    CLC   DMDSKADD,KEY+14                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'SPTFILE ',KEY+14,AIO                   
         B     VRXIT                                                            
         EJECT                                                                  
*             NOW ADD PRODUCT TO CLTHDR                                         
VR255    MVC   SAVEKEY,KEY         SAVE KEY                                     
         LA    R1,SVCLIST                                                       
         CLC   QPRD,=C'POL'                                                     
         BNE   VR280                                                            
         MVI   PCODE+1,X'FF'                                                    
VR260    CLI   0(R1),0       ENDOF LIST                                         
         BE    VR270                                                            
         CLI   3(R1),X'FF'                                                      
         BE    VR400           POL ALREADY THERE                                
         LA    R1,4(R1)                                                         
         B     VR260                                                            
VR270    LA    R3,SVCLIST+876                                                   
         CR    R1,R3                                                            
         BNL   FULLERR              CLIST FULL                                  
         MVC   0(3,R1),=C'POL'                                                  
         MVI   3(R1),X'FF'                                                      
         B     VR400                                                            
*                                                                               
VR280    XC    ELEM(220),ELEM                                                   
         LA    R0,220                                                           
         SR    R4,R4             USED TO COUNT PRDS                             
VR290    CLI   0(R1),0                                                          
         BE    VR320                                                            
         CLI   3(R1),X'FF'                                                      
         BNE   VR300                                                            
         LA    R4,1(R4)                                                         
         MVC   0(3,R1),=3X'FF'             FOR PROPER LAST ENTRY                
*                                   OF BINSRCH TABLE                            
         B     VR310                                                            
*                                                                               
VR300    SR    R5,R5                                                            
         IC    R5,3(R1)                                                         
         LA    R5,ELEM-1(R5)                                                    
         MVI   0(R5),X'FF'   SET USED FLAG                                      
         LA    R4,1(R4)         BUMP PRD COUNTER                                
VR310    LA    R1,4(R1)                                                         
         B     VR290                                                            
*                                                                               
VR320    LA    R1,ELEM      FIND LOWEST UNUSED CODE                             
VR330    CLI   0(R1),0                                                          
         BE    VR340                                                            
         LA    R1,1(R1)                                                         
         B     VR330                                                            
*                                                                               
VR340    LA    R0,ELEM-1                                                        
         SR    R1,R0                                                            
         STH   R1,HALF                                                          
         MVC   PCODE,HALF                                                       
         B     VR350                                                            
         EJECT                                                                  
VR350    MVC   WORK(3),QPRD                                                     
         MVC   WORK+3(1),PCODE+1               R4 HAS NUMBER OF PRDS            
         GOTO1 =V(BINSRCH),DMCB,(X'01',WORK),SVCLIST,(R4),4,(0,3),219, X        
               RR=RELO                                                          
         OC    DMCB(4),DMCB                                                     
         BNZ   VR360                                                            
         B     FULLERR                 TOO MANY PRODUCTS                        
*                                                                               
VR360    CLI   DMCB,1                 PRD NOT IN LIST                           
         BE     VR370                                                           
         L     R1,DMCB          HAS ADDR OF PRD                                 
         MVC   PCODE+1(1),3(R1)                                                 
*                                                                               
VR370    LA    R1,SVCLIST                                                       
VR380    CLI   0(R1),0                                                          
         BE    VR400                                                            
         CLC   0(3,R1),=3X'FF'                                                  
         BE    VR390                                                            
         LA    R1,4(R1)                                                         
         B     VR380                                                            
*                                                                               
VR390    MVC   0(3,R1),=C'POL'          RESET TO POL                            
         B     VR400                                                            
*                                                                               
VR400    DS    0H                                                               
*                       WRITE BACK CLTHDR                                       
         MVC   AIO,AIO2                                                         
         XC    KEY,KEY                                                          
         L     R1,AIO1                                                          
         MVC   KEY+1(3),1(R1)                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATAMGR,DMCB,(X'80',=C'DMRDIR'),=C'SPTFILE ',KEY+14,AIO          
*                                                                               
         L     R6,AIO2                                                          
         USING CLTHDR,R6                                                        
         LA    R4,SVCLIST                                                       
         LA    RE,CLIST                                                         
         LA    RF,880                                                           
         MVCL  RE,R4                                                            
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'SPTFILE ',KEY+14,AIO                   
*                                                                               
         MVC   KEY,SAVEKEY         RESET PROGREC READING IN AIO1                
         MVC   AIO,AIO1                                                         
         L     R6,AIO                                                           
         USING PKEY,R6                                                          
******   MVC   PLEN,=H'240'                                                     
         MVC   PLEN,=H'336'                                                     
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'SPTFILE ',KEY+14,AIO                   
*                                                                               
VRXIT    B     REQREC                                                           
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*         ON ENTRY:                                                             
*             AUSR = A(INPUT FIELD),                                            
*             LEN  = ALLOWABLE LENGTH OF INPUT,                                 
*             UTYPE = TYPE ALLOWABLE FOR INPUT,                                 
*             FLAG1 = FIRST FLAG,                                               
*             FLAG2 = SECOND FLAG.                                              
*         ON EXIT:                                                              
*             USERDATA = INPUTTED DATA OR NULLS.                                
*                                                                               
*=============================== EDTUSR ==============================*         
EDTUSR   NTR1                                                                   
         L     R3,AUSR                                                          
         CLI   5(R3),0             CHECK FOR ANY INPUT                          
         BNE   EDTUSR10            THERE IS INPUT, PROCESS IT                   
*                                                                               
         MVI   ERROR,MISSING       NO INPUT, ASSUME IT'S MISSING                
         TM    FLAG1,CFLGREQQ      WAS INPUT REQUIRED?                          
         BZ    XEDTUSR             NO, SO IT'S OK                               
         B     TRAPERR             YES IT WAS, SHOW ERROR                       
*                                                                               
EDTUSR10 MVI   ERROR,TOOLONG       ASSUME INPUT IS TOO LONG                     
         CLC   LEN,5(R3)           CHECK IF L(INPUT) IS VALID                   
         BL    TRAPERR             NOT VALID                                    
*                                                                               
         MVI   ERROR,INVALID       ASSUME INPUT IS NOT VALID.                   
         CLI   UTYPE,C' '          IS TYPE SUPPOSE TO BE "WILD"?                
         BNH   EDTUSR80                                                         
         CLI   UTYPE,C'C'          IS TYPE SUPPOSE TO BE CHARACTER?             
         BNE   EDTUSR60                                                         
         LA    R4,8(R3)            R4-->INPUT                                   
         ZIC   R1,5(R3)            R1=L(INPUT)                                  
*                                                                               
EDTUSR40 CLI   0(R4),C'0'          ALLOW ALL INPUT EXCEPT NUMBERS               
         BL    EDTUSR50                                                         
         CLI   0(R4),C'9'                                                       
         BNH   TRAPERR                                                          
*                                                                               
EDTUSR50 LA    R4,1(R4)            CHECK NEXT CHAR IN INPUT                     
         BCT   R1,EDTUSR40                                                      
         B     EDTUSR80                                                         
*                                                                               
EDTUSR60 CLI   UTYPE,C'N'          IS TYPE SUPPOSE TO BE NUMERIC?               
         BNE   EDTUSR70             NOPE, THEN IT'S S/B DATE                    
         BAS   RE,CHKNTYP           YES, SO IS THE INPUT NUMERIC?               
         BE    EDTUSR80              YEP, INPUT IS VALID.                       
         B     TRAPERR               NO, ERROR DETECTED.                        
*                                                                               
EDTUSR70 MVI   ERROR,INVDATE       ASSUME INVALID DATE FORMAT                   
         CLI   UTYPE,C'D'          IS UTYPE SUPPOSE TO BE DATE?                 
         BE    *+6                  YES, VALIDATE INPUT FOR DATE                
         DC    H'0'                 NO, SOMETHING IS AMISS.                     
         GOTO1 DATVAL,DMCB,(0,8(R3)),TDATE                                      
         OC    DMCB(4),DMCB        ANY ERRORS?                                  
         BZ    TRAPERR              YEP                                         
         L     R1,0(R1)            L'INPUT FIELD                                
         ZIC   R4,5(R3)                                                         
         SR    R1,R4                                                            
         BNZ   TRAPERR                                                          
*                                                                               
EDTUSR80 ZIC   R1,5(R3)            R1=L(INPUT).                                 
         BCTR  R1,0                                                             
         EXMVC R1,USERDATA,8(R3)   MOVE INPUT INTO USERDATA.                    
*                                                                               
XEDTUSR  XIT1                                                                   
         EJECT                                                                  
*-------------------- VALIDATE INPUT FOR NUMERICS --------------------*         
CHKNTYP  DS    0H                                                               
         ST    RE,SAVERE           SAVE RETURN ADDRESS                          
         LA    R4,8(R3)            R4-->INPUT                                   
         ZIC   R1,5(R3)            R1=L(INPUT)                                  
CHKN10   LA    R5,VALDNTBL         R5-->TABLE OF VALID DIGITS                   
CHKN20   CLC   0(1,R4),0(R5)       MATCH DIGIT BY DIGIT                         
         BE    CHKN30               MATCHED,                                    
         LA    R5,1(R5)             ELSE, TRY NEXT CHAR IN TABLE                
         CLI   0(R5),C'*'          ARE WE AT END-OF-TABLE YET?                  
         BE    XCHKN                YEP, EXIT WITH CC<>0                        
         B     CHKN20               NO, WE STILL HAVE HOPE                      
CHKN30   LA    R4,1(R4)            MATCHED, CHECK NEXT CHAR IN INPUT            
         BCT   R1,CHKN10                                                        
*                                                                               
XCHKN    L     RE,SAVERE                                                        
         LTR   R1,R1                                                            
         BR    RE                                                               
*                                                                               
VALDNTBL DC    C'0123456789-/*'                                                 
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
* GENERATE REQUEST RECORD                                                       
***********************************************************************         
REQREC   XC    REC(150),REC                                                     
         LA    R1,REC                                                           
         MVI   10(R1),41                                                        
         MVI   14(R1),106                                                       
         LA    R1,REC+26                                                        
         MVI   0(R1),X'40'                                                      
         MVC   1(79,R1),0(R1)                                                   
         MVC   0(2,R1),=C'41'                                                   
         MVC   2(2,R1),14(RA)                                                   
         MVC   4(1,R1),PRDMED                                                   
         MVC   5(3,R1),PRDCLT                                                   
         OC    5(3,R1),SPACES                                                   
         MVC   11(3,R1),PRDPRD                                                  
         OC    11(3,R1),SPACES                                                  
         MVC   68(7,R1),=C'CONTROL'                                             
         MVI   61(R1),C'P'                                                      
         MVI   63(R1),C'A'                                                      
         CLI   ACTNUM,ACTADD                                                    
         BE    *+8                                                              
         MVI   63(R1),C'C'                                                      
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'REQUEST',REC,REC                       
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
GETPRD   NTR1                                                                   
         LA    R2,SVCLIST                                                       
GP5      CLI   3(R2),0             IF E-O-F CLIST                               
         BE    GPX                 SET TO UNDEFINED                             
         CLC   0(3,R2),QPRD                                                     
         BE    GP10                                                             
         LA    R2,4(R2)            INCREMENT CLIST                              
         B     GP5                 RETURN TO LOOP                               
GP10     MVC   BPRD,3(R2)      SET 3 CHAR PRINTABLE PRD CODE                    
GPX      B     EXIT                                                             
         EJECT                                                                  
         SPACE 2                                                                
FNDUF    TM    1(R2),X'20'    FIND NEXT UNPROTECTED FIELD                       
         BCR   8,RE                                                             
FNDNXUF  SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BNE   FNDUF                                                            
         DC    H'0'                END OF SCREEN                                
         EJECT                                                                  
*                                                                               
CLRSCRN  NTR1                                                                   
         SR    RE,RE                                                            
*                                                                               
CS2      IC    RE,0(R2)                                                         
         SH    RE,=H'9'                                                         
         TM    1(R2),X'20'         SKIP PROTECTED FIELDS                        
         BO    CS4                                                              
         EX    RE,CSCLC                                                         
         BE    CS4                                                              
         EX    RE,CSOC                                                          
         BZ    CS4                                                              
         EX    RE,CSXC                                                          
         OI    6(R2),X'80'                                                      
*                                                                               
CS4      LA    R2,9(RE,R2)                                                      
         CLI   0(R2),9                                                          
         BH    CS2                                                              
         B     EXIT                                                             
*                                                                               
CSCLC    CLC   8(0,R2),SPACES                                                   
CSOC     OC    8(0,R2),8(R2)                                                    
CSXC     XC    8(0,R2),8(R2)                                                    
         EJECT                                                                  
*                                                                               
LOCKOUT  MVI   ERROR,SECLOCK                                                    
         LA    R2,PRDCLTH                                                       
         B     TRAPERR                                                          
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
         B     EXIT                                                             
*                                                                               
FULLERR  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(26),=C'*** ERROR CLIENT LIST FULL'                       
         B     MYERR                                                            
*                                                                               
NOCLIENT XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(26),=C'*** ERROR NO CLIENT RECORD'                       
         B     MYERR                                                            
*                                                                               
ESTERR   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(26),=C'*** ERROR EST REC ATTACHED'                       
         B     MYERR                                                            
*                                                                               
MYERR    GOTO1 ERREX2                                                           
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*--LIST RECORDS AND PRINT RECORDS ROUTINE                                       
***********************************************************************         
         DS    0F                                                               
         DROP  RB                                                               
LISTREC  NMOD1 0,**C02LR*                                                       
         USING LISTREC,RB                                                       
         L     R9,4(R1)                                                         
         L     RA,8(R1)                                                         
         L     RC,12(R1)                                                        
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         CLI   0(R1),1                                                          
         BE    LR                                                               
         B     LRXIT                                                            
*                                                                               
LR       CLI   MODE,PRINTREP                                                    
         BNE   LR02                                                             
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         LA    R1,HDRTN                                                         
         ST    R1,HEADHOOK                                                      
LR02     MVI   NLISTS,15           SET NUM OF LIST LINES                        
         MVC   AIO,AIO1                                                         
*                                                                               
         CLI   PREVFLAG,C'Y'                                                    
         BE    LR03                                                             
         OC    KEY,KEY                                                          
         BNZ   LR05                                                             
         MVC   KEY(2),SVKEY                                                     
         OC    PRDCLT,PRDCLT       CLIENT                                       
         BZ    LR05                                                             
         CLI   PRDCLTH+5,2                                                      
         BL    LR02A                                                            
         OC    PRDCLT,SPACES                                                    
         GOTO1 CLPACK,DMCB,PRDCLT,KEY+2                                         
         B     LR05                                                             
LR02A    XC    FULL,FULL                                                        
         MVC   FULL(3),=3C'A'                                                   
         ZIC   R1,PRDCLTH+5                                                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FULL(0),PRDCLT                                                   
         GOTO1 CLPACK,DMCB,FULL,KEY+2                                           
         B     LR05                                                             
*                                                                               
LR03     MVC   KEY,PREVKEY                                                      
         MVI   PREVFLAG,C'N'                                                    
*                                                                               
LR05     GOTO1 HIGH                                                             
         B     LR22                                                             
         SPACE                                                                  
LR20     GOTO1 SEQ                                                              
         SPACE                                                                  
LR22     CLC   KEY(2),KEYSAVE                                                   
         BNE   LRX                                                              
         CLC   KEY+4(3),=6X'00'                                                 
         BE    LR20                                                             
         CLC   KEY+7(6),=6X'00'                                                 
         BNE   LR20                                                             
LR30     OC    PRDPRD,PRDPRD       PRODUCT                                      
         BZ    LR35                                                             
         OC    PRDPRD,SPACES                                                    
         CLC   KEY+4(3),PRDPRD     PRODUCT                                      
         BNE   LR20                                                             
         SPACE                                                                  
LR35     GOTO1 GETREC                                                           
         SPACE                                                                  
         LA    R5,LISTAR                                                        
         USING PCLT,R5                                                          
         XC    LISTAR,LISTAR                                                    
         L     R6,AIO                                                           
         USING PKEY,R6                                                          
         GOTO1 CLUNPK,DMCB,PKEYCLT,PCLT     * CLIENT                            
         MVC   PPRD,PKEYPRD        * PRODUCT                                    
*                                                                               
         CLI   PACCT,X'FF'                                                      
         BNE   LR38                                                             
         UNPK  PCCDE(5),PACCT+1(3)                                              
         B     *+10                                                             
LR38     MVC   PCCDE(4),PACCT      * CLIENT/PRODUCT CODE                        
         CLI   PCLASS,0                                                         
         BE    LR40                                                             
         MVC   PCLAS(1),PCLASS                                                  
         MVI   PCLAS+1,C' '                                                     
         CLI   PCLASS,X'99'        TEST 2 CLASSES                               
         BH    LR40                NO                                           
         PACK  PCLAS(1),PCLASS     FIRST CLASS                                  
         NI    PCLAS,X'0F'                                                      
         OI    PCLAS,X'C0'         MAKE A - I                                   
         CLI   PCLASS+1,0                                                       
         BE    LR40                                                             
         MVC   PCLAS+1(1),PCLASS   2ND CLASS                                    
         NI    PCLAS+1,X'0F'                                                    
         OI    PCLAS+1,X'C0'       MAKE A - I                                   
*                                                                               
*                                   AGENCY FEE                                  
LR40     CLC   PAGYFEE(2),=6X'00'                                               
         BE    LR45                                                             
         EDIT  (2,PAGYFEE),(5,PRDOAF),2                                         
*                                                                               
LR45     MVC   PNME,PNAME            PRODUCT NAME                               
*                                                                               
         CLI   PLOCK,X'40'           PRODUCT LOCK                               
         BNH   LR48                                                             
         MVC   PLOK(1),PLOCK                                                    
         GOTO1 DATCON,DMCB,(2,PLKDAT),(8,PLOK+2)                                
*                                                                               
LR48     CLI   MODE,PRINTREP                                                    
         BE    LR50                                                             
         GOTO1 LISTMON                                                          
         B     LR20                GOTO READ SEQ                                
         SPACE                                                                  
LR50     DS    0H                                                               
         MVC   PBILLTO(30),PADDR1                                               
         XC    PBILLDAT(30),PBILLDAT                                            
         CLC   PBILLCOM(4),=6X'00'                                              
         BE    LR60                                                             
*                                                                               
         MVC   PBILLDAT(5),=CL5'CNET'                                           
         TM    PBILLBAS,X'50'                                                   
         BO    LR60                                                             
         MVC   PBILLDAT(5),=CL5'NET'                                            
         TM    PBILLBAS,X'10'                                                   
         BO    LR60                                                             
         MVC   PBILLDAT(5),=C'CGROS'                                            
         TM    PBILLBAS,X'40'                                                   
         BO    LR60                                                             
         MVC   PBILLDAT(5),=C'GROSS'                                            
LR60     DS    0H                                                               
         EDIT  (4,PBILLCOM),(9,PCOMMPCT),4,FLOAT=-                              
         MVC   PCOMMBAS(5),=C'GROSS'                                            
         TM    PBILLBAS,X'01'                                                   
         BZ    LR70                                                             
         MVC   PCOMMBAS(5),=C'NET  '                                            
LR70     DS    0H                                                               
         XC    PEFFMON(30),PEFFMON                                              
         CLC   PBILLDT,=6X'00'                                                  
         BE    LR80                                                             
         GOTO1 DATCON,DMCB,(3,PBILLDT),(9,PEFFMON)                              
         DROP  R5                                                               
LR80     DS    0H                                                               
* PRINTING THE LINE                                                             
         LA    R4,P                                                             
         USING PLINED,R4                                                        
         MVC   PCLT(PLEND-PCLT),LISTAR   KEY DATA FROM LIST TO P                
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         XC    PCLT(PLEND-PCLT),PCLT     KEY DATA FROM LIST TO P                
         MVC   PBILLTO(30),PADDR2                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         MVC   PBILLTO(30),PADDR3                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         MVC   PBILLTO(30),PADDR4                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         B     LR20                GET NEXT RECORD                              
         SPACE                                                                  
LRX      DS    0H                                                               
         B     LRXIT                                                            
         DROP  R4,R6                                                            
LRXIT    XIT1                                                                   
         EJECT                                                                  
*                                                                               
HEADING  DS    0H                                                               
         SSPEC H1,3,REQUESTOR                                                   
         SSPEC H3,3,C'MEDIA N'                                                  
         SSPEC H1,46,C'NETWORK PRODUCT RECORDS'                                 
         SSPEC H2,46,C'-----------------------'                                 
         SSPEC H1,93,AGYNAME                                                    
         SSPEC H2,93,AGYADD                                                     
         SSPEC H3,93,REPORT                                                     
         SSPEC H4,93,RUN                                                        
         SSPEC H5,103,PAGE                                                      
         DC    X'00'                                                            
*                                                                               
HDRTN    NTR1                                                                   
         LA    R2,H8                                                            
         USING PLINED,R2                                                        
         MVC   PCLT(3),=C'CLT'                                                  
         MVC   PCLT+132(3),=50C'-'                                              
         MVC   PPRD(3),=C'PRD'                                                  
         MVC   PPRD+132(3),=50C'-'                                              
         MVC   PCCDE(8),=C'C/P CODE'                                            
         MVC   PCCDE+132(8),=50C'-'                                             
         MVC   PCLAS(5),=C'CLASS'                                               
         MVC   PCLAS+132(5),=50C'-'                                             
         MVC   PAGYFE(3),=C'FEE'                                                
         MVC   PAGYFE+132(4),=50C'-'                                            
         MVC   PNME(12),=C'PRODUCT NAME'                                        
         MVC   PNME+132(20),=50C'-'                                             
         MVC   PLOK(4),=C'LOCK'                                                 
         MVC   PLOK+132(4),=50C'-'                                              
         MVC   PBILLTO+11(7),=C'BILL-TO'                                        
         MVC   PBILLTO+132(30),=50C'-'                                          
         MVC   PBILLDAT(9),=C'BILL DATA'                                        
         MVC   PBILLDAT+132(9),=50C'-'                                          
         MVC   PCOMMPCT+1(8),=C'COMM PCT'                                       
         MVC   PCOMMPCT+132(9),=50C'-'                                          
         MVC   PCOMMBAS(8),=C'COMM BAS'                                         
         MVC   PCOMMBAS+132(8),=50C'-'                                          
         MVC   PEFFMON(7),=C'EFF MON'                                           
         MVC   PEFFMON+132(8),=50C'-'                                           
         B     LRXIT                                                            
         DROP  R8                                                               
         EJECT                                                                  
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 2                                                                
*                                                                               
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
PLINED   DSECT                                                                  
         DS    CL2                                                              
PCLT     DS    CL3                                                              
         DS    CL1                                                              
PPRD     DS    CL3                                                              
         DS    CL1                                                              
PCCDE    DS    CL5                                                              
         DS    CL4                                                              
PCLAS    DS    CL2                                                              
         DS    CL4                                                              
PAGYFE   DS    CL4                                                              
         DS    CL1                                                              
PNME     DS    CL20                                                             
         DS    CL1                                                              
PLOK     DS    CL4                                                              
         DS    CL1                                                              
PBILLTO  DS    CL30                                                             
         DS    CL1                                                              
PBILLDAT DS    CL9                                                              
         DS    CL1                                                              
PCOMMPCT DS    CL9                                                              
         DS    CL1                                                              
PCOMMBAS DS    CL8                                                              
         DS    CL1                                                              
PEFFMON  DS    CL8                                                              
         DS    CL1                                                              
PLEND    EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FAFACTS                                                        
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE NESFMFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NESFMC3D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NESFMC4D                                                       
         EJECT                                                                  
       ++INCLUDE NESFMWORKD                                                     
         ORG   SYSSPARE                                                         
*                           *******  T31C02 WORK AREA  *******                  
WORKAREA DS    0F                                                               
RELO     DS    F                                                                
SAVERE   DS    F                                                                
MYDMWRK  DS    CL8                                                              
PREVFLAG DS    CL1                                                              
PREVKEY  DS    CL48                                                             
SAVEKEY  DS    CL48                                                             
AGYALPHA DS    H                                                                
AUSR     DS    A                                                                
FLAG1    DS    XL1                                                              
FLAG2    DS    XL1                                                              
LEN      DS    XL1                                                              
UTYPE    DS    XL1                                                              
TDATE    DS    CL6                                                              
USERDATA DS    CL32                                                             
REC      DS    CL150                                                            
         EJECT                                                                  
CLIENTD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
PRODD    DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'085NESFM02S  05/01/02'                                      
         END                                                                    
