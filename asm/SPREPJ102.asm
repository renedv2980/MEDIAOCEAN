*          DATA SET SPREPJ102  AT LEVEL 056 AS OF 05/09/05                      
*PHASE SPJ102A                                                                  
*INCLUDE SORTER                                                                 
         TITLE 'SPJ102 - JWT BUYER INCENTIVE REPORT'                            
SPJ102   CSECT                                                                  
         PRINT NOGEN,NOUHEAD                                                    
         NMOD1 0,SPJ102,RR=R8                                                   
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         USING SPJ102+4096,RC                                                   
         ST    R8,RELO                                                          
*                                                                               
         L     R9,0(R1)                                                         
         USING SPWORKD,R9,RA                                                    
         LA    RA,2048(R9)                                                      
         LA    RA,2048(RA)                                                      
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    SP10                                                             
         CLI   MODE,REQFRST                                                     
         BE    SP20                                                             
         CLI   MODE,CLTFRST                                                     
         BE    SP26                                                             
         CLI   MODE,REQLAST                                                     
         BE    SP200                                                            
EXIT     XIT1                                                                   
         EJECT                                                                  
* RUNFRST                                                                       
*                                                                               
SP10     DS    0H                                                               
         B     EXIT                                                             
*                                                                               
RELO     DS    A                                                                
         EJECT                                                                  
* REQFRST                                                                       
*                                                                               
SP20     DS    0H                                                               
         OI    RQOPTS,RQOPTS_1DEC  FORCE 1-DEC PRECISION                        
         MVC   SVQDATES,QSTART     SAVE REQUEST DATES                           
         XC    CLTTBCNT,CLTTBCNT   CLEAR CLIENT TABLE COUNT                     
*                                                                               
         LA    R0,BFREC                                                         
         ST    R0,BUFFIO                                                        
         L     R0,=A(BUFFALOC)                                                  
         ST    R0,BUFFBUFF                                                      
*                                                                               
         GOTO1 BUFFALO,DMCB,=C'SET',BUFFBUFF                                    
         SPACE 1                                                                
         L     R2,MEDBUFF          CLEAR MEDBUFF                                
         LA    R0,5                                                             
         XC    0(256,R2),0(R2)                                                  
         LA    R2,256(R2)                                                       
         BCT   R0,*-10                                                          
*                                                                               
         L     R2,MEDBUFF                                                       
         USING MEDBLOCK,R2                                                      
*                                                                               
         XC    MEDNUMWK,MEDNUMWK                                                
         MVC   MEDNUMMO,=F'3'                                                   
         MVC   MEDNUMQT,=F'1'                                                   
         MVC   MEDNUMPE,=F'1'                                                   
         MVI   RQDAYPT,C'Y'                                                     
         MVI   RQEQUIV,C'Y'                                                     
         MVI   MEDEXTDM,1          SET FOR 1 DEMO                               
         MVC   MEDLCHNK,=AL4(MEDBY2-MEDDATA)                                    
*                                                                               
         GOTO1 =V(SORTER),DMCB,=C'END'                                          
*                                                                               
         LA    R0,L'BFKEY                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SORTCARD+15(3),DUB                                               
*                                                                               
         LA    R0,L'BFREC                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  RECCARD+21(3),DUB                                                
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD,0                               
         B     SP25                                                             
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,XXX,BI,A),WORK=1'                            
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=XXX'                                   
         EJECT                                                                  
* CLEAR MGRTAB *                                                                
         SPACE 1                                                                
SP25     LA    R0,20000/250                                                     
         L     R1,AMGRTAB          GET MGRTAB ADDRESS                           
*                                                                               
         XC    0(250,R1),0(R1)                                                  
         LA    R1,250(R1)                                                       
         BCT   R0,*-10                                                          
         SPACE 1                                                                
* READ THE DEFAULT DAYPART MENU FOR THIS AGENCY *                               
         SPACE 1                                                                
         XC    DMCB(12),DMCB                                                    
         MVC   DMCB(3),QAGY        A-M                                          
         MVI   DMCB+3,C'0'                                                      
         GOTO1 DPTRD,DMCB,,ADDPTTAB                                             
         B     EXIT                                                             
         EJECT                                                                  
* CLTFRST                                                                       
*                                                                               
SP26     DS    0H                                                               
         CLI   QMGR,C' '           TEST MKTGRP REQUEST                          
         BE    SP40                NO                                           
         SPACE 1                                                                
* BUILD MKTGRP ASSGN TABLE *                                                    
         SPACE 1                                                                
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D82'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         MVC   KEY+8(1),QMGR                                                    
*                                                                               
SP27     GOTO1 HIGH                                                             
         CLC   KEY(9),KEYSAVE                                                   
         BE    SP31                                                             
         DC    H'0'                NO MARKETS ASSIGNED                          
*                                                                               
SP29     GOTO1 SEQ                                                              
*                                                                               
SP31     CLC   KEY(9),KEYSAVE                                                   
         BNE   SP33                                                             
*                                                                               
         CLI   QSTA,C'0'           TEST ONE MKTGRP REQUEST                      
         BL    SP32                NO                                           
*                                                                               
         PACK  DUB(3),QSTA(5)                                                   
         CLC   DUB(2),KEY+9        MATCH REQUEST                                
         BNE   SP29                NO - CONTINUE                                
*                                                                               
SP32     SR    RE,RE                                                            
         ICM   RE,3,KEY+11         GET MKT NUM                                  
         AR    RE,RE               X 2                                          
         A     RE,AMGRTAB          + TABLE START                                
         MVC   0(2,RE),KEY+9       MOVE MKTGRP NUMBER                           
         B     SP29                                                             
         EJECT                                                                  
* SET MASKS FOR MKTGRPS *                                                       
         SPACE 1                                                                
SP33     CLC   MGR3LEN,MGR2LEN     TEST 3 LEVELS OF MKTGRP                      
         BE    SP35                NO                                           
         ZIC   R0,MGR2LEN                                                       
         BAS   RE,SETMASK                                                       
         STH   R0,MGR2MASK                                                      
*                                                                               
SP35     CLC   MGR2LEN,MGR1LEN     TEST 2 LEVELS OF MKTGRP                      
         BE    SP40                                                             
         ZIC   R0,MGR1LEN                                                       
         BAS   RE,SETMASK                                                       
         STH   R0,MGR1MASK                                                      
         B     SP40                                                             
         SPACE 2                                                                
SETMASK  LTR   R0,R0                                                            
         BP    *+6                                                              
         DC    H'0'                                                             
         LA    RF,MGRMASKS-2                                                    
         LA    RF,2(RF)                                                         
         BCT   R0,*-4                                                           
         LH    R0,0(RF)                                                         
         BR    RE                                                               
*                                                                               
MGRMASKS DC    X'0FFF'             FOR BREAK LEN = 3                            
         DC    X'00FF'                             2                            
         DC    X'000F'                             1                            
         EJECT                                                                  
SP40     CLC   =C'ALL',QCLT                                                     
         BNE   SP40X                                                            
*                                                                               
         L     R6,ADCLT                                                         
         USING CLTHDRD,R6                                                       
         TM    COPT2,COP2EXJ1      EXCLUDE THIS CLIENT?                         
         BNO   SP40X                                                            
         MVI   MODE,CLTLAST        SET TO SKIP CLIENT                           
         B     EXIT                                                             
         EJECT                                                                  
** IT IS ABSOLUTELY ESSENTIAL TO REBUILD THE DATE TABLE HERE **                 
         SPACE 1                                                                
SP40X    MVI   SPOTPROF+2,0        SUPPRESS ALL CLIENT                          
         MVI   SPOTPROF+6,0          SPECIAL PERIODS                            
         MVI   SPOTPROF+7,0                                                     
         MVC   QSTART(12),SVQDATES  AND RESTORE REQUEST DATES                   
*                                                                               
         GOTO1 MEDDATE,DMCB,(R9)                                                
*                                                                               
         GOTO1 BUFFALO,DMCB,=C'RESET',BUFFBUFF                                  
*                                                                               
         L     R6,ADCLT                                                         
         USING CLTHDRD,R6                                                       
         MVC   WORK(2),2(R6)       CLIENT CODE IS KEY                           
         MVC   WORK+2(20),CNAME    CLIENT NAME IS DATA                          
         DROP  R6                                                               
* USE BINSRCH TO INSERT IN CASE COMING IN OFFICE SEQUENCE                       
         GOTO1 BINSRCH,BPARS,(X'01',WORK)                                       
         OC    0(4,R1),0(R1)       TEST TABLE FULL                              
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R1,AESTTAB          CLEAR EST TABLE                              
         LA    R0,220                                                           
SP41     XC    0(256,R1),0(R1)     CLEAR 512 BYTES FOR EACH BRAND               
         XC    256(256,R1),256(R1)                                              
         LA    R1,512(R1)                                                       
         BCT   R0,SP41                                                          
         SPACE 1                                                                
* SAVE PRIMARY DEMO FOR EACH PRD/EST IN REQUEST PERIOD *                        
         SPACE 1                                                                
         L     R6,ADCLT                                                         
         MVC   KEY(13),0(R6)                                                    
         MVI   KEY+4,C'A'          FORCE PAST CLTHDR                            
         GOTO1 HIGH                                                             
         CLC   KEY(4),KEYSAVE      A-M/C                                        
         BNE   EXIT                                                             
*                                                                               
SP42     GOTO1 HIGH                                                             
         CLC   KEY(4),KEYSAVE                                                   
         BNE   SP48                                                             
*                                                                               
SP44     CLC   =C'POL',KEY+4       SKIP POL ESTIMATES                           
         BNE   SP45                                                             
         MVC   KEY+7(5),XFF        FORCE NEXT PRD                               
         B     SP42                                                             
*                                                                               
SP45     CLI   KEY+7,0             TEST EST                                     
         BE    SP46                NO                                           
         OC    KEY+8(5),KEY+8      TEST BILL                                    
         BNZ   SP46                YES                                          
         L     R6,ADEST                                                         
         ST    R6,AREC                                                          
         GOTO1 GET                                                              
         USING ESTHDRD,R6                                                       
*                                                                               
         CLC   EEND,QSTART         EST END BEFORE REQ START                     
         BL    SP46                                                             
         CLC   ESTART,QEND         EST START AFTER REQ END                      
         BH    SP46                                                             
*                                                                               
         CLI   EPRDCD+1,220        MAKE SURE PRD CODE VALID                     
         BNH   *+6                                                              
         DC    H'0'                                                             
         ZIC   RE,EPRDCD+1         GET PRD NUM                                  
         BCTR  RE,0                                                             
         SLL   RE,9                X 512                                        
         ZIC   R0,7(R6)            EST NUM                                      
         BCTR  R0,0                                                             
         AR    R0,R0               X 2                                          
         AR    RE,R0                                                            
         A     RE,AESTTAB                                                       
         MVC   0(2,RE),EDEMOS+1    SAVE LAST 2 BYTES OF DEMO 1                  
         DROP  R6                                                               
*                                                                               
SP46     MVC   KEY+8(5),XFF                                                     
         B     SP42                                                             
         SPACE 1                                                                
* SET UP TO READ THROUGH BUY RECORDS *                                          
         SPACE 1                                                                
SP48     DS    0H                                                               
         XC    SVMKT,SVMKT         CLEAR LAST MARKET                            
*                                                                               
         XC    KEY,KEY                                                          
         L     R6,ADCLT                                                         
         MVC   KEY(3),1(R6)        MOVE A-M/CLT                                 
*                                                                               
         CLC   =C'ALL',QMKT                                                     
         BE    SP50                                                             
         PACK  DUB,QMKT            ALLOW ONE MKT FOR TESTING                    
         CVB   R0,DUB                                                           
         STCM  R0,3,BMKT                                                        
*                                                                               
SP50     GOTO1 HIGH                                                             
*                                                                               
SP52     CLC   KEY(3),KEYSAVE      SAME A-M/C                                   
         BNE   SP100                                                            
         CLI   QOPT5,C'Y'         DDS TESTING PURPOSES ONLY                     
         BNE   SP52X                                                            
         CP    MYCOUNT,=P'5000'   ONLY READ 5,000 RECORDS                       
         BH    SP100              GO DO REPORT                                  
         AP    MYCOUNT,=P'1'                                                    
*                                                                               
SP52X    CLC   =C'ALL',QMKT                                                     
         BE    SP54                                                             
         CLC   KEY+4(2),BMKT                                                    
         BE    SP54                                                             
         BH    SP53                                                             
         EJECT                                                                  
* IF ACTUAL MKT LOW, FORCE READ FOR MARKET                                      
         SPACE 1                                                                
         XC    KEY+4(9),KEY+4                                                   
         MVC   KEY+4(2),BMKT                                                    
         B     SP50                                                             
         SPACE 1                                                                
* IF ACTUAL MKT HIGH, FORCE READ FOR NEXT PRODUCT                               
        SPACE 1                                                                 
SP53     MVC   KEY+4(9),XFF                                                     
         B     SP50                                                             
*                                                                               
SP54     CLI   KEY+10,0            TEST ACTIVE POINTER                          
         BE    SP60                YES - PROCESS                                
*                                                                               
SP56     MVC   KEY+10(3),XFF       FORCE NEW EST                                
         B     SP50                                                             
         EJECT                                                                  
* PROCESS BUY RECORD *                                                          
         SPACE 1                                                                
SP60     DS    0H                                                               
         CLC   KEY+4(2),SVMKT                                                   
         BE    SP61                                                             
*                                                                               
         CLI   QSTA,C'0'           TEST ONE MKTGRP REQUEST                      
         BL    SP60A               NO                                           
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,KEY+4                                                       
         AR    RE,RE                                                            
         A     RE,AMGRTAB                                                       
         OC    0(2,RE),0(RE)       TEST MKTGRP NUMBER PRESENT                   
         BNZ   SP60A               YES - PROCESS                                
         MVC   KEY+6(7),XFF        FORCE NEXT MARKET                            
         B     SP50                                                             
         SPACE 1                                                                
* NEED TO READ MARKET RECORD                                                    
         SPACE 1                                                                
SP60A    LA    R6,SRECOLD                                                       
         USING SRECD,R6                                                         
         MVC   SRMKT(2),KEY+4      MOVE MKT NUM FOR GETMKT ROUTINE              
         DROP  R6                                                               
*                                                                               
         MVC   MYKEYS(64),KEY      SAVE KEY AND KEYSAVE                         
         BAS   RE,GETMKT           READ MARKET RECORD                           
         MVC   KEY(64),MYKEYS      RESTORE KEY AND KEYSAVE                      
*                                                                               
         L     RE,ADMARKET                                                      
         LA    RE,MKTKMKT-MKTRECD(RE)                                           
         MVC   MKT,0(RE)           SET FOR ACTUAL BOOK LOGIC                    
*                                                                               
SP61     MVC   AREC,ADBUY                                                       
         GOTO1 GET                                                              
*                                                                               
         L     R6,ADBUY                                                         
         CLC   KEY+4(2),4(R6)             TEST SPILL MARKET                     
         BNE   SP80                                                             
*                                                                               
         XC    PSLIST,PSLIST                                                    
         GOTO1 MEDPSL,DMCB,(R9),PSLIST    BUILD PRD/SLN LIST                    
         CLI   PSLIST,0                   TEST NOTHING IN LIST                  
         BE    SP80                       YES - SKIP                            
*                                                                               
SP65     LA    R4,PSLIST                                                        
*                                                                               
SP70     L     R2,MEDBUFF                                                       
         USING MEDBLOCK,R2                                                      
*                                                                               
         MVC   MEDBRAND,0(R4)                                                   
         MVC   MEDSPTLN,1(R4)                                                   
*                                                                               
         ZIC   RE,0(R4)            GET PRD NUM                                  
         BCTR  RE,0                                                             
         SLL   RE,9                X 512                                        
         ZIC   R0,KEY+9            EST NUM                                      
         BCTR  R0,0                                                             
         AR    R0,R0               X 2                                          
         AR    RE,R0                                                            
         A     RE,AESTTAB                                                       
         OC    0(2,RE),0(RE)       TEST DEMO PRESENT                            
         BZ    SP56                NO - SKIP TO NEXT ESTIMATE                   
         SPACE 1                                                                
* NEED TO SET DEMO IN PRDBUFF ENTRY                                             
         SPACE 1                                                                
         ZIC   RF,0(R4)            PRD NUM                                      
         BCTR  RF,0                                                             
         MH    RF,PRDBUFLN                                                      
         A     RF,PRDBUFF                                                       
         USING PTBUFFD,RF                                                       
         MVC   PTDEMO+1(2),0(RE)   SET PRIMARY DEMO IN BUFFER                   
         DROP  RF                                                               
         EJECT                                                                  
* NEED STATION CALL LETTERS IN STA FOR ACT BOOK OPTION                          
         SPACE 1                                                                
         CLC   SVSTA(3),KEY+6      TEST SAME STATION                            
         BE    SP72                                                             
         MVC   SVSTA(3),KEY+6                                                   
         GOTO1 MSUNPK,DMCB,KEY+4,WORK,STA                                       
         CLI   STA+4,C' '                                                       
         BNE   *+8                                                              
         MVI   STA+4,C'T'                                                       
*                                                                               
SP72     XC    BFREC,BFREC         CLEAR BUFFALO DATA                           
         SR    RE,RE                                                            
         ICM   RE,3,KEY+4          GET MKT NUMBER                               
         AR    RE,RE               X 2                                          
         A     RE,AMGRTAB                                                       
         MVC   BFMGR2,0(RE)                                                     
         MVC   BFMGR1,BFMGR2                                                    
         OC    BFMGR1,MGR1MASK                                                  
*                                                                               
         OC    BFMGR2,BFMGR2                                                    
         BNZ   *+16                                                             
         MVC   BFMGR1,=X'9999'                                                  
         MVC   BFMGR2,=X'9999'                                                  
*                                                                               
         MVC   BFMKT,KEY+4                                                      
         XC    BFSTA,BFSTA                                                      
         CLI   QOPT3,C'Y'          TEST ALL MKTS                                
         BNE   *+10                                                             
         MVC   BFSTA,KEY+6         PRINT STATION DETAILS FOR TESTING            
*                                                                               
         MVC   BFCLT,KEY+1                                                      
*                                                                               
         LA    R0,2                SET FOR BUYER'S DEMOS                        
         ST    R0,DMCB+4                                                        
         GOTO1 MEDGETBY,DMCB,(R9)                                               
*                                                                               
         LA    R5,MEDQRT01                                                      
         L     R1,4(R5)                                                         
         USING MEDDATA,R1                                                       
*                                                                               
         MVC   BFSPT,MEDBYSPT      SPOTS                                        
         MVC   BFDOL,MEDBYD        ACTUAL DOLLARS                               
         MVC   BFDM1P,MEDBY1       DEMO 1 PURCHASED                             
         DROP  R1                                                               
*                                                                               
         OC    BFSPT(8),BFSPT      TEST FOR SPOTS OR DOLLARS                    
         BZ    SP78                                                             
*                                                                               
         LA    R0,7                SET FOR AFFID RERATED                        
         ST    R0,DMCB+4                                                        
         GOTO1 MEDGETBY,DMCB,(R9)                                               
*                                                                               
         LA    R5,MEDQRT01                                                      
         L     R1,4(R5)                                                         
         USING MEDDATA,R1                                                       
*                                                                               
         MVC   BFDM1R,MEDBY1       DEMO 1 RERATED                               
         DROP  R1                                                               
*                                                                               
         CLI   QOPT4,C'Y'                                                       
         BNE   SP74                                                             
         MVC   P(4),=C'BPUT'                                                    
         GOTO1 HEXOUT,DMCB,ADBUY,P+75,13,=C'N'                                  
         BAS   RE,BUFFTRC                                                       
*                                                                               
SP74     DS    0H                                                               
         GOTO1 BUFFALO,DMCB,=C'PUT',BUFFBUFF,BFREC,1                            
*                                                                               
SP78     LA    R4,2(R4)            NEXT PSLIST ENTRY                            
         CLI   0(R4),0             TEST E-O-L                                   
         BNZ   SP70                                                             
*                                                                               
SP80     GOTO1 SEQ                                                              
         B     SP52                                                             
         EJECT                                                                  
* END OF CLIENT - GENERATE SORT RECORDS FROM BUFFALO DATA *                     
         SPACE 1                                                                
SP100    DS    0H                                                               
         XC    BFREC,BFREC                                                      
         GOTO1 BUFFALO,DMCB,=C'HIGH',BUFFBUFF,BFREC,1                           
         TM    8(R1),X'80'                                                      
         BO    SP110                                                            
         B     SP104                                                            
*                                                                               
SP102    GOTO1 BUFFALO,DMCB,=C'SEQ',BUFFBUFF,BFREC,1                            
         TM    8(R1),X'80'                                                      
         BO    SP110                                                            
*                                                                               
SP104    CLI   QOPT4,C'Y'          TEST BUFFALO TRACE                           
         BNE   *+14                                                             
         MVC   P(4),=C'BGET'                                                    
         BAS   RE,BUFFTRC                                                       
*                                                                               
         GOTO1 =V(SORTER),DMCB,=C'PUT',BFREC                                    
         CLI   QOPT4,C'S'         TEST SORTER TRACE                             
         BNE   *+14                                                             
         MVC   P(4),=C'SPUT'                                                    
         BAS   RE,BUFFTRC                                                       
*                                                                               
         MVC   BFMKT,XFF           GENERATE MKTGRP2 RECAP DATA                  
         MVC   BFSTA,XFF                                                        
         GOTO1 =V(SORTER),DMCB,=C'PUT',BFREC                                    
         CLI   QOPT4,C'S'         TEST SORTER TRACE                             
         BNE   *+14                                                             
         MVC   P(4),=C'SPUT'                                                    
         BAS   RE,BUFFTRC                                                       
*                                                                               
         MVC   BFMGR2,XFF           GENERATE MKTGRP1 RECAP DATA                 
         GOTO1 =V(SORTER),DMCB,=C'PUT',BFREC                                    
         CLI   QOPT4,C'S'         TEST SORTER TRACE                             
         BNE   *+14                                                             
         MVC   P(4),=C'SPUT'                                                    
         BAS   RE,BUFFTRC                                                       
         B     SP102                                                            
*                                                                               
SP110    MVI   MODE,CLTLAST                                                     
         B     EXIT                                                             
         SPACE 1                                                                
* SUBROUTINE TO PRINT BUFFALO RECORDS *                                         
         SPACE 1                                                                
BUFFTRC  NTR1                                                                   
         L     RF,BUFFBUFF                                                      
         L     R0,BUFFLALL-BUFFALOD(RF)                                         
         GOTO1 HEXOUT,DMCB,BFREC,P+5,(R0),=C'N'                                 
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         SPACE 1                                                                
         EJECT                                                                  
****************************                                                    
*                          *                                                    
*  SORT OUTPUT PROCESSING  *                                                    
*                          *                                                    
****************************                                                    
         SPACE 2                                                                
SP200    MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         XC    SVMGR2,SVMGR2                                                    
         XC    SVMGR1,SVMGR1                                                    
         XC    SVMKT,SVMKT                                                      
         XC    SRECNEW,SRECNEW                                                  
         XC    MKTDATA,MKTDATA                                                  
         XC    MGR1DATA,MGR1DATA                                                
         XC    MGR2DATA,MGR2DATA                                                
         SPACE 1                                                                
* GET FIRST SORTED RECORD *                                                     
         SPACE 1                                                                
         BAS   RE,GETSORT                                                       
         CLI   SRECNEW,X'FF'       TEST E-O-F ON FIRST READ                     
         BE    EXIT                                                             
         SPACE 1                                                                
* GET NEXT SORTED RECORD *                                                      
         SPACE 2                                                                
SP202    BAS   RE,GETSORT                                                       
*                                                                               
         LA    R6,SRECOLD          POINT TO CURRENT REC                         
         USING SRECD,R6                                                         
         EJECT                                                                  
* PRINT DATA RECORD *                                                           
         SPACE 1                                                                
SP210    DS    0H                                                               
*                                                                               
         MVI   RCSUBPRG,1          SET SPROG                                    
         CLC   SVMKT,SRMKT         TEST NEW MARKET                              
         BE    SP215                                                            
*                                                                               
         OC    SVMKT,SVMKT                                                      
         BZ    *+8                                                              
         BAS   RE,ENDMKT           PRINT MARKET TOTALS                          
         BAS   RE,GETMKT           GET NEW MARKET                               
*                                                                               
SP215    CLC   SRMGR2,SVMGR2       TEST NEW MKTGRP                              
         BE    SP220                                                            
*                                                                               
         BAS   RE,GETMGR           GET NEW MKTGRP                               
*                                                                               
SP220    DS    0H                                                               
         CLC   SRMKT,XFF           TEST DUMMY MKT                               
         BE    SP230               YES - GO PRINT MKTGRP RECAP                  
*                                                                               
         MVI   MODE,MKTLAST        FORCE MKTNAME TO PRINT                       
         LA    R6,SRECOLD                                                       
         USING SRECD,R6                                                         
         LA    R4,SRDATA           POINT TO SPOTS                               
         BAS   RE,FORMAT           FORMAT DATA LINE                             
*                                                                               
         GOTO1 REPORT                                                           
         SPACE 1                                                                
* ADD TO MARKET TOTALS *                                                        
         SPACE 1                                                                
         LA    R0,L'SRDATA/4                                                    
         LA    R4,SRDATA                                                        
         LA    R5,MKTDATA                                                       
*                                                                               
SP222    L     R1,0(R4)                                                         
         A     R1,0(R5)                                                         
         ST    R1,0(R5)                                                         
         LA    R4,4(R4)                                                         
         LA    R5,4(R5)                                                         
         BCT   R0,SP222                                                         
*                                                                               
SP224    CLC   SRKEY,XFF                                                        
         BNE   SP202                                                            
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* PRINT MKTGRP RECAP                                                            
*                                                                               
SP230    DS    0H                                                               
         LA    R1,SRECOLD                                                       
         USING SRECD,R1                                                         
         CLC   SRMGR2,XFF          MGR1 RECAP?                                  
         BE    SP240                                                            
         DROP  R1                                                               
         MVI   MODE,MGR2LAST       SUPPRESS MKTNAME                             
         MVI   RCSUBPRG,2          BUYER RECAP HEADLINE                         
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
SP232    LA    R6,SRECOLD                                                       
         USING SRECD,R6                                                         
         LA    R4,SRDATA           POINT TO SPOTS                               
         BAS   RE,FORMAT           FORMAT DATA LINE                             
*                                                                               
         GOTO1 REPORT                                                           
         SPACE 1                                                                
* ADD TO MKTGRP TOTALS *                                                        
         SPACE 1                                                                
         LA    R0,L'SRDATA/4                                                    
         LA    R4,SRDATA                                                        
         LA    R5,MGR2DATA                                                      
*                                                                               
SP234    L     R1,0(R4)                                                         
         A     R1,0(R5)                                                         
         ST    R1,0(R5)                                                         
         LA    R4,4(R4)                                                         
         LA    R5,4(R5)                                                         
         BCT   R0,SP234                                                         
*                                                                               
         BAS   RE,GETSORT                                                       
         CLC   SRMGR2,SVMGR2       TEST SAME MKTGRP                             
         BE    SP232                                                            
*                                                                               
         BAS   RE,ENDMGR2          PRINT MKTGRP TOTALS                          
         MVI   FORCEHED,C'Y'       AND FORCE NEW PAGE                           
         XC    SVMKT,SVMKT         AND SUPPRESS ENDMKT                          
*                                                                               
         CLC   SRKEY,XFF           TEST EOF                                     
         BNE   SP210                                                            
         B     EXIT                                                             
         SPACE 1                                                                
ENDMGR2  NTR1                                                                   
         MVI   MODE,MGR2LAST       SUPPRESS MKTNAME                             
         LA    R4,MGR2DATA                                                      
         MVI   TOTSW,C'G'          INDICATE MGR INDEX                           
         BAS   RE,FORMAT                                                        
         GOTO1 REPORT                                                           
         XC    MGR2DATA,MGR2DATA                                                
         B     EXIT                                                             
         EJECT                                                                  
SP240    DS    0H                  MGR1 RECAP                                   
         MVI   MODE,MGR1LAST       SUPPRESS MKTNAME                             
         MVI   RCSUBPRG,3          OFFICE RECAP HEADLINE                        
         MVI   FORCEHED,C'Y'                                                    
         MVC   SVMGR1,SRMGR1                                                    
*                                                                               
SP242    LA    R6,SRECOLD                                                       
         USING SRECD,R6                                                         
         LA    R4,SRDATA           POINT TO SPOTS                               
         BAS   RE,FORMAT           FORMAT DATA LINE                             
*                                                                               
         GOTO1 REPORT                                                           
         SPACE 1                                                                
* ADD TO MKTGRP TOTALS *                                                        
         SPACE 1                                                                
         LA    R0,L'SRDATA/4                                                    
         LA    R4,SRDATA                                                        
         LA    R5,MGR1DATA                                                      
*                                                                               
SP244    L     R1,0(R4)                                                         
         A     R1,0(R5)                                                         
         ST    R1,0(R5)                                                         
         LA    R4,4(R4)                                                         
         LA    R5,4(R5)                                                         
         BCT   R0,SP244                                                         
*                                                                               
         BAS   RE,GETSORT                                                       
         CLC   SRMGR1,SVMGR1       TEST SAME MKTGRP                             
         BE    SP242                                                            
*                                                                               
         BAS   RE,ENDMGR1          PRINT MKTGRP TOTALS                          
         MVI   FORCEHED,C'Y'       AND FORCE NEW PAGE                           
         XC    SVMKT,SVMKT         AND SUPPRESS ENDMKT                          
*                                                                               
         CLC   SRKEY,XFF           TEST EOF                                     
         BNE   SP210                                                            
         B     EXIT                                                             
         SPACE 1                                                                
ENDMGR1  NTR1                                                                   
         MVI   MODE,MGR1LAST       SUPPRESS MKTNAME                             
         LA    R4,MGR1DATA                                                      
         MVI   TOTSW,C'G'          INDICATE MGR INDEX                           
         BAS   RE,FORMAT                                                        
         GOTO1 REPORT                                                           
         XC    MGR1DATA,MGR1DATA                                                
         B     EXIT                                                             
         EJECT                                                                  
ENDMKT   NTR1                                                                   
         MVI   MODE,MKTLAST        FORCE MKTNAME                                
         LA    R4,MKTDATA                                                       
         MVI   TOTSW,C'K'          INDICATE MKT INDEX                           
         BAS   RE,FORMAT                                                        
         GOTO1 REPORT                                                           
         XC    MKTDATA,MKTDATA                                                  
         B     EXIT                                                             
         EJECT                                                                  
*======================================================*                        
* SUBROUTINE TO FORMAT DATA LINES TO PRINT             *                        
* R4 POINTS TO ACCUMULATORS                            *                        
* SRECOLD CONTAINS THE KEY OF DATA BEING PRINTED       *                        
*======================================================*                        
         SPACE 1                                                                
FORMAT   NTR1                                                                   
*                                                                               
         LA    R6,SRECOLD                                                       
         USING SRECD,R6                                                         
*                                                                               
         LA    R2,P                                                             
         USING PLINED,R2                                                        
*                                                                               
         MVI   SPACING,2                                                        
         MVI   PSTAR,0                                                          
*                                                                               
         CLI   TOTSW,0             TEST TOTALS                                  
         BE    FMT4                                                             
         MVI   PSTAR,C'*'                                                       
         MVC   PCLTNM(20),=C'** MARKET TOTALS **'                               
         CLI   TOTSW,C'K'                                                       
         BE    FMT6                                                             
         MVC   PCLTNM(20),=C'** OFFICE TOTALS **'                               
         CLI   MODE,MGR1LAST                                                    
         BE    *+10                                                             
         MVC   PCLTNM(20),=C'** BUYER TOTALS **'                                
         B     FMT6                                                             
*                                                                               
FMT4     DS    0H                                                               
         GOTO1 CLUNPK,DMCB,SRCLT,PCLT                                           
         SPACE 1                                                                
* LOOK UP NEW CLIENT NAME IN TABLE *                                            
         SPACE 1                                                                
         XC    WORK,WORK                                                        
         MVC   WORK(2),SRCLT                                                    
         GOTO1 BINSRCH,BPARS,(2,WORK)                                           
         CLI   0(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,0(R1)                                                         
         MVC   PCLTNM,2(RE)        MOVE CLIENT NAME                             
*                                                                               
FMT6     LA    RE,SRNFDATA-SRDATA(R4)                                           
         OC    0(L'SRNFDATA,RE),0(RE)                                           
         BZ    FMT8                                                             
         L     R0,SRNFDM1P-SRDATA(R4)  GET PURCHASED                            
         EDIT  (R0),(9,PNFDPUR),1                                               
         MVC   PNFDPUR+9(1),PSTAR                                               
         L     R0,SRNFDM1R-SRDATA(R4)  GET RERATED                              
         EDIT  (R0),(9,PNFDRR),1                                                
         MVC   PNFDRR+9(1),PSTAR                                                
         L     R0,SRNFDM1D-SRDATA(R4)  GET DIFF                                 
         EDIT  (R0),(6,PNFDDIFF),1                                              
         MVC   PNFDDIFF+6(1),PSTAR                                              
*                                                                               
FMT8     LA    RE,SRFDDATA-SRDATA(R4)                                           
         OC    0(L'SRFDDATA,RE),0(RE)                                           
         BZ    FMT10                                                            
         L     R0,SRFDDM1P-SRDATA(R4)  GET PURCHASED                            
         EDIT  (R0),(9,PFDPUR),1                                                
         MVC   PFDPUR+9(1),PSTAR                                                
         L     R0,SRFDDM1R-SRDATA(R4)  GET RERATED                              
         EDIT  (R0),(9,PFDRR),1                                                 
         MVC   PFDRR+9(1),PSTAR                                                 
         L     R0,SRFDDM1D-SRDATA(R4)  GET DIFF                                 
         EDIT  (R0),(6,PFDDIFF),1                                               
         MVC   PFDDIFF+6(1),PSTAR                                               
*                                                                               
FMT10    CLI   QOPT3,C'Y'          TEST TO PRINT STA/SPOTS/DOLLARS              
         BNE   FMT20                                                            
         SPACE 1                                                                
* PRINT STATION/SPOTS/DOLLARS FOR TESTING                                       
         SPACE 1                                                                
         CLI   TOTSW,0                                                          
         BNE   FMT12                                                            
         CLC   SRMKT,XFF                                                        
         BE    FMT12                                                            
         GOTO1 MSUNPK,DMCB,SRMKT,WORK,PSTA                                      
FMT12    L     R0,0(R4)                                                         
         EDIT  (R0),(6,PSPOTS)                                                  
         MVC   PSPOTS+6(1),PSTAR                                                
         L     R0,4(R4)                                                         
         EDIT  (R0),(10,PDOLS),2                                                
         MVC   PDOLS+10(1),PSTAR                                                
         SPACE 1                                                                
FMT20    CLI   TOTSW,0             TEST TOTAL LINE                              
         BNE   FMT22               YES - PRINT INDEX                            
         CLC   SRMKT,XFF           TEST OFFICE RECAP DATA                       
         BNE   FMT28               NO - SKIP                                    
*                                                                               
FMT22    DS    0H                                                               
         XC    NFINDX,NFINDX            CLEAR                                   
         XC    FDINDX,FDINDX            CLEAR                                   
         L     RE,SRNFDM1P-SRDATA(R4)   GET NON-FDA PURCH                       
         LTR   RE,RE                    TEST 0                                  
         BZ    FMT24                                                            
*                                                                               
         L     R1,SRNFDM1D-SRDATA(R4)   GET NON-FDA DIFF                        
         LTR   R1,R1                                                            
         BZ    FMT24                                                            
         M     R0,=F'20000'             X 10000 X 2                             
         DR    R0,RE                                                            
         SRL   R1,1                                                             
         LR    R0,R1                                                            
         ST    R0,NFINDX           SAVE RESULT                                  
         MVC   PNFDINDX(4),=C'HIGH'                                             
         C     R0,=F'99999'                                                     
         BH    FMT24                                                            
         EDIT  (R0),(6,PNFDINDX),2                                              
         MVC   PNFDINDX+6(1),PSTAR                                              
         ICM   R0,15,NFINDX        SET NON-ZERO VALUE IF 0                      
         BNZ   *+8                                                              
         LA    R0,1                                                             
         ST    R0,NFINDX                                                        
         EJECT                                                                  
FMT24    L     RE,SRFDDM1P-SRDATA(R4)   GET FDA PURCH                           
         LTR   RE,RE                    TEST 0                                  
         BZ    FMT28                                                            
*                                                                               
         L     R1,SRFDDM1D-SRDATA(R4)   GET FDA DIFF                            
         LTR   R1,R1                                                            
         BZ    FMT28                                                            
         M     R0,=F'20000'             X 10000 X 2                             
         DR    R0,RE                                                            
         SRL   R1,1                                                             
         LR    R0,R1                                                            
         ST    R0,FDINDX                                                        
         MVC   PFDINDX(4),=C'HIGH'                                              
         C     R0,=F'99999'                                                     
         BH    FMT28                                                            
         EDIT  (R0),(6,PFDINDX),2                                               
         MVC   PFDINDX+6(1),PSTAR                                               
         ICM   R0,15,FDINDX        SET NON-ZERO VALUE IF 0                      
         BNZ   *+8                                                              
         LA    R0,1                                                             
         ST    R0,FDINDX                                                        
*                                                                               
FMT26    CLI   TOTSW,C'G'          TEST MKTGRP RECAP                            
         BE    *+12                                                             
         CLI   TOTSW,C'K'          OR MARKET TOTALS                             
         BNE   FMT28               NO                                           
* 4/7/95 JWT ASKED TO HAVE INDEX OF TOTAL DIFF/TOTAL PURCH                      
         L     R1,SRNFDM1D-SRDATA(R4)   GET NON-FDA DIFF                        
         A     R1,SRFDDM1D-SRDATA(R4)   ADD FDA DIFF                            
         LTR   R1,R1                                                            
         BZ    FMT28                                                            
         L     RE,SRNFDM1P-SRDATA(R4)   GET NON-FDA PURCH                       
         A     RE,SRFDDM1P-SRDATA(R4)   ADD FDA PURCH                           
         LTR   RE,RE                                                            
         BZ    FMT28                                                            
         M     R0,=F'20000'             X 10000 X 2                             
         DR    R0,RE                                                            
         SRL   R1,1                                                             
         LR    R0,R1                                                            
         MVC   PWGTDIX(4),=C'HIGH'                                              
         C     R0,=F'99999'                                                     
         BH    FMT28                                                            
         EDIT  (R0),(6,PWGTDIX),2                                               
         MVC   PWGTDIX+6(1),PSTAR                                               
*                                                                               
FMT28    MVI   TOTSW,0             RESET                                        
         B     EXIT                                                             
         EJECT                                                                  
*===========================================================*                   
* SUBROUTINE TO READ NEXT SORT RECORD AND ADD IF KEYS EQUAL *                   
* ON EXIT, SRECOLD CONTAINS RECORD TO BE PROCESSED          *                   
* AND BFREC CONTAINS LAST RECORD FROM SORT (ALSO SRECNEW)   *                   
*===========================================================*                   
         SPACE 1                                                                
GETSORT  NTR1                                                                   
         MVC   SRECOLD,SRECNEW     NEW REC BECOMES CURRENT                      
         LA    R6,SRECOLD                                                       
         USING SRECD,R6                                                         
         CLC   SRKEY,XFF                                                        
         BE    EXIT                                                             
         DROP  R6                                                               
*                                                                               
GETSORT2 GOTO1 =V(SORTER),DMCB,=C'GET',0                                        
         ICM   RE,15,4(R1)                                                      
         BNZ   GETSORT3                                                         
         MVC   SRECNEW(L'SRKEY),XFF                                             
         B     EXIT                                                             
*                                                                               
GETSORT3 DS    0H                                                               
         MVC   BFREC,0(RE)         MOVE TO BFREC AREA                           
*                                                                               
         L     R0,BFDM1R           GET RERATED POINTS                           
         S     R0,BFDM1P           LESS PURCHASED POINTS                        
         LPR   R0,R0               MAKE DIFFERENCE POSITIVE                     
         ST    R0,BFDM1D           AND SAVE AS DIFFERENCE                       
         SPACE 1                                                                
* NOW MOVE TO CORRECT SLOT IN SORT RECORD *                                     
         SPACE 1                                                                
         LA    R6,SRECNEW                                                       
         USING SRECD,R6                                                         
*                                                                               
         MVC   SRKEY,BFKEY                                                      
         XC    SRDATA,SRDATA                                                    
         LA    R5,SRNFSPT          POINT TO NON-FDA ACCUMS                      
         CLC   SRCLT,CLTXA         TEST FDA CLIENT                              
         BL    *+8                 NO                                           
         LA    R5,SRFDSPT          POINT TO FDA ACCUMS                          
         MVC   0(L'SRNFDATA,R5),BFDATA  AND MOVE TO RECORD                      
*                                                                               
         CLC   SRECNEW(L'SRKEY),SRECOLD                                         
         BNE   EXIT                                                             
         SPACE 1                                                                
* KEYS EQUAL - ADD RECORDS *                                                    
         SPACE 1                                                                
         LA    R0,L'SRDATA/4       ADD BOTH FDA AND NFDA ACCUMS                 
         LA    R4,(SRDATA-SRKEY)+SRECOLD                                        
         LA    R5,(SRDATA-SRKEY)+SRECNEW                                        
*                                                                               
GETSORT4 L     R1,0(R4)                                                         
         A     R1,0(R5)                                                         
         ST    R1,0(R4)                                                         
         LA    R4,4(R4)                                                         
         LA    R5,4(R5)                                                         
         BCT   R0,GETSORT4                                                      
         B     GETSORT2                                                         
*                                                                               
CLTXA    DC    X'DC00'             PACKED VALUE OF CLIENT CODE XAA              
         EJECT                                                                  
*=====================================================*                         
* GET NEW MKTGRP                                      *                         
*=====================================================*                         
         SPACE 1                                                                
         USING SRECD,R6                                                         
         SPACE 1                                                                
GETMGR   NTR1                                                                   
         CLC   SRMGR2,XFF          TEST DUMMY                                   
         BE    EXIT                                                             
*                                                                               
         MVC   SVMGR2,SRMGR2       SAVE THIS MKTGRP                             
         MVI   FORCEHED,C'Y'       FORCE NEW HEADLINES                          
         SPACE 1                                                                
* READ NEW MKTGRP *                                                             
         SPACE 1                                                                
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D02'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         MVC   KEY+8(3),QMGR                                                    
         CLC   =X'9999',SVMGR2                                                  
         BE    GETMGR2                                                          
         MVC   KEY+9(2),SVMGR2                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETMKTGR                                                         
         B     EXIT                                                             
*                                                                               
GETMGR2  CLI   MGRLEN,1                                                         
         BNE   GETMGR4                                                          
*                                                                               
         MVC   MGR1N+1(4),=C'9999'                                              
         MVC   MGR2N+1(4),=C'9999'                                              
         MVC   MGR3N+1(4),=C'9999'                                              
         B     GETMGR6                                                          
*                                                                               
GETMGR4  MVC   MGR1N+1(4),=C'9999'                                              
         MVC   MGR2N+1(4),=C'9999'                                              
         MVC   MGR3N+1(4),=C'9999'                                              
*                                                                               
GETMGR6  MVC   MGR1NM,=CL24'*** UNDEFINED ***'                                  
         MVC   MGR2NM,=CL24'*** UNDEFINED ***'                                  
         MVC   MGR3NM,=CL24'*** UNDEFINED ***'                                  
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
         USING SRECD,R6                                                         
*                                                                               
GETMKT   NTR1                                                                   
         CLC   SRMKT,XFF           TEST DUMMY                                   
         BE    EXIT                                                             
*                                                                               
         MVC   SVMKT,SRMKT         SAVE NEW MARKET                              
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),QMED                                                    
         SR    R0,R0                                                            
         ICM   R0,3,SVMKT                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  KEY+2(4),DUB                                                     
         MVC   MARKET,KEY+2        SET TO PRINT ALSO                            
         MVC   KEY+6(2),QAGY                                                    
         GOTO1 HIGHMKT                                                          
*                                                                               
         L     R6,ADMARKET                                                      
         USING MKTRECD,R6                                                       
         LA    RE,MKTNAME                                                       
         CLC   KEY(6),0(R6)                                                     
         BE    *+8                                                              
         LA    RE,=CL24'*** UNKNOWN ***'                                        
         MVC   MKTNM,0(RE)                                                      
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
MYCOUNT  DC    PL4'0'             DDS TESTING ONLY                              
         DS    0D                                                               
BPARS    DS    0XL24                                                            
         DS    A                   KEYARG                                       
         DC    A(CLTTAB)           A(TABLE)                                     
CLTTBCNT DC    A(0)                CURRENT RECORD COUNT                         
         DC    AL4(L'CLTTAB)       ENTRY LENGTH                                 
         DC    AL1(0),AL3(2)       KEY DSPL/KEY LEN                             
         DC    AL4(CLTTABN)        MAX RECORDS                                  
*                                                                               
TOTSW    DC    X'00'                                                            
PSTAR    DC    X'00'                                                            
SVQDATES DS    CL12                                                             
FDINDX   DS    F                                                                
NFINDX   DS    F                                                                
         DS    0D                                                               
         DC    C'**BFREC*'                                                      
BFREC    DS    0XL32                                                            
*                                                                               
BFKEY    DS    0XL12                                                            
BFMGR1   DS    XL2                                                              
BFMGR2   DS    XL2                                                              
BFMKT    DS    XL2                                                              
BFSTA    DS    XL3                                                              
BFCLT    DS    XL2                                                              
         DS    XL1                 SPARE                                        
*                                                                               
BFDATA   DS    0XL20                                                            
BFSPT    DS    XL4                                                              
BFDOL    DS    XL4                                                              
BFDM1P   DS    XL4                                                              
BFDM1R   DS    XL4                                                              
BFDM1D   DS    XL4                                                              
*                                                                               
         DS    0D                                                               
         DC    CL8'SRECOLD'                                                     
SRECOLD  DS    XL52                                                             
*                                                                               
         DS    0D                                                               
         DC    CL8'SRECNEW'                                                     
SRECNEW  DS    XL52                                                             
*                                                                               
MKTDATA  DS    XL40                                                             
*                                                                               
         DS    0F                                                               
MGR1DATA DS    XL40                                                             
MGR2DATA DS    XL40                                                             
*                                                                               
XFF      DC    16X'FF'                                                          
AMGRTAB  DC    A(MGRTAB)                                                        
AESTTAB  DC    A(ESTTAB)                                                        
ACLTTAB  DC    A(CLTTAB)                                                        
ACLTTABX DC    A(CLTTABX)                                                       
SVMGR1   DC    H'0'                                                             
SVMGR2   DC    H'0'                                                             
MGR1MASK DC    H'0'                                                             
MGR2MASK DC    H'0'                                                             
MGR3MASK DC    H'0'                                                             
MYKEYS   DS    XL64                                                             
*                                                                               
         DS    0D                                                               
         DC    CL8'*PSLIST*'                                                    
PSLIST   DS    XL128                                                            
         LTORG                                                                  
         EJECT                                                                  
* NOTE THAT BUFF DEFN DOES NOT INCLUDE DIFFERENCE COLUMNS                       
         BUFF  LINES=300,ROWS=1,COLUMNS=4,FLAVOR=BINARY,               X        
               KEYLIST=(12,A)                                                   
*                                                                               
         DS    0D                                                               
         DC    C'*MGRTAB*'                                                      
MGRTAB   DS    2500D                                                            
*                                                                               
         DS    0D                                                               
         DC    C'*ESTTAB*'                                                      
ESTTAB   DS    (220*256)CL2        220 PRDS * 2 BYTES PER EST                   
*                                                                               
         DS    0D                                                               
         DC    C'*CLTTAB*'                                                      
CLTTAB   DS    2400XL22             2 BYTE CLT CODE/20 BYTE NAME                
CLTTABX  EQU   *                                                                
CLTTABN  EQU   (CLTTABX-CLTTAB)/L'CLTTAB                                        
         EJECT                                                                  
* DSECT FOR SORT RECORD DATA                                                    
         SPACE 1                                                                
SRECD    DSECT                                                                  
*                                                                               
SRKEY    DS    0XL12                                                            
SRMGR1   DS    XL2                                                              
SRMGR2   DS    XL2                                                              
SRMKT    DS    XL2                                                              
SRSTA    DS    XL3                                                              
SRCLT    DS    XL2                                                              
         DS    XL1                 SPARE                                        
*                                                                               
SRDATA   DS    0XL40                                                            
*                                                                               
SRNFDATA DS    0CL20                                                            
SRNFSPT  DS    F                   ** NON-FDA DATA **                           
SRNFDOL  DS    F                                                                
SRNFDM1P DS    F                                                                
SRNFDM1R DS    F                                                                
SRNFDM1D DS    F                                                                
*                                                                               
SRFDDATA DS    0CL20                                                            
SRFDSPT  DS    F                   ** FDA DATA **                               
SRFDDOL  DS    F                                                                
SRFDDM1P DS    F                                                                
SRFDDM1R DS    F                                                                
SRFDDM1D DS    F                                                                
*                                                                               
SRDATAX  EQU   *-SRDATA                                                         
         EJECT                                                                  
* DSECT FOR PRINT LINE DATA                                                     
         SPACE 1                                                                
PLINED   DSECT                                                                  
*                                                                               
PCLT     DS    CL3                                                              
         DS    CL1                                                              
PCLTNM   DS    CL18                                                             
         DS    CL1                                                              
PNFDPUR  DS    CL9  +23                                                         
         DS    CL1                                                              
PNFDRR   DS    CL9  +33                                                         
         DS    CL1                                                              
PNFDDIFF DS    CL6  +43                                                         
         DS    CL1                                                              
PNFDINDX DS    CL6  +50                                                         
         DS    CL1                                                              
PFDPUR   DS    CL9  +57                                                         
         DS    CL1                                                              
PFDRR    DS    CL9  +67                                                         
         DS    CL1                                                              
PFDDIFF  DS    CL6  +77                                                         
         DS    CL1                                                              
PFDINDX  DS    CL6  +84                                                         
         DS    CL1                                                              
PWGTDIX  DS    CL6  +91                                                         
         ORG   PWGTDIX                                                          
PSTA     DS    CL5                                                              
         DS    CL1                                                              
PSPOTS   DS    CL4                                                              
         DS    CL1                                                              
PDOLS    DS    CL8                                                              
         EJECT                                                                  
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
       ++INCLUDE SPREPPTBUF                                                     
         PRINT OFF                                                              
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
*                                                                               
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPMEDBLOCK                                                     
       ++INCLUDE DDBUFFALOD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'056SPREPJ102 05/09/05'                                      
         END                                                                    
