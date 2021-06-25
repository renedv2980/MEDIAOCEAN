*          DATA SET SPTRA27    AT LEVEL 010 AS OF 08/30/04                      
*PHASE T21627A                                                                  
         SPACE 2                                                                
********************************************************************            
*                                                                  *            
* LEV 04    MAY11/93 ADD TRAFFIC SYSTEM                            *            
* LEV 05    JUL21/94 CHANGE TO FILENAME                            *            
* LEV 06  SMUR NOV10/99 USE RECUP FROM FACPAK                      *            
* LEV 07  SMUR APR10/01 USE TRAFFIC OFFICE                         *            
* LEV 09 BGRI JAN15/04 CHGE SVSPARE TO TO SVSPAREX                 *            
* LEV 10 BGRI AUG02/04 SOX                                         *            
*                                                                  *            
********************************************************************            
*                                                                  *            
* I/O AREA USAGE                                                   *            
*      IO1 - ALL TRANSIENT RECORDS (CLT/PRD/EST + NEW BUYREC)      *            
*      IO2 - TEMPORARY AREA                                        *            
*      IO3 -                                                       *            
*                                                                  *            
********************************************************************            
         TITLE 'T21627 - TRAFFIC - BUY COPY/MOVE/DELETE'                        
T21627   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21627**,RA,RR=R2                                              
         ST    R2,RELO                                                          
*                                                                               
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
*                                                                               
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         L     RA,ATWA                                                          
         USING T216FFD,RA                                                       
*                                                                               
         SPACE 1                                                                
         CLI   MODE,VALKEY                                                      
         BE    VK                                                               
         CLI   OFFLINE,C'Y'                                                     
         BNE   EXIT                                                             
         CLI   MODE,PRINTREP                                                    
         BE    PRINTIT                                                          
         B     EXIT                                                             
*                                                                               
EQXIT    CR    RB,RB               SET CC EQUAL                                 
         B     EXIT                                                             
*                                                                               
NEQXIT   LTR   RB,RB               SET CC NOT EQUAL                             
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
RELO     DS    A                                                                
         EJECT                                                                  
* VALIDATE KEY *                                                                
         SPACE                                                                  
VK       DS    0H                                                               
         MVI   PQSW,1              SUPPRESS PRTQUE OPEN                         
*                                                                               
         TM    SOXSW,SOXOKFLG      DDS & FACTEST?                               
         BO    VK02                                                             
         TM    SOXSW,SOXERFLG      SOX ERROR?                                   
         BZ    VK02                                                             
         SPACE                                                                  
         GOTO1 VSOXERR                                                          
         SPACE                                                                  
VK02     DS    0H                                                               
         BAS   RE,TSTNEWKY         SEE IF ANY KEY FIELDS CHANGED                
         BNZ   VK5                 YES                                          
         MVC   KEY,SVLSTKEY        SEE IF CONTINUATION                          
         XC    SVLSTKEY,SVLSTKEY   BUT DON'T START HERE AGAIN                   
         OC    KEY(13),KEY                                                      
         BNZ   PR122               YES                                          
*                                                                               
VK5      BAS   RE,CLRMED           NEW KEY - UNVALIDATE FIELDS                  
         XC    SVLSTKEY,SVLSTKEY   BUT DON'T START HERE AGAIN                   
         XC    BUYTOTS,BUYTOTS     CLEAR ACCUMULATORS                           
*                                                                               
         OC    TRAOUT1,TRAOUT1     CLEAR PREVIOUS MESSAGES                      
         BZ    *+14                                                             
         XC    TRAOUT1,TRAOUT1                                                  
         OI    TRAOUT1H+6,X'80'                                                 
*                                                                               
         OC    TRAOUT2,TRAOUT2                                                  
         BZ    *+14                                                             
         XC    TRAOUT2,TRAOUT2                                                  
         OI    TRAOUT2H+6,X'80'                                                 
*                                                                               
         OC    TRAOUT3,TRAOUT3                                                  
         BZ    *+14                                                             
         XC    TRAOUT3,TRAOUT3                                                  
         OI    TRAOUT3H+6,X'80'                                                 
*                                                                               
         OC    TRAOUT4,TRAOUT4                                                  
         BZ    *+14                                                             
         XC    TRAOUT4,TRAOUT4                                                  
         OI    TRAOUT4H+6,X'80'                                                 
*                                                                               
         XC    SVLSTKEY,SVLSTKEY                                                
         LA    R2,TRAMEDH                                                       
         TM    4(R2),X'20'         TEST PREVIOUSLY VALID                        
         BO    VK10                                                             
         BAS   RE,CLRMED                                                        
         GOTO1 VALIMED                                                          
         OI    4(R2),X'20'         SET VALID                                    
*                                                                               
VK10     LA    R2,TRACLTH                                                       
         TM    4(R2),X'20'         TEST PREVIOUSLY VALID                        
         BO    VK20                                                             
         GOTO1 VALICLT                                                          
         BAS   RE,FPRO             GET PROFILES                                 
         OI    4(R2),X'20'                                                      
*                                                                               
VK20     DS    0H                  VALIDATE PRDOUCT                             
         LA    R2,TRAPRDH                                                       
         BAS   RE,VPR                                                           
*                                                                               
*                                                                               
VK30     DS    0H                                                               
*                                                                               
VK40     DS    0H                                                               
         LA    R2,TRAOPTH          VALIDATE OPTIONS                             
         BAS   RE,VOPT                                                          
         OI    4(R2),X'20'         VALIDATED                                    
         EJECT                                                                  
* VALIDATE 'FROM' CODE AND PERIOD                                               
         SPACE 1                                                                
VK50     BAS   RE,CLRPRES                                                       
         LA    R2,TRALENFH         VALIDATE FROM LENGHT                         
         MVI   WORK,30           SET DEFAULT                                    
         CLI   5(R2),0             SEE IF INPUT                                 
         BNE   VK50D                                                            
         MVC   TRALENF(2),=C'30'                                                
         FOUT  TRALENFH                                                         
         B     VK50E                                                            
*                                                                               
VK50D    GOTO1 VALISLN                                                          
VK50E    OI    4(R2),X'20'         SET VALIDATED                                
         MVC   SVLENF,WORK         SAVE 'FROM' SPOT LENGHT                      
*                                                                               
VK52     DS    0H                  VALIDATE PARTNER - LEN                       
         LA    R2,TRAPTRFH                                                      
         BAS   RE,VPP                                                           
         MVC   SVPTRF,BPRD2                                                     
         MVC   SVPTRLF,BSLN2                                                    
         LA    R2,TRACODFH                                                      
         BAS   RE,VCD                                                           
         MVC   SVCODF,CODE                                                      
*                                                                               
         LA    R2,TRAPERFH                                                      
         GOTO1 =A(VPER),RR=RELO                                                 
         OC    PEREND,PEREND    CHECK FOR END DATE                              
         BZ    MISSERR                                                          
         MVC   SVPERF,PERDTS                                                    
*                                                                               
*                               IF CODE WAS ESTIMATE - ALLOW 'ES'               
*                               AND GET DATES FROM ESTIMATE                     
*                                                                               
VK54     CLI   ACTNUM,ACTCNCL      CANCEL MUST NOT HAVE 'TO' DATA               
         BNE   VK60                                                             
VK56     MVI   ERROR,INVALID                                                    
         LA    R2,TRACODTH                                                      
         CLI   5(R2),0                                                          
         BNE   SCANERR                                                          
         LA    R2,TRAPERTH                                                      
         CLI   5(R2),0                                                          
         BNE   SCANERR                                                          
*                                                                               
         B     VK100               NO 'TO' EDITS FOR CANCEL                     
         EJECT                                                                  
         SPACE 1                                                                
* VALIDATE 'TO' CODE AND PERIOD                                                 
         SPACE 1                                                                
VK60     DS    0H                                                               
         LA    R2,TRALENTH         VALIDATE FROM LENGHT                         
         MVI   WORK,30             SET DEFAULT                                  
         CLI   5(R2),0                                                          
         BNE   VK60D                                                            
         MVC   TRALENT(2),=C'30'                                                
         FOUT  TRALENTH                                                         
         B     VK60E                                                            
*                                                                               
VK60D    GOTO1 VALISLN                                                          
VK60E    OI    4(R2),X'20'         SET VALIDATED                                
         MVC   SVLENT,WORK         SAVE 'TO' SPOT LENGHT                        
         LA    R2,TRAPTRTH                                                      
         BAS   RE,VPP                                                           
         MVC   SVPTRT,BPRD2                                                     
         MVC   SVPTRLT,BSLN2                                                    
VK62     LA    R2,TRACODTH                                                      
         BAS   RE,VCD                                                           
         MVC   SVCODT,CODE                                                      
*                                                                               
*                                                                               
VK65     LA    R2,TRAPERTH                                                      
         CLI   5(R2),0             TEST 'TO' PERIOD ENTERED                     
         BNE   VK70                                                             
         B     MISSERR                                                          
*                                                                               
*                                                                               
VK70     DS    0H                                                               
         GOTO1 =A(VPER),RR=RELO                                                 
         MVC   SVPERT,PERDTS       'TO' PERIOD                                  
*                                                                               
         EJECT                                                                  
* NOW MAKE SURE ESTIMATE DATES OVERLAP *                                        
         SPACE 1                                                                
* FINAL VALIDATION ON INPUT PARAMETERS *                                        
         SPACE 1                                                                
VK80     LA    R2,TRACODFH                                                      
         CLC   SVTODATA,SVFRDATA   TEST FROM DATA = TO DATA                     
         BNE   VK82                                                             
         MVI   ERROR,SAMEDATA                                                   
         B     SCANERR                                                          
*                                                                               
VK82     DS    0H                                                               
         EJECT                                                                  
* PROCESS REQUEST *                                                             
         SPACE 1                                                                
VK100    DS    0H                                                               
         OI    TRALENFH+4,X'20'    SET VALIDATED                                
         OI    TRAPTRFH+4,X'20'    SET VALIDATED                                
         OI    TRACODFH+4,X'20'    SET VALIDATED                                
         OI    TRAPERFH+4,X'20'    SET VALIDATED                                
         OI    TRALENTH+4,X'20'    SET VALIDATED                                
         OI    TRAPTRTH+4,X'20'    SET VALIDATED                                
         OI    TRACODTH+4,X'20'    SET VALIDATED                                
         OI    TRAPERTH+4,X'20'    SET VALIDATED                                
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING TBYRECD,R6                                                       
         MVC   TBYKID(2),=X'0A32'                                               
         MVC   TBYKAM,BAGYMD                                                    
         MVC   TBYKCLT,BCLT                                                     
         MVC   TBYKPRD,BPRD                                                     
         DROP  R6                                                               
         MVC   SVKEY,KEY                                                        
         SPACE 1                                                                
PR120    DS    0H                                                               
         GOTO1 GETFACT,DMCB,0                                                   
         L     R4,DMCB            ADDR OF GETFACT WORKAREA                      
         USING FACTSD,R4                                                        
         SR    R1,R1                                                            
         ICM   R1,3,FATMAXIO                                                    
         M     R0,=F'80'           SET LIMIT = 80 PCT OF MAX                    
         D     R0,=F'100'                                                       
         STH   R1,SVMAXIO                                                       
         DROP  R4                                                               
*                                                                               
PR122    DS    0H                                                               
         NI    DMINBTS,X'F7'       DO NOT PASS DELETES                          
         GOTO1 HIGH                                                             
         B     PR126                                                            
*                                                                               
PR124    DS    0H                                                               
         NI    DMINBTS,X'F7'       DO NOT PASS DELETES                          
         GOTO1 SEQ                                                              
*                                                                               
PR126    CLC   KEY(6),SVKEY       AM/CLT/PRD                                    
         BNE   PR900                                                            
         GOTO1 GETFACT,DMCB,0                                                   
         L     R4,DMCB             ADDR OF GETFACT WORKAREA                     
         USING FACTSD,R4                                                        
         CLC   FATIOCNT,SVMAXIO    TEST REACHED LIMIT                           
         BL    PR128               NO                                           
         MVC   SVLSTKEY,KEY        SAVE START KEY FOR NEXT TIME                 
         B     PR900               AND GET OUT NOW                              
         DROP  R4                                                               
*                                                                               
PR128    DS    0H                                                               
         EJECT                                                                  
* WE HAVE AN TRAFFIC BUY                                                        
         SPACE 1                                                                
PR129    MVC   SVKEY,KEY           SAVE CURRENT KEY                             
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
PR129C   BAS   RE,NEXTEL                                                        
         BNE   PR210               FINISHED WITH THIS BUY                       
         USING TBYDTAEL,R6                                                      
         CLC   SVLENF(3),TBYSLN    FROM SPOT LEN/PARTNER/SPOT LEN               
         BNE   PR129C                                                           
         CLC   TBYCODE,SVCODF      MUST MATCH 'FROM' CODE                       
         BNE   PR129C                                                           
         CLC   TBYSTART(6),SVPERF  MUST MATCH 'FROM' PERIOD                     
         BNE   PR129C                                                           
*                                                                               
         CLI   ACTNUM,ACTCOPY                                                   
         BNE   PR160                                                            
         BAS   RE,CHKDUP                                                        
         CLI   DUPSW,C'Y'         ALREADY THERE                                 
         BE    PR174X             SKIP PUTREC                                   
*                                 FIRST CHECK FOR OVERLAP                       
*                                 FOR 'TO' COPY AND PERIOD                      
         ST    R6,ELEADDR         SAVE ADDRESS OF ELEM TO BE COPIED             
         BAS   RE,CHKOV           CHECK FOR 'TO' OVERLAP                        
*                                                                               
PR135C   BAS   RE,ADDBUY          REALLY ADDS ELEM                              
         B     PR174                                                            
*                                                                               
PR160    CLI   ACTNUM,ACTMOVE                                                   
         BNE   PR170                                                            
         MVC   OLDELEM(TBYDTAX-TBYDTAEL),0(R6)  SAVE ELEM TO MOVE               
         BAS   RE,CHKOV           CHECK FOR 'TO' OVERLAP                        
         BAS   RE,DELBUY                                                        
         LA    R6,OLDELEM                                                       
         ST    R6,ELEADDR                                                       
         BAS   RE,ADDBUY                                                        
         B     PR174                                                            
*                                                                               
PR170    CLI   ACTNUM,ACTCNCL                                                   
         BNE   PR172                                                            
         BAS   RE,DELBUY                                                        
         B     PR174                                                            
*                                                                               
PR172    DC    H'0'                                                             
*                                                                               
PR174    CLI   TESTRUN,C'Y'                                                     
         BE    PR174X              DON'T MARK FILE                              
*                                                                               
         GOTO1 PUTREC                                                           
PR174X   MVC   KEY,SVKEY           RESTORE LAST KEY READ                        
         OI    DMINBTS,X'08'       IT MAY VERY WELL BE DELETED                  
         GOTO1 HIGH                RESTORE FOR SEQ                              
         B     PR210                                                            
         EJECT                                                                  
*                                                                               
PR210    B     PR124                                                            
         DROP  R6                                                               
         EJECT                                                                  
PR900    DS    0H                                                               
         OC    TRAOUT1,TRAOUT1     CLEAR PREVIOUS MESSAGES                      
         BZ    *+14                                                             
         XC    TRAOUT1,TRAOUT1                                                  
         OI    TRAOUT1H+6,X'80'                                                 
*                                                                               
         OC    TRAOUT2,TRAOUT2                                                  
         BZ    *+14                                                             
         XC    TRAOUT2,TRAOUT2                                                  
         OI    TRAOUT2H+6,X'80'                                                 
*                                                                               
         OC    TRAOUT3,TRAOUT3                                                  
         BZ    *+14                                                             
         XC    TRAOUT3,TRAOUT3                                                  
         OI    TRAOUT3H+6,X'80'                                                 
*                                                                               
         OC    TRAOUT4,TRAOUT4                                                  
         BZ    *+14                                                             
         XC    TRAOUT4,TRAOUT4                                                  
         OI    TRAOUT4H+6,X'80'                                                 
*                                                                               
         OC    SVLSTKEY,SVLSTKEY   TEST CONTINUATION PENDING                    
         BNZ   PR901                                                            
*                                                                               
         LA    R2,TRAMEDH                                                       
         OC    BUYTOTS,BUYTOTS                                                  
         BZ    NORECERR           NO RECORDS                                    
*                                                                               
PR901    DS    0H                                                               
*                                                                               
         LA    R2,TRAOUT1H                                                      
         ICM   R0,15,DELBUYS                                                    
         BZ    PR905                                                            
         MVC   8(16,R2),=C'999 BUYS DELETED'                                    
         CLI   TESTRUN,C'Y'                                                     
         BNE   *+10                                                             
         MVC   8(40,R2),=C'999 BUYS WOULD BE DELETED - **TEST RUN**'            
         CH    R0,=H'1'                                                         
         BNE   *+8                                                              
         MVI   15(R2),C' '                                                      
         EDIT  (R0),(3,8(R2))                                                   
         OI    6(R2),X'80'         SET TO XMT                                   
         LA    R2,TRAOUT2H         ADVANCE OUTPUT POINTER                       
*                                                                               
PR905    DS    0H                                                               
         ICM   R0,15,ADDBUYS                                                    
         BZ    PR999                                                            
         MVC   8(14,R2),=C'999 BUYS ADDED'                                      
         CLI   TESTRUN,C'Y'                                                     
         BNE   *+10                                                             
         MVC   8(38,R2),=C'999 BUYS WOULD BE ADDED - **TEST RUN**'              
         CH    R0,=H'1'                                                         
         BNE   *+8                                                              
         MVI   15(R2),C' '                                                      
         EDIT  (R0),(3,8(R2))                                                   
         OI    6(R2),X'80'         SET TO XMT                                   
         B     PR999                                                            
         SPACE 1                                                                
*                                                                               
PR999    XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(22),=C'** ACTION COMPLETED **'                           
         OC    SVLSTKEY,SVLSTKEY   TEST CONTINUATION ALLOWED                    
         BZ    *+10                                                             
         MVC   CONHEAD(32),=C'HIT ENTER TO CONTINUE PROCESSING'                 
         B     PRX                                                              
*                                                                               
PRX      DS    0H                                                               
         GOTO1 ERREX2              EXIT WITH MESSAGE IN PLACE                   
*                                                                               
PRINTIT  DS    0H                                                               
         GOTO1 OPENPQ                                                           
*****************************************************************               
         DC    H'0'                PRINT REPORT HERE                            
*****************************************************************               
         EJECT                                                                  
CLRMED   NI    TRAMEDH+4,X'DF'                                                  
CLRCLT   NI    TRACLTH+4,X'DF'                                                  
CLRPRD   NI    TRAPRDH+4,X'DF'                                                  
CLROPT   NI    TRAOPTH+4,X'DF'                                                  
*                                                                               
*                                                                               
CLRPRES  DS    0H                                                               
         NI    TRALENFH+4,X'DF'                                                 
         NI    TRAPTRFH+4,X'DF'                                                 
         NI    TRACODFH+4,X'DF'                                                 
         NI    TRAPERFH+4,X'DF'                                                 
         NI    TRALENTH+4,X'DF'                                                 
         NI    TRAPTRTH+4,X'DF'                                                 
         NI    TRACODTH+4,X'DF'                                                 
         NI    TRAPERTH+4,X'DF'                                                 
*                                                                               
         XC    SVTODATA,SVTODATA                                                
         XC    SVFRDATA,SVFRDATA                                                
         BR    RE                                                               
         EJECT                                                                  
* VALIDATE COPY CODE (ESTIMATE IF PROFILE COPY CODE = EST ON) *                 
         SPACE                                                                  
VCD      NTR1                                                                   
         MVI   CODE,0                                                           
         MVI   BEST,0                                                           
         CLI   SVPROF11,C'E'       COPY CODE = EST                              
         BE    VCD20                                                            
         SPACE                                                                  
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VCD10                                                            
         CLI   5(R2),1             COPY CODE ONLY 1 CHAR                        
         BH    CPYCDER                                                          
         SPACE                                                                  
         GOTO1 ANY                                                              
         SPACE                                                                  
         MVC   CODE,WORK                                                        
VCD10    OI    4(R2),X'20'         VALIDATED                                    
         XIT1                                                                   
         SPACE                                                                  
* VALIDATE ESTIMATE AND STORE EST DATES *                                       
         SPACE                                                                  
VCD20    CLI   5(R2),0                                                          
         BE    NOESTER                                                          
         SPACE                                                                  
         CLC   =C'NO',8(R2)        NO ESTIMATE                                  
         BE    VCD10                                                            
         SPACE                                                                  
         GOTO1 VALINUM                                                          
         SPACE                                                                  
         MVC   CODE,ACTUAL                                                      
         SPACE                                                                  
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING EKEY,R4                                                          
         MVC   EKEYAM(3),BAGYMD                                                 
         MVC   EKEYPRD,QPRD                                                     
         MVC   EKEYEST,ACTUAL                                                   
         MVC   BEST,ACTUAL                                                      
         DROP  R4                                                               
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTDIR' SWITCH TO SPOT SYSTEM                      
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   BDESTER                                                          
         SPACE                                                                  
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTFIL' SWITCH TO SPOT SYSTEM                      
         GOTO1 GETREC                                                           
         USING ESTHDRD,R6                                                       
         GOTO1 DATCON,DMCB,(0,ESTART),(3,ESTSTR)                                
         GOTO1 (RF),(R1),(0,EEND),(3,ESTEND)                                    
         SPACE                                                                  
         XC    FILENAME,FILENAME                                                
         SPACE                                                                  
         B     VCD10                                                            
         SPACE                                                                  
BDESTER  MVI   ERROR,BADESTS       BAD ESTIMATE NUMBER                          
         GOTO1 ERREX                                                            
*                                                                               
EQVPRDER MVC   CONHEAD,EQVPRDMS                                                 
         B     VCDERX                                                           
NORECERR MVC   CONHEAD,NORECMSG                                                 
         B     VCDERX                                                           
NOESTER  MVC   CONHEAD,NOESTMS                                                  
         B     VCDERX                                                           
CPYCDER  MVC   CONHEAD,CPYCDMS                                                  
VCDERX   GOTO1 ERREX2                                                           
NOESTMS  DC    CL60'* ERROR * ESTIMATE MUST BE ENTERED IN COPY CODE *'          
CPYCDMS  DC    CL60'* ERROR * COPY CODE CAN ONLY BE 1 CHARACTER *'              
NORECMSG DC    CL60'* ERROR * NO BUYS FOUND *'                                  
EQVPRDMS DC    CL60'* ERROR * PROD AND PTR EQUIVALENT TO BASE PRD *'            
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* GET PROFILE REC(S)                                                            
         SPACE                                                                  
         DS    0H                                                               
FPRO     NTR1                                                                   
         SPACE                                                                  
* READ T0 PROFILE *                                                             
         SPACE                                                                  
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0T0'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),TRAMED                                                 
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCLTOFF                                              
         GOTO1 GETPROF,DMCB,WORK,SVPROF,DATAMGR                                 
         SPACE                                                                  
         MVC   ORGPRF13,SVPROF13                                                
         MVI   SVPROF13,C'N'                                                    
         SPACE                                                                  
* READ T1 PROFILE *                                                             
         SPACE                                                                  
         MVI   WORK+3,C'1'                                                      
         GOTO1 GETPROF,DMCB,WORK,SVT1PROF,DATAMGR                               
         SPACE                                                                  
* READ TB PROFILE *                                                             
         SPACE                                                                  
         MVI   WORK+3,C'B'                                                      
         GOTO1 GETPROF,DMCB,WORK,SVTBPROF,DATAMGR                               
         SPACE                                                                  
         B     EXIT                                                             
         EJECT                                                                  
* VALIDATE PRODUCT *                                                            
         SPACE                                                                  
         DS    0H                                                               
VPR      NTR1                      VALIDATE PRODUCT                             
         GOTO1 VALIPRD                                                          
         CLC   WORK(3),=C'POL'     INVALID                                      
         BE    PRDERR                                                           
         MVC   QPRD,WORK                                                        
         CLI   WORK+4,0            SPOT LENGTH  - INVALID HERE                  
         BNE   PRDERR                                                           
         SPACE                                                                  
VPR10    MVC   BPRD(1),WORK+3                                                   
         SPACE                                                                  
VPR20    OI    4(R2),X'20'         VALIDATED                                    
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* VALIDATE PARTNER PRODUCT *                                                    
         SPACE                                                                  
         DS    0H                                                               
VPP      NTR1                                                                   
         XC    BPRD2(2),BPRD2                                                   
         XC    QPRD2,QPRD2                                                      
         CLI   5(R2),0                                                          
         BE    VPP20                                                            
         GOTO1 VALIPRD                                                          
         CLC   WORK(3),=C'POL'     INVALID                                      
         BE    PRDERR                                                           
         MVC   QPRD2,WORK                                                       
         CLI   WORK+4,0            VALID SPOT LENGTH                            
         BNE   VPP10                                                            
         MVI   WORK+4,30                                                        
         LA    R1,11(R2)         HEADER +3                                      
         CLI   10(R2),C' '       HEADER +2                                      
         BH    *+6                                                              
         BCTR  R1,0                                                             
         MVC   0(3,R1),=C'-30'                                                  
         OI    6(R2),X'80'                                                      
         SPACE                                                                  
VPP10    MVC   BPRD2(2),WORK+3                                                  
         SPACE                                                                  
         CLC   QPRD,QPRD2          ARE PRODUCTS IN SEQ                          
         BE    EQPRDER                                                          
         BH    PRDSEQER                                                         
         SPACE                                                                  
VPP20    OI    4(R2),X'20'         VALIDATED                                    
         CLI   ORGPRF13,C'Y'       THIS A PROD EQUIV CLT                        
         BNE   EXIT                                                             
         SPACE                                                                  
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         USING PEQKEY,R4                                                        
         MVC   PEQPID,=X'0AB7'                                                  
         MVC   PEQPAM(3),BAGYMD & BCLT                                          
         MVC   PEQPEPRD,QPRD                                                    
         MVC   WORK(3),QPRD                                                     
         SPACE                                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(8),KEYSAVE      THIS A EQUIV PRODUCT                         
         BNE   VPP30                NO, CK PTR                                  
         MVC   WORK(3),PEQPBPRD                                                 
         SPACE                                                                  
VPP30    XC    KEY,KEY                                                          
         MVC   PEQPID,=X'0AB7'                                                  
         MVC   PEQPAM(3),BAGYMD & BCLT                                          
         MVC   PEQPEPRD,QPRD2                                                   
         MVC   WORK+(3),QPRD2                                                   
         SPACE                                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(8),KEYSAVE      THIS A EQUIV PRODUCT                         
         BNE   VPP40                NO, CK PTR                                  
         MVC   WORK+3(3),PEQPBPRD                                               
VPP40    CLC   WORK(3),WORK+3      ARE PRODUCTS SAME BASE?                      
         BE    EQVPRDER             YES                                         
         SPACE                                                                  
         B     EXIT                                                             
         EJECT                                                                  
* VALIDATE OPTIONS                                                              
         SPACE                                                                  
         DS    0H                                                               
VOPT     NTR1                                                                   
         SPACE                                                                  
         XC    OPTIONS,OPTIONS                                                  
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VOPT96              NO                                           
         CLI   8(R2),C'?'          HELP                                         
         BE    VOPTHLP             YES                                          
         CLI   5(R2),4                                                          
         BNH   VOPT02                                                           
         LA    R1,4                                                             
         B     VOPT04                                                           
VOPT02   ZIC   R1,5(R2)                                                         
VOPT04   EX    R1,VOPTCLCH         HELP                                         
         BE    VOPTHLP                                                          
         SPACE                                                                  
         GOTO1 SCANNER,DMCB,TRAOPTH,(7,BLOCK+64)                                
         ZIC   R3,DMCB+4           GET NUMBER OF BLOCKS                         
         LTR   R3,R3               SEE IF SCANNER FOUND ANYTHING                
         BZ    MISSERR             NO                                           
         LA    R4,BLOCK+64         ADDRESS OF FIRST BLOCK                       
VOPT10   ZIC   R1,0(R4)            GET LENGTH                                   
         BCTR  R1,0                                                             
         SPACE                                                                  
VOPT14   EX    R1,VOPTCLCA         TEST                                         
         BNE   VOPTHLP                                                          
         MVI   TESTRUN,C'Y'                                                     
         B     VOPT90                                                           
         SPACE                                                                  
VOPT90   LA    R4,32(,R4)          POINT TO NEXT BLOCK                          
         BCT   R3,VOPT10           FOR NUMBER OF BLOCKS FOUND                   
         SPACE                                                                  
VOPT96   B     EXIT                                                             
         SPACE 3                                                                
VOPTCLCA CLC   12(0,R4),=CL4'TEST'                                              
VOPTCLCH CLC   8(0,R2),=CL4'HELP'                                               
         SPACE 2                                                                
VOPTHLP  L     R1,=A(OPTHLPMS)                                                  
         A     R1,RELO                                                          
         MVC   CONHEAD,0(R1)                                                    
         GOTO1 ERREX2                                                           
         EJECT                                                                  
         SPACE 1                                                                
CHKOV    NTR1                    CHECK FOR OVERLAP (COPY OR MOVE)               
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
OV135    BAS   RE,NEXTEL                                                        
         BNE   OV135C                                                           
         USING TBYDTAEL,R6                                                      
         CLC   SVLENT(3),TBYSLN    'TO' SPOT LEN/PARTNER/SPOT LEN               
         BNE   OV135                                                            
         CLC   TBYCODE,SVCODT      MATCH 'TO' CODE                              
         BNE   OV135                                                            
         CLC   TBYSTART(3),SVPERT  MATCH 'TO' PERIOD                            
         BH    OV135                                                            
         CLC   TBYEND(3),SVPERTND                                               
         BNL   DATOVLER           OVERLAP ERROR                                 
         B     OV135                                                            
*                                                                               
OV135C   XIT1                                                                   
*                                                                               
*                                                                               
DATOVLER MVC   CONHEAD,DATOVLMS                                                 
         GOTO1 DATCON,DMCB,(3,TBYSTART),(5,CONHEAD+34)                          
         MVI   CONHEAD+42,C'-'                                                  
         GOTO1 DATCON,DMCB,(3,TBYEND),(5,CONHEAD+43)                            
         DROP  R6                                                               
         L     R6,AIO                                                           
         USING TBYRECD,R6                                                       
         MVC   WORK(7),SPACES                                                   
         GOTO1 MSUNPK,DMCB,TBYKMKT,CONHEAD,WORK                                 
         CLI   WORK+4,C' '                                                      
         BNE   *+8                                                              
         MVI   WORK+4,C'T'                                                      
         MVC   CONHEAD+5(7),SPACES                                              
         MVC   CONHEAD+5(4),WORK                                                
         LA    RE,CONHEAD+8                                                     
         CLI   0(RE),C' '                                                       
         BNE   *+6                                                              
         BCTR  RE,0                                                             
         MVI   1(RE),C'-'                                                       
         MVC   2(1,RE),WORK+4                                                   
         CLI   QMED,C'T'                                                        
         BE    DAT02                                                            
         MVI   3(RE),C'M'                                                       
         CLI   QMED,C'R'                                                        
         BE    DAT02                                                            
         MVI   3(RE),C' '                                                       
*                                                                               
DAT02    LA    R2,TRAOUT1H                                                      
         ICM   R0,15,DELBUYS                                                    
         BZ    DAT05                                                            
         MVC   8(16,R2),=C'999 BUYS DELETED'                                    
         CLI   TESTRUN,C'Y'                                                     
         BNE   *+10                                                             
         MVC   8(40,R2),=C'999 BUYS WOULD BE DELETED - **TEST RUN**'            
         CH    R0,=H'1'                                                         
         BNE   *+8                                                              
         MVI   15(R2),C' '                                                      
         EDIT  (R0),(3,8(R2))                                                   
         OI    6(R2),X'80'         SET TO XMT                                   
         LA    R2,TRAOUT2H         ADVANCE OUTPUT POINTER                       
*                                                                               
DAT05    DS    0H                                                               
         ICM   R0,15,ADDBUYS                                                    
         BZ    DAT99                                                            
         MVC   8(14,R2),=C'999 BUYS ADDED'                                      
         CLI   TESTRUN,C'Y'                                                     
         BNE   *+10                                                             
         MVC   8(38,R2),=C'999 BUYS WOULD BE ADDED - **TEST RUN**'              
         CH    R0,=H'1'                                                         
         BNE   *+8                                                              
         MVI   15(R2),C' '                                                      
         EDIT  (R0),(3,8(R2))                                                   
         OI    6(R2),X'80'         SET TO XMT                                   
         B     DAT99                                                            
         SPACE 1                                                                
*                                                                               
DAT99    DS    0H                                                               
         LA    R2,TRAOUT3H                                                      
         XC    TRAOUT3,TRAOUT3                                                  
         OI    TRAOPTH+1,X'01'   SET TO MODIFIED                                
         FOUT  TRAOPTH                                                          
         NI    DMINBTS,X'F7'                                                    
         GOTO1 SEQ               GET PAST OFFENDING BUY                         
         OI    DMINBTS,X'08'     RESET                                          
         MVC   SVLSTKEY,KEY      PROCESSING INTERRUPTED                         
*                                SAVE KEY FOR NEXT TIME                         
         MVC   8(32,R2),=C'HIT ENTER TO CONTINUE PROCESSING'                    
         OI    6(R2),X'80'         SET TO XMT                                   
         GOTO1 ERREX2                                                           
DATOVLMS DC    CL70'MMMM SSSSSS BUY OVERLAPS EXISTING MONDA/YY-MONDA/YYC        
                *'                                                              
         DROP  R6                                                               
         EJECT                                                                  
         SPACE 1                                                                
CHKDUP   NTR1                    CHECK FOR DUPLICATE (COPY)                     
         MVI   DUPSW,C'N'                                                       
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
DUP135   BAS   RE,NEXTEL                                                        
         BNE   DUP135C                                                          
         USING TBYDTAEL,R6                                                      
         CLC   SVLENT(3),TBYSLN    'TO' SPOT LEN/PARTNER/SPOT LEN               
         BNE   DUP135                                                           
         CLC   TBYCODE,SVCODT      MATCH 'TO' CODE                              
         BNE   DUP135                                                           
         CLC   TBYSTART(3),SVPERT  MATCH 'TO' PERIOD                            
         BNE   DUP135                                                           
         CLC   TBYEND(3),SVPERTND                                               
         BNE   DATOVLER                                                         
         MVI   DUPSW,C'Y'           ALREADY THERE                               
DUP135C  XIT1                                                                   
         SPACE                                                                  
         DROP  R6                                                               
         EJECT                                                                  
**********************************************                                  
* SUBROUTINE TO ADD NEW TRAFFICE ELEM        *                                  
**********************************************                                  
         SPACE 1                                                                
ADDBUY   NTR1                                                                   
         L     R6,ELEADDR         RESET R6                                      
         L     R1,ADDBUYS                                                       
         LA    R1,1(R1)                                                         
         ST    R1,ADDBUYS                                                       
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R7,ELEM                                                          
         MVC   ELEM(TBYDTAX-TBYDTAEL),0(R6)                                     
         USING TBYDTAEL,R7                                                      
         MVC   TBYSLN,SVLENT       SET 'TO' LEN                                 
         MVC   TBYPRD2,SVPTRT      SET 'TO' PARTNER                             
         MVC   TBYSLN2,SVPTRLT     SET 'TO' PARTNER LEN                         
         MVC   TBYCODE,SVCODT      SET 'TO' CODE                                
         MVC   TBYSTART(6),SVPERT  SET 'TO' PERIOD                              
         GOTO1 ADDELEM                                                          
         B     EXIT                                                             
*                                                                               
*****************************************************************               
* SUBROUTINE TO DELETE TRAFFIC BUY ELEM (MOVE OR CANCEL)        *               
*****************************************************************               
         SPACE 1                                                                
DELBUY   NTR1                                                                   
         MVC   KEY,SVKEY           RESTORE BUY KEY                              
         L     R1,DELBUYS                                                       
         LA    R1,1(R1)                                                         
         ST    R1,DELBUYS                                                       
*                                                                               
         GOTO1 VRECUP,DMCB,AIO,(R6)                                             
         B     EXIT                                                             
         EJECT                                                                  
* SUBROUTINE TO TEST IF ANY KEY FIELDS CHANGED                                  
* IF YES, EXIT WITH CC NEQ                                                      
         SPACE 1                                                                
TSTNEWKY NTR1                                                                   
         TM    TRAMEDH+4,X'20'                                                  
         BZ    NEQXIT                                                           
         TM    TRACLTH+4,X'20'                                                  
         BZ    NEQXIT                                                           
         TM    TRAPRDH+4,X'20'                                                  
         BZ    NEQXIT                                                           
         TM    TRAOPTH+4,X'20'                                                  
         BZ    NEQXIT                                                           
         TM    TRALENFH+4,X'20'                                                 
         BZ    NEQXIT                                                           
         TM    TRAPTRFH+4,X'20'                                                 
         BZ    NEQXIT                                                           
         TM    TRACODFH+4,X'20'                                                 
         BZ    NEQXIT                                                           
         TM    TRAPERFH+4,X'20'                                                 
         BZ    NEQXIT                                                           
         TM    TRALENTH+4,X'20'                                                 
         BZ    NEQXIT                                                           
         TM    TRAPTRTH+4,X'20'                                                 
         BZ    NEQXIT                                                           
         TM    TRACODTH+4,X'20'                                                 
         BZ    NEQXIT                                                           
         TM    TRAPERTH+4,X'20'                                                 
         BZ    NEQXIT                                                           
         B     EQXIT                                                            
         EJECT                                                                  
SCANERR  DS    0H                                                               
         GOTO1 ERREX                                                            
*                                                                               
         EJECT                                                                  
GETEL    AH    R6,DATADISP                                                      
FIRSTEL  CLI   0(R6),0                                                          
         BNE   *+10                                                             
         CLI   0(R6),1                                                          
         BR    RE                                                               
         CLI   ELCODE,0                                                         
         BCR   8,RE                                                             
         CLC   ELCODE,0(R6)                                                     
         BCR   8,RE                                                             
NEXTEL   SR    RF,RF                                                            
         IC    RF,1(R6)                                                         
         LTR   RF,RF                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,RF                                                            
         B     FIRSTEL                                                          
         EJECT                                                                  
MISSERR  MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
FLTDTER  MVI   ERROR,NOTFLTDT      PER END NO MATCH TO FLIGHT END DATE          
         B     TRAPERR                                                          
PRDERR   MVI   ERROR,INVPROD                                                    
         B     TRAPERR                                                          
PRDSEQER MVI   ERROR,INVPRDSQ                                                   
         B     TRAPERR                                                          
EQPRDER  MVI   ERROR,INVEQPRD                                                   
         B     TRAPERR                                                          
NUMERR   MVI   ERROR,NOTNUM                                                     
         B     TRAPERR                                                          
*                                                                               
DATERR   MVI   ERROR,INVDATE                                                    
TRAPERR  GOTO1 ERREX                                                            
*                                                                               
OPTHLPMS DC    CL60'VALID OPTIONS=TEST'                                         
         EJECT                                                                  
         SPACE 2                                                                
         LTORG                                                                  
HEADING  SSPEC H1,3,C'TRAFFIC SYSTEM'                                           
         SSPEC H2,3,C'MEDIA'                                                    
         SSPEC H3,3,C'CLIENT'                                                   
         SSPEC H4,3,C'PRODUCT'                                                  
         SSPEC H1,30,C'S T A T I O N  B U Y  L I S T'                           
         SSPEC H2,30,C'-----------------------------'                           
         SSPEC H1,73,AGYNAME                                                    
         SSPEC H2,73,AGYADD                                                     
         SSPEC H4,73,REPORT                                                     
         SSPEC H4,85,RUN                                                        
         SSPEC H5,93,PAGE                                                       
         SSPEC H5,73,REQUESTOR                                                  
         SSPEC H8,3,C'MARKET'                                                   
         SSPEC H9,3,C'------'                                                   
         SSPEC H8,13,C'MARKET NAME'                                             
         SSPEC H9,13,C'------------------------'                                
         SSPEC H8,39,C'STATION'                                                 
         SSPEC H9,39,C'-------'                                                 
         SSPEC H8,49,C'PROD/SLN'                                                
         SSPEC H9,49,C'--------'                                                
         SSPEC H8,59,C'PTNR/SLN'                                                
         SSPEC H9,59,C'--------'                                                
         SSPEC H8,72,C'COPY'                                                    
         SSPEC H9,72,C'CODE'                                                    
         SSPEC H8,81,C'PERIOD START/END DATES'                                  
         SSPEC H9,81,C'----------------------'                                  
         DC    X'00'               END MARKER FOR SSPEC                         
         DROP  RB,RC,R7                                                         
         SPACE 2                                                                
ACTCOPY  EQU   20                                                               
ACTMOVE  EQU   21                                                               
ACTCNCL  EQU   22                                                               
SAMEDATA EQU   2               INVALID INPUT                                    
NOOVRLAP EQU   2               INVALID INPUT                                    
         EJECT                                                                  
* SUBROUTINE VALIDATES START/END DATES FOR PERIOD *                             
         SPACE                                                                  
VPER     DS    0D                                                               
         NMOD1 0,**VPER**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
         SPACE                                                                  
VPER02   CLI   8(R2),C'?'          IF QUESTION MK, TELL MEL FLT DATES           
         BNE   VPER30                                                           
         CLI   SVPROF11,C'E'       COPY CODE = EST                              
         BE    VPER26                                                           
         CLI   5(R2),1             SEE IF DATE ENTERED TOO                      
         BE    VPER04              NO                                           
         GOTO1 DATVAL,DMCB,9(R2),WORK                                           
         L     R4,DMCB             GET LENGTH OF FIELD                          
         LTR   R4,R4                                                            
         BZ    DATERRA                                                          
         GOTO1 DATCON,(R1),(0,WORK),(3,PERST)                                   
         B     VPER06                                                           
VPER04   GOTO1 DATCON,DMCB,(5,0),(3,PERST) TODAY'S DATE                         
         SPACE                                                                  
VPER06   XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A27'                                                  
         MVC   KEY+2(4),BAGYMD BCLT AND BPRD                                    
         GOTO1 HIGH                                                             
         CLC   KEY(6),KEYSAVE                                                   
         BE    VPER10                                                           
         MVC   KEY,KEYSAVE         RESTORE ORIGINAL KEY                         
         MVI   KEY+5,0             CLEAR PRD TO TRY FOR CLT DATA                
         GOTO1 HIGH                                                             
         CLC   KEY(6),KEYSAVE                                                   
         BNE   FLTRECER                                                         
*                                                                               
VPER10   CLC   PERST,KEY+6         FIRST TLCST DATE TO RECORD END DATE          
         BNH   VPER14                                                           
         MVC   KEYSAVE,KEY                                                      
         GOTO1 SEQ                                                              
         CLC   KEY(6),KEYSAVE                                                   
         BE    VPER10                                                           
         MVC   KEY,KEYSAVE         GET LAST DATE BEFORE TODAY                   
         GOTO1 HIGH                                                             
         MVC   PERST(1),KEY+6      CHANGE YEAR                                  
*                                                                               
VPER14   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(5),=C'*END='                                             
         GOTO1 DATCON,DMCB,(3,KEY+6),(5,CONHEAD+5)                              
         SPACE 2                                                                
         XC    TRAOUT1,TRAOUT1                                                  
         XC    TRAOUT2,TRAOUT2                                                  
         XC    TRAOUT3,TRAOUT3                                                  
         XC    TRAOUT4,TRAOUT4                                                  
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
         SPACE                                                                  
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL1                                                        
         BNE   VPER25                                                           
         USING FLTDTAEL,R6                                                      
         LA    R4,4          DISPLAY REST OF DATES IN TRAOUT1,2,3,4             
         LA    R3,6                                                             
         LA    R5,TRAOUT1H                                                      
         ST    R5,FULL          SAVE R5                                         
         LA    R5,8(R5)                                                         
         B     VPER21C                                                          
*                                                                               
VPER21   CLI   0(R6),0          END OF RECORD                                   
         BE    VPER25                                                           
         BAS   RE,NEXTEL1                                                       
         BNE   VPER25                                                           
VPER21C  CLC   PERST,FLTEND                                                     
         BNH   VPER22                                                           
         CLC   PERST,FLTSTART                                                   
         BH    VPER21                                                           
         SPACE                                                                  
VPER22   GOTO1 DATCON,DMCB,(3,FLTSTART),(4,0(R5))                               
         MVI   5(R5),C'-'                                                       
         GOTO1 (RF),(R1),(3,FLTEND),(4,6(R5))                                   
         LA    R5,12(,R5)                                                       
         BCT   R3,VPER21                                                        
*                                                                               
         LA    R3,6                                                             
         L     R5,FULL                                                          
         LA    R5,L'TRAOUT1+8(R5)   BUMP TO NEXT FIELD HEADER                   
         ST    R5,FULL                                                          
         LA    R5,8(R5)                                                         
         BCT   R4,VPER21                                                        
*                                                                               
VPER25   FOUT  TRAOUT1H                                                         
         FOUT  TRAOUT2H                                                         
         FOUT  TRAOUT3H                                                         
         FOUT  TRAOUT4H                                                         
         B     ERREXIT3                                                         
         SPACE                                                                  
VPER26   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(13),=C'ESTIMATE RUNS'                                    
         GOTO1 DATCON,DMCB,(3,ESTSTR),(5,CONHEAD+14)                            
         MVI   CONHEAD+23,C'-'                                                  
         GOTO1 (RF),(R1),(3,ESTEND),(5,CONHEAD+25)                              
         B     ERREXIT3                                                         
         EJECT                                                                  
VPER30   XC    PERDTS,PERDTS                                                    
         CLI   5(R2),0             ANY ENTRY                                    
         BE    MISSERRA                                                         
         CLI   SVPROF11,C'E'       COPY CODE = EST                              
         BNE   VPER34                                                           
         CLC   =C'ES',8(R2)        USE ESTIMATE DATES                           
         BNE   VPER34                                                           
         CLI   BEST,0                                                           
         BE    BLKESTER                                                         
         MVC   PERDTS,ESTDTS                                                    
         GOTO1 DATCON,DMCB,(3,PERST),(5,8(R2))                                  
         MVI   16(R2),C'-'                                                      
         GOTO1 (RF),(R1),(3,PEREND),(5,17(R2))                                  
         OI    6(R2),X'80'                                                      
         B     VPER50                                                           
         SPACE                                                                  
VPER34   LA    R5,8(,R2)           START DATE                                   
         GOTO1 DATVAL,DMCB,(R5),WORK                                            
         L     R4,DMCB             GET LENGTH OF FIELD                          
         LTR   R4,R4                                                            
         BZ    DATERRA                                                          
         GOTO1 DATCON,(R1),(0,WORK),(3,PERST)                                   
         SPACE                                                                  
         CLM   R4,1,5(R2)          ONLY 1 DATE ENTERED                          
         BE    VPER40              YES                                          
         SPACE                                                                  
         LA    R5,1(R4,R5)         POINT TO END DATE                            
         GOTO1 DATVAL,(R1),(R5),WORK                                            
         OC    DMCB(4),DMCB                                                     
         BZ    DATERRA                                                          
         GOTO1 DATCON,(R1),(0,WORK),(3,PEREND)                                  
         CLC   PERST,PEREND                                                     
         BH    DATERRA                                                          
         EJECT                                                                  
VPER40   CLI   SVPROF11,C'E'       COPY CODE BY ESTIMATE                        
         BNE   VPER44                                                           
         BAS   RE,FEST             GO CK EST DATES                              
         B     VPER50                                                           
VPER44   BAS   RE,FFLT                                                          
         SPACE                                                                  
* FORCE PERIOD START TO MONDAYS *                                               
         SPACE                                                                  
VPER50   GOTO1 DATCON,DMCB,(3,PERST),(0,WORK)                                   
         GOTO1 GETDAY,(R1),WORK,WORK+6                                          
         CLI   0(R1),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         ZIC   R0,0(R1)                                                         
         BCTR  R0,0                                                             
         LCR   R0,R0                                                            
         GOTO1 ADDAY,(R1),WORK,WORK+6,(R0)                                      
         GOTO1 DATCON,(R1),(0,WORK+6),(2,PERMOSTB)                              
         GOTO1 (RF),(R1),(0,WORK+6),(3,PERMONST)                                
         GOTO1 (RF),(R1),(3,PERST),(2,PERSTB)                                   
         GOTO1 (RF),(R1),(3,PEREND),(2,PERENDB)                                 
         B     EXIT1                                                            
         EJECT                                                                  
***************************************************************                 
* SUBROUTINE DETERMINES FLIGHT DATES FOR GIVEN TELECAST DATES *                 
***************************************************************                 
         SPACE                                                                  
         DS    0H                                                               
FFLT     NTR1                                                                   
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A27'                                                  
         MVC   KEY+2(4),BAGYMD BCLT AND BPRD                                    
         GOTO1 HIGH                                                             
         CLC   KEY(6),KEYSAVE                                                   
         BE    FFLT10                                                           
         MVC   KEY,KEYSAVE         RESTORE ORIGINAL KEY                         
         MVI   KEY+5,0             CLEAR PRD TO TRY FOR CLT DATA                
         GOTO1 HIGH                                                             
*                                                                               
FFLT10   CLC   KEY(6),KEYSAVE                                                   
         BNE   FLTRECER                                                         
         CLC   PERST,KEY+6         FIRST TLCST DATE TO RECORD END DATE          
         BNH   FFLT14                                                           
         GOTO1 SEQ                                                              
         B     FFLT10                                                           
*                                                                               
FFLT14   L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL1                                                        
         B     *+8                                                              
*                                                                               
FFLT16   BAS   RE,NEXTEL1                                                       
         BNE   FLTELER                                                          
*                                                                               
         USING FLTDTAEL,R6                                                      
*                                                                               
         CLC   PERST,FLTEND        FIRST TLCST AFTER FLIGHT END                 
         BH    FFLT16                                                           
         SPACE                                                                  
         OC    PEREND,PEREND       ANY END DATE ENTERED                         
         BZ    FFLT20                                                           
         SPACE                                                                  
         CLC   PEREND,FLTSTART     LAST TLCST BEFORE FLIGHT START               
         BL    FFLT16                                                           
         SPACE                                                                  
* TELECAST DATES SHOULD FALL ENTIRELY WITHIN THIS FLIGHT *                      
         SPACE                                                                  
         CLC   PEREND,FLTEND       LAST TLCST DATE TO FLT END                   
         BH    FLTOVLER                                                         
         CLC   PERST,FLTSTART                                                   
         BL    FLTOVLER                                                         
         B     EXIT1                                                            
         SPACE                                                                  
* ONLY ONE DATE GIVEN, MUST MATCH FLIGHT START                                  
         SPACE                                                                  
FFLT20   CLC   PERST,FLTSTART      PER START MATCH FLIGHT START                 
         BNE   FFLT16                                                           
         MVC   PEREND,FLTEND                                                    
         GOTO1 DATCON,DMCB,(3,PERST),(5,8(R2))                                  
         MVI   16(R2),C'-'                                                      
         GOTO1 (RF),(R1),(3,PEREND),(5,17(R2))                                  
         OI    6(R2),X'80'                                                      
         B     EXIT1                                                            
         EJECT                                                                  
*****************************************************************               
* SUBROUTINE DETERMINES ESTIMATE DATES FOR GIVEN TELECAST DATES *               
*****************************************************************               
         SPACE                                                                  
         DS    0H                                                               
FEST     NTR1                                                                   
         SPACE                                                                  
* TELECAST DATES SHOULD FALL ENTIRELY WITHIN THIS ESTIMATE *                    
         SPACE                                                                  
         CLC   PERST,ESTEND        FIRST TLCST AFTER EST END                    
         BH    ESTDTERR                                                         
         CLC   PERST,ESTSTR        FIRST TLCST BEFORE EST STR                   
         BL    ESTDTERR                                                         
         SPACE                                                                  
         OC    PEREND,PEREND       ANY END DATE ENTERED                         
         BZ    FEST10                                                           
         SPACE                                                                  
         CLC   PEREND,ESTSTR       LAST TLCST BEFORE EST START                  
         BL    ESTDTERR                                                         
         CLC   PEREND,ESTEND       LAST TLCST BEFORE EST END                    
         BH    ESTDTERR                                                         
         SPACE                                                                  
         B     EXIT1                                                            
         SPACE                                                                  
* ONLY ONE DATE GIVEN, MUST MATCH FLIGHT START                                  
         SPACE                                                                  
FEST10   CLC   PERST,ESTSTR        PER START MATCH ESTIMATE START               
         BNE   ESTDTERR                                                         
         MVC   PEREND,ESTEND                                                    
         GOTO1 DATCON,DMCB,(3,PERST),(5,8(R2))                                  
         MVI   16(R2),C'-'                                                      
         GOTO1 (RF),(R1),(3,PEREND),(5,17(R2))                                  
         OI    6(R2),X'80'                                                      
EXIT1    XIT1                                                                   
         EJECT                                                                  
GETEL1   AH    R6,DATADISP                                                      
FIRSTEL1 CLI   0(R6),0                                                          
         BNE   *+10                                                             
         CLI   0(R6),1                                                          
         BR    RE                                                               
         CLI   ELCODE,0                                                         
         BCR   8,RE                                                             
         CLC   ELCODE,0(R6)                                                     
         BCR   8,RE                                                             
NEXTEL1  SR    RF,RF                                                            
         IC    RF,1(R6)                                                         
         LTR   RF,RF                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,RF                                                            
         B     FIRSTEL1                                                         
ESTDTERR MVC   CONHEAD,ESTDTEMS                                                 
         B     ERREXIT3                                                         
         SPACE                                                                  
BLKESTER MVC   CONHEAD,BLKESTMS                                                 
         SPACE                                                                  
ERREXIT3 GOTO1 ERREX2                                                           
         SPACE                                                                  
MISSERRA MVI   ERROR,MISSING                                                    
         B     TRAPERRA                                                         
FLTRECER MVI   ERROR,NOFLTREC                                                   
         B     TRAPERRA                                                         
FLTELER  MVI   ERROR,NOFLTEL                                                    
         B     TRAPERRA                                                         
FLTOVLER MVI   ERROR,FLTOVLAP                                                   
         B     TRAPERRA                                                         
DATERRA  MVI   ERROR,INVDATE                                                    
TRAPERRA GOTO1 ERREX                                                            
BLKESTMS DC    CL60'* ERROR * ESTIMATE MUST BE ENTERED FOR PERIOD ES *'         
ESTDTEMS DC    CL60'* ERROR * DATE(S) NOT IN ESTIMATE PERIOD *'                 
         DROP  R6,RB,RC                                                         
         SPACE                                                                  
         LTORG                                                                  
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDFLDIND                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE SPTRAFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPTRA87D                                                       
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE SPTRAWORKD                                                     
         EJECT                                                                  
SYSD     DSECT                                                                  
         ORG   SVSPAREX                                                         
SVTODATA DS    0XL10          'TO' DATA                                         
SVLENT   DS    XL1            LENGHT                                            
SVPTRT   DS    XL1            PARTNER                                           
SVPTRLT  DS    XL1            PARTNER LENGHT                                    
SVCODT   DS    CL1            CODE                                              
SVPERT   DS    XL6            PERIOD                                            
         ORG   SVPERT                                                           
SVPERTST DS    XL3                                                              
SVPERTND DS    XL3                                                              
*                                                                               
SVFRDATA DS    0XL10         'FROM' DATA                                        
SVLENF   DS    XL1            LENGHT                                            
SVPTRF   DS    XL1            PARTNER                                           
SVPTRLF  DS    XL1            PARTNER LENGHT                                    
SVCODF   DS    CL1           CODE                                               
SVPERF   DS    CL6           PERIOD                                             
         ORG   SVPERF                                                           
SVPERFST DS    XL3                                                              
SVPERFND DS    XL3                                                              
*                                                                               
         DS    CL7                                                              
PERDTS   DS    0XL6                                                             
PERST    DS    XL3                                                              
PEREND   DS    XL3                                                              
PERMONST DS    XL3                 PERIOD START (PRECEEDING MONDAY)             
PERMOSTB DS    XL2                 BINARY DATES                                 
PERSTB   DS    XL2                 BINARY DATES                                 
PERENDB  DS    XL2                 BINARY DATES                                 
*                                                                               
BSTART   DS    XL3                                                              
BEND     DS    XL3                                                              
DATE     DS    CL6                                                              
*                                                                               
ESTDTS   DS    0XL6                                                             
ESTSTR   DS    XL3                                                              
ESTEND   DS    XL3                                                              
*                                                                               
CODE     DS    CL1                                                              
DELSW    DS    CL1                                                              
DUPSW    DS    CL1                                                              
OLDELEM  DS    CL100                                                            
ELEADDR  DS    F                                                                
SAVERE   DS    A                                                                
ORGPRF13 DS    CL1                                                              
SVTBPROF DS    CL16                                                             
SVTBPR1  EQU   SVTBPROF+0                                                       
SVTBPR2  EQU   SVTBPROF+1                                                       
SVTBPR3  EQU   SVTBPROF+2                                                       
SVMAXIO  DS    H                                                                
SVLSTKEY DS    XL13                                                             
*                                                                               
OPTIONS  DS    0CL8                                                             
TESTRUN  DS    CL1                 'Y' FOR TEST RUN                             
         DS    CL7                 FUTURE OPTIONS                               
         DS    0F                                                               
BUYTOTS  DS    XL8                                                              
         ORG   BUYTOTS                                                          
ADDBUYS  DS    F                   NUM BUYS ADDED                               
DELBUYS  DS    F                   NUM BUYS DELETED                             
         SPACE 2                                                                
         EJECT                                                                  
       ++INCLUDE SPTRBUY                                                        
         EJECT                                                                  
       ++INCLUDE SPTRPRDEQV                                                     
         EJECT                                                                  
       ++INCLUDE SPTRFLT                                                        
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
         EJECT                                                                  
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
         EJECT                                                                  
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE FAFACTS                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010SPTRA27   08/30/04'                                      
         END                                                                    
