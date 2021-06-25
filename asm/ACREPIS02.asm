*          DATA SET ACREPIS02  AT LEVEL 031 AS OF 04/10/15                      
*PHASE ACIS02A                                                                  
*INCLUDE SORTER                                                                 
*INCLUDE CHOPPER                                                                
*INCLUDE SQUASHER                                                               
*INCLUDE DATVAL                                                                 
*INCLUDE GETBROAD                                                               
*INCLUDE ADDAY                                                                  
*INCLUDE CLPACK                                                                 
*INCLUDE MEDGET                                                                 
         TITLE 'INTEREP INTERFACE TAPE'                                         
ACIS02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACIS**,R9       R9=2ND BASE REGISTER                         
         L     RA,0(R1)                                                         
         USING ACWORKD,RA          RA=A(GLOBAL W/S)                             
         LA    RC,SPACEND                                                       
         USING LWSD,RC             RC=A(SAVE W/S)                               
         L     R8,VBIGPRNT                                                      
         USING BIGPRNTD,R8                                                      
*                                                                               
         CLI   MODE,RUNFRST        RUN FIRST                                    
         BE    RUNF                                                             
         CLI   MODE,REQFRST        REQ FIRST                                    
         BE    REQF                                                             
         CLI   MODE,PROCACC        PROCESS ACCOUNT                              
         BE    PACC                                                             
         CLI   MODE,PROCTRNS       PROCESS TRANSACTIONS                         
         BE    PTRN                                                             
         CLI   MODE,REQLAST        REQ LAST                                     
         BE    REQL                                                             
EXIT     XMOD1 1                                                                
         EJECT                                                                  
*---------------------------------------------------------------------*         
*              RUN FIRST                                              *         
*---------------------------------------------------------------------*         
RUNF     DS    0H                                                               
         LA    RE,ATYPES           MOVE ATYPES TO W/S                           
         LA    R0,ADCONS                                                        
         LA    RF,ATYPLNQ                                                       
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
*                                                                               
         L     R2,=A(BOXRC)                                                     
         ST    RC,0(R2)            SAVE RC                                      
         L     R2,VEXTRAS                                                       
         USING RUNXTRAD,R2                                                      
         MVC   AUTL,VUTL                                                        
         L     R2,ADMASTD                                                       
         USING MASTD,R2                                                         
         L     R2,MCBXAREA                                                      
         ST    R2,ADBOX            STORE ADDR OF BOX ROUTINE                    
         L     R2,=A(BXHOOK)                                                    
         ST    R2,HEADHOOK                                                      
         LA    R2,IO1                                                           
         ST    R2,AIO1                                                          
         LA    R2,IO2                                                           
         ST    R2,AIO2                                                          
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*---------------------------------------------------------------------*         
*              REQUEST FIRST                                          *         
*---------------------------------------------------------------------*         
REQF     DS    0H                                                               
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         ZAP   TAPECNT,=P'0'                                                    
         ZAP   CONTOT,=P'0'                                                     
         ZAP   REPTOT,=P'0'                                                     
         XC    CONSAVE,CONSAVE                                                  
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD,0                                   
*                                                                               
         USING CPYELD,R4                                                        
         L     R4,ADCMPEL                                                       
         MVC   MEDAGY,CPYALPHA                                                  
*                                                                               
         BAS   RE,SPOTOPEN         OPEN SPOT FILES                              
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*---------------------------------------------------------------------*         
*              PROCACC                                                *         
*---------------------------------------------------------------------*         
PACC     DS    0H                                                               
         USING ACTRECD,R3                                                       
         L     R3,ADACC            R3=A(ACCOUNT RECORD)                         
         MVC   MEDMED,ACTKACT                                                   
         LA    R4,LEDGTAB                                                       
PACC10   CLI   0(R4),X'FF'                                                      
         BE    EXIT                                                             
         CLC   ACTKUNT,0(R4)                                                    
         BE    PACC15                                                           
         LA    R4,1(R4)                                                         
         B     PACC10                                                           
PACC15   MVC   SIC,SPACES          CLEAR STATION INTERFACE CODE                 
         MVC   CALLCODE,SPACES     CLEAR CALL LETTER CODE                       
*                                                                               
         L     R5,ADACC            R5=A(ACCOUNT RECORD)                         
         MVI   ELCODE,X'20'        NAME ELEMENT - GET CALL LETTERS              
         BAS   RE,GETEL                                                         
         BNE   PACC25                                                           
         MVC   WORK,SPACES                                                      
         USING NAMELD,R5                                                        
         ZIC   R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q)                                                   
         EXMVC R1,WORK,NAMEREC                                                  
         MVC   CALLCODE,NAMEREC                                                 
*                                                                               
PACC25   L     R5,ADACC            R5=A(ACCOUNT RECORD)                         
         MVI   ELCODE,ADRELQ       ADDRESS ELEMENT                              
         BAS   RE,GETEL                                                         
         BNE   PACCX                                                            
*                                                                               
         USING ADRELD,R5                                                        
         MVC   SIC,3(R5)                                                        
         OC    SIC,SPACES                                                       
*                                                                               
PACCX    B     EXIT                                                             
         EJECT                                                                  
*---------------------------------------------------------------------*         
*              PROCTRNS                                               *         
*---------------------------------------------------------------------*         
PTRN     DS    0H                                                               
         USING ACTRECD,R3                                                       
         L     R3,ADTRANS            R3=CURRENT TRAN RECORD                     
         SH    R3,DATADISP                                                      
         LA    R4,LEDGTAB                                                       
PTRN10   CLI   0(R4),X'FF'                                                      
         BE    EXIT                                                             
         CLC   ACTKUNT,0(R4)                                                    
         BE    PTRN15                                                           
         LA    R4,1(R4)                                                         
         B     PTRN10                                                           
PTRN15   DS    0H                                                               
*                                                                               
         USING TRNELD,R5                                                        
         L     R5,ADTRANS            R5=A(TRANSACTION ELEMENT)                  
         TM    TRNSTAT,X'80'         ONLY TAKE CREDITS                          
         BO    EXIT                                                             
*                                                                               
         CLI   TRNTYPE,X'22'                                                    
         BNE   PTRN20                                                           
         USING TRNRECD,R6                                                       
         LR    R6,R5                                                            
         SH    R6,DATADISP                                                      
         MVC   CALLCODE,TRNKCACT+1                                              
         MVC   SIC,SPACES                                                       
         DROP  R3,R5                                                            
*                                                                               
         USING ACTRECD,R5                                                       
         L     R5,AIO1                                                          
         MVC   0(49,R5),SPACES                                                  
         MVC   ACTKCPY,TRNKCPY                                                  
         MVC   ACTKUNT,TRNKUNT                                                  
         MVC   ACTKLDG,TRNKLDG                                                  
         MVC   ACTKACT(6),TRNKCACT                                              
         DROP  R6                                                               
         DROP  R5                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,DMREAD,=C'ACCOUNT',AIO1,AIO2                        
         L     R5,AIO2                                                          
         CLI   DMCB+8,0                                                         
         BNE   PTRN20                                                           
         MVI   ELCODE,ADRELQ                                                    
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SIC,3(R5)                                                        
*                                                                               
         USING SRTRECD,R6                                                       
PTRN20   L     R6,ASRTREC              R6 COVERS SORT RECORD                    
         XC    0(SRTINFO,R6),0(R6)  CLEAR OUR SORT AREA                         
         USING ACKEYD,R3                                                        
         MVC   SRTCLI,ACKEYCON+12                                               
         MVC   SRTPRD,ACKEYREF                                                  
*                                                                               
         USING TRNELD,R5                                                        
         L     R5,ADTRANS            R5=A(TRANSACTION ELEMENT)                  
         MVC   SRTSIC,SIC                                                       
         MVC   SRTCALL,CALLCODE                                                 
         CLI   SRTCALL+L'SRTCALL-1,C'-'                                         
         BNE   *+10                                                             
         MVC   SRTCALL+L'SRTCALL-1(1),CALLCODE+L'CALLCODE-1                     
         ZAP   SRTAMT,TRNAMNT                                                   
*                                                                               
         LA    R3,SRTCOMS                                                       
         LA    R4,5                                                             
PTR45    MVC   0(L'SRTCOM1,R3),SPACES                                           
         LA    R3,L'SRTCOM1(R3)                                                 
         BCT   R4,PTR45                                                         
*                                                                               
         ZIC   R3,TRNLN                                                         
         LA    R4,TRNLN1Q                                                       
         SR    R3,R4                                                            
         STC   R3,BYTE                                                          
         CH    R3,=H'1'                                                         
         BNH   PTR100                                                           
         GOTO1 CHOPPER,DMCB,(BYTE,TRNNARR),(78,SRTCOMS),(78,5)                  
         LA    R3,78                                                            
         GOTO1 SQUASHER,DMCB,SRTCOM1,(R3),RR=RB                                 
         GOTO1 SQUASHER,DMCB,SRTCOM2,(R3),RR=RB                                 
         GOTO1 SQUASHER,DMCB,SRTCOM3,(R3),RR=RB                                 
         GOTO1 SQUASHER,DMCB,SRTCOM4,(R3),RR=RB                                 
         GOTO1 SQUASHER,DMCB,SRTCOM5,(R3),RR=RB                                 
*                                                                               
         USING XPYELD,R5                                                        
PTR100   ZIC   R1,1(R5)                                                         
         AR    R5,R1                                                            
         CLI   0(R5),0                                                          
         BE    PTR130                                                           
         CLI   0(R5),XPYELQ                                                     
         BNE   PTR100                                                           
         MVC   SRTINV,XPYINV                                                    
         OC    SRTINV,SPACES                                                    
         MVC   SRTEST,XPYEST+1                                                  
         GOTO1 DATCON,DMCB,(0,XPYPER),(0,XPYPER)                                
         GOTO1 DATCON,DMCB,(0,XPYPER+6),(0,XPYPER+6)                            
         GOTO1 GETBROAD,DMCB,(1,XPYPER),WORK,GETDAY,ADDAY                       
         GOTO1 GETBROAD,DMCB,(1,XPYPER+6),WORK+12,GETDAY,ADDAY                  
         MVC   SRTBRDYR,WORK+6                                                  
         MVC   SRTBRDMO,WORK+8                                                  
         CLC   SRTBRD,QSTART                                                    
         BL    EXIT                                                             
         CLC   SRTBRD,QEND                                                      
         BH    EXIT                                                             
*                                                                               
PTR130   DS    0H                                                               
         BAS   RE,SPOTREAD                                                      
         GOTO1 SORTER,DMCB,=C'PUT',(R6)                                         
         B     EXIT                                                             
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        RETRIEVE RECORDS FROM SORTER                                 *         
*---------------------------------------------------------------------*         
REQL     DS    0H                                                               
         BAS   RE,INITTAPE                                                      
*                                                                               
REQL100  DS    0H                                                               
         GOTO1 SORTER,DMCB,=C'GET'                                              
         L     R6,DMCB+4                                                        
         LTR   R6,R6                                                            
         BZ    REQX                END OF RECORDS FROM SORT                     
         ST    R6,ASORT                                                         
         BAS   RE,BLDTAPE                                                       
         BAS   RE,PRNTRC                                                        
         B     REQL100                                                          
*                                                                               
REQX     DS    0H                                                               
         BAS   RE,PRNREP                                                        
         GOTO1 SORTER,DMCB,=C'END'                                              
         B     EXIT                                                             
         EJECT                                                                  
*---------------------------------------------------------------------*         
*              OPEN SPOT FILES                                        *         
*---------------------------------------------------------------------*         
SPOTOPEN NTR1                                                                   
         L     R5,AUTL                                                          
         MVC   ACCUTL,4(R5)                                                     
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'OPEN',=C'CONTROL',CTFILEL                        
         XC    MYKEY,MYKEY                                                      
         USING CT5REC,R2                                                        
         LA    R2,MYKEY                                                         
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,MEDAGY                                                  
         GOTO1 DATAMGR,DMCB,DMREAD,CTFILE,MYKEY,AIOAREA                         
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOAREA                                                       
         LA    R3,CT5DATA                                                       
         SR    R0,R0                                                            
*                                                                               
         USING CTSYSD,R3                                                        
SPOP30   CLI   0(R3),0             FIND SYSTEM ELEMENT                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R3),CTSYSELQ                                                   
         BNE   *+12                                                             
         CLI   CTSYSNUM,X'02'      TEST POST FILE                               
         BE    SPOP50                                                           
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     SPOP30                                                           
*                                                                               
SPOP50   MVC   SENUM,CTSYSSE       GET  SE NUMBER                               
*                                                                               
         L     R5,AUTL                                                          
         MVC   4(1,R5),SENUM                                                    
         GOTO1 DATAMGR,DMCB,=C'OPEN',=C'SPOT',FILELIST,AIOAREA                  
         MVC   4(1,R5),ACCUTL                                                   
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*---------------------------------------------------------------------*         
*              READ SPOT FILES                                        *         
*---------------------------------------------------------------------*         
SPOTREAD NTR1                                                                   
         L     R5,AUTL                                                          
         MVC   4(1,R5),SENUM                                                    
         USING SRTRECD,R6                                                       
         L     R6,ASRTREC              R6 COVERS SORT RECORD                    
         USING ESTHDR,R5                                                        
         XC    MYKEY,MYKEY                                                      
         LA    R5,MYKEY                                                         
         MVI   EKEYTYPE,X'00'                                                   
         GOTO1 MEDGET,DMCB,(MEDMED,MEDAGY),DATAMGR,AGYMED                       
         CLI   DMCB+8,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   EKEYAM,AGYMED                                                    
         GOTO1 CLPACK,DMCB,SRTCLI,MYKEY+2                                       
         MVC   EKEYPRD,SRTPRD                                                   
         MVC   EKEYEST,SRTEST                                                   
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR',MYKEY,MYKEY2                  
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R5,MYKEY2                                                        
         CLC   EKEYAM,AGYMED                                                    
         BNE   RDERR                                                            
         CLC   EKEYCLT,MYKEY+2                                                  
         BNE   RDERR                                                            
         CLC   EKEYPRD,SRTPRD                                                   
         BNE   RDERR                                                            
         CLC   EKEYEST,SRTEST                                                   
         BNE   RDERR                                                            
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFILE',MYKEY2+14,AIOAREA,  X        
               (0,DMWORK)                                                       
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R5,AIOAREA                                                       
         MVC   SRTNUM,SPACES                                                    
         LA    R4,EDESC                                                         
         LA    R3,SRTNUM                                                        
         LA    R2,8                                                             
SPRD50   CLI   0(R4),C' '                                                       
         BE    SPRDXIT                                                          
         MVC   0(1,R3),0(R4)                                                    
         LA    R4,1(R4)                                                         
         LA    R3,1(R3)                                                         
         BCT   R2,SPRD50                                                        
         B     SPRDXIT                                                          
*                                                                               
RDERR    DS    0H                                                               
SPRDXIT  DS    0H                                                               
         L     R5,AUTL                                                          
         MVC   4(1,R5),ACCUTL                                                   
         B     EXIT                                                             
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        INITIALIZE TAPE OUTPUT                                       *         
*---------------------------------------------------------------------*         
INITTAPE NTR1                                                                   
         CLI   QOPT1,C'Y'          Y=DRAFT                                      
         BNE   EXIT                                                             
         MVC   DSPARMID,ALPHAID                                                 
         ZAP   DUB,=P'1'                                                        
         CVB   R4,DUB                                                           
         GOTO1 DYNALLOC,DMCB,(0,=CL8'TAPE01'),((R4),DSPARM)                     
         OPEN  (TAPE01,OUTPUT)                                                  
         B     EXIT                                                             
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        BUILD TAPE RECORD                                            *         
*---------------------------------------------------------------------*         
BLDTAPE  NTR1                                                                   
         USING TPRECD,RE                                                        
         LA    RE,TAPEREC          CLEAR OUT TAPE AREA                          
         LH    RF,=Y(TPLNQ)                                                     
         LA    R0,XSPACES                                                       
         SR    R1,R1                                                            
         ICM   R1,8,XSPACES                                                     
         MVCL  RE,R0                                                            
         DROP  RE                                                               
*                                                                               
         USING TPRECD,R7                                                        
         LA    R7,TAPEREC          CLEAR OUT TAPE AREA                          
         USING SRTRECD,R6                                                       
         L     R6,ASORT                                                         
         MVC   TPCNUM,SRTNUM                                                    
         MVC   TPSIC,SRTSIC                                                     
         MVC   TPCALL,SRTCALL                                                   
         MVC   TPINV,SRTINV                                                     
         OC    TPINV,SPACES                                                     
*                                                                               
         MVC   WORK(L'SRTBRD),SRTBRD   PUT YYMM IN WORK                         
         MVC   WORK+4(2),=C'01'        YYMM01                                   
         GOTO1 DATCON,DMCB,(0,WORK),(20,WORK+6)  YYYYMMDD                       
         MVC   TPBRDYR,WORK+6          YYYY                                     
*                                                                               
         MVC   TPBRDMO,SRTBRDMO                                                 
         MVC   TPCOM1,SRTCOM1                                                   
         MVC   TPCOM2,SRTCOM2                                                   
         MVC   TPCOM3,SRTCOM3                                                   
         MVC   TPCOM4,SRTCOM4                                                   
         MVC   TPCOM5,SRTCOM5                                                   
*                                                                               
         ZAP   DUB,SRTAMT                                                       
         UNPK  TPAMT,DUB           TOTAL TRANSACTION AMOUNT                     
         OI    TPAMT+(L'TPAMT-1),X'F0'                                          
         CP    SRTAMT,=P'0'                                                     
         BNL   *+8                                                              
         NI    TPAMT+L'TPAMT-1,X'DF'                                            
         LA    R4,TPAMT                                                         
         LA    R3,L'TPAMT                                                       
BLDT30   CLI   0(R4),C'0'                                                       
         BNE   BLDT50                                                           
         MVI   0(R4),C' '                                                       
         LA    R4,1(R4)                                                         
         BCT   R3,BLDT30                                                        
*                                                                               
BLDT50   AP    TAPECNT,=P'1'                                                    
         CLI   QOPT1,C'Y'          ONLY PRODUCE TAPE IF QOPT1=Y                 
         BNE   EXIT                                                             
         PUT   TAPE01,TAPEREC      PUT RECORD TO TAPE                           
         B     EXIT                                                             
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        PRINT OUT RECORD                                             *         
*---------------------------------------------------------------------*         
PRNTRC   NTR1                                                                   
         L     R6,ASORT                                                         
         USING SRTRECD,R6                                                       
         LA    R5,XP                                                            
         USING PRNTD,R5                                                         
         MVC   XP,XSPACES                                                       
*                                                                               
         OC    CONSAVE,CONSAVE                                                  
         BZ    PRN10                                                            
         CLC   CONSAVE,SRTNUM                                                   
         BE    PRN10                                                            
         MVI   SPACING,2                                                        
         EDIT  (P10,CONTOT),(16,PAMT),2,MINUS=YES                               
         MVC   XP+3(21),=C'**TOTAL FOR CONTRACT '                               
         MVC   XP+27(L'CONSAVE),CONSAVE                                         
         GOTO1 ACREPORT                                                         
         AP    REPTOT,CONTOT                                                    
         ZAP   CONTOT,=P'0'                                                     
*                                                                               
PRN10    MVC   PCNUM,SRTNUM                                                     
         MVC   PCLI,SRTCLI                                                      
         MVI   PSLASH1,C'/'                                                     
         MVC   PPROD,SRTPRD                                                     
         MVC   PCALL,SRTCALL                                                    
         MVC   PSIC,SRTSIC                                                      
         MVC   PINV,SRTINV                                                      
         MVC   PBRDMO,SRTBRDMO                                                  
         MVI   PSLASH2,C'/'                                                     
*                                                                               
         MVC   WORK(L'SRTBRD),SRTBRD   PUT YYMM IN WORK                         
         MVC   WORK+4(2),=C'01'        YYMM01                                   
         GOTO1 DATCON,DMCB,(0,WORK),(20,WORK+6)   YYYYMMDD                      
         MVC   PBRDYR,WORK+6           YYYY                                     
*                                                                               
         EDIT  (P6,SRTAMT),(16,PAMT),2,MINUS=YES                                
         MVC   PCOM,SRTCOM1                                                     
         GOTO1 ACREPORT                                                         
         AP    CONTOT,SRTAMT                                                    
         MVC   CONSAVE,SRTNUM                                                   
*                                                                               
*T         LA    R3,SRTCOM2                                                     
*T         LA    R4,4                                                           
*TPRN30    OC    0(L'SRTCOM1,R3),R3                                             
*T         BZ    PRN50                                                          
*T         MVC   PCOM,0(R3)                                                     
*T         GOTO1 ACREPORT                                                       
*T         LA    R3,L'SRTCOM1(R3)                                               
*T         BCT   R4,PRN30                                                       
*                                                                               
         CLC   SRTCOM2,SPACES                                                   
         BE    PRN50                                                            
         MVC   PCOM,SRTCOM2                                                     
         GOTO1 ACREPORT                                                         
*                                                                               
         CLC   SRTCOM3,SPACES                                                   
         BE    PRN50                                                            
         MVC   PCOM,SRTCOM3                                                     
         GOTO1 ACREPORT                                                         
*                                                                               
         CLC   SRTCOM4,SPACES                                                   
         BE    PRN50                                                            
         MVC   PCOM,SRTCOM4                                                     
         GOTO1 ACREPORT                                                         
*                                                                               
         CLC   SRTCOM5,SPACES                                                   
         BE    PRN50                                                            
         MVC   PCOM,SRTCOM5                                                     
         GOTO1 ACREPORT                                                         
*                                                                               
PRN50    GOTO1 ACREPORT                                                         
         B     EXIT                                                             
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        PRINT REPORT TOTAL                                                     
*---------------------------------------------------------------------*         
PRNREP   NTR1                                                                   
         LA    R5,XP                                                            
         USING PRNTD,R5                                                         
         MVC   XP,XSPACES                                                       
*                                                                               
         MVI   SPACING,2                                                        
         EDIT  (P10,CONTOT),(16,PAMT),2,MINUS=YES                               
         MVC   XP+3(21),=C'**TOTAL FOR CONTRACT '                               
         MVC   XP+27(L'CONSAVE),CONSAVE                                         
         GOTO1 ACREPORT                                                         
         AP    REPTOT,CONTOT                                                    
         ZAP   CONTOT,=P'0'                                                     
*                                                                               
         GOTO1 ACREPORT                                                         
         MVI   SPACING,2                                                        
         EDIT  (P10,REPTOT),(16,PAMT),2,MINUS=YES                               
         MVC   XP+3(13),=C'REPORT TOTAL '                                       
         GOTO1 ACREPORT                                                         
         EDIT  (P8,TAPECNT),(16,PAMT),0                                         
         MVC   XP+3(22),=C'TOTAL RECORDS TO TAPE '                              
         GOTO1 ACREPORT                                                         
         B     EXIT                                                             
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        CONSTANT DECLARATIONS                                        *         
*---------------------------------------------------------------------*         
*                                                                               
         GETEL R5,DATADISP,ELCODE                                               
*                                                                               
FILELIST DC    C'NSPTDIR NSPTFIL NSTAFIL NCTFILE X'                             
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=(435,,,,) '                            
SORTCARD DC    CL80'SORT FIELDS=(1,35,A),FORMAT=CH,WORK=1 '                     
*                                                                               
TAPE01   DCB   DSORG=PS,                                               X        
               MACRF=PM,                                               X        
               DDNAME=TAPE01,                                          X        
               RECFM=FB,                                               X        
               BLKSIZE=5000,                                           X        
               LRECL=500                                                        
*                                                                               
DSPARM   DC    CL20' '             DATASET NAME FOR DYNALLOC                    
         ORG   DSPARM                                                           
         DC    C'ACCTAPE.AC0IS'                                                 
DSPARMID DC    CL2'**'                                                          
         DC    C'1'                                                             
         ORG                                                                    
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        EXTERNAL ADDRESS LIST                                        *         
*---------------------------------------------------------------------*         
ADCONS   DS    0F                                                               
         DC    V(SORTER)                                                        
         DC    V(SQUASHER)                                                      
         DC    V(DATVAL)                                                        
         DC    V(GETBROAD)                                                      
         DC    V(CLPACK)                                                        
         DC    V(MEDGET)                                                        
         DC    A(SRTREC)                                                        
         DC    A(SRTAREA)                                                       
         DC    A(IOAREA)                                                        
         DC    A(CONIO)                                                         
         DC    X'FF'                                                            
*---------------------------------------------------------------------*         
*        LITERAL DECLARATIONS                                         *         
*---------------------------------------------------------------------*         
ACCFIL   DC    CL8'ACCOUNT'                                                     
SENUM    DC    C' '                                                             
*                                                                               
CTFILE   DC    C'CTFILE  '                                                      
CTFILEL  DC    C'NCTFILE X'                                                     
*                                                                               
LEDGTAB  DS    0H                                                               
         DC    C'S'                                                             
         DC    X'FF'                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        BOX HOOK                                                     *         
*---------------------------------------------------------------------*         
BXHOOK   DS    0D                                                               
         NMOD1 0,*BHOOK                                                         
         L     RC,BOXRC            RESTORE REG C                                
         L     R4,ADBOX                                                         
         USING BOXD,R4                                                          
         MVC   BOXCOLS(198),XSPACES                                             
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+4,C'T'            SET ROWS                               
         MVI   BOXROWS+8,C'M'                                                   
         MVI   BOXROWS+56,C'B'                                                  
         MVI   BOXCOLS,C'L'              SET LH MARGIN                          
*                                                                               
         MVI   BOXCOLS+11,C'C'                                                  
         MVI   BOXCOLS+20,C'C'                                                  
         MVI   BOXCOLS+29,C'C'                                                  
         MVI   BOXCOLS+40,C'C'                                                  
         MVI   BOXCOLS+53,C'C'                                                  
         MVI   BOXCOLS+61,C'C'                                                  
         MVI   BOXCOLS+79,C'C'                                                  
*                                                                               
         MVI   BOXCOLS+PLNQ,C'R'                                                
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
BOXX     XMOD1 1                                                                
BOXRC    DC    A(0)                                                             
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        STORAGE                                                      *         
*---------------------------------------------------------------------*         
*                                                                               
IOAREA   DS    0D                  IOAREA #1                                    
         DS    2000C                                                            
*                                                                               
CONIO    DS    0D                                                               
         DS    625D                                                             
*                                                                               
SRTAREA  DS    0D                  SORT BUFFER                                  
         DS    10000C                                                           
*                                                                               
SRTREC   DS    0D                  WHERE SORT RECORD IS BUILT                   
         DS    500C                                                             
*                                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        DSECT TO COVER SAVE W/S                                      *         
*---------------------------------------------------------------------*         
LWSD     DSECT                                                                  
ATYPES   DS    0F                  EXTERNAL ADDRESSES                           
SORTER   DS    A                   SORTER                                       
SQUASHER DS    A                   SQUASHER                                     
DATVAL   DS    A                   DATVAL                                       
GETBROAD DS    A                   GETBROAD                                     
CLPACK   DS    A                   CLIENT PACK                                  
MEDGET   DS    A                   GET 1 CHAR AGENCY/MEDIA                      
ASRTREC  DS    A                   A(SORT AREA)                                 
ASRTAREA DS    A                   A(SORT AREA)                                 
AIOAREA  DS    A                   IO AREA #1                                   
ACONIO   DS    A                   CONIO                                        
ATYPLNQ  EQU   *-ATYPES                                                         
*                                                                               
AUTL     DS    A                   UTL                                          
ADBOX    DS    A                   ADDRESS OF BOX ROUTINE                       
ASORT    DS    A                   RETURN FROM SORTER                           
AIO1     DS    A                                                                
AIO2     DS    A                                                                
ADWRK    DS    12D                                                              
DSKADR   DS    F                                                                
*                                                                               
ELCODE   DS    XL1                 USED IN GETEL ROUTINE                        
SIC      DS    CL7                 STATION INTERFACE CODE                       
CALLCODE DS    CL6                 CALL LETTERS                                 
ACCUTL   DS    XL1                 MUST SAVE ORIGINAL UTL TO RESTORE            
AGYMED   DS    XL1                 AGENCY MEDIA CODE                            
SAVEKEY  DS    CL42                                                             
MYKEY    DS    CL42                                                             
MYKEY2   DS    CL42                                                             
MEDAGY   DS    CL2                                                              
MEDMED   DS    CL1                                                              
*                                                                               
CONSAVE  DS    CL8                 FLAG CONTRACT NUM CHANGE                     
CONTOT   DS    PL10                NETWORK CONTRACT NUM SUBTOTAL                
REPTOT   DS    PL10                REPORT TOTAL                                 
TAPECNT  DS    PL8                 NUMBER OF RECORDS PUT TO TAPE                
*                                                                               
TAPEREC  DS    CL(TPLNQ)           TAPE BUFFER                                  
*                                                                               
         DS    F                                                                
IO1      DS    CL2000                                                           
*                                                                               
         DS    F                                                                
IO2      DS    CL2000                                                           
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        SORT RECORD DSECT                                            *         
*---------------------------------------------------------------------*         
SRTRECD  DSECT                                                                  
SRTNUM   DS    CL8                 CONTRACT NUMBER                              
SRTCALL  DS    CL5                 CALL LETTERS                                 
SRTSIC   DS    CL7                 STATION INTERFACE CODE                       
SRTINV   DS    CL11                INVOICE NUMBER                               
SRTBRD   DS    0CL4                                                             
SRTBRDYR DS    CL2                 BROADCAST YEAR                               
SRTBRDMO DS    CL2                 BROACAST MONTH                               
SRTKEY   EQU   *-SRTNUM                                                         
SRTCLI   DS    CL3                                                              
SRTPRD   DS    CL3                                                              
SRTEST   DS    XL1                                                              
SRTAMT   DS    PL6                 AMOUNT                                       
SRTINFO  EQU   *-SRTNUM                                                         
SRTCOMS  DS    0C                                                               
SRTCOM1  DS    CL78                COMMENT - LINE 1                             
SRTCOM2  DS    CL78                COMMENT - LINE 1                             
SRTCOM3  DS    CL78                COMMENT - LINE 1                             
SRTCOM4  DS    CL78                COMMENT - LINE 1                             
SRTCOM5  DS    CL78                COMMENT - LINE 1                             
SRTLNQ   EQU   *-SRTRECD                                                        
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        TAPE OUTPUT DSECT                                            *         
*---------------------------------------------------------------------*         
TPRECD   DSECT                                                                  
TPCNUM   DS    CL8                 CONTRACT NUMBER                              
TPSIC    DS    CL7                 STATION INTERFACE CODE                       
TPCALL   DS    CL5                 CALL LETTERS                                 
TPAMT    DS    CL9                 AMOUNT                                       
TPINV    DS    CL11                INVOICE NUMBER                               
TPBRDMO  DS    CL2                 BROACAST MONTH                               
TPBRDYR  DS    CL4                 BROADCAST YEAR                               
TPCOM1   DS    CL78                COMMENT - LINE 1                             
TPCOM2   DS    CL78                COMMENT - LINE 2                             
TPCOM3   DS    CL78                COMMENT - LINE 3                             
TPCOM4   DS    CL78                COMMENT - LINE 4                             
TPCOM5   DS    CL78                COMMENT - LINE 5                             
TPLNQ    EQU   *-TPRECD                                                         
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        PRINT DSECT                                                            
*---------------------------------------------------------------------*         
PRNTD    DSECT                                                                  
         DS    CL2                                                              
PCNUM    DS    CL8                 CONTRACT NUMBER                              
         DS    CL2                                                              
PCLI     DS    CL3                                                              
PSLASH1  DS    CL1                                                              
PPROD    DS    CL3                                                              
         DS    CL3                                                              
PCALL    DS    CL5                 CALL LETTERS                                 
         DS    CL4                                                              
PSIC     DS    CL7                 STATION INTERFACE CODE                       
         DS    CL4                                                              
PINV     DS    CL11                INVOICE NUMBER                               
         DS    CL1                                                              
PBRDMO   DS    CL2                 BROACAST MONTH                               
PSLASH2  DS    CL1                                                              
PBRDYR   DS    CL4                 BROADCAST YEAR                               
         DS    CL1                                                              
PAMT     DS    CL16                AMOUNT                                       
         DS    CL3                                                              
PCOM     DS    CL78                COMMENT - LINE 1                             
         DS    CL4                                                              
PLNQ     EQU   *-PCNUM                                                          
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        OTHER INCLUDES                                               *         
*---------------------------------------------------------------------*         
* DDREPXTRAD                                                                    
* DDREPMASTD                                                                    
* DDCNTRL                                                                       
* ACBIGPRNTD                                                                    
* DDBOXEQUS                                                                     
* DDBIGBOX                                                                      
* DDLOGOD                                                                       
* ACGENPOST                                                                     
* ACREPWORKD                                                                    
* ACGENFILE                                                                     
* ACGENMODES                                                                    
* ACMASTD                                                                       
* DDREMOTED                                                                     
* SPGENEST                                                                      
* CTGENFILE                                                                     
* DDSYSELD                                                                      
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDREPXTRAD                                                     
       ++INCLUDE DDREPMASTD                                                     
       ++INCLUDE DDCNTRL                                                        
       ++INCLUDE ACBIGPRNTD                                                     
       ++INCLUDE DDBOXEQUS                                                      
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDLOGOD                                                        
       ++INCLUDE ACGENPOST                                                      
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACMASTD                                                        
       ++INCLUDE DDREMOTED                                                      
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDSYSELD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'031ACREPIS02 04/10/15'                                      
         END                                                                    
