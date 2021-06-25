*          DATA SET SRSHT00    AT LEVEL 002 AS OF 04/06/15                      
*PHASE T15400A                                                                  
*INCLUDE TIMBER                                                                 
         TITLE '$SHUTTLE - DISPLAY SHUTTLE ACTIVITY'                            
         PRINT NOGEN                                                            
SHT      CSECT                                                                  
         NMOD1 WRKX-WRKD,*$SHT**,CLEAR=YES,RR=R4                                
         USING WRKD,RC                                                          
         ST    R4,RELO                                                          
         SAM31                                                                  
         LR    R2,R1                                                            
         USING SRPARMD,R2          R2=A(S/R PARAM LIST)                         
         L     RA,SRPARM1                                                       
         USING SYSFACD,RA          RA=A(SYS FAC LIST)                           
         L     R3,SRPARM6                                                       
         USING SRSHTFFD,R3         R3=A(TWA)                                    
*                                                                               
         L     R4,SRPARM4                                                       
         USING COMFACSD,R4         R4=A(COM FAC LIST)                           
         MVC   VHEXIN,CHEXIN                                                    
         MVC   VHEXOUT,CHEXOUT                                                  
         MVC   VTERMVAL,CTERMVAL                                                
         MVC   VGETFACT,CGETFACT                                                
*                                                                               
         L     RE,VADRBUFF         ADRFILE DATA STORED AHEAD OF ADRBUFF         
         AHI   RE,-16                                                           
         MVC   BUFFMAX,0(RE)       GET RECORDS PER BLOCK                        
         MVC   BUFFREC,2(RE)       GET RECORD LENGTH                            
         LH    R0,BUFFMAX                                                       
         MH    R0,BUFFREC                                                       
         STH   R0,BUFFLEN          SET BLOCK LENGTH                             
*                                                                               
         MVC   POOLMAX,=H'16'                                                   
         LA    R0,L'POOL                                                        
         STH   R0,POOLREC                                                       
         LH    R0,POOLMAX                                                       
         MH    R0,POOLREC                                                       
         STH   R0,POOLLEN          SET POOL LENGTH                              
*                                                                               
         MVC   TUPERSEC,=F'38400'  SET TU'S PER SECOND                          
*                                                                               
         L     RE,=V(TIMBER)                                                    
         A     RE,RELO                                                          
         ST    RE,VTIMBER                                                       
         LA    RE,BUFF                                                          
         ST    RE,ABUFF                                                         
         AH    RE,BUFFLEN                                                       
         ST    RE,ABUFFX                                                        
         LA    RE,POOL                                                          
         ST    RE,APOOL                                                         
         AH    RE,POOLLEN                                                       
         ST    RE,APOOLX                                                        
*                                                                               
         NI    SRVIDH+6,X'BF'                                                   
         XC    MSG,MSG                                                          
         MVC   SRVHL,DISPHDR                                                    
         MVC   SRVUL,DISPUND                                                    
         MVC   MYID,SHUTID         DEFAULT IS TO LOOK AT SHUTTLE STUFF          
*                                                                               
         GOTO1 VGETFACT,DMCB,(X'80',0),F#MIOST  TURN ON CAN EXCEED I/OS         
         EJECT                                                                  
VALP1    LA    R4,SRVP1H           P1=SHUTTLE ID (TRMNUM OR LUID)               
         USING FLDHDRD,R4                                                       
         ST    R4,ACURSOR                                                       
*                                                                               
VALPTST  CLC   SRVP1(7),=C'TESTLOG'                                             
         BNE   VALPTSTX                                                         
         MVC   LOGTEST,LOGDATA                                                  
         MVC   LOGTEST+3(1),SRVP1+7                                             
         CLI   LOGTEST+3,C' '                                                   
         BNL   *+8                                                              
         MVI   LOGTEST+3,C'A'                                                   
         THMS                                                                   
         STCM  R1,15,LOGTEST+12                                                 
         CLI   LOGTEST+3,C'I'      SET BUFFER NUM IN INTERMEDIATE REC           
         BNE   *+10                                                             
         MVC   LOGTEST+16(4),=C'0002'                                           
         CLI   LOGTEST+3,C'L'      SET BUFFER/LINES/PAGES IN LAST REC           
         BNE   VALPTST1                                                         
         MVC   LOGTEST+16(4),=C'0003'                                           
         MVC   LOGTEST+40(4),=F'500'                                            
         MVC   LOGTEST+44(2),=H'10'                                             
VALPTST1 GOTO1 VLOGGER,LOGTEST                                                  
         MVC   MSG(30),=C'Test log data added to adrfile'                       
         B     MSGOUT                                                           
VALPTSTX EQU   *                                                                
*                                                                               
         XC    TRMUTL,TRMUTL       INITIALISE LUID DATA                         
         XC    TRMNUM,TRMNUM                                                    
         MVC   TRMLUID,SPACES                                                   
         CLI   FLDILEN,0           SHUTTLE/PRINTER ID IS REQUIRED               
         BE    ERR0                                                             
         LLC   RE,FLDLEN           POSITION CURSOR TO NEXT FIELD                
         AR    RE,R4                                                            
         ST    RE,ACURSOR                                                       
VP1A     CLI   FLDILEN,3           ALLOW ALL FOR ALL SHUTTLES                   
         BNE   VP1B                                                             
         CLC   FLDDATA(3),=C'ALL'                                               
         BE    VP1X                                                             
         CLC   FLDDATA(3),=C'$LC'                                               
         BNE   VP1B                                                             
         MVC   MYID,VTAMID                                                      
         B     VP1X                                                             
VP1B     GOTO1 VTERMVAL,DMCB,(R4)  VALIDATE TERM NUM OR TERMINAL ID             
         ICM   R5,15,DMCB+4        GET A(UTL) ENTRY                             
         BZ    ERR1                                                             
         CLC   TXPRNT-UTLD(4,R5),=F'0'                                          
         BE    ERR2                TERMINAL IS NOT A SHUTTLE OR PRINTER         
         MVC   TRMNUM,TNUM-UTLD(R5)                                             
VP1B1    ST    R5,TRMUTL                                                        
         MVC   TRMLUID,TSYM-UTLD(R5)                                            
VP1X     EQU   *                                                                
*                                                                               
VALP2    LA    R4,SRVP2H           P2=LOG MESSAGE TYPES                         
         XC    TYPES,TYPES                                                      
         LLC   R1,FLDILEN                                                       
         LTR   R1,R1               TEST IF ANY TYPE DATA INPUT                  
         BZ    VP2X                NO                                           
         LLC   RE,FLDLEN           POSITION CURSOR TO NEXT FIELD                
         AR    RE,R4                                                            
         ST    RE,ACURSOR                                                       
VP2A     LA    RE,L'TYPES          TEST LENGTH OF INPUT                         
         BCTR  RE,0                                                             
         CR    R1,RE                                                            
         BH    ERR1                                                             
         CLC   FLDDATA(3),=C'ALL'  ALLOW ALL FOR ALL TYPES                      
         BE    VP2X                                                             
         CLC   FLDDATA(4),=C'DATA' ALLOW DATA FOR DATA BUFFERS                  
         BNE   *+14                                                             
         MVC   TYPES(3),=C'FIL'                                                 
         B     VP2X                                                             
         CLC   FLDDATA(4),=C'CONT' ALLOW CONT FOR CONTROL MESSAGES              
         BNE   *+14                                                             
         MVC   TYPES(5),=C'ABCDE'                                               
         B     VP2X                                                             
         CLC   FLDDATA(4),=C'TIME' ALLOW TIME FOR TIMER MESSAGES                
         BNE   *+14                                                             
         MVC   TYPES(2),=C'TU'                                                  
         B     VP2X                                                             
         CLC   FLDDATA(4),=C'MSGS' ALLOW MSGS FOR SHUTTLE MESSAGES              
         BNE   *+14                                                             
         MVC   TYPES(1),=C'M'                                                   
         B     VP2X                                                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TYPES(0),FLDDATA                                                 
VP2X     EQU   *                                                                
*                                                                               
VALP3    LA    R4,SRVP3H           P3=TIME(S)                                   
         ZAP   STIME,=P'0'         STIME IS ZERO FOR NO START TIME              
         ZAP   ETIME,=P'0'         ETIME IS ZERO FOR NO END TIME                
         LLC   RF,FLDILEN                                                       
         LTR   RF,RF               TEST IF ANY TIME DATA INPUT                  
         BZ    VP3X                NO                                           
         LLC   RE,FLDLEN           POSITION CURSOR TO NEXT FIELD                
         AR    RE,R4                                                            
         ST    RE,ACURSOR                                                       
         CLC   FLDDATA(3),=C'ALL'  ALLOW ALL AS INPUT FOR ALL TIMES             
         BE    VP3X                                                             
VP3A     GOTO1 VTIMBER,DMCB,(X'80',(RF)),(0,FULL),(0,FLDDATA)                   
         CLI   DMCB,0                                                           
         BNE   ERR1                FULL HAS START/END TIMES IN BINARY           
*                                                                               
VP3B     LH    R0,FULL+0           FULL+0(2)=START TIME IN BINARY               
         MHI   R0,100                                                           
         CVD   R0,DUB                                                           
         MVC   STIME,DUB+4         STIME=P'0HHMMSS+'                            
         SR    RE,RE                                                            
         LH    RF,FULL+0                                                        
         D     RE,=F'100'                                                       
         MHI   RE,60                                                            
         MHI   RF,3600                                                          
         AR    RF,RE               RF=START TIME IN SECONDS                     
         M     RE,TUPERSEC                                                      
         ST    RF,STIMETU                                                       
*                                                                               
VP3C     LH    R0,FULL+2           FULL+2(2)=END TIME IN BINARY                 
         MHI   R0,100                                                           
         CVD   R0,DUB                                                           
         MVC   ETIME,DUB+4         ETIME=P'0HHMMSS+'                            
         SR    RE,RE                                                            
         LH    RF,FULL+2                                                        
         D     RE,=F'100'                                                       
         MHI   RE,60                                                            
         MHI   RF,3600                                                          
         AR    RF,RE               RF=START TIME IN SECONDS                     
         M     RE,TUPERSEC                                                      
         ST    RF,ETIMETU                                                       
VP3X     EQU   *                                                                
         EJECT                                                                  
INIT     L     R5,VADRFILE         GET ADRFILE RECORDS PER TRACK                
         LH    RF,BUFFLEN                                                       
         GOTO1 VDADDS,P1,VDARPT,,(RF),(R5)                                      
         LH    RE,P3+2                                                          
         LTR   RE,RE                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         STC   RE,DSKTRK           SAVE DISK FILE RECORDS/TRACK                 
         MVC   DSKADR,DNEXT-DTF(R5)                                             
         IC    RE,DSKADR+2                                                      
         LA    RE,1(RE)                                                         
         STC   RE,DSKADR+2                                                      
*                                                                               
INIT1    L     RE,VADRBUFF         POINT TO CURRENT ADRFILE CORE BUFF           
         LR    R1,RE                                                            
         AHI   R1,-12              BACK UP TO BUFFER POINTER                    
         L     R1,0(R1)                                                         
         SR    R1,RE               GET LENGTH OF ACTIVE BUFFER                  
         LTR   R5,R1                                                            
         BZ    INIT1A                                                           
         L     RF,ABUFF            COPY ACTIVE ADRBUFF TO MY BUFF               
         MOVE  ((RF),(R1)),(RE)                                                 
INIT1A   A     R5,ABUFF                                                         
         ST    R5,ABUFFNXT         R5=BUFF POINTER                              
         USING LOGRECD,R5                                                       
*                                                                               
INIT2    L     R6,APOOL            R6=POOL POINTER                              
         AH    R6,POOLLEN                                                       
         ST    R6,APOOLNXT                                                      
*                                                                               
INIT3    MVC   P1,VRDID            INITIALISE DADDS PLIST FOR READS             
         MVC   P2,ABUFF                                                         
         XC    P3,P3                                                            
         MVC   P4,VADRFILE                                                      
         LA    RE,DSKADR                                                        
         ST    RE,P5                                                            
         EJECT                                                                  
NXTREC   L     R5,ABUFFNXT         DECR TO NEXT REC IN BUFF                     
         SH    R5,BUFFREC                                                       
         C     R5,ABUFF                                                         
         BL    NXTBLK                                                           
         ST    R5,ABUFFNXT                                                      
         CLC   LOGID(3),MYID       TEST IF SHUTTLE TYPE RECORD                  
         BE    FLTREC                                                           
         B     NXTREC                                                           
*                                                                               
NXTBLK   LLC   RE,DSKADR+2         DECR TO NEXT BLK ON TRACK                    
         AHI   RE,-1                                                            
         BZ    NXTTRK                                                           
         STC   RE,DSKADR+2                                                      
         GOTO1 VDADDS,P1           READ BLK INTO BUFF                           
         OC    P3(2),P3                                                         
         BNZ   NXTBLK              IGNORE DISK ERRORS                           
NXTBLK1  L     R5,ABUFF                                                         
         CLI   0(RE),C'*'          IGNORE SPECIAL BLOCKS                        
         BE    NXTBLK                                                           
         AH    R5,BUFFLEN                                                       
         ST    R5,ABUFFNXT         INITIALISE BUFFER POINTER                    
         B     NXTREC                                                           
*                                                                               
NXTTRK   LH    RE,DSKADR           DECR TO NEXT TRACK                           
         AHI   RE,-1                                                            
         BZ    DSPREC                                                           
         SLL   RE,16                                                            
         ST    RE,DSKADR                                                        
         IC    RE,DSKTRK                                                        
         LA    RE,1(RE)                                                         
         STC   RE,DSKADR+2                                                      
         B     NXTBLK                                                           
         EJECT                                                                  
FLTREC   L     R1,CBUFF            BUMP RECORDS IN BUFF COUNTER                 
         LA    R1,1(R1)                                                         
         ST    R1,CBUFF                                                         
*                                                                               
FREC1    CLC   TRMLUID,SPACES      FILTER ON LUID                               
         BE    FREC2                                                            
         CLC   TRMLUID,LOGLUID                                                  
         BNE   NXTREC                                                           
*                                                                               
FREC2    CP    STIME,=P'0'         FILTER ON START TIME                         
         BNE   *+14                                                             
         CP    ETIME,=P'0'                                                      
         BE    FREC3                                                            
         CP    LOGTIME,STIME                                                    
         BL    NXTREC                                                           
         CP    ETIME,=P'0'         FILTER ON END TIME                           
         BE    FREC3                                                            
         CP    LOGTIME,ETIME                                                    
         BH    NXTREC                                                           
*                                                                               
FREC3    CLI   TYPES,0             FILTER ON MESSAGE TYPES                      
         BE    FREC4                                                            
         MVC   DUB(1),LOGID+3                                                   
         LA    RE,TYPES            SEARCH INPUT TYPES TABLE                     
         CLI   TYPES,C'-'                                                       
         BNE   *+8                                                              
         LA    RE,1(RE)            BUMP PAST MINUS IF FIRST ENTRY               
FREC3A   CLI   0(RE),0                                                          
         BE    FREC3C                                                           
         CLC   0(1,RE),DUB                                                      
         BE    FREC3B                                                           
         LA    RE,1(RE)                                                         
         B     FREC3A                                                           
FREC3B   CLI   TYPES,C'-'          MESSAGE TYPE IN TABLE                        
         BE    NXTREC                                                           
         B     FREC4                                                            
FREC3C   CLI   TYPES,C'-'          MESSAGE TYPE NOT IN TABLE                    
         BE    FREC4                                                            
         B     NXTREC                                                           
*                                                                               
FREC4    EQU   *                                                                
         EJECT                                                                  
SAVREC   L     R1,QPOOL            BUMP QUALIFYING RECORDS                      
         LA    R1,1(R1)                                                         
         ST    R1,QPOOL                                                         
         CLI   FPOOL,0             TEST POOL FULL                               
         BNE   SREC2                                                            
*                                                                               
SREC1    L     R6,APOOLNXT         DECR TO NEXT REC IN POOL                     
         SH    R6,POOLREC                                                       
         ST    R6,APOOLNXT                                                      
         L     R1,CPOOL            MOVE REC TO POOL AND BUMP COUNT              
         LA    R1,1(R1)                                                         
         ST    R1,CPOOL                                                         
         MVC   0(L'LOGREC,R6),0(R5)                                             
         C     R6,APOOL                                                         
         BNE   NXTREC                                                           
         MVI   FPOOL,1             SET POOL FULL FLAG                           
         CP    STIME,=P'0'                                                      
         BE    DSPREC              POOL IS FULL AND NO START TIME               
         B     NXTREC                                                           
*                                                                               
SREC2    LH    R1,POOLMAX                                                       
         BCTR  R1,0                R1=NUM OF ITEMS IN POOL MINUS ONE            
         LH    R0,POOLLEN                                                       
         L     RE,APOOL            SET TO PUSH DOWN POOL                        
         LR    RF,RE                                                            
         AR    RF,R0                                                            
SREC2A   MVC   0(L'LOGREC,RF),0(RE)                                             
         AR    RE,R0                                                            
         AR    RF,R0                                                            
         BCT   R1,SREC2A                                                        
         L     R6,APOOL            MOVE LATEST REC TO TOP OF POOL               
         MVC   0(L'LOGREC,R6),0(R5)                                             
         B     NXTREC                                                           
         EJECT                                                                  
DSPREC   L     R5,APOOL            R5=POOL POINTER                              
         ST    R5,APOOLNXT                                                      
         USING LOGRECD,R5                                                       
         LA    R4,SRVL1H           R4=TWA DISPLAY LINE                          
         USING LIND,R4                                                          
         OC    CPOOL,CPOOL         TEST ANY RECORDS IN POOL                     
         BNZ   DREC1                                                            
         MVC   MSG(25),=C'No shuttle log data found'                            
         B     MSGOUT                                                           
*                                                                               
DREC1    OC    LOGID,LOGID         TEST EMPTY SLOT IN POOL                      
         BNZ   DREC2                                                            
         AH    R5,POOLREC          BUMP TO NEXT REC IN POOL                     
         C     R5,APOOLX                                                        
         BL    DREC1                                                            
         DC    H'0'                BUG IF END OF POOL IS EMPTY                  
*                                                                               
DREC2    MVC   LINLUID,LOGLUID     DISPLAY LUID ID                              
*                                                                               
DREC3    MVC   WRK(10),=X'402020204B20204B2020'                                 
         ED    WRK(10),LOGTIME                                                  
         OC    WRK+2(8),=C'00.00.00'                                            
         MVC   LINTIME,WRK+2                                                    
*                                                                               
DREC4    MVC   BYTE,LOGID+3        DISPLAY LOG DATA TYPE                        
         LA    RE,TYPETBL                                                       
DREC4A   CLC   BYTE,0(RE)          SEARCH TYPE TABLE                            
         BE    DREC4B                                                           
         LA    RE,L'TYPETBL(RE)                                                 
         CLI   0(RE),X'FF'                                                      
         BNE   DREC4A                                                           
DREC4B   MVC   LINTYPE(1),BYTE                                                  
         MVC   LINTYPE+2(22),2(RE)                                              
         TM    1(RE),X'40'         TEST IF BUFFER NUMBER DEFINED                
         BZ    DREC4C                                                           
         MVI   LINTYPE+18,C'#'                                                  
         MVC   LINTYPE+19(4),LOGNUM                                             
DREC4C   CLI   BYTE,C'L'           TEST IF LAST DATA BUFFER                     
         BNE   DREC4D                                                           
         XR    R0,R0               TEST IF PAGES PRINTED DEFINED                
         ICM   R0,3,LOGPAGES                                                    
         BZ    DREC4D                                                           
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DUB1,DUB                                                         
         CLI   DUB1+3,C'0'         TEST IF >9999 PAGES                          
         BNE   *+8                                                              
         MVI   DUB1+3,C'='                                                      
         MVC   LINTYPE+07(5),=C'pages'                                          
         MVC   LINTYPE+12(5),DUB1+3                                             
DREC4D   TM    1(RE),X'80'         TEST IF REPORT NAME DEFINED                  
         BO    DREC5                                                            
         MVC   LINUSER,NOTREL                                                   
         MVC   LINREPT,NOTREL                                                   
DREC4E   TM    1(RE),X'01'         TEST IF MESSAGE TEXT DEFINED                 
         BZ    DREC4F                                                           
         MVC   LINTYPE+2(16),LOGTEXT                                            
         B     DREC6                                                            
DREC4F   CLI   0(RE),C'S'          TEST START OF FACPAK                         
         BNE   DREC6                                                            
         MVC   LINTYPE+11(4),LOGREC+42  FACPAK NAME                             
         MVC   LINTYPE+15(2),SPACES                                             
         B     DREC6                                                            
*                                                                               
DREC5    CLI   LOGREPU,C'A'        DISPLAY REPORT NAME                          
         BL    DREC6                                                            
         CLI   LOGREPU,C'Z'                                                     
         BH    DREC6                                                            
         MVC   LINUSER,LOGREPU     DISPLAY REPORT USER ID                       
         MVC   LINREPT(3),LOGREPI                                               
         MVI   LINREPT+3,C','                                                   
         MVC   LINREPT+4(5),LOGRENO                                             
*                                                                               
DREC6    CLI   LOGID+3,C'Q'        TRACE OF PRINTER ACTIVITY                    
         BNE   DRECA                                                            
         MVC   SRVHL,DISPPHDR      SET PRINTER TRACE HEADLINES                  
         MVC   SRVUL,DISPPUND                                                   
         MVC   LINTYPE+2(15),SPACES                                             
         MVI   LINTYPE+18,C' '                                                  
         GOTO1 VHEXOUT,DMCB,LOGFLAGS,LINTYPE+2,1,=C'MIX'                        
         GOTO1 VHEXOUT,DMCB,LOGSST,LINTYPE+5,2,=C'MIX'                          
DREC6A   CLC   LOGSST(2),LOGXST    TEST STATUS/STATUS1 CHANGE                   
         BE    DREC6B                                                           
         GOTO1 VHEXOUT,DMCB,LOGXST,LINTYPE+10,2,=C'MIX'                         
DREC6B   MVI   LINTYPE+15,C'.'     NORMAL STATUS                                
         TM    LOGXST,X'80'                                                     
         BZ    *+8                                                              
         MVI   LINTYPE+15,C'E'     ERROR STOPPED                                
         TM    LOGXST,X'08'                                                     
         BZ    *+8                                                              
         MVI   LINTYPE+15,C'A'     ACTIVE                                       
DREC6E   TM    LOGSNEX,X'01'       TEST EOR PENDING AT START                    
         BZ    DREC7                                                            
         TM    LOGXST,X'08'        TEST INACTIVE AT END                         
         BO    DREC7                                                            
         MVC   LINUSER,NOTREL      CLEAR REPORT STUFF                           
         MVC   LINREPT,NOTREL                                                   
         MVC   LINTYPE+19(4),NOTREL                                             
         B     DRECA                                                            
*                                                                               
DREC7    MVC   LINMISC,SPACES      MISCELLANEOUS STUFF                          
         LA    R7,LINMISC                                                       
*                                                                               
DREC7A   SR    RF,RF               LINES PRINTED                                
         ICM   RF,7,LOGXLNS                                                     
         BZ    DREC7AX                                                          
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(4,R7),DUB                                                      
DREC7AX  LA    R7,5(R7)                                                         
*                                                                               
DREC7B   SR    RF,RF               PAGES PRINTED                                
         ICM   RF,3,LOGXPGS                                                     
         BZ    DREC7BX                                                          
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(3,R7),DUB                                                      
DREC7BX  LA    R7,4(R7)                                                         
*                                                                               
DREC7C   GOTO1 VHEXOUT,DMCB,LOGXNEX,0(R7),1,=C'MIX'                             
         LA    R7,3(R7)                                                         
*                                                                               
DRECA    LH    R1,DPOOL            BUMP DISPLAYED FROM POOL COUNT               
         LA    R1,1(R1)                                                         
         STH   R1,DPOOL                                                         
         AH    R5,POOLREC          BUMP TO NEXT REC IN POOL                     
         C     R5,APOOLX                                                        
         BNL   DRECB                                                            
         LA    R4,8+L'LINDATA(R4)  BUMP TO NEXT SCREEN LINE                     
         B     DREC1                                                            
*                                                                               
DRECB    MVC   MSG(36),=C'NN log records out of NNNN displayed'                 
         LH    R1,DPOOL                                                         
         CVD   R1,DUB                                                           
         UNPK  MSG+00(2),DUB       SET NUM OF RECS DISPLAYED                    
         OI    MSG+01,C'0'                                                      
         L     R1,QPOOL                                                         
         CVD   R1,DUB                                                           
         UNPK  MSG+22(4),DUB       SET NUM OF RECS QUALIFIED                    
         OI    MSG+25,C'0'                                                      
         EJECT                                                                  
MSGOUT   MVC   SRVMSG,MSG          MOVE OUTPUT MESSAGE TO SCREEN                
         OI    SRVMSGH+6,X'80'                                                  
         L     R4,ACURSOR                                                       
         OI    6(R4),X'40'         POSN CURSOR ON REQUIRED FIELD                
         B     EXIT                                                             
*                                                                               
ERR0     MVC   MSG(17),=C'MISSING PARAMETER'                                    
         B     ERRX                                                             
ERR1     MVC   MSG(17),=C'INVALID PARAMETER'                                    
         B     ERRX                                                             
ERR2     MVC   MSG(25),=C'TERMINAL IS NOT A SHUTTLE'                            
         B     ERRX                                                             
*                                                                               
ERRX     MVC   SRVMSG(12),=C'***ERROR*** '                                      
         MVC   SRVMSG+12(48),MSG                                                
         OI    SRVMSGH+6,X'80'                                                  
         OI    6(R4),X'40'         POSN CURSOR ON INVALID FIELD                 
*                                                                               
EXIT     XMOD1 1                                                                
         EJECT                                                                  
TYPETBL  DS    0CL24               TABLE OF VALID LOG RECORD TYPES              
*                                                                               
         DC    C'A',X'00',CL15'Start of sess  ',CL7' '                          
         DC    C'B',X'00',CL15'Restarted sess ',CL7' '                          
         DC    C'C',X'00',CL15'Normal end rept',CL7' '                          
         DC    C'D',X'00',CL15'Abnorm end rept',CL7' '                          
         DC    C'E',X'00',CL15'End of session ',CL7' '                          
         DC    C'F',X'C0',CL15'Frst buff      ',CL7' '                          
         DC    C'I',X'C0',CL15'Intr buff      ',CL7' '                          
         DC    C'L',X'C0',CL15'Last buff      ',CL7' '                          
         DC    C'M',X'01',CL15'Message record ',CL7' '                          
         DC    C'Q',X'C0',CL15'Printer trace  ',CL7' '                          
         DC    C'S',X'00',CL15'Start of facpak',CL7' '                          
         DC    C'T',X'00',CL15'Timer pop      ',CL7' '                          
         DC    C'U',X'00',CL15'Timer auto strt',CL7' '                          
*                                                                               
TYPETBLX DC    XL02'FF00',CL15'Unknown        ',CL7' '                          
*                                                                               
SHUTID   DC    C'$PQ '             ID OF SHUTTLE LOG RECORDS                    
VTAMID   DC    C'$LC '             ID OF SHUTTLE LOG RECORDS                    
*                                                                               
NOTREL   DC    40C'.'                                                           
SPACES   DC    40C' '                                                           
*                                                                               
LOGDATA  DC    C'$PQA',C'DDSL4040',PL4'0'                                       
         DC    C'0001',C'     '                                                 
         DC    C'DDSTSTLOG1234',XL2'00'                                         
LOGDATA1 DC    24X'00'                                                          
LOGDATA2 DC    16X'00'                                                          
*                                                                               
DISPHDR  DC    CL79'Luid     hh.mm.ss T Log data        Buff#  Userid RX        
               eport-id'                                                        
DISPUND  DC    CL79'-------- -------- - --------------- -----  ------ -X        
               --------'                                                        
DISPPHDR DC    CL79'Prt Luid hh.mm.ss T Fg Ssta Esta St  Buff  Userid RX        
               eport-id Lnes Pgs Px        '                                    
DISPPUND DC    CL79'-------- -------- - -- ---- ---- --  ----  ------ -X        
               -------- ---- --- --        '                                    
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
LIND     DSECT                     DISPLAY LINE FORMAT                          
LINHDR   DS    CL8                                                              
LINDATA  DS    0CL79                                                            
LINLUID  DS    CL8                                                              
         DS    CL1                                                              
LINTIME  DS    CL8                                                              
         DS    CL1                                                              
LINTYPE  DS    CL24                                                             
         DS    CL1                                                              
LINUSER  DS    CL6                                                              
         DS    CL1                                                              
LINREPT  DS    CL9                                                              
         DS    CL1                                                              
LINMISC  DS    CL19                                                             
         EJECT                                                                  
LOGRECD  DSECT                                                                  
LOGREC   DS    0CL80               CURRENT ADRFILE RECORD SIZE                  
*                                                                               
LOGHDR   DS    0CL40                                                            
LOGID    DS    CL4                                                              
LOGLUID  DS    CL8                                                              
LOGTIME  DS    PL4                                                              
LOGTEXT  DS    0CL16                                                            
LOGNUM   DS    CL4                                                              
         DS    CL4                                                              
LOGREPU  DS    CL6                                                              
LOGREPI  DS    CL3                                                              
LOGRENO  DS    CL5                                                              
         DS    CL2                                                              
*                                                                               
LOGMISC  DS    0CL24               MISC INFO - DEPENDS ON REC TYPE              
LOGFLAGS DS    XL1                                                              
*                                                                               
LOGSST   DS    XL1                                                              
LOGSST1  DS    XL1                                                              
LOGSST2  DS    XL1                                                              
LOGSSMOD DS    XL1                                                              
LOGSQNE  DS    XL1                                                              
LOGSST3  DS    XL1                                                              
LOGSNEX  DS    XL1                                                              
*                                                                               
LOGXST   DS    XL1                                                              
LOGXST1  DS    XL1                                                              
LOGXST2  DS    XL1                                                              
LOGXSMOD DS    XL1                                                              
LOGXQNE  DS    XL1                                                              
LOGXST3  DS    XL1                                                              
LOGXNEX  DS    XL1                                                              
*                                                                               
LOGXLNS  DS    XL3                                                              
LOGXPGS  DS    XL2                                                              
         DS    XL4                                                              
*                                                                               
         ORG   LOGMISC                                                          
LOGLLAST DS    0CL24               ENTRY FOR LAST BUFFER LOG RECORD             
LOGLINES DS    XL4                                                              
LOGPAGES DS    XL2                                                              
         DS    CL18                                                             
*                                                                               
TRDATA   DS    0XL16               TRACE DATA FOR LOG RECORD                    
TRFLAGS  DS    XL1                 TRACE FLAGS                                  
TRSTAT   DS    XL1                 START STATUS                                 
TRSTAT1  DS    XL1                 START STATUS 1                               
TRSTAT2  DS    XL1                 START STATUS 2                               
TRQMODE  DS    XL1                 START MODE                                   
TRQNE    DS    XL1                 START QUEUE NUMBER OF ENTRIES                
TRSTAT3  DS    XL1                 START STATUS 3                               
TRNEX    DS    XL1                 START STATUS 3                               
         DS    XL1                                                              
TRKEY    DS    XL7                 START REPORT KEY                             
*                                                                               
LOGRECX  EQU   *                                                                
         EJECT                                                                  
WRKD     DSECT                                                                  
DUB      DS    D                                                                
DUB1     DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
BYTE1    DS    X                                                                
DMCB     DS    6F                                                               
*                                                                               
P1       DS    F                                                                
P2       DS    F                                                                
P3       DS    F                                                                
P4       DS    F                                                                
P5       DS    F                                                                
P6       DS    F                                                                
*                                                                               
ABUFF    DS    A                                                                
ABUFFX   DS    A                                                                
ABUFFNXT DS    A                                                                
CBUFF    DS    F                                                                
*                                                                               
APOOL    DS    A                                                                
APOOLX   DS    A                                                                
APOOLNXT DS    A                                                                
CPOOL    DS    F                                                                
QPOOL    DS    F                                                                
FPOOL    DS    X                                                                
         DS    X                                                                
DPOOL    DS    H                                                                
*                                                                               
BUFFMAX  DS    H                                                                
BUFFREC  DS    H                                                                
BUFFLEN  DS    H                                                                
*                                                                               
POOLMAX  DS    H                                                                
POOLREC  DS    H                                                                
POOLLEN  DS    H                                                                
*                                                                               
TUPERSEC DS    F                   NUMBER OF TU'S PER SECOND                    
*                                                                               
RELO     DS    A                                                                
ACURSOR  DS    A                                                                
VHEXOUT  DS    A                                                                
VHEXIN   DS    A                                                                
VTERMVAL DS    A                                                                
VGETFACT DS    A                                                                
VTIMBER  DS    A                                                                
*                                                                               
TRMUTL   DS    A                                                                
TRMNUM   DS    H                                                                
         DS    H                                                                
TRMLUID  DS    CL8                                                              
*                                                                               
STIME    DS    PL4                                                              
ETIME    DS    PL4                                                              
STIMETU  DS    F                                                                
ETIMETU  DS    F                                                                
*                                                                               
TYPES    DS    CL12                                                             
*                                                                               
DSKADR   DS    A                                                                
DSKTRK   DS    X                                                                
         DS    XL3                                                              
MYID     DS    CL4                                                              
*                                                                               
WRK      DS    CL80                                                             
MSG      DS    CL60                                                             
LOGTEST  DS    CL80                                                             
POOL     DS    16CL80                                                           
BUFF     DS    100CL80                                                          
*                                                                               
WRKX     DS    0C                                                               
         EJECT                                                                  
*DDFLDHDR                                                                       
       ++INCLUDE DDFLDHDR                                                       
         EJECT                                                                  
*DMDTFPH                                                                        
       ++INCLUDE DMDTFPH                                                        
         EJECT                                                                  
*DDCOMFACS                                                                      
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
*FADSECTS                                                                       
       ++INCLUDE FADSECTS                                                       
         EJECT                                                                  
*FAFACTS                                                                        
       ++INCLUDE FAFACTS                                                        
         EJECT                                                                  
SRSHTFFD DSECT                                                                  
         DS    CL64                                                             
*SRSHTFFD                                                                       
       ++INCLUDE SRSHTFFD                                                       
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002SRSHT00   04/06/15'                                      
         END                                                                    
