*          DATA SET SRPQT00A   AT LEVEL 018 AS OF 05/01/02                      
*PHASE T13400A                                                                  
         TITLE '$RPTEST - GENERATE RP TEST REPORTS'                             
         PRINT NOGEN                                                            
RPTEST   CSECT                                                                  
         NMOD1 WRKX-WRKD,**$PQT**                                               
         USING WRKD,RC                                                          
         L     R2,20(R1)           R2=A(TWA)                                    
         USING SRPQTFFD,R2                                                      
         L     R3,0(R1)                                                         
         L     R3,0(R3)            R3=A(DATAMGR)                                
         MVC   ATIA,4(R1)                                                       
         SPACE 1                                                                
         MVC   WLINEL,=C'**LINE**'                                              
         SPACE 1                                                                
         LA    R8,WLINE            INITIALISE OPEN PRINT LINE                   
         USING PQPLD,R8                                                         
         XC    WLINE,WLINE                                                      
         MVI   QLEXTRA,X'FF'       SET NEW STYLE CALL                           
         MVI   WBA,C'B'                                                         
         EJECT                                                                  
SUB      LA    R4,PPPSUBH          REPORT SUB ID (1-3 CHRS)                     
         CLI   5(R4),1                                                          
         BL    MISS                                                             
         CLI   5(R4),3                                                          
         BH    INV                                                              
         CLC   8(3,R4),=C'MEL'                                                  
         BNE   SUB0                                                             
         GOTO1 (R3),DMCB,=C'DMADD',=C'TSTRCVR',1024,(R2)                        
         B     EXIT                                                             
SUB0     EQU   *                                                                
         MVC   QLSUBID,SPACES                                                   
         ZIC   R1,5(R4)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   QLSUBID(0),8(R4)                                                 
SUBX     EQU   *                                                                
         SPACE 1                                                                
PAG      LA    R4,PPPPAGH          NUM OF PAGES (01-99)                         
         CLI   5(R4),1                                                          
         BL    MISS                                                             
         CLI   5(R4),2                                                          
         BH    INV                                                              
         TM    4(R4),X'08'                                                      
         BZ    INV                                                              
         ZIC   R1,5(R4)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R4)                                                      
         CVB   RF,DUB                                                           
         STH   RF,WPAG                                                          
         SPACE 1                                                                
LIN      LA    R4,PPPLINH          NUM OF LINES (01-99)                         
         CLI   5(R4),1                                                          
         BL    MISS                                                             
         CLI   5(R4),2                                                          
         BH    INV                                                              
         TM    4(R4),X'08'                                                      
         BZ    INV                                                              
         ZIC   R1,5(R4)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R4)                                                      
         CVB   RF,DUB                                                           
         STH   RF,WLIN                                                          
         SPACE 1                                                                
LRET     LA    R4,PPPLRETH         LIVE RETAIN HOURS                            
         CLI   5(R4),0                                                          
         BE    LRETX                                                            
         CLI   5(R4),4                                                          
         BH    INV                                                              
         BL    LRET1                                                            
         CLC   8(4,R4),=C'PERM'                                                 
         BNE   LRET1                                                            
         MVC   QLRETNL,=X'FFFF'                                                 
         B     LRETX                                                            
LRET1    TM    4(R4),X'08'                                                      
         BZ    INV                                                              
         ZIC   R1,5(R4)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R4)                                                      
         CVB   RF,DUB                                                           
         STH   RF,QLRETNL                                                       
LRETX    EQU   *                                                                
         SPACE 1                                                                
DRET     LA    R4,PPPDRETH         DEAD RETAIN HOURS                            
         CLI   5(R4),0                                                          
         BE    DRETX                                                            
         CLI   5(R4),4                                                          
         BH    INV                                                              
         BL    DRET1                                                            
         CLC   8(4,R4),=C'PERM'                                                 
         BNE   DRET1                                                            
         MVC   QLRETND,=X'FFFF'                                                 
         B     DRETX                                                            
DRET1    TM    4(R4),X'08'                                                      
         BZ    INV                                                              
         ZIC   R1,5(R4)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R4)                                                      
         CVB   RF,DUB                                                           
         STH   RF,QLRETND                                                       
DRETX    EQU   *                                                                
         SPACE 1                                                                
STAT     LA    R4,PPPSTATH         STATUS H/K/I                                 
         CLI   5(R4),0                                                          
         BE    STATX                                                            
         CLI   5(R4),3                                                          
         BH    INV                                                              
         LA    R4,8(R4)                                                         
         ZIC   R1,5(R4)                                                         
STAT1    CLI   0(R4),C'H'          HOLD                                         
         BNE   *+8                                                              
         OI    QLSTAT,X'40'                                                     
         CLI   0(R4),C'K'          KEEP                                         
         BNE   *+8                                                              
         OI    QLSTAT,X'08'                                                     
         CLI   0(R4),C'I'          INVISIBLE                                    
         BNE   *+8                                                              
         OI    QLSTAT,X'02'                                                     
         LA    R4,1(R4)                                                         
         BCT   R1,STAT1                                                         
STATX    EQU   *                                                                
         SPACE 1                                                                
         MVI   QLCLASS,C'G'                                                     
CLAS     LA    R4,PPPCLASH         REPORT CLASS                                 
         CLI   5(R4),0                                                          
         BE    CLASX                                                            
         MVC   QLCLASS,8(R4)                                                    
CLASX    EQU   *                                                                
         SPACE 1                                                                
         MVC   QLDESC,=C'EDICT TEST '                                           
DESC     LA    R4,PPPDESCH         REPORT DESCRIPTION                           
         SR    R1,R1                                                            
         IC    R1,5(R4)                                                         
         SH    R1,=H'1'                                                         
         BM    DESCX                                                            
         MVC   QLDESC,SPACES                                                    
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   QLDESC(0),8(R4)                                                  
         CLC   QLDESC(7),=C'$PQFULL'                                            
         BNE   DESCX                                                            
         DC    H'0',C'$PQFULL '                                                 
DESCX    EQU   *                                                                
         SPACE 1                                                                
PSWD     LA    R4,PPPPSWDH         REPORT PASSWORD                              
         SR    R1,R1                                                            
         IC    R1,5(R4)                                                         
         SH    R1,=H'1'                                                         
         BM    PSWDX                                                            
         MVC   QLPSWD,SPACES                                                    
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   QLPSWD(0),8(R4)                                                  
PSWDX    EQU   *                                                                
         SPACE 1                                                                
FORM     LA    R4,PPPFORMH         FORMS CODE                                   
         SR    R1,R1                                                            
         IC    R1,5(R4)                                                         
         SH    R1,=H'1'                                                         
         BM    FORMX                                                            
         MVC   QLFORMS,SPACES                                                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   QLFORMS(0),8(R4)                                                 
FORMX    EQU   *                                                                
         SPACE 1                                                                
CHAR     LA    R4,PPPCHARH         CHARACTER SET CODE                           
         SR    R1,R1                                                            
         IC    R1,5(R4)                                                         
         SH    R1,=H'1'                                                         
         BM    CHARX                                                            
         MVC   QLCHARS,SPACES                                                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   QLCHARS(0),8(R4)                                                 
CHARX    EQU   *                                                                
         SPACE 1                                                                
COPY     LA    R4,PPPCOPYH         NUM OF COPIES                                
         CLI   5(R4),1                                                          
         BL    COPYX                                                            
         CLI   5(R4),2                                                          
         BH    INV                                                              
         TM    4(R4),X'08'                                                      
         BZ    INV                                                              
         ZIC   R1,5(R4)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R4)                                                      
         CVB   RF,DUB                                                           
         STC   RF,QLCOPIES                                                      
COPYX    EQU   *                                                                
         SPACE 1                                                                
MAKE     LA    R4,PPPMAKEH         MAKER SPPFF                                  
         SR    R1,R1                                                            
         IC    R1,5(R4)                                                         
         SH    R1,=H'1'                                                         
         BNM   *+14                                                             
         MVC   QLMAKER,=C'DSR  '   SET DEFAULT IF NOT SPECIFIED                 
         B     MAKEX                                                            
         MVC   QLMAKER,SPACES                                                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   QLMAKER(0),8(R4)                                                 
         CLC   QLMAKER(5),=C'NONE '                                             
         BNE   *+10                                                             
         XC    QLMAKER,QLMAKER                                                  
MAKEX    EQU   *                                                                
         SPACE 1                                                                
LPP      LA    R4,PPPLPPH          LINES PER PAGE (01-99) OPT                   
         CLI   5(R4),1                                                          
         BL    LPPX                                                             
         CLI   5(R4),2                                                          
         BNE   INV                                                              
         TM    4(R4),X'08'                                                      
         BZ    INV                                                              
         PACK  DUB,8(2,R4)                                                      
         CVB   RF,DUB                                                           
         STC   RF,QLLPP                                                         
LPPX     EQU   *                                                                
         SPACE 1                                                                
LTYPE    LA    R4,PPPTYPEH         TYPE FCNNN                                   
         MVC   SVLTYPE,=C'FC132'   FIXED/CCCHR/132CHR PRINT LINE                
         MVC   SVLLEN,=H'132'                                                   
         XC    QLLINET(2),QLLINET                                               
         CLI   5(R4),0                                                          
         BE    LTYPEX                                                           
         MVC   SVLTYPE,8(R4)                                                    
LTYPEF   CLI   SVLTYPE,C'F'        TEST F/L                                     
         BNE   *+12                                                             
         OI    QLLINET,QLLTFL                                                   
         B     *+12                                                             
         CLI   SVLTYPE,C'V'                                                     
         BNE   INV                                                              
         CLI   SVLTYPE+1,C'C'      TEST CC CHR                                  
         BNE   *+12                                                             
         OI    QLLINET,QLLTCC                                                   
         B     *+12                                                             
         CLI   SVLTYPE+1,C'N'                                                   
         BNE   INV                                                              
         MVC   FULL,=4C'0'                                                      
         MVZ   FULL+1(3),SVLTYPE+2                                              
         CLC   FULL,=4C'0'                                                      
         BNE   INV                                                              
         PACK  DUB,SVLTYPE+2(3)                                                 
         CVB   R0,DUB                                                           
         CH    R0,=H'4'                                                         
         BL    INV                                                              
         CH    R0,=H'255'                                                       
         BH    INV                                                              
         STH   R0,SVLLEN                                                        
         STC   R0,QLLINEW                                                       
LTYPEX   EQU   *                                                                
         SPACE 1                                                                
LBA      LA    R4,PPPBAH           BEFORE/AFTER MODE (A/B) OPT                  
         CLI   5(R4),0                                                          
         BE    LBAX                                                             
         MVC   WBA,8(R4)                                                        
         CLI   WBA,C'A'                                                         
         BE    *+12                                                             
         CLI   WBA,C'B'                                                         
         BNE   INV                                                              
LBAX     EQU   *                                                                
         EJECT                                                                  
GEN      BAS   RA,OPEN             SET OPEN PRINT LINE PARAMS                   
         MVC   OKMSG,SPACES                                                     
         MVC   OKMSG(13),=CL13'REPORT ====> '                                   
         LA    RE,OKMSG+13                                                      
         MVC   0(3,RE),QLSUBID                                                  
         CLI   0(RE),C' '                                                       
         BE    *+12                                                             
         LA    RE,1(RE)                                                         
         B     *-12                                                             
         MVI   0(RE),C','                                                       
         LA    RE,1(RE)                                                         
         SR    R0,R0                                                            
         ICM   R0,3,QLREPRNO       GET ASSIGNED REPORT NUMBER                   
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(4,RE),DUB                                                      
         SPACE 2                                                                
GEN1     MVI   WCTL1,C'1'          SET UP BASIC PRINT CONTROL CHRS              
         MVI   WCTL2,C' '                                                       
         CLI   WBA,C'A'                                                         
         BE    *+12                                                             
         MVI   WCTL1,X'89'         WRITE AND SKIP TO CHAN 1                     
         MVI   WCTL2,X'09'         WRITE AND SKIP ONE LINE                      
         SPACE 2                                                                
GEN2     MVC   WDAT,SPACES         SET UP PRINT LINE AT WDAT+4                  
         SR    R4,R4                                                            
         IC    R4,PPPDAT1H+5                                                    
         SH    R4,=H'1'                                                         
         BM    LOOP                NO DATA IN FIRST INPUT FIELD                 
         CLC   PPPDAT1(8),=C'HEXTEST1'                                          
         BNE   *+14                                                             
         MVC   WDAT+4(96),HEXTEST1                                              
         B     LOOP                                                             
         CLC   PPPDAT1(8),=C'HEXTEST2'                                          
         BNE   *+14                                                             
         MVC   WDAT+4(96),HEXTEST2                                              
         B     LOOP                                                             
         CLC   PPPDAT1(4),=C'LEN='                                              
         BE    GEN3                DATA DEFINED BY LEN= COMMAND                 
         CLC   PPPDAT1(4),=X'9385957E'                                          
         BE    GEN3                DATA DEFINED BY LEN= COMMAND                 
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   WDAT+4(0),PPPDAT1   MOVE FIRST HALF OF DATA                      
         LA    R5,WDAT+1(R4)                                                    
         IC    R4,PPPDAT2H+5                                                    
         SH    R4,=H'1'                                                         
         BM    LOOP                NO DATA IN SECOND INPUT FIELD                
         EX    R4,*+8                                                           
         B     LOOP                                                             
         MVC   4(0,R5),PPPDAT2     MOVE SECOND HALF OF DATA                     
         SPACE 2                                                                
GEN3     MVC   DUB(3),PPPDAT1+4    LEN=NNNX OPTION                              
         OC    DUB(3),=C'000'                                                   
         PACK  DUB+4(4),DUB(3)                                                  
         XC    DUB(4),DUB                                                       
         CVB   R1,DUB                                                           
         CH    R1,=H'4'                                                         
         BNL   *+8                                                              
         LA    R1,4                                                             
         CH    R1,=H'132'                                                       
         BNH   *+8                                                              
         LA    R1,=H'132'                                                       
         SH    R1,=H'5'                                                         
         BM    GEN4                                                             
         MVC   WDAT+4(1),PPPDAT1+7                                              
         CLI   WDAT+4,X'00'                                                     
         BE    *+12                                                             
         CLI   WDAT+4,C' '                                                      
         BNE   *+8                                                              
         MVI   WDAT+4,C'X'         SET DEFAULT REPEAT CHR                       
         EX    R1,*+8                                                           
         B     GEN4                                                             
         MVC   WDAT+5(0),WDAT+4                                                 
         SPACE 2                                                                
GEN4     CLC   PPPDAT2(6),=C'DOUBLE'                                            
         BE    GEN4A                                                            
         CLC   PPPDAT2(6),=X'8496A4829385'                                      
         BNE   LOOP                                                             
GEN4A    MVI   WCTL1,C'1'                                                       
         MVI   WCTL2,C'0'                                                       
         CLI   WBA,C'A'                                                         
         BE    *+12                                                             
         MVI   WCTL1,X'89'         WRITE AND SKIP TO CHAN 1                     
         MVI   WCTL2,X'11'         WRITE AND SKIP TWO LINES                     
         EJECT                                                                  
LOOP     LH    R4,WPAG             SET NUM OF PAGES                             
LOOP1    LH    R5,WLIN             SET NUM OF LINES                             
         MVC   WCTL,WCTL1                                                       
         CLI   WBA,C'A'                                                         
         BE    LOOP2                                                            
         CLI   SVLTYPE+1,C'C'                                                   
         BNE   LOOP2                                                            
         MVC   WDAT1,WDAT          BEFORE MODE WRITE BLANKS AND CHAN 1          
         MVC   WDAT,SPACES                                                      
         BAS   RA,PRINT                                                         
         MVC   WDAT,WDAT1                                                       
         MVC   WCTL,WCTL2                                                       
LOOP2    LH    R7,WPAG             SET PPLL IN FIRST FOUR CHRS                  
         SR    R7,R4                                                            
         LA    R7,1(R7)                                                         
         CVD   R7,DUB                                                           
*********UNPK  WDAT(2),DUB                                                      
*********OI    WDAT+1,X'F0'                                                     
         LH    R7,WLIN                                                          
         SR    R7,R5                                                            
         LA    R7,1(R7)                                                         
         CVD   R7,DUB                                                           
*********UNPK  WDAT+2(2),DUB                                                    
*********OI    WDAT+3,X'F0'                                                     
         BC    0,*+14                                                           
         MVI   *-3,X'F0'                                                        
         MVC   WDAT,=CL132'SJR *HDR*FAX 6335678'                                
         BAS   RA,PRINT                                                         
         MVC   WDAT,=CL132'THIS IS AN EDICT TEST'                               
         MVC   WCTL,WCTL2                                                       
         BCT   R5,LOOP2                                                         
         BCT   R4,LOOP1                                                         
         SPACE 2                                                                
         MVI   WCTL,X'FF'          SET END OF REPORT CONTROL CHR                
         XC    WDAT,WDAT                                                        
         BAS   RA,CLOSE                                                         
LOOPX    MVC   PPPMSG,OKMSG                                                     
         OI    PPPMSGH+6,X'80'                                                  
         SPACE 2                                                                
EXIT     XMOD1 1                                                                
         EJECT                                                                  
MISS     LA    R5,=CL60'ERROR 01 MISSING INPUT FIELD'                           
         B     *+8                                                              
INV      LA    R5,=CL60'ERROR 02 INVALID INPUT FIELD'                           
         MVC   PPPMSG(60),0(R5)                                                 
         OI    PPPMSGH+6,X'80'                                                  
         OI    6(R4),X'40'                                                      
         B     EXIT                                                             
         SPACE 2                                                                
OPEN     GOTO1 (R3),DMCB,DMOPEN,PRTQUE,0,WLINE,ATIA                             
         CLI   8(R1),0                                                          
         BNE   ERROR                                                            
         BR    RA                                                               
         SPACE 2                                                                
PRINT    LA    RF,WLINE           WLINE=CPPLLXXX...                             
         CLC   SVLTYPE(2),=C'FC'                                                
         BE    PRINTP                                                           
         LA    RF,WLINE+1                                                       
         CLC   SVLTYPE(2),=C'FN'                                                
         BE    PRINTP                                                           
PRINT1   CLC   SVLTYPE(2),=C'VC'                                                
         BNE   PRINT2                                                           
         LA    RF,WLINE-2                                                       
         LH    R1,SVLLEN           GET DATA LENGTH                              
         LA    R1,4(R1)            ADJ FOR PAGE (2) AND LINE (2)                
         LA    R1,3(R1)            ADJ FOR LEN (2) AND CC (1)                   
         STH   R1,0(RF)                                                         
         B     PRINTP                                                           
PRINT2   LA    RF,WLINE+3                                                       
         LH    R1,SVLLEN           GET DATA LENGTH                              
         LA    R1,2(R1)            ADJ FOR LEN (2)                              
         STH   R1,0(RF)                                                         
         B     PRINTP                                                           
PRINTP   GOTO1 (R3),DMCB,DMPRINT,PRTQUE,0,(RF),ATIA                             
         CLI   8(R1),0                                                          
         BNE   ERROR                                                            
         BR    RA                                                               
         SPACE 2                                                                
CLOSE    GOTO1 (R3),DMCB,DMCLOSE,PRTQUE,0,WLINE,ATIA                            
         CLI   8(R1),0                                                          
         BNE   ERROR                                                            
         BR    RA                                                               
         SPACE 2                                                                
ERROR    MVC   PPPMSG,SPACES                                                    
         MVC   PPPMSG(8),=C'ERROR 00'                                           
         IC    R4,8(R1)                                                         
         SRL   R4,4                                                             
         STC   R4,DUB                                                           
         IC    R4,8(R1)                                                         
         SLL   R4,28                                                            
         SRL   R4,28                                                            
         STC   R4,DUB+1                                                         
         OC    PPPMSG+6(2),DUB                                                  
         OI    PPPMSGH+6,X'80'                                                  
         TM    8(R1),X'80'                                                      
         BZ    EXIT                                                             
         DC    H'0',C'$PQFULL '                                                 
         B     EXIT                                                             
         SPACE 2                                                                
         EJECT                                                                  
DMOPEN   DC    CL8'OPEN   '                                                     
DMPRINT  DC    CL8'DMPRINT'                                                     
DMCLOSE  DC    CL8'CLOSE  '                                                     
PRTQUE   DC    CL8'PRTQUE'                                                      
SPACES   DC    CL256' '                                                         
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
HEXTEST1 DC    X'404142434445464748494A4B4C4D4E4F'                              
         DC    X'505152535455565758595A5B5C5D5E5F'                              
         DC    X'606162636465666768696A6B6C6D6E6F'                              
         DC    X'707172737475767778797A7B7C7D7E7F'                              
         DC    X'808182838485868788898A8B8C8D8E8F'                              
         DC    X'909192939495969798999A9B9C9D9E9F'                              
HEXTEST2 DC    X'A0A1A2A3A4A5A6A7A8A9AAABACADAEAF'                              
         DC    X'B0B1B2B3B4B5B6B7B8B9BABBBCBDBEBF'                              
         DC    X'C0C1C2C3C4C5C6C7C8C9CACBCCCDCECF'                              
         DC    X'D0D1D2D3D4D5D6D7D8D9DADBDCDDDEDF'                              
         DC    X'E0E1E2E3E4E5E6E7E8E9EAEBECEDEEEF'                              
         DC    X'F0F1F2F3F4F5F6F7F8F9FAFBFCFDFEFF'                              
WRKD     DSECT                                                                  
DUB      DS    D                                                                
FULL     DS    A                                                                
ATIA     DS    A                                                                
DMCB     DS    6F                                                               
WPAG     DS    H                                                                
WLIN     DS    H                                                                
SVLLEN   DS    H                                                                
WBA      DS    C                                                                
SVLTYPE  DS    CL5                                                              
*                                                                               
WCTL1    DS    C                                                                
WCTL2    DS    C                                                                
WDAT1    DS    CL255                                                            
OKMSG    DS    CL60                                                             
*                                                                               
         DS    0D                                                               
WLINEL   DS    CL8                                                              
WLINE    DS    0CL255                                                           
WCTL     DS    C                                                                
WDAT     DS    CL254                                                            
*                                                                               
WRKX     EQU   *                                                                
         EJECT                                                                  
SRPQTFFD DSECT                                                                  
         DS    CL64                                                             
* SRPQTFFD                                                                      
       ++INCLUDE SRPQTFFD                                                       
         EJECT                                                                  
* DMPRTQL                                                                       
       ++INCLUDE DMPRTQL                                                        
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'018SRPQT00A  05/01/02'                                      
         END                                                                    
