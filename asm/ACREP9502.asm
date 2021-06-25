*          DATA SET ACREP9502  AT LEVEL 038 AS OF 06/03/15                      
*PHASE AC9502A,+0                                                               
*INCLUDE SORTER                                                                 
*INCLUDE SQUASHER                                                               
         TITLE 'AC95 - PAYROLL COST ANALYSIS'                                   
AC9502   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**AC9502,RR=R5                                                 
         LA    R8,2048(,RB)                                                     
         LA    R8,2048(,R8)                                                     
*                                                                               
         USING AC9502+4096,R8      R8=SECOND BASE                               
*                                                                               
         L     RC,0(,R1)                                                        
*                                                                               
         USING ACWORKD,RC          RC=A(GLOBAL W/S)                             
*                                                                               
         LA    R9,SPACEND                                                       
*                                                                               
         USING WORKD,R9            R9=A(TEMP W/S)                               
*                                                                               
         ST    R5,RELO                                                          
         EJECT ,                                                                
*              HANDLE CONTROLLER MODES                                          
*                                                                               
         CLI   MODE,PROCHIST       *                                            
         BE    AC10                                                             
         CLI   MODE,SBACFRST       *                                            
         BE    AC20                                                             
         CLI   MODE,SBACLAST       *                                            
         BE    AC30                                                             
         CLI   MODE,PROCACC        *                                            
         BE    AC40                                                             
         CLI   MODE,LEVCFRST       *                                            
         BE    AC45                                                             
         CLI   MODE,LEVBFRST       *                                            
         BE    AC50                                                             
         CLI   MODE,LEVAFRST       *                                            
         BE    AC60                                                             
         CLI   MODE,LEDGFRST       *                                            
         BE    AC70                                                             
         CLI   MODE,REQFRST        *                                            
         BE    AC80                                                             
         CLI   MODE,REQLAST        *                                            
         BE    AC90                                                             
         CLI   MODE,RUNFRST        *                                            
         BE    AC100                                                            
*                                                                               
EXIT     XMOD1 1                                                                
         EJECT ,                                                                
*              PROCTRNS - CALCULATE HOURS AND POST TO ACCUMS                    
*                                                                               
AC10     L     R3,ADTRANS                                                       
         CLI   THISACC,C'Y'                                                     
         BNE   EXIT                                                             
*&&UK                                                                           
*                                                                               
         USING TRSUBHD,R2                                                       
*                                                                               
         L     R2,ADSUBAC          SKIP ANY TIME CHARGE CONTRAS                 
         CLC   TRSBACNT+1(2),=C'1J'                                             
         BE    EXIT                                                             
*                                                                               
         DROP  R2                  KEEP IT CLEAN                                
*&&                                                                             
*                                                                               
         USING TRHISTD,R3                                                       
*                                                                               
         CLI   TRHSEL,X'45'        HISTORIES ONLY                               
         BNE   EXIT                                                             
         CLI   BUCKTYPE,C' '                                                    
         BE    *+12                                                             
         CLI   BUCKTYPE,C'H'                                                    
         BNE   EXIT                                                             
         CLC   TRHSYEAR(2),DATAB+2 FOR THIS YEAR                                
         BL    EXIT                                                             
         CLC   TRHSYEAR(2),DATAB+24                                             
         BH    EXIT                                                             
         LA    R2,2                                                             
         ZAP   DUB(6),TRHSDR                                                    
         CLI   BUCKTYPE,C'H'                                                    
         BNE   AC11                                                             
         LA    R2,1                                                             
*&&US*&& ZAP   DUB(6),TRHSCR                                                    
*&&UK*&& ZAP   DUB(6),TRHSDR                                                    
*&&UK*&& AP    DUB(6),TRHSCR                                                    
*                                                                               
AC11     GOTO1 ,DMCB,3,ACCUMS                                                   
         L     RF,PROLLER                                                       
         GOTO1 (RF),(R1),,,DUB,4,(R2)                                           
         CLC   TRHSYEAR(2),DATAB                                                
         BL    AC12                                                             
         CLC   TRHSYEAR(2),DATAB+2                                              
         BL    AC12                                                             
         GOTO1 (RF),(R1),,,DUB,3,(R2)                                           
*                                                                               
AC12     CLC   TRHSYEAR(2),DATAB                                                
         BL    AC14                                                             
         CLC   TRHSYEAR(2),DATAB+20                                             
         BL    AC14                                                             
         GOTO1 (RF),(R1),,,DUB,2,(R2)                                           
*                                                                               
AC14     CLC   TRHSYEAR(2),DATAB+24                                             
         BNE   AC16                                                             
         GOTO1 (RF),(R1),,,DUB,1,(R2)                                           
*                                                                               
AC16     B     EXIT                                                             
*                                                                               
         DROP  R3                  KEEP IT CLEAN                                
         EJECT ,                                                                
*              SBACFRST - EXTRACT CONTRA-ACCOUNT CODE & NAME                    
*                                                                               
AC20     CLI   THISACC,C'Y'                                                     
         BNE   EXIT                                                             
         MVC   ECODE,SPACES                                                     
         MVC   ENAME,SPACES                                                     
         ICM   R2,15,ADSUBAC                                                    
         BZ    AC20X                                                            
*                                                                               
         USING TRSUBHD,R2                                                       
*                                                                               
         MVC   ECODE,TRSBACNT+3                                                 
         SR    R1,R1                                                            
         IC    R1,TRSBLEN                                                       
         SH    R1,=H'18'                                                        
         BM    AC20X                                                            
         EXMVC R1,ENAME,TRSBNAME                                                
*                                                                               
AC20X    GOTO1 PROLLER,DMCB,0,ACCUMS,4,2                                        
         B     EXIT                                                             
*                                                                               
         DROP  R2                                                               
         EJECT ,                                                                
*              SBACLAST - BUILD RECORD(S) AND PASS TO SORTER                    
*                                                                               
AC30     L     R2,AOPTAB                                                        
         CLI   THISACC,C'Y'                                                     
         BNE   EXIT                                                             
         SR    R1,R1                                                            
*                                                                               
AC32     CLI   0(R2),X'FF'                                                      
         BE    AC33                                                             
         CLC   0(1,R2),QOPT1                                                    
         BE    AC33                                                             
         IC    R1,1(,R2)           BUMP TO NEXT                                 
         AR    R2,R1                                                            
         B     AC32                                                             
*                                                                               
AC33     LA    R2,2(,R2)           R2=A(LIST OF REPTAB INDEXES)                 
         MVC   SORTACCS,ACCUMS+8   MOVE ACCUMS TO RECORD                        
         CP    SORTACCS+6(6),=P'0'                                              
         BNE   AC33A               IGNORE IF BOTH THIS MONTH                    
         CP    SORTACCS+42(6),=P'0'                                             
         BE    EXIT                AND 12 MONTH ARE ZERO                        
*                                                                               
AC33A    CLI   QOPT3,C'Y'          3/12MONTH SUPPRESSION                        
         BE    ACC33B                                                           
         CP    SORTACCS+30(6),=P'0'                                             
         BNE   ACC33B              YES - CHECK YTD $ NOT ZERO                   
         CP    SORTACCS+24(6),=P'0'                                             
         BE    EXIT                YES - CHECK YTD HOURS NOT ZERO               
*                                                                               
ACC33B   XC    SORTLN(4),SORTLN                                                 
         MVC   SORTLN,=H'298'                                                   
*                                                                               
AC34     CLI   0(R2),X'FF'         END-OF-LIST                                  
         BE    EXIT                                                             
         SR    R1,R1                                                            
         IC    R1,0(,R2)                                                        
         BCTR  R1,0                                                             
         MH    R1,=H'16'                                                        
         A     R1,AREPTAB                                                       
         MVC   SORTYPE(1),0(R1)                                                 
         LA    R1,1(,R1)                                                        
         CLI   LEVELS,3                                                         
         BL    *+8                                                              
         LA    R1,5(,R1)                                                        
         CLI   LEVELS,4                                                         
         BL    *+8                                                              
         LA    R1,5(,R1)                                                        
         MVC   SORTYPE+1(5),0(R1)                                               
         XC    SORTAC(SORTACCS-SORTAC),SORTAC                                   
         SR    RE,RE                                                            
         LA    RF,5                                                             
         LA    R3,SORTAC                                                        
         LA    R4,SORTAN                                                        
*                                                                               
AC36     ZIC   RE,0(,R1)                                                        
         SH    RE,=H'1'                                                         
         BM    AC38                LEVEL NOT DEFINED                            
         MH    RE,=H'48'                                                        
         LA    RE,CODENAME(RE)                                                  
         MVC   0(12,R3),0(RE)      MOVE CODE/NAME TO RECORD                     
         MVC   0(36,R4),12(RE)                                                  
*                                                                               
AC38     LA    R1,1(,R1)                                                        
         LA    R3,12(,R3)                                                       
         LA    R4,36(,R4)                                                       
         BCT   RF,AC36                                                          
         GOTO1 VSORTER,DMCB,=C'PUT',SORTREC                                     
         MVI   SORTSW,C'Y'         SET SORT ACTIVE                              
         LA    R2,1(,R2)                                                        
         B     AC34                                                             
         EJECT ,                                                                
*              PROCACC - EXTRACT NAME/HOURLY RATE & ESTABLISH DIVISION          
*                                                                               
AC40     L     R1,ADACC                                                         
*&&UK                                                                           
         MVI   THISACC,C'N'        DEDUCE IF EXPENSE/STAFF ACCOUNT              
         ZIC   R2,LEVTAB+34                                                     
         LA    R2,4(R2,R1)         R2=A(2ND BYTE OF STAFF CODE)                 
         CLI   REPTYPE,C'E'                                                     
         BNE   *+16                                                             
         CLI   0(R2),C' '                                                       
         BNE   EXIT                IGNORE STAFF IF EXPENSE                      
         B     *+12                                                             
         CLI   0(R2),C' '                                                       
         BE    EXIT                IGNORE EXPENSE IF PAYROLL                    
*&&                                                                             
         MVI   THISACC,C'Y'                                                     
         BAS   RE,GETNAME                                                       
         MVC   DCODE,3(R1)         MOVE CODE/NAME TO WORK                       
         MVC   DNAME,WORK                                                       
         L     R2,ADACCSTA                                                      
*                                                                               
         USING ACSTATD,R2                                                       
*                                                                               
         MVC   WORK(1),ACSTCOST                                                 
         CLI   WORK,X'40'                                                       
         BH    *+8                                                              
         MVI   WORK,X'FF'                                                       
*&&UK*&& MVC   LEVCCOST,WORK                                                    
*&&US*&& MVC   LEVDCOST,WORK                                                    
*                                                                               
         DROP  R2                                                               
*                                  BUILD RATAB (DATE/RATEX13)                   
AC44     MVI   WORK,X'FF'                                                       
         LA    R1,LEVACOST                                                      
*&&UK*&& LA    R2,3                                                             
*&&US*&& LA    R2,4                                                             
*                                                                               
AC46     CLI   0(R1),X'FF'                                                      
         BE    *+10                                                             
         MVC   WORK(1),0(R1)                                                    
         LA    R1,1(,R1)                                                        
         BCT   R2,AC46                                                          
         LA    R1,DIVLIST          WORK CONTAINS COMPOSITE DIVISION             
*                                  LOOK-UP DIVISION NAME                        
AC48     CLI   0(R1),X'FF'                                                      
         BE    AC4A                                                             
         CLC   0(1,R1),WORK                                                     
         BE    AC4A                                                             
         LA    R1,L'DIVLIST(,R1)                                                
         B     AC48                                                             
*                                                                               
AC4A     MVC   FCODE(1),WORK                                                    
         MVC   FNAME,1(R1)                                                      
         B     EXIT                                                             
         EJECT ,                                                                
*              LEVCFRST - EXTRACT NAME/CODE                                     
*                                                                               
AC45     L     R1,ADHEIRC                                                       
         BAS   RE,GETNAME                                                       
         MVC   CCODE,3(R1)         MOVE CODE/NAME TO WORK                       
         MVC   CNAME,WORK                                                       
         L     R2,ADLVCSTA                                                      
*                                                                               
         USING ACSTATD,R2                                                       
*                                                                               
         MVC   WORK(1),ACSTCOST                                                 
         CLI   WORK,X'40'                                                       
         BH    *+8                                                              
         MVI   WORK,X'FF'                                                       
         MVC   LEVCCOST,WORK       SAVE SUB-DEPT DIVISION                       
         MVI   LEVDCOST,X'FF'                                                   
         B     EXIT                                                             
*                                                                               
         DROP  R2                                                               
         EJECT ,                                                                
*              LEVBFRST - EXTRACT NAME/CODE                                     
*                                                                               
AC50     L     R1,ADHEIRB                                                       
         BAS   RE,GETNAME                                                       
         MVC   BCODE,3(R1)         MOVE CODE/NAME TO WORK                       
         MVC   BNAME,WORK                                                       
         L     R2,ADLVBSTA                                                      
*                                                                               
         USING ACSTATD,R2                                                       
*                                                                               
         MVC   WORK(1),ACSTCOST                                                 
         CLI   WORK,X'40'                                                       
         BH    *+8                                                              
         MVI   WORK,X'FF'                                                       
         MVC   LEVBCOST,WORK       SAVE SUB-DEPT DIVISION                       
         MVC   LEVCCOST(2),=X'FFFF'                                             
         B     EXIT                                                             
*                                                                               
         DROP  R2                                                               
         EJECT ,                                                                
*              LEVAFRST - EXTRACT NAME/CODE                                     
*                                                                               
AC60     MVI   CODENAME,C' '       CLEAR CODE/NAME TABLE                        
         MVC   CODENAME+1(255),CODENAME                                         
         MVC   CODENAME+256(32),CODENAME                                        
         L     R1,ADHEIRA                                                       
         BAS   RE,GETNAME                                                       
         MVC   ACODE,3(R1)         MOVE CODE/NAME TO WORK                       
         MVC   ANAME,WORK                                                       
         L     R2,ADLVASTA                                                      
*                                                                               
         USING ACSTATD,R2                                                       
*                                                                               
         MVC   WORK(1),ACSTCOST                                                 
         CLI   WORK,X'40'                                                       
         BH    *+8                                                              
         MVI   WORK,X'FF'                                                       
         MVC   LEVACOST,WORK       SAVE DEPARTMENT DIVISION                     
         MVC   LEVBCOST(3),=X'FFFFFF'                                           
         B     EXIT                                                             
*                                                                               
         DROP  R2                                                               
         EJECT ,                                                                
*              LEDGFRST - EXTRACT HEIRARCHY DETAILS                             
*                                                                               
         USING ACHEIRD,R2                                                       
*                                                                               
AC70     L     R2,ADLDGHIR                                                      
         LA    R3,ACHRLEVA                                                      
         LA    R4,4                                                             
         MVI   WORK,0                                                           
         LA    R5,LEVTAB                                                        
         SR    R1,R1                                                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
*                                                                               
AC72     MVC   0(1,R5),WORK        DISP TO KEY                                  
         IC    R1,WORK                                                          
         IC    RE,0(,R3)                                                        
         SR    RE,R1                                                            
         SH    RE,=H'1'                                                         
         BM    AC74                DEAL WITH 2 LEVEL LEDGER                     
         STC   RE,1(,R5)            L'KEY FIELD-1                               
         MVC   2(15,R5),1(R3)      NAME                                         
         MVC   WORK(1),0(R3)                                                    
         LA    RF,1(,RF)                                                        
*                                                                               
AC74     LA    R3,16(,R3)                                                       
         LA    R5,17(,R5)                                                       
         BCT   R4,AC72                                                          
         STC   RF,LEVELS                                                        
*&&UK                                                                           
         CLI   REPTYPE,C'E'                                                     
         BNE   *+10                                                             
         MVC   LEVTAB+19(15),=CL15'EXPENSE TYPE'                                
*&&                                                                             
         CH    RF,=H'2'                                                         
         BH    *+10                                                             
         MVC   LEVTAB+34(17),LEVTAB+17                                          
         CH    RF,=H'3'                                                         
         BH    *+10                                                             
         MVC   LEVTAB+51(17),LEVTAB+34                                          
         MVC   0(2,R5),=X'000B'    FIX OTHER LEVELS                             
         MVC   2(15,R5),=CL15'CLIENT'                                           
         MVC   17(2,R5),=X'0000'                                                
         MVC   19(15,R5),=CL15'DIVISION'                                        
         MVC   34(2,R5),=X'FFFF'                                                
         MVC   36(15,R5),=CL15'REQUEST'                                         
         B     EXIT                                                             
*                                                                               
         DROP  R2                                                               
         EJECT ,                                                                
*              REQFIRST - ESTABLISH REPORT PERIOD                               
*                                                                               
AC80     GOTO1 VSORTER,DMCB,SORTCARD,RECCARD,0                                  
         MVI   SORTSW,C'N'                                                      
         LA    R2,DATAB                                                         
         MVC   QEND+4(2),=C'01'    1ST  ENTRY IN DATAB IS START DATE            
*                                  GET  END DATE YYMM01                         
         GOTO1 DATCON,DMCB,(0,QEND),(1,DUB)                                     
         MVC   0(1,R2),DUB         SAVE YY AND                                  
         MVI   1(R2),1             SET  MM TO JAN                               
         CLC   QSTART,SPACES       ANY  START DATE ?                            
         BE    AC81                NO,  SKIP                                    
         MVC   QSTART+4(2),=C'01'  SET  DAY TO 01                               
*                                  GET  START DATE YYMM01                       
         GOTO1 DATCON,DMCB,(0,QSTART),(1,DUB)                                   
         MVC   0(2,R2),DUB         SAVE YYMM                                    
*                                                                               
AC81     LA    R2,2(,R2)                                                        
*                                  FOLLOWED BY 12 2-BYTE ENTRIES                
*                                  BASED ON END DATE                            
*                                                                               
*                                  QEND DATE MINUS 1 YEAR                       
         GOTO1 ADDAY,DMCB,(C'Y',QEND),WORK,F'-1'                                
         LA    R3,12               SET LOOP COUNT                               
*                                                                               
AC82     DS    0H                  GET  NEXT MONTH                              
         GOTO1 ADDAY,DMCB,(C'M',WORK),WORK,F'1'                                 
*                                  NEXT MONTH PACKED                            
         GOTO1 DATCON,DMCB,(0,WORK),(1,WORK+6)                                  
         MVC   0(2,R2),WORK+6      SAVE YYMM                                    
         LA    R2,2(,R2)           ->   NEXT ENTRY                              
         BCT   R3,AC82             LOOP FOR  12  ENTRIES                        
*                                                                               
         MVC   QEND+4(2),SPACES    CLEAR THE DAY                                
         MVC   QSTART+4(2),SPACES  CLEAR THE DAY                                
*                                                                               
         CLC   QPROG(2),=C'94'                                                  
         BNE   AC86                                                             
         MVI   RCSUBPRG,0                                                       
         CLI   QOPT3,C'Y'          SUPPRESS 3MONTH/12MONTH ?                    
         BE    *+8                 OVERRIDE TO SUPPRESS                         
         MVI   RCSUBPRG,1                                                       
         LA    R1,OPTABP                                                        
         LA    R2,REPTABP                                                       
         STM   R1,R2,AOPTAB                                                     
         MVI   REPTYPE,C'P'                                                     
         B     EXIT                                                             
*                                                                               
AC86     MVI   RCSUBPRG,2                                                       
         LA    R1,OPTABE                                                        
         LA    R2,REPTABE                                                       
         STM   R1,R2,AOPTAB                                                     
         MVI   REPTYPE,C'E'                                                     
         B     EXIT                                                             
*                                                                               
SORTCARD DC    C'SORT FIELDS=(5,66,A),FORMAT=BI,WORK=1 '                        
RECCARD  DC    C'RECORD TYPE=V,LENGTH=(298,,,,) '                               
         EJECT ,                                                                
*              REQLAST - PRINT REPORT                                           
*                                                                               
AC90     XC    SORTREC(20),SORTREC PUT DUMMY RECORD TO SORT                     
         MVC   SORTLN,=H'298'                                                   
         MVI   SORTYPE,X'FF'                                                    
         GOTO1 VSORTER,DMCB,=C'PUT',SORTREC                                     
         XC    CODENAME(144),CODENAME                                           
         XC    CODENAME+144(144),CODENAME+144                                   
         GOTO1 PROLLER,DMCB,0,ACCUMS,6,8                                        
         XC    LASTYPE,LASTYPE                                                  
*                                                                               
AC92     GOTO1 VSORTER,DMCB,=C'GET'                                             
         L     R2,DMCB+4                                                        
         LTR   R2,R2                                                            
         BZ    AC93                                                             
         MOVE  (SORTREC,298),(R2)                                               
         CLI   SORTYPE,X'FF'                                                    
         BE    AC92                IGNORE DUMMY RECORD                          
         B     AC94                                                             
*                                  HANDLE SORT E-O-F                            
AC93     MVI   SORTYPE,X'FF'                                                    
         CLI   SORTSW,C'Y'                                                      
         BNE   EXIT                                                             
         MVI   SORTSW,C'E'         SET E-O-F                                    
*                                                                               
AC94     CLC   SORTYPE,LASTYPE     REPORT CONTROL BREAK                         
         BE    AC96                                                             
         CLI   LASTYPE,0           YES - IS THIS FIRST TIME                     
         BE    *+12                                                             
         MVI   TOTLEV,C'6'         NO  - PRINT TOTALS FOR LAST REPORT           
         BAS   RE,TOTALS                                                        
         CLI   SORTSW,C'E'         WAS E-O-F ON SORT SET                        
         BE    EXIT                                                             
         MVC   LASTYPE,SORTYPE     NO  - SET REPORT IN PROCESS                  
         XC    CODENAME(144),CODENAME                                           
         XC    CODENAME+144(144),CODENAME+144                                   
         MVI   REQFORM,C'N'        NEED TO FORMAT REQUEST DETAILS               
*                                  LEVEL 1 CONTROL BREAK                        
AC96     CLC   SORTAC,ECODE                                                     
         BE    AC98                                                             
         OC    ECODE,ECODE                                                      
         BZ    *+12                                                             
         MVI   TOTLEV,C'5'                                                      
         BAS   RE,TOTALS                                                        
         MVC   ECODE,SORTAC                                                     
         MVC   ENAME,SORTAN                                                     
         XC    CODENAME(ECODE-CODENAME),CODENAME                                
         MVI   HEADFORM,C'N'       NEED TO FORMAT HEADLINES                     
*                                  LEVEL 2 CONTROL BREAK                        
AC98     CLC   SORTBC,DCODE                                                     
         BE    AC9A                                                             
         OC    DCODE,DCODE                                                      
         BZ    *+12                                                             
         MVI   TOTLEV,C'4'                                                      
         BAS   RE,TOTALS                                                        
         MVC   DCODE,SORTBC                                                     
         MVC   DNAME,SORTBN                                                     
         XC    CODENAME(DCODE-CODENAME),CODENAME                                
         MVI   HEADFORM,C'N'       NEED TO FORMAT HEADLINES                     
*                                  LEVEL 3 CONTROL BREAK                        
AC9A     CLC   SORTCC,CCODE                                                     
         BE    AC9C                                                             
         OC    CCODE,CCODE                                                      
         BZ    *+12                                                             
         MVI   TOTLEV,C'3'                                                      
         BAS   RE,TOTALS                                                        
         MVC   CCODE,SORTCC                                                     
         MVC   CNAME,SORTCN                                                     
         XC    CODENAME(CCODE-CODENAME),CODENAME                                
         MVI   HEADFORM,C'N'       NEED TO FORMAT HEADLINES                     
*                                  LEVEL 4 CONTROL BREAK                        
AC9C     CLC   SORTDC,BCODE                                                     
         BE    AC9D                                                             
         OC    BCODE,BCODE                                                      
         BZ    *+12                                                             
         MVI   TOTLEV,C'2'                                                      
         BAS   RE,TOTALS                                                        
         MVC   BCODE,SORTDC                                                     
         MVC   BNAME,SORTDN                                                     
         XC    CODENAME(BCODE-CODENAME),CODENAME                                
         MVI   MIDFORM,C'N'        NEED TO FORMAT MIDLINES                      
*                                  LEVEL 5 CONTROL BREAK                        
AC9D     CLC   SORTEC,ACODE                                                     
         BE    AC9E                                                             
         OC    ACODE,ACODE                                                      
         BZ    *+12                                                             
         MVI   TOTLEV,C'1'                                                      
         BAS   RE,TOTALS                                                        
         MVC   ACODE,SORTEC                                                     
         MVC   ANAME,SORTEN                                                     
*                                  ROLL SORTACCS INTO ACCUMS                    
AC9E     LA    R1,ACCUMS+8                                                      
         LA    R2,6                                                             
*                                                                               
AC9G     LA    R3,8                                                             
         LA    R4,SORTACCS                                                      
*                                  ADD TO LEVEL 1-5 ACCUMS                      
AC9I     AP    0(6,R1),0(6,R4)                                                  
         LA    R1,6(,R1)                                                        
         LA    R4,6(,R4)                                                        
         BCT   R3,AC9I                                                          
         BCT   R2,AC9G                                                          
         B     AC92                GET NEXT RECORD                              
         EJECT ,                                                                
*              RUNFIRST - BUILD DIVISION CODE LIST                              
*                                                                               
AC100    MVC   DMWORK,SPACES                                                    
         MVC   DMWORK(1),RCCOMPFL                                               
         MVC   DMWORK+1(3),=X'F1F441'                                           
         MVC   DUB(3),DMWORK                                                    
         LA    R2,DIVLIST                                                       
         MVI   0(R2),X'FF'                                                      
         MVC   1(36,R2),=CL36'UNKNOWN'                                          
*                                                                               
AC102    GOTO1 DATAMGR,DMCB,DMRDHI,=C'ACCOUNT',DMWORK,SORTREC,0                 
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   SORTREC(3),DUB                                                   
         BNE   AC104                                                            
         LA    R1,SORTREC                                                       
         BAS   RE,GETNAME                                                       
         MVC   0(1,R2),SORTREC+3                                                
         MVC   1(36,R2),WORK                                                    
         LA    R2,L'DIVLIST(,R2)                                                
         MVI   0(R2),X'FF'                                                      
         MVC   1(36,R2),=CL36'UNKNOWN'                                          
         MVC   DMWORK(50),SORTREC                                               
         MVI   DMWORK+14,X'FF'                                                  
         B     AC102                                                            
*                                  RELOCATE ADCONS                              
AC104    L     R1,=V(SORTER)                                                    
         A     R1,RELO                                                          
         ST    R1,VSORTER                                                       
         L     R1,=V(SQUASHER)                                                  
         A     R1,RELO                                                          
         ST    R1,VSQUASH                                                       
         B     EXIT                                                             
         EJECT ,                                                                
*              HANDLE PRINTING OF TOTALS UP TO 5 LEVELS AT A TIME               
*                                                                               
TOTALS   NTR1                                                                   
         PACK  DUB,TOTLEV                                                       
         CVB   R2,DUB              R2=NUMBER OF LEVELS TO PRINT                 
         LA    R3,ACCUMS+8                                                      
         MVC   THISTYPE,LASTYPE                                                 
         MVI   THISTYPE,7                                                       
         LA    R4,THISTYPE+5       R4=A(CONTROL BREAK INDIC)                    
         LA    R5,ACODE            R5=A(CODENAME LIST)                          
         MVI   THISLEV,1                                                        
*                                                                               
TOTALS2  ZIC   R1,0(,R4)           R1=INDEX INTO LEVTAB                         
         SH    R1,=H'1'                                                         
         BM    TOTALSA             DONT PRINT THIS LEVEL TOTALS                 
         MVC   PA,SPACES                                                        
         MVC   PB,SPACES                                                        
         MH    R1,=H'17'                                                        
         LA    R1,LEVTAB(R1)       R1=A(LEVTAB ENTRY)                           
         LA    R6,WORK                                                          
         MVC   WORK,SPACES                                                      
         CLI   THISLEV,1           FORMAT LHS OF PRINT LINE(S)                  
         BE    TOTALS4                                                          
         MVC   0(10,R6),=C'TOTALS FOR'                                          
         MVC   11(15,R6),2(R1)                                                  
         LA    R6,28(,R6)                                                       
*                                                                               
TOTALS4  CLI   0(R1),X'FF'         REQUEST TOTALS                               
         BE    TOTALS5                                                          
         ZIC   RE,0(,R1)           EXTRACT CODE FROM KEY                        
         ZIC   RF,1(,R1)                                                        
         LA    RE,0(R5,RE)                                                      
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R6),0(RE)                                                    
         LA    R6,2(R6,RF)                                                      
         CLI   THISLEV,1                                                        
         BNE   *+10                                                             
         MVC   0(36,R6),12(R5)     AND NAME IF REQUIRED                         
*                                                                               
TOTALS5  GOTO1 VSQUASH,DMCB,WORK,64                                             
         L     R0,DMCB+4                                                        
         LA    R7,PA+1                                                          
         CLI   THISLEV,1                                                        
         BE    *+8                                                              
         LA    R7,3(,R7)           INSET TOTALS                                 
         GOTO1 CHOPPER,DMCB,((R0),WORK),(30,(R7)),(C'P',2)                      
         LA    RE,0(,R3)                                                        
         LA    RF,4                                                             
         LA    R7,PA+26                                                         
         CLI   REPTYPE,C'E'                                                     
         BE    TOTALS6                                                          
         LA    R7,PA+30                                                         
         CLI   RCSUBPRG,0                                                       
         BE    TOTALS6                                                          
         LA    R7,PA+40                                                         
         LA    RF,2                                                             
*                                  FORMAT ACCUMS INTO PRINT LINE                
TOTALS6  CP    0(6,RE),=P'0'                                                    
         BE    TOTALS7                                                          
         EDIT  (P6,0(RE)),(10,0(R7)),2,FLOAT=-                                  
*                                                                               
TOTALS7  CLI   QOPT2,C'S'          SUPPRESS CASH OPTION                         
         BE    TOTALS8                                                          
         ZAP   WORK(10),6(6,RE)    ROUND TO DOLLARS                             
         AP    WORK(10),=P'50'                                                  
         DP    WORK(10),=P'100'                                                 
         ZAP   DOUBLE,WORK(8)                                                   
         EDIT  (P8,DOUBLE),(10,10(R7)),FLOAT=-,ZERO=BLANK                       
*                                                                               
TOTALS8  CLI   RCSUBPRG,1          HANDLE 3MONTH/12MONTH SUPPRESSION            
         BNE   *+16                                                             
         LA    RE,24(,RE)                                                       
         LA    R7,35(,R7)                                                       
         B     *+12                                                             
         LA    RE,12(,RE)                                                       
         LA    R7,20(,R7)                                                       
         BCT   RF,TOTALS6                                                       
         BAS   RE,PRINTIT          PRINT THIS LINE                              
*                                                                               
TOTALSA  ZAP   0(6,R3),=P'0'       CLEAR ACCM LINE JUST PRINTED                 
         MVC   6(42,R3),0(R3)                                                   
         ZIC   R1,THISLEV          UPDATE LEVEL INDICATOR                       
         LA    R1,1(,R1)                                                        
         STC   R1,THISLEV                                                       
         LA    R3,48(,R3)          BUMP TO NEXT ACCUM                           
         LA    R5,48(,R5)                       CODE                            
         BCTR  R4,0                                                             
         BCT   R2,TOTALS2                                                       
         B     EXIT                                                             
         EJECT ,                                                                
*              FORMAT HEADLINES/MIDLINES & PRINT REPORT                         
*                                                                               
PRINTIT  NTR1                                                                   
         CLI   REQFORM,C'Y'                                                     
         BE    PRINT2                                                           
         MVC   HEADA,SPACES        FORMAT REQUEST                               
         MVC   HEADB,SPACES                                                     
         MVC   HEADC,SPACES                                                     
         MVC   HEADA+85(16),=C'FOR THE MONTH OF'                                
         MVC   DUB(4),QEND         GET  END  DATE YYMM                          
         MVC   DUB+4(2),=C'01'     SET  DAY  TO   01                            
         GOTO1 DATCON,DMCB,(0,DUB),(11,WORK) GET  MMMDD/YY                      
         MVC   HEADA+102(3),WORK   SAY  MMM                                     
         MVC   HEADA+105(3),WORK+5 SAY  /YY                                     
*                                                                               
         MVC   HEADB+85(11),=C'REPORT TYPE'                                     
         ZIC   R1,LASTYPE                                                       
         LA    RE,HEADB+97                                                      
         EDIT  (R1),(2,0(RE)),ALIGN=LEFT                                        
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVI   REQFORM,C'Y'                                                     
*                                                                               
PRINT2   CLI   HEADFORM,C'Y'                                                    
         BE    PRINT6                                                           
         MVC   HEADA(70),SPACES    FORMAT HEADLINES (LEVELS 1&2)                
         MVC   HEADB(70),SPACES                                                 
         MVC   HEADC(70),SPACES                                                 
         MVI   FORCEHED,C'Y'                                                    
         MVI   HEADFORM,C'Y'                                                    
         ZIC   R1,LASTYPE+1                                                     
         SH    R1,=H'1'                                                         
         BM    PRINT4                                                           
         LA    R5,ECODE                                                         
         BAS   RE,SQUASHIT                                                      
         MVC   HEADA+1(15),2(R1)                                                
         MVC   HEADA+17(50),WORK                                                
         GOTO1 VSQUASH,DMCB,HEADA+1,68                                          
*                                                                               
PRINT4   ZIC   R1,LASTYPE+2                                                     
         SH    R1,=H'1'                                                         
         BM    PRINT5                                                           
         LA    R5,DCODE                                                         
         BAS   RE,SQUASHIT                                                      
         MVC   HEADB+1(15),2(R1)                                                
         MVC   HEADB+17(50),WORK                                                
         GOTO1 VSQUASH,DMCB,HEADB+1,68                                          
*                                                                               
PRINT5   ZIC   R1,LASTYPE+3                                                     
         SH    R1,=H'1'                                                         
         BM    PRINT6                                                           
         LA    R5,CCODE                                                         
         BAS   RE,SQUASHIT                                                      
         MVC   HEADC+1(15),2(R1)                                                
         MVC   HEAD5+17(50),WORK                                                
         GOTO1 VSQUASH,DMCB,HEADC+1,68                                          
*                                                                               
PRINT6   CLI   MIDFORM,C'Y'                                                     
         BE    PRINT8                                                           
         MVI   MIDFORM,C'Y'        FORMAT MIDLINES (LEVEL 3)                    
         ZIC   R1,LASTYPE+4                                                     
         SH    R1,=H'1'                                                         
         BM    PRINT8                                                           
         LA    R5,BCODE                                                         
         BAS   RE,SQUASHIT                                                      
         MVC   P+1(50),WORK                                                     
         L     R1,DMCB+4                                                        
         MVI   PSECOND+1,C'-'                                                   
         SH    R1,=H'2'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PSECOND+2(0),PSECOND+1                                           
         MVI   PTHIRD,0                                                         
         MVC   HEAD5,HEADA                                                      
         MVC   HEAD6,HEADB                                                      
         GOTO1 ACREPORT                                                         
*                                  PRINT DATA                                   
PRINT8   MVC   HEAD5,HEADA                                                      
         MVC   HEAD6,HEADB                                                      
         MVC   HEAD7,HEADC                                                      
         LA    R1,P                                                             
         CLI   THISLEV,2                                                        
         BNE   *+12                                                             
         MVI   0(R1),0                                                          
         LA    R1,132(,R1)                                                      
         MVC   0(132,R1),PA                                                     
         MVC   132(132,R1),PB                                                   
         GOTO1 ACREPORT                                                         
         CLI   THISLEV,1                                                        
         BE    EXIT                                                             
         CLI   THISLEV,6                                                        
         BE    EXIT                                                             
         MVC   HEAD5,HEADA                                                      
         MVC   HEAD6,HEADB                                                      
         MVC   HEAD7,HEADC                                                      
         BASR  RE,RF               ON TOTALS DO A SPACE LINE                    
         B     EXIT                                                             
         EJECT ,                                                                
*              EXTRACT CODE/NAME FROM WORK & SQUASH TOGETHER                    
*              R1=LEVEL NUMBER, R5=A(CODE/NAME)                                 
*                                                                               
SQUASHIT NTR1                                                                   
         MH    R1,=H'17'                                                        
         LA    R1,LEVTAB(R1)                                                    
         ST    R1,DUB                                                           
         MVC   WORK,SPACES                                                      
         ZIC   RE,0(,R1)                                                        
         ZIC   RF,1(,R1)                                                        
         LA    RE,0(R5,RE)                                                      
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(RE)                                                    
         MVC   WORK+13(36),12(R5)                                               
         GOTO1 VSQUASH,DMCB,WORK,50                                             
         L     R1,DUB              RETURN A(LEVTAB ENTRY) IN R1                 
         XIT1  REGS=(R1)                                                        
         EJECT ,                                                                
*              EXTRACT NAME FROM ANY RECORD INTO WORK                           
*              R1=A(RECORD)                                                     
*                                                                               
GETNAME  NTR1                                                                   
         AH    R1,DATADISP                                                      
         SR    RE,RE                                                            
         MVC   WORK,SPACES                                                      
*                                                                               
GETNAME2 CLI   0(R1),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R1),X'20'                                                      
         BE    *+14                                                             
         IC    RE,1(,R1)                                                        
         AR    R1,RE                                                            
         B     GETNAME2                                                         
         IC    RE,1(,R1)                                                        
         SH    RE,=H'3'                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),2(R1)                                                    
         B     EXIT                                                             
         EJECT ,                                                                
*              LITERALS ETC.                                                    
*                                                                               
         LTORG                                                                  
         EJECT ,                                                                
* REPORT TYPE OPTION TABLE/REPORT DEFINITION TABLE                              
*                                                                               
* OPTAB  BYTE 0    - VALUE OF OPTION1 ON REQUEST CARD.                          
*             1    - LENGTH OF TABLE ENTRY.                                     
*             2-N  - LIST OF REPORT DEFINITIONS FOR THIS OPTION                 
*                    DELIMITED BY X'FF'. (INDEX INTO REPTAB).                   
*                                                                               
* REPTAB BYTE 0    - REPORT TYPE (PRINTED ON REPORT).                           
*             1-4  - 4 1BYTE VALUES (BETWEEN 0&5) DEFINING THE SORTING          
*                    & PRINTING FORMAT FOR THIS REPORT. THESE VALUES            
*                    ARE:-                                                      
*                                                                               
*                    0=NO CONTROL BREAK                                         
*                    1=OFFICE/UNIT                                              
*                    2=DEPARTMENT                                               
*                    3=SUB-DEPARTMENT                                           
*                    4=STAFF/EXPENSE                                            
*                    5=CLIENT/NON-CLIENT                                        
*                    6=DIVISION (COSTING GROUP)                                 
*                                                                               
*&&UK                                                                           
OPTABP   DS    0C                  PAYROLL COST ANALYSIS (AC94)                 
         DC    C'1',AL1(04),AL1(01,255)                                         
         DC    C'2',AL1(04),AL1(02,255)                                         
         DC    C'3',AL1(04),AL1(03,255)                                         
         DC    C'4',AL1(04),AL1(04,255)                                         
         DC    C'5',AL1(04),AL1(05,255)                                         
         DC    C'6',AL1(04),AL1(06,255)                                         
         DC    X'FF',AL1(09),AL1(01,02,03,04,05,06,255)                         
*                                                                               
REPTABP  DS    0CL6                                                             
         DC    AL1(01),AL1(5,0,0,6,4,5,0,0,6,4,5,0,0,6,4)                       
         DC    AL1(02),AL1(1,0,0,5,4,2,0,0,5,4,3,0,0,5,4)                       
         DC    AL1(03),AL1(1,0,0,4,5,2,0,0,4,5,3,0,0,4,5)                       
         DC    AL1(04),AL1(6,0,0,1,5,6,0,0,2,5,6,0,0,3,5)                       
         DC    AL1(05),AL1(6,0,0,5,1,6,0,0,5,2,6,0,0,5,3)                       
         DC    AL1(06),AL1(1,0,0,6,4,2,0,0,6,4,3,0,0,6,4)                       
*                                                                               
OPTABE   DS    0C                  DEPARTMENTAL EXPENSE ANALYSIS (AC95)         
         DC    C'1',AL1(04),AL1(01,255)                                         
         DC    C'2',AL1(04),AL1(02,255)                                         
         DC    C'3',AL1(04),AL1(03,255)                                         
         DC    X'FF',AL1(06),AL1(01,02,03,255)                                  
*                                                                               
REPTABE  DS    0CL6                                                             
         DC    AL1(01),AL1(6,0,0,1,2,6,0,0,2,3,6,0,0,3,4)                       
         DC    AL1(02),AL1(2,0,0,1,5,3,0,0,2,5,4,0,0,3,5)                       
         DC    AL1(03),AL1(5,0,0,1,2,5,0,0,2,3,5,0,0,3,4)                       
*&&                                                                             
*&&US                                                                           
OPTABP   DS    0C                  PAYROLL COST ANALYSIS (AC94)                 
         DC    C'1',AL1(04),AL1(01,255)                                         
         DC    C'2',AL1(04),AL1(02,255)                                         
         DC    C'3',AL1(04),AL1(03,255)                                         
         DC    C'4',AL1(04),AL1(04,255)                                         
         DC    C'5',AL1(04),AL1(05,255)                                         
         DC    C'6',AL1(04),AL1(06,255)                                         
         DC    C'7',AL1(04),AL1(07,255)                                         
         DC    C'8',AL1(04),AL1(08,255)                                         
         DC    X'FF',AL1(11),AL1(01,02,03,04,05,06,07,08,255)                   
*                                                                               
REPTABP  DS    0CL6                                                             
         DC    AL1(01),AL1(5,1,0,0,4,5,1,0,2,4,5,2,0,3,4)                       
         DC    AL1(02),AL1(1,0,0,5,4,1,2,0,5,4,2,3,0,5,4)                       
         DC    AL1(03),AL1(1,0,0,4,5,1,2,0,4,5,2,3,0,4,5)                       
         DC    AL1(04),AL1(6,1,0,0,5,6,1,0,2,5,6,2,0,3,5)                       
         DC    AL1(05),AL1(6,5,0,0,1,6,5,0,1,2,6,5,0,2,3)                       
         DC    AL1(06),AL1(1,0,0,6,4,1,2,0,6,4,2,3,0,6,4)                       
         DC    AL1(07),AL1(1,0,0,0,5,1,0,0,0,5,2,0,0,0,5)                       
         DC    AL1(08),AL1(5,0,0,0,6,5,0,0,0,6,5,0,0,0,6)                       
*                                  DEPARTMENTAL EXPENSE ANALYSIS (AC95)         
OPTABE   DS    0C                                                               
         DC    C'1',AL1(04),AL1(01,255)                                         
         DC    C'2',AL1(04),AL1(02,255)                                         
         DC    C'3',AL1(04),AL1(03,255)                                         
         DC    X'FF',AL1(06),AL1(01,02,03,255)                                  
*                                                                               
REPTABE  DS    0CL6                                                             
         DC    AL1(01),AL1(6,0,0,1,2,6,0,0,2,3,6,0,0,3,4)                       
         DC    AL1(02),AL1(2,0,0,1,5,3,0,0,2,5,4,0,0,3,5)                       
         DC    AL1(03),AL1(5,0,0,1,2,5,0,0,2,3,5,0,0,3,4)                       
*&&                                                                             
         EJECT ,                                                                
*              DSECT TO COVER SPACEND                                           
*                                                                               
WORKD    DSECT                                                                  
RELO     DS    F                                                                
VSORTER  DS    V                                                                
VSQUASH  DS    V                                                                
AOPTAB   DS    A                                                                
AREPTAB  DS    A                                                                
*                                                                               
SORTREC  DS    0C                                                               
SORTLN   DS    H                   RECORD LENGTH                                
         DS    H                                                                
SORTYPE  DS    CL6  REPORT TYPE/PRINTING FORMAT                                 
SORTAC   DS    CL12                LEVEL A-D CODES                              
SORTBC   DS    CL12                                                             
SORTCC   DS    CL12                                                             
SORTDC   DS    CL12                                                             
SORTEC   DS    CL12                                                             
SORTAN   DS    CL36                LEVEL A-D NAMES                              
SORTBN   DS    CL36                                                             
SORTCN   DS    CL36                                                             
SORTDN   DS    CL36                                                             
SORTEN   DS    CL36                                                             
SORTACCS DS    CL48                HOURS/CASH X 4                               
         DS    750C                                                             
*                                                                               
ACCUMS   DS    D                                                                
         DS    48PL6               6*8 6BYTE ACCUMS                             
*                                                                               
CODENAME DS    0C                                                               
ACODE    DS    CL12                DEPARTMENT                                   
ANAME    DS    CL36                                                             
BCODE    DS    CL12                SUB-DEPARTMENT                               
BNAME    DS    CL36                                                             
CCODE    DS    CL12                PERSONNEL                                    
CNAME    DS    CL36                                                             
DCODE    DS    CL12                CLIENT                                       
DNAME    DS    CL36                                                             
ECODE    DS    CL12                DIVISION                                     
ENAME    DS    CL36                                                             
FCODE    DS    CL12                                                             
FNAME    DS    CL36                                                             
*                                                                               
LEVACOST DS    C                   OFFICE   DIVISION                            
LEVBCOST DS    C                   DEPT     DIVISION                            
LEVCCOST DS    C                   SUB-DEPT DIVISION                            
LEVDCOST DS    C                   STAFF    DIVISION                            
*                                                                               
LEVELS   DS    C                                                                
LEVTAB   DS    7CL17                                                            
LASTYPE  DS    CL6                                                              
THISTYPE DS    CL6                                                              
THISACC  DS    C                                                                
REPTYPE  DS    C                                                                
DIVLIST  DS    30CL37              DIVISION CODE/NAME X 30                      
DATAB    DS    CL26                13X2 YYMM (PWOS)                             
*                                                                               
SORTSW   DS    C                                                                
RATE     DS    PL4                                                              
HOURS    DS    PL6                                                              
CASH     DS    PL6                                                              
*                                  FORMAT INDICATORS                            
REQFORM  DS    C                                                                
HEADFORM DS    C                                                                
MIDFORM  DS    C                                                                
TOTLEV   DS    C                                                                
THISLEV  DS    C                                                                
HEADA    DS    CL132               PRINTING SPACE                               
HEADB    DS    CL132                                                            
HEADC    DS    CL132                                                            
PA       DS    CL132                                                            
PB       DS    CL132                                                            
         EJECT ,                                                                
* ACGENBOTH                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
* ACREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
* ACGENMODES                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'038ACREP9502 06/03/15'                                      
         END                                                                    
