*          DATA SET ACREPPD02  AT LEVEL 015 AS OF 05/01/02                      
*PHASE ACPD02A                                                                  
*INCLUDE ACLIST                                                                 
         TITLE 'ORDER RECORD LIST/DELETE/RESTORE'                               
ACPD02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACPD**,R9                                                    
         L     RA,0(R1)            RA=A(ACWORKD)                                
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND          RC=A(PROGRAM W/S)                            
         USING ACPCD,RC                                                         
         LA    RF,HOOK                                                          
         ST    RF,HEADHOOK                                                      
*                                                                               
         CLI   MODE,RUNFRST        FIRST FOR RUN                                
         BE    RUNF                                                             
         CLI   MODE,REQFRST        FIRST FOR REQUEST                            
         BE    REQF                                                             
         CLI   MODE,LEDGFRST       FIRST FOR LEDGER                             
         BE    LEDGF                                                            
         CLI   MODE,PROCORD        READ AN ORDER RECORD                         
         BE    ORDER                                                            
         CLI   MODE,REQLAST        LAST FOR REQUEST                             
         BE    REQL                                                             
*                                                                               
XIT      XIT1  ,                   PROGRAM EXIT POINT                           
         EJECT                                                                  
RUNF     GOTO1 ADDICTAT,DMCB,C'LU  ',INLIST,OUTLIST                             
*                                                                               
RUNFX    B     XIT                                                              
         EJECT                                                                  
REQF     MVI   FORCECLR,C'Y'       REQFRST                                      
         GOTO1 ACREPORT            CLEARS HEADS/MIDS/PRINTS AND RETURNS         
*                                                                               
         LA    R1,TITLIST          LIST REPORT TITLE                            
         CLI   QOPT2,C'Y'          LIVE RUN?                                    
         BNE   REQF02                                                           
*                                                                               
         LA    R1,TITREST          RESTORE REPORT TITLE                         
         CLI   QOPT3,C'R'          LIVE, BUT RESTORING?                         
         BE    REQF02              YES                                          
*                                                                               
         LA    R1,TITDELE          DELETE REPORT TITLE                          
         CLI   QOPT3,C'D'          LIVE, BUT PHYSICALLY DELETING?               
         BE    REQF02              YES                                          
*                                                                               
         LA    R1,TITLOGC          NO, MUST BE LOGICALLY DELETING               
         CLI   QOPT3,C'L'                                                       
         BE    REQF02                                                           
         DC    H'0'                                                             
         DC    C'OPTION#3 MUST BE SET'                                          
*                                                                               
         USING VARSTATD,R1                                                      
REQF02   SR    RE,RE                                                            
         LA    RF,HEAD1+52         RF=A(START POINT FOR TITLE)                  
*                                                                               
REQF04   SR    R2,R2                                                            
         ICM   R2,3,VARDISP                                                     
         LA    R2,OUTLIST(R2)                                                   
         IC    RE,VARLEN                                                        
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),0(R2)                                                    
         AR    RF,RE               INCREMENT RF BY L'TEXT-1                     
         CLI   0(RF),C' '          SEARCH FOR NON-SPACE                         
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         CLI   VARSUFF,EOT         IS THIS THE LAST ENTRY?                      
         BE    REQF06              YES - QUIT LOOP.  RF=A(LAST CHAR)            
         MVC   1(1,RF),VARSUFF     INSERT SUFFIX (COMMA/SPACE)                  
         LA    RF,2(RF)            BUMP BEYOND                                  
         LA    R1,VARSLNQ(R1)      POINT R1 TO NEXT TEXT ENTRY                  
         B     REQF04              AND RETURN                                   
*                                                                               
REQF06   LA    R1,HEAD1+52         R1=A(FIRST CHAR OF TITLE)                    
         SR    RF,R1               RF=L'TITLE-1                                 
         LA    R1,L'P(R1)          POINT TO HEAD2+52                            
         MVI   0(R1),BOTF          BOX FLAT                                     
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R1),0(R1)       AND UNDERLINE TITLE WITH BOX FLATS           
*                                                                               
         MVI   CLEARHED,C'N'       DON'T CLEAR HEADS                            
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         LA    RF,ACCUMS           RF=A(TOTALS ACCUMULATORS)                    
         LA    R0,ACCUNUM          R0=NUMBER OF TOTALS ACCUMULATORS             
         ZAP   0(ACCULQ,RF),=P'0'  ZAP ACCUMULATOR                              
         LA    RF,ACCULQ(RF)       NEXT ACCUMULATOR                             
         BCT   R0,*-10                                                          
         XC    DISPS(DISPQ),DISPS  CLEAR PRINT DISPLACEMENTS                    
         ZAP   MARKED,=P'0'                                                     
         XC    LSRTMAJ,LSRTMAJ     CLEAR LAST SORT MAJOR FIELD                  
*                                                                               
         XC    BINPARMS(BINPARML),BINPARMS                                      
         LA    R0,BINBUFF                                                       
         ST    R0,BINP2            SAVE A(BINSRCH BUFFER)                       
         LA    R0,L'BINREC                                                      
         ST    R0,BINP4            SAVE L'BINREC                                
         LA    R0,L'BINKEY                                                      
         ST    R0,BINP5            SAVE L'BINREC KEY                            
         LA    R0,BINMAX                                                        
         ST    R0,BINP6            SAVE MAXIMUM ENTRIES                         
*                                                                               
         XC    PSTART,PSTART                                                    
         MVC   PEND,=3X'FF'                                                     
*                                  DTUSED=TODAY IF QOPT3 NEQ R                  
         GOTO1 DATCON,DMCB,(5,0),(2,DTUSED)                                     
         CLC   QSTART,SPACES       START DATE PRESENT?                          
         BE    REQF10                                                           
         CLI   QOPT3,C'R'          PSTART=3X'00', IF QOPT3=R                    
         BNE   REQF08                                                           
*                                  DTUSED=QSTART, IF QOPT3=R                    
         GOTO1 (RF),(R1),(0,QSTART),(2,DTUSED)                                  
         B     REQF10                                                           
*                                  PSTART=QSTART IF QOPT3 NEQ R                 
REQF08   GOTO1 (RF),(R1),(0,QSTART),(1,PSTART)                                  
*                                                                               
REQF10   CLC   QEND,SPACES                                                      
         BE    REQF20                                                           
         GOTO1 DATCON,DMCB,(0,QEND),(1,PEND)                                    
*                                                                               
REQF20   SR    RF,RF               LOCATE OPTION TABLE MENU FOR QOPT1           
         LA    R1,SORTTAB          R1=A(SORT OPTION TABLE)                      
         USING MENUD,R1                                                         
REQF30   CLC   MUOPTVAL,QOPT1      OPTION TABLE MENU MATCH?                     
         BE    REQF40              YES                                          
         IC    RF,MULENGTH         TAKE MENU LENGTH                             
         AR    R1,RF               POINT TO NEXT MENU                           
         CLI   0(R1),EOT           END OF OPTION TABLE REACHED?                 
         BNE   REQF30              RETURN IF OK                                 
         DC    H'0'                FALLEN OFF TABLE                             
*                                                                               
REQF40   ST    R1,SMUFIX           SAVE A(SORTTAB MENU FIXED PORTION)           
         SR    RE,RE                                                            
         IC    RE,MUTXDISP                                                      
         LA    RE,OUTLIST(RE)      A(TEXT)                                      
         IC    R3,MUTXLEN          L'TEXT-1                                     
         EX    R3,*+8              SORT MAJOR INTO RIGHT HEAD5                  
         B     *+10                                                             
         MVC   HEAD5+108+L'AC@SRTBY+1(0),0(RE)                                  
         MVC   HEAD5+108(L'AC@SRTBY),AC@SRTBY                                   
         MVC   HCBLEN,MUDALEN      L'HCBREAK-1 FOR EXECUTED COMPARE             
         LA    R2,MUVARIA                                                       
         TM    VARMATTR-VARMENUD(R2),HCB  TEST FOR HEAD CONTROL BREAK           
         BZ    REQF44                                                           
         EX    R3,*+8                                                           
         B     REQF44                                                           
         MVC   HEAD5+1(0),0(RE)    SORT MAJOR DESC. IN LEFT HEAD5, TOO          
*                                                                               
REQF44   BAS   RE,SETSORT                                                       
*                                                                               
REQF50   MVI   PRINTCOM,FORMHB     FORMAT HEADS AND BOXES - DON'T PRINT         
         BAS   RE,PRINTREC         GO TO PRINT ROUTINE                          
         B     REQF60                                                           
         EJECT                                                                  
*                                  DEAL WITH QOPT3/4 STATUS EXCLUSIONS          
REQF60   SR    R0,R0                                                            
         LA    R1,STATTAB          STATUS TABLE                                 
         USING STATD,R1                                                         
REQF64   CLC   STOPTVAL,QOPT3      MATCH WITH QOPT3/4                           
         BE    REQF80                                                           
         IC    R0,STLENGTH                                                      
         AR    R1,R0                                                            
         CLI   STLENGTH,EOT        END OF TABLE                                 
         BNE   REQF64                                                           
         DC    H'0'                                                             
*                                                                               
REQF80   MVC   FULLCC,STFULLCC                                                  
         MVC   PARTCC,STPARTCC                                                  
         MVC   LOGICC,STLOGICC                                                  
         MVC   DELECC,STDELECC                                                  
*                                                                               
         LA    R1,STVARIA                                                       
         USING VARSTATD,R1                                                      
         SR    RE,RE                                                            
         LA    RF,HEAD4+1                                                       
REQF84   SR    R2,R2                                                            
         ICM   R2,3,VARDISP                                                     
         LA    R2,OUTLIST(R2)                                                   
         IC    RE,VARLEN                                                        
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),0(R2)                                                    
         CLI   VARSUFF,EOT         IS THIS THE LAST ENTRY?                      
         BE    REQFX               YES - LEAVE LOOP                             
*                                                                               
         AR    RF,RE               NO - INCREMENT RF BY L'TEXT-1                
         CLI   0(RF),C' '          SEARCH FOR NON-SPACE                         
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVC   1(1,RF),VARSUFF     INSERT SUFFIX (COMMA/SPACE)                  
         LA    RF,2(RF)            BUMP BEYOND                                  
         CLI   VARSUFF,COMMA       IF IT'S A COMMA                              
         BNE   *+8                                                              
         LA    RF,1(RF)            BUMP EXTRA SPACE                             
         LA    R1,VARSLNQ(R1)      POINT R1 TO NEXT TEXT ENTRY                  
         B     REQF84              AND RETURN                                   
*                                                                               
REQFX    B     XIT                                                              
         DROP  R1                                                               
         EJECT                                                                  
LEDGF    CLI   QLEDGER,C'J'        LEDGFRST - IS IT PRODUCTION?                 
         BNE   LEDGF10             NO - THEN IT'S EXPENSES                      
         CLI   QOPT1,C'C'          CLIENT SORT                                  
         BE    LEDGF02                                                          
         CLI   QOPT1,C'P'          CLIENT SORT (WITH PERCENTAGES)               
         BNE   LEDGF06                                                          
LEDGF02  L     RE,ADLDGHIR         RE=A(LEDGER HEIRARCHY ELEMENT)               
         SR    RF,RF                                                            
         IC    RF,ACHRLEVA-ACHEIRD(RE)                                          
         LA    RF,CUL-1(RF)        RF= L'LEVA + CUL - 1                         
         STC   RF,HCBLEN           AMEND L'HCBREAK-1                            
LEDGF06  MVC   HEAD4+108(L'AC@PRD),AC@PRD                                       
         LA    R1,HEAD4+108+L'AC@PRD-1                                          
         B     LEDGF20                                                          
*                                                                               
LEDGF10  MVC   HEAD4+108(L'AC@EXP),AC@EXP                                       
         LA    R1,HEAD4+108+L'AC@EXP-1                                          
*                                                                               
LEDGF20  CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         MVC   2(L'AC@ORDS,R1),AC@ORDS                                          
         B     LEDGFX                                                           
*                                                                               
LEDGFX   B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* TEST FOR INCLUSION AND BUILD RECORD FOR SORTER OR FOR STRAIGHT      *         
* PRINT.  FULLCC, PARTCC, LOGICC AND DELECC BRANCH CONDITION CODES    *         
* WHICH ARE SET AT REQFRST.                                           *         
***********************************************************************         
*                                                                               
ORDER    L     R2,ADACC            PROCORD                                      
         USING ACKEYD,R2                                                        
         CLC   ACOKNUM,=C'000000'  SKIP CONTROL RECORD                          
         BE    ORDERX                                                           
*                                                                               
         CLC   QAPPL,SPACES                                                     
         BNH   ORD10                                                            
         CLC   QAPPL(L'QAPPL-6),SPACES                                          
         BNH   ORD05                                                            
         CLC   ACOKNUM,QAPPL                                                    
         BL    ORDERX                                                           
*                                                                               
ORD05    CLC   QAPPL+6(L'QAPPL-6),SPACES                                        
         BNH   ORD10                                                            
         CLC   ACOKNUM,QAPPL+6                                                  
         BH    ORDERX                                                           
*                                                                               
ORD10    IC    RF,FULLCC           SET BRANCH FOR FULLY MATCHED                 
         TM    ACSTATUS,ACORDMCH   TEST IF FULLY MATCHED                        
         EX    RF,*+4                                                           
         NOP   ORDERX                                                           
*                                                                               
         IC    RF,LOGICC           SET BRANCH FOR LOGICALLY DELETED             
         TM    ACSTATUS,ACORDDEL   TEST FOR LOGICAL DELETE                      
         EX    RF,*+4                                                           
         NOP   ORDERX                                                           
*                                                                               
         IC    RF,DELECC           SET BRANCH FOR DELETED                       
         TM    ACSTATUS,ACORDPHS   TEST FOR DELETE                              
         EX    RF,*+4                                                           
         NOP   ORDERX                                                           
*                                                                               
         CLI   QOPT3,C'R'          IF WE ARE RESTORING                          
         BNE   ORD20                                                            
         CLC   ACDTUSED,DTUSED     MUST BE REQUEST 'START' DAY                  
         BNE   ORDERX                                                           
*                                                                               
ORD20    LA    R0,SORTREC          CLEAR SORT RECORD AREA                       
         LA    R1,SORTRECL                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         MVC   SRTMAJOR(L'ACOKNUM),ACOKNUM  DEFAULT MAJOR FIELD                 
         MVC   SRTORDNO,ACOKNUM    ORDER NUMBER FROM KEY                        
         ZAP   SRTORDAM,=P'0'                                                   
         ZAP   SRTINVAM,=P'0'                                                   
         MVC   SRTWCODE,SPACES                                                  
         AH    R2,DATADISP                                                      
*                                                                               
ORD22    CLI   0(R2),0                                                          
         BE    ORD80                                                            
         CLI   0(R2),X'67'                                                      
         BE    ORD40                                                            
         CLI   0(R2),X'68'                                                      
         BE    ORD60                                                            
*                                                                               
ORD24    SR    RF,RF                                                            
         IC    RF,1(R2)                                                         
         AR    R2,RF                                                            
         B     ORD22                                                            
         EJECT                                                                  
         USING ACORDRD,R2                                                       
ORD40    CLI   OFFICE,0            IS THIS AN OFFICE LOGON                      
         BE    ORD42               NO - CARRY ON                                
         BAS   RE,FILTOFF          YES - GO AND SEE IF CLIENT IN LIST           
         BNE   ORDERX              FILTOFF SETS CC EQU/NEQ                      
*                                                                               
ORD42    CLC   ACORDATE,PSTART     APPLY DATE FILTERS                           
         BL    ORDERX                                                           
         CLC   ACORDATE,PEND                                                    
         BH    ORDERX                                                           
         CLC   QSELECT,SPACES      SUPPLIER CODE IN QSELECT                     
         BE    ORD50                                                            
         L     RF,VEXTRAS                                                       
         USING RUNXTRAD,RF                                                      
         OC    VLISTREC,VLISTREC                                                
         BZ    ORD44                                                            
         MVC   WORK,SPACES                                                      
         MVC   WORK(12),ACORSUPP+CUL                                            
         GOTO1 ACLIST,DMCB,VLISTREC,WORK                                        
         CLI   DMCB,0                                                           
         BNE   *+6                                                              
         DC    H'0'                BAD LIST RECORD                              
         CLI   DMCB,C'I'           INCLUDE SUPPLIER?                            
         BNE   ORDERX                                                           
         B     ORD50                                                            
         DROP  RF                                                               
*                                                                               
ORD44    LA    R1,QSELECT          STRAIGHT COMPARE FOR SUPPLIER                
         LA    RF,L'QSELECT-1(R1)                                               
         CLI   0(RF),C' '                                                       
         BH    ORD46                                                            
         BCT   RF,*-8                                                           
         DC    H'0'                NOTHING > X'40' - BAD QSELECT                
*                                                                               
ORD46    SR    RF,R1                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   ACORSUPP+CUL(0),QSELECT                                          
         BNE   ORDERX              CLIENT DOES NOT MATCH - EXCLUDE              
*                                                                               
ORD50    MVC   ACORSAVE,ACORDRD    MOVE OUT ELEMENT                             
         MVC   ACORSAVE+(ACORMEDC-ACORDRD)(L'ACORMEDC),FCRULMED                 
         MVC   SRTSUPP,ACORSUPP    SUPPLIER                                     
         MVC   SRTJOBEX,ACORJOB    JOB/EXPENSE ACCOUNT CODE                     
         MVC   SRTAUTH,ACORAUTH    AUTHORISER                                   
         MVC   SRTDATE,ACORDATE    ORDER DATE                                   
         MVC   SRTMEDC,ACORSAVE+(ACORMEDC-ACORDRD)                              
*                                                                               
         L     R1,SMUFIX           R1=A(SORTTAB MENU FIXED PORTION)             
         USING MENUD,R1                                                         
         SR    RE,RE                                                            
         ICM   RE,1,MUDADISP       MUDADISP=0, SRTMAJOR=ACOKNUM                 
         BZ    ORD24                                                            
         XC    SRTMAJOR,SRTMAJOR   ELSE, CLEAR SRTMAJOR CODE                    
         LA    RE,ACORSAVE(RE)     A(DATA)                                      
         IC    RF,MUDALEN          L'DATA-1                                     
         EX    RF,*+8                                                           
         B     ORD24                                                            
         MVC   SRTMAJOR(0),0(RE)   SORT RECORD MAJOR FIELD                      
*                                                                               
         USING ACOAMTD,R2                                                       
ORD60    L     RE,ADACC            GET BACK ACCOUNT                             
         TM    ACSTATUS-ACKEYD(RE),ACORDMCH     IF FULLY MATCHED                
         BO    ORD62               ORDER MADE IT THIS FAR, SKIP TEST            
         IC    RF,PARTCC           PARTLY MATCHED BRANCH CC                     
         CP    ACOAINUM,=P'0'      CHECK NUMBER OF INVOICES TO DATE             
         EX    RF,*+4                                                           
         NOP   ORDERX                                                           
*                                                                               
ORD62    AP    SRTORDAM,ACOAMT     ORDER AMOUNT (ESTIMATE)                      
         AP    SRTINVAM,ACOAIVAL   INVOICED AMOUNT                              
         CLC   SRTWCODE,SPACES     ONLY TAKE FIRST WORKCODE                     
         BNE   ORD24                                                            
         MVC   SRTWCODE,ACOAWC     ORDER WORKCODE                               
         B     ORD24                                                            
*                                                                               
ORD80    BAS   RE,PUTSORT          PUT RECORD TO SORTER                         
*                                                                               
ORD120   AP    MARKED,=P'1'        ADD ONE TO MARKED RECORDS COUNT              
         CLI   QOPT2,C'Y'          LIVE - CHANGE STATUS AND WRITE BACK          
         BNE   ORDERX              DRAFT - QUIT                                 
         L     R2,ADACC            POINT BACK TO ORDER RECORD KEY               
         USING ACKEYD,R2                                                        
         CLI   QOPT3,C'R'          QOPT3 R=RESTORE, ELSE DELETE                 
         BE    ORD122                                                           
         CLI   QOPT3,C'D'          ARE WE REALLY DELETING?                      
         BNE   ORD121              NO, MUST BE LOGICAL                          
         OI    ACSTATUS,ACORDPHS   YES, MARK IT SO                              
         B     ORD124                                                           
*                                                                               
ORD121   MVC   ACDTUSED,DTUSED     LOGICAL DELETE - UPDATE ACDTUSED             
         OI    ACSTATUS,ACORDDEL   AND SET LOGICAL STATUS                       
         B     ORD124                                                           
*                                                                               
ORD122   XC    ACDTUSED,ACDTUSED   RESTORE - CLEAR ACDTUSED                     
         NI    ACSTATUS,X'5F'      AND CLEAR LOGICAL AND DELETE STATUS          
         B     ORD124                                                           
*                                  WRITE BACK RECORD WITH NEW STATUS            
ORD124   GOTO1 DATAMGR,DMCB,DMWRT,=C'ACCOUNT',ACKEYD,ACKEYD                     
         CLI   8(R1),0                                                          
         BE    ORDERX                                                           
         DC    H'0'                BAD RETURN FROM DATAMGR                      
*                                                                               
ORDERX   B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
REQL     CLI   SORTACTV,C'Y'       NOTHING PUT TO SORTER - ENDSORT              
         BNE   REQL40                                                           
*                                                                               
REQL20   BAS   RE,GETSORT          ELSE READ SORTED RECORDS                     
         L     R1,SMUFIX           R1=A(SORTTAB MENU FIXED PORTION)             
         LA    R2,MUVARIA          TEST FIRST VARIABLE ENTRY HCB                
         TM    VARMATTR-VARMENUD(R2),HCB                                        
         BZ    REQL30                                                           
         OC    SORTKEY,SORTKEY     LAST TIME?                                   
         BZ    REQL22                                                           
         OC    LSRTMAJ,LSRTMAJ     FIRST TIME?                                  
         BZ    REQL24                                                           
         IC    RF,HCBLEN           L'HCBREAK-1 FOR EXECUTED COMPARE             
         EX    RF,*+8              HAS A CONTROL BREAK OCCURRED?                
         B     *+10                                                             
         CLC   LSRTMAJ(0),SRTMAJOR                                              
         BE    REQL30              NO - DON'T DO TOTALS                         
*                                                                               
REQL22   SR    RF,RF               YES - CONTROL BREAK TOTALS                   
         IC    RF,TTXDISP                                                       
         LA    RF,PSECOND+1(RF)                                                 
         MVC   0(L'AC@TFOR,RF),AC@TFOR                                          
         LA    RF,L'AC@TFOR-1(RF)  TO LAST BYTE OF FIELD                        
         CLI   0(RF),C' '                                                       
         BH    *+8                 COUNT BACK TO FIND A NON-SPACE               
         BCT   RF,*-8                                                           
         SR    RE,RE                                                            
         IC    RE,MUTXDISP                                                      
         LA    RE,OUTLIST(RE)      A(TEXT)                                      
         IC    R3,MUTXLEN          L'TEXT-1                                     
         EX    R3,*+8              SORT MAJOR TEXT FOR TOTALS                   
         B     *+10                                                             
         MVC   2(0,RF),0(RE)                                                    
         BAS   RE,PRINTTOT         REQUEST/CONTROL BREAK TOTALS                 
         OC    SORTKEY,SORTKEY     LAST TIME - SKIP TO REQUEST TOTALS           
         BZ    REQL40                                                           
         ZAP   TOTORDT,=P'0'       CLEAR ORDER AMOUNT TOTAL                     
         ZAP   TOTINVT,=P'0'       CLEAR INVOICED AMOUNT TOTAL                  
         MVI   FORCEHED,C'Y'       PAGE BREAK, PLEASE                           
*                                                                               
REQL24   MVC   LSRTMAJ,SRTMAJOR    UPDATE LAST MAJOR FIELD                      
         MVI   PRINTCOM,PRINTP     TELL ROUTINE TO PROCESS LINE DATA            
         MVC   OUTWORK,SPACES                                                   
         SR    RF,RF                                                            
         ICM   RF,7,VARMADDR-VARMENUD(R2)                                       
         BASR  RE,RF               GET CODE/NAME FROM 'PRINT' ROUTINE           
         SR    RF,RF                                                            
         IC    RF,OUTLEN           L'OUTPUT CODE/NAME                           
         SR    RE,RE                                                            
         IC    RE,MUTXLEN          L'TEXT-1                                     
         LA    RE,HEAD5+3(RE)      TEXT IS ALREADY IN HEAD5 (REQFRST)           
         EX    RF,*+8                                                           
         B     REQL30                                                           
         MVC   0(0,RE),OUTWORK                                                  
*                                                                               
REQL30   OC    SORTKEY,SORTKEY     LAST TIME?                                   
         BNZ   REQL34                                                           
         MVI   FORCEHED,C'N'       HOLD THE PAGE BREAK, PLEASE                  
         B     REQL40                                                           
*                                                                               
REQL34   MVI   PRINTCOM,PRINTP     FORMAT AND PRINT LINE DATA                   
         BAS   RE,PRINTREC         PRINT SORTED RECORD                          
         B     REQL20                                                           
         EJECT                                                                  
REQL40   BAS   RE,ENDSORT                                                       
         CLI   SORTACTV,C'Y'       NOTHING PUT TO SORTER - QUIT                 
         BNE   REQLX                                                            
         SR    R2,R2               REQUEST TOTALS                               
         IC    R2,TTXDISP                                                       
         LA    R2,PSECOND+1(R2)                                                 
         MVC   0(L'AC@TREQ,R2),AC@TREQ                                          
         ZAP   TOTORDT,REQORDT     REQUEST ORDER AMOUNT TOTAL                   
         ZAP   TOTINVT,REQINVT     REQUEST INVOICED AMOUNT TOTAL                
         LA    R2,2*L'P(R2)        R2=PFOURTH+TTXDISP                           
         UNPK  WORK(7),MARKED      GET MARKED COUNT INTO WORK                   
         OI    WORK+6,X'F0'        TURN SIGN NIBBLE TO CHARACTER                
         LA    RF,WORK             RF=A(START BYTE OF ZONED NUMBER)             
         LA    R1,WORK+6           R1=A(END BYTE OF ZONED NUMBER)               
         CLI   0(RF),C'0'          SEEK FIRST SIGNIFICANT DIGIT                 
         BNE   *+12                                                             
         LA    RF,1(RF)                                                         
         B     *-12                                                             
         SR    R1,RF               R1=SIGNIFICANT LENGTH-1                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(RF)       MOVE SIGNIFICANT DIGITS INTO PTHIRD          
         LA    R2,2(R1,R2)         POINT PAST                                   
         CP    MARKED,=P'1'                                                     
         BNE   REQL84                                                           
         MVC   0(L'AC@ORDER,R2),AC@ORDER                                        
         LA    R2,L'AC@ORDER-1(R2)                                              
         B     REQL86                                                           
*                                                                               
REQL84   MVC   0(L'AC@ORDS,R2),AC@ORDS                                          
         LA    R2,L'AC@ORDS-1(R2)                                               
*                                                                               
REQL86   CLI   QOPT2,C'Y'          RUN LIVE?                                    
         BNE   REQL100                                                          
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,REQL86                                                        
         LA    R2,2(R2)            POINT TO NEXT POSITION                       
         MVC   0(L'AC@DELD,R2),AC@DELD                                          
         CLI   QOPT3,C'R'          RESTORING?                                   
         BNE   REQL100                                                          
         MVC   0(L'AC@DELD,R2),SPACES   REPLACE 'DELETED'                       
         MVC   0(L'AC@RSRD,R2),AC@RSRD  WITH 'RESTORED'                         
*                                                                               
REQL100  BAS   RE,PRINTTOT         REQUEST/CONTROL BREAK TOTALS                 
         B     REQLX                                                            
*                                                                               
REQLX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE FILTERS BY OFFICE                                           *         
***********************************************************************         
*                                                                               
FILTOFF  DS    0H                  ROOM FOR EXPANSION, HERE                     
         CR    RB,RB                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE BUILDS DYNAMIC PRINT LINE FROM OPTION MENU, THEN PRINTS     *         
***********************************************************************         
*                                                                               
PRINTREC NTR1  ,                                                                
         L     R4,SMUFIX           R4=A(SORTTAB MENU FIXED PORTION)             
         USING MENUD,R4                                                         
         LA    R2,MUVARIA          R2=A(SORTTAB MENU VARIABLE PORTION)          
         USING VARMENUD,R2                                                      
         LA    R3,P+2              R3=A(P+2)                                    
         CLI   PRINTCOM,FORMHB                                                  
         BNE   PRIN10                                                           
         LA    R3,HEAD8+2          R3=A(HEAD8+2)                                
         L     R5,ADBXAREA         SET BOX ROWS AND BOX LEFT                    
         USING BOXD,R5                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXYORN,C'Y'                                                     
         MVC   BOXROWS,SPACES                                                   
         MVC   BOXCOLS,SPACES                                                   
         MVI   BOXROWS+6,C'T'                                                   
         MVI   BOXROWS+9,C'M'                                                   
         MVI   BOXROWS+56,C'B'                                                  
         MVI   BOXCOLS+1,C'L'                                                   
*                                                                               
PRIN10   TM    VARMATTR,HCB        IS FIRST ENTRY IN UPPER HEADS?               
         BO    PRIN32              YES - SKIP IT                                
         TM    VARMATTR,SJO        TEST IF ENTRY IS FOR SJ ONLY                 
         BZ    PRIN12              NO - CARRY ON                                
         CLI   QLEDGER,C'J'        YES TEST LEDGER IS SJ                        
         BNE   PRIN32              NO - SKIP ENTRY                              
*                                                                               
PRIN12   ST    R2,SMUVNTRY         SAVE THIS ENTRY ADDRESS                      
         MVC   OUTWORK,SPACES                                                   
         SR    RF,RF                                                            
         ICM   RF,7,VARMADDR       TAKE A(FORMAT ROUTINE)                       
         BASR  RE,RF               AND BRANCH TO IT                             
         B     PRIN20                                                           
         EJECT                                                                  
*                                  MOVE IN TEXT/DATA/HEADING ETC.               
PRIN20   SR    RF,RF                                                            
         IC    RF,OUTLEN                                                        
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),OUTWORK                                                  
         CLI   PRINTCOM,FORMHB     FOR HEADINGS                                 
         BE    PRIN22                                                           
         TM    VARMATTR,CHOPPED    OR IF CHOPPED                                
         BZ    PRIN24                                                           
*                                                                               
PRIN22   LA    RE,OUTWORK+1(RF)    TRY SECOND LINE                              
         EX    RF,*+8                                                           
         BE    PRIN24                                                           
         CLC   0(0,RE),SPACES      IF OUTWORK PART 2 IS NON-SPACE               
         EX    RF,*+8              MOVE SECOND LINE INTO PSECOND                
         B     PRIN24                                                           
         MVC   L'P(0,R3),0(RE)                                                  
*                                                                               
PRIN24   CLI   PRINTCOM,FORMHB                                                  
         BNE   PRIN28                                                           
         LR    RE,R3                                                            
         LA    R0,HEAD8+1                                                       
         SR    RE,R0                                                            
         TM    VARMATT2,TTX                                                     
         BZ    *+8                                                              
         STC   RE,TTXDISP          POSITION FOR REQUEST TOTAL TEXT              
         TM    VARMATT2,OAT                                                     
         BZ    *+8                                                              
         STC   RE,OATDISP          POSITION FOR ORDER AMOUNT TOTAL              
         TM    VARMATT2,IAT                                                     
         BZ    *+8                                                              
         STC   RE,IATDISP          POSITION FOR INVOICED AMOUNT TOTAL           
         TM    VARMATT2,DFT                                                     
         BZ    *+8                                                              
         STC   RE,DFTDISP          POSITION FOR DIFFERENCE TOTAL                
         TM    VARMATT2,PCT                                                     
         BZ    PRIN28                                                           
         STC   RE,PCTDISP          POSITION FOR PERCENTAGE TOTAL                
*                                                                               
PRIN28   LA    R3,2(RF,R3)         INCREMENT LINE POSITION                      
         CLI   PRINTCOM,FORMHB                                                  
         BNE   PRIN30                                                           
         LA    RE,2(RF,RE)                                                      
         CH    RE,=Y(L'P)          MAX COLUMNS                                  
         BNH   *+6                                                              
         DC    H'0'                REPORT IS TOO WIDE                           
         LA    RE,BOXCOLS(RE)                                                   
         MVI   0(RE),C'C'                                                       
*                                                                               
PRIN30   L     R2,SMUVNTRY         CURRENT ENTRY                                
*                                                                               
PRIN32   LA    R2,VARMLNQ(R2)      GET NEXT ENTRY                               
         CLC   0(3,R2),=AL3(EOT)   END OF MENU REACHED?                         
         BNE   PRIN10                                                           
*                                                                               
         CLI   PRINTCOM,FORMHB                                                  
         BNE   PRIN60                                                           
         MVI   0(RE),C'R'                                                       
         B     PRINTREX            DON'T ACTUALLY PRINT                         
*                                                                               
PRIN60   GOTO1 ACREPORT                                                         
*                                                                               
PRINTREX B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINES FORMAT COLUMN DATA/HEADINGS                                *         
***********************************************************************         
*                                                                               
FMSUPCN  NTR1  ,                                                                
         MVI   OUTLEN,50                                                        
         CLI   PRINTCOM,FORMHB                                                  
         BE    FMSUP20                                                          
         MVC   OUTWORK(14),SRTSUPP+1                                            
         GOTO1 GETNAME,SRTSUPP                                                  
         B     FMSUPX                                                           
*                                                                               
FMSUP20  L     R1,SMUVNTRY                                                      
         TM    VARMATTR-VARMENUD(R1),CHOPPED                                    
         BZ    *+8                                                              
         MVI   OUTLEN,25           'CHOPPED' LENGTH                             
         LA    R1,OUTWORK                                                       
         MVC   0(L'AC@SUPCN,R1),AC@SUPCN                                        
         B     FMSUPX                                                           
*                                                                               
FMSUPX   B     XIT                                                              
         EJECT                                                                  
FMORDNO  NTR1  ,                                                                
         MVI   OUTLEN,5                                                         
         CLI   PRINTCOM,FORMHB                                                  
         BE    FMORD20                                                          
         MVC   OUTWORK(L'SRTORDNO),SRTORDNO                                     
         B     FMORDX                                                           
*                                                                               
FMORD20  LA    R1,OUTWORK                                                       
         MVC   0(L'AC@ORDER,R1),AC@ORDER                                        
         SR    RF,RF                                                            
         IC    RF,OUTLEN                                                        
         LA    R1,1(RF,R1)                                                      
         MVC   0(L'AC@NUM,R1),AC@NUM                                            
         B     FMORDX                                                           
*                                                                               
FMORDX   B     XIT                                                              
         EJECT                                                                  
FMCLICN  NTR1  ,                                                                
         MVI   OUTLEN,48                                                        
         CLI   PRINTCOM,FORMHB                                                  
         BE    FMCLI20                                                          
         MVC   OUTWORK(12),SPACES                                               
         MVC   WORK(CUL),SRTJOBEX                                               
         SR    R1,R1                                                            
         IC    R1,HCBLEN           L'HCBREAK-1 FOR EXECUTED MOVE                
         SH    R1,=Y(CUL)          MINUS C/U/L                                  
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   OUTWORK(0),SRTJOBEX+CUL                                          
         MVC   WORK+CUL(12),OUTWORK                                             
         GOTO1 GETNAME,WORK                                                     
         B     FMCLIX                                                           
*                                                                               
FMCLI20  DS    0H                  CLIENT IS NEVER A COLUMN                     
         B     FMCLIX                                                           
*                                                                               
FMCLIX   B     XIT                                                              
         EJECT                                                                  
FMPRJOB  NTR1  ,                                                                
         MVI   OUTLEN,48                                                        
         CLI   PRINTCOM,FORMHB                                                  
         BE    FMPRJ20                                                          
         SR    R1,R1                                                            
         IC    R1,HCBLEN           R1 = HCBLEN = CUL + L'LEVA - 1               
         SH    R1,=Y(CUL-1)        R1 = L'LEVA                                  
         LA    RF,12               ACCOUNT LENGTH                               
         SR    RF,R1               SUBTRACT ACTUAL L'LEVA                       
         LA    R1,SRTJOBEX+CUL(R1) BEYOND C/U/L/LEVA                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   OUTWORK(0),0(R1)    PRODUCT/JOB CODE                             
         GOTO1 GETNAME,SRTJOBEX                                                 
         B     FMPRJX                                                           
*                                                                               
FMPRJ20  L     R1,SMUVNTRY                                                      
         TM    VARMATTR-VARMENUD(R1),CHOPPED                                    
         BZ    *+8                                                              
         MVI   OUTLEN,25           'CHOPPED' LENGTH                             
         LA    R1,OUTWORK                                                       
         MVC   0(L'AC@PRO,R1),AC@PRO                                            
         LA    R1,L'AC@PRO-1(R1)                                                
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         MVC   2(L'AC@AND,R1),AC@AND                                            
         LA    R1,2+L'AC@AND-1(R1)                                              
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         MVC   2(L'AC@JOB,R1),AC@JOB                                            
         B     FMPRJX                                                           
*                                                                               
FMPRJX   B     XIT                                                              
         EJECT                                                                  
FMJOBEX  NTR1  ,                                                                
         MVI   OUTLEN,48                                                        
         CLI   PRINTCOM,FORMHB                                                  
         BE    FMJOB20                                                          
         MVC   OUTWORK(12),SRTJOBEX+CUL                                         
         GOTO1 GETNAME,SRTJOBEX                                                 
         B     FMJOBX                                                           
*                                                                               
FMJOB20  L     R1,SMUVNTRY                                                      
         TM    VARMATTR-VARMENUD(R1),CHOPPED                                    
         BZ    *+8                                                              
         MVI   OUTLEN,25                  'CHOPPED' LENGTH                      
         LA    R1,OUTWORK                                                       
         CLI   QLEDGER,C'E'               EXPENSE REPORT?                       
         BE    FMJOB22                                                          
         MVC   0(L'AC@JOBC,R1),AC@JOBC    JOB CODE                              
         LA    R1,L'AC@JOBC-1(R1)                                               
         B     FMJOB24                                                          
*                                                                               
FMJOB22  MVC   0(L'AC@EXPA,R1),AC@EXPA    EXPENSE A/C                           
         LA    R1,L'AC@EXPA-1(R1)                                               
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         MVC   2(L'AC@CODE,R1),AC@CODE    CODE                                  
         LA    R1,2+L'AC@CODE-1(R1)                                             
*                                                                               
FMJOB24  CLI   0(R1),C' '                 COMMON FOR EXPENSES & JOBS            
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         MVC   2(L'AC@AND,R1),AC@AND      AND                                   
         LA    R1,2+L'AC@AND-1(R1)                                              
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         MVC   2(L'AC@NAME,R1),AC@NAME    NAME                                  
         B     FMJOBX                                                           
*                                                                               
FMJOBX   B     XIT                                                              
         EJECT                                                                  
FMWCODE  NTR1  ,                                                                
         MVI   OUTLEN,3                                                         
         CLI   PRINTCOM,FORMHB                                                  
         BE    FMWCO20                                                          
         MVC   OUTWORK+1(L'SRTWCODE),SRTWCODE                                   
         B     FMWCOX                                                           
*                                                                               
FMWCO20  LA    R1,OUTWORK                                                       
         MVC   0(L'AC@WORK,R1),AC@WORK                                          
         SR    RF,RF                                                            
         IC    RF,OUTLEN                                                        
         LA    R1,1(RF,R1)                                                      
         MVC   0(L'AC@CODE,R1),AC@CODE                                          
         B     FMWCOX                                                           
*                                                                               
FMWCOX   B     XIT                                                              
         EJECT                                                                  
FMORDAT  NTR1  ,                                                                
         MVI   OUTLEN,7                                                         
         CLI   PRINTCOM,FORMHB                                                  
         BE    FMODA20                                                          
         GOTO1 DATCON,DMCB,(1,SRTDATE),(8,OUTWORK)                              
         B     FMODAX                                                           
*                                                                               
FMODA20  LA    R1,OUTWORK                                                       
         MVC   0(L'AC@ORDER,R1),AC@ORDER                                        
         SR    RF,RF                                                            
         IC    RF,OUTLEN                                                        
         LA    R1,1(RF,R1)                                                      
         MVC   0(L'AC@DATE,R1),AC@DATE                                          
         B     FMODAX                                                           
*                                                                               
FMODAX   B     XIT                                                              
         EJECT                                                                  
FMAUTHR  NTR1  ,                                                                
         MVI   OUTLEN,14                                                        
         CLI   PRINTCOM,FORMHB                                                  
         BE    FMAUT20                                                          
         MVC   OUTWORK(L'SRTAUTH),SRTAUTH                                       
         B     FMAUTX                                                           
*                                                                               
FMAUT20  LA    R1,OUTWORK                                                       
         MVC   0(L'AC@ATHR,R1),AC@ATHR                                          
         B     FMAUTX                                                           
*                                                                               
FMAUTX   B     XIT                                                              
         EJECT                                                                  
FMORDAM  NTR1  ,                                                                
         MVI   OUTLEN,11                                                        
         CLI   PRINTCOM,FORMHB                                                  
         BE    FMOAM20                                                          
         GOTO1 BLDED,DMCB,(L'SRTORDAM,SRTORDAM),(12,OUTWORK)                    
         GOTO1 ADEDITOR,DMCB,MYTEMP                                             
         AP    TOTORDT,SRTORDAM                                                 
         AP    REQORDT,SRTORDAM                                                 
         B     FMOAMX                                                           
*                                                                               
FMOAM20  LA    R1,OUTWORK                                                       
         MVC   0(L'AC@ORDAM,R1),AC@ORDAM                                        
         B     FMOAMX                                                           
*                                                                               
FMOAMX   B     XIT                                                              
         EJECT                                                                  
FMINVAM  NTR1  ,                                                                
         MVI   OUTLEN,11                                                        
         CLI   PRINTCOM,FORMHB                                                  
         BE    FMINV20                                                          
         GOTO1 BLDED,DMCB,(L'SRTINVAM,SRTINVAM),(12,OUTWORK)                    
         GOTO1 ADEDITOR,DMCB,MYTEMP                                             
         AP    TOTINVT,SRTINVAM                                                 
         AP    REQINVT,SRTINVAM                                                 
         B     FMINVX                                                           
*                                                                               
FMINV20  LA    R1,OUTWORK                                                       
         MVC   0(L'AC@INVCD,R1),AC@INVCD                                        
         SR    RF,RF                                                            
         IC    RF,OUTLEN                                                        
         LA    R1,1(RF,R1)                                                      
         MVC   0(L'AC@TODT,R1),AC@TODT                                          
         B     FMINVX                                                           
*                                                                               
FMINVX   B     XIT                                                              
         EJECT                                                                  
FMMEDCN  NTR1  ,                                                                
         MVI   OUTLEN,L'ACMDDESC+2                                              
         CLI   PRINTCOM,FORMHB                                                  
         BE    FMMED20                                                          
         MVC   OUTWORK(L'ACMDDESC+2),SPACES                                     
         MVC   OUTWORK(L'SRTMEDC),SRTMEDC                                       
         L     R1,ADCOMP                                                        
         LA    R1,ACRECORD-ACKEYD(R1)                                           
         SR    R0,R0                                                            
         USING ACMEDIAD,R1                                                      
FMMED10  CLI   ACMDEL,0                                                         
         BE    FMMEDCNX                                                         
         CLI   ACMDEL,X'11'                                                     
         BNE   FMMED12                                                          
         CLC   ACMDCODE,SRTMEDC                                                 
         BE    FMMED14                                                          
FMMED12  IC    R0,ACMDLEN                                                       
         AR    R1,R0                                                            
         B     FMMED10                                                          
FMMED14  MVC   OUTWORK+2(L'ACMDDESC),ACMDDESC                                   
         B     FMMEDCNX                                                         
         DROP  R1                                                               
*                                                                               
FMMED20  DS    0H                  MEDIA IS NEVER A COLUMN                      
         B     FMMEDCNX                                                         
*                                                                               
FMMEDCNX B     XIT                                                              
         EJECT                                                                  
FMORDUE  NTR1  ,                                                                
         MVI   OUTLEN,6                                                         
         CLI   PRINTCOM,FORMHB                                                  
         BE    FMODU20                                                          
         GOTO1 DATCON,DMCB,(1,SRTDUEDT),(8,OUTWORK)                             
         B     FMODUX                                                           
*                                                                               
FMODU20  LA    R1,OUTWORK                                                       
         MVC   0(L'AC@DUE,R1),AC@DUE                                            
         SR    RF,RF                                                            
         IC    RF,OUTLEN                                                        
         LA    R1,1(RF,R1)                                                      
         MVC   0(L'AC@DATE,R1),AC@DATE                                          
         B     FMODUX                                                           
*                                                                               
FMODUX   B     XIT                                                              
         EJECT                                                                  
FMOIDIF  NTR1  ,                                                                
         MVI   OUTLEN,11                                                        
         CLI   PRINTCOM,FORMHB                                                  
         BE    FMDIF20                                                          
         ZAP   PWORK,SRTORDAM                                                   
         SP    PWORK,SRTINVAM                                                   
         GOTO1 BLDED,DMCB,(L'PWORK,PWORK),(12,OUTWORK)                          
         GOTO1 ADEDITOR,DMCB,MYTEMP                                             
         B     FMDIFX                                                           
*                                                                               
FMDIF20  LA    R1,OUTWORK                                                       
         MVC   0(L'AC@DFRNC,R1),AC@DFRNC                                        
         B     FMDIFX                                                           
*                                                                               
FMDIFX   B     XIT                                                              
         EJECT                                                                  
FMOIPCT  NTR1  ,                                                                
         MVI   OUTLEN,11                                                        
         CLI   PRINTCOM,FORMHB                                                  
         BE    FMPCT20                                                          
         CP    SRTORDAM,=P'0'                                                   
         BE    FMPCTX              IF DIVIDEND=0, SKIP PERCENTAGE               
         ZAP   PWORK12,SRTINVAM                                                 
         BZ    FMPCTX              IF DIVISOR=0, DON'T BOTHER EITHER            
         SRP   PWORK12,4,0                                                      
         DP    PWORK12,SRTORDAM                                                 
         SRP   PWORK12(L'PWORK12-L'SRTORDAM),64-1,5                             
         GOTO1 BLDED,DMCB,(L'PWORK12-L'SRTORDAM,PWORK12),(12,OUTWORK)           
         MVI   MYTEMP+(EBDECS-EBLOCK),X'01'  1 DP                               
         MVI   MYTEMP+(EBTRAIL-EBLOCK),C'%'  TRAIL WITH PCT SIGN                
         GOTO1 ADEDITOR,DMCB,MYTEMP                                             
         B     FMPCTX                                                           
*                                                                               
FMPCT20  LA    R1,OUTWORK                                                       
         MVC   0(L'AC@PCT,R1),AC@PCT                                            
         B     FMPCTX                                                           
*                                                                               
FMPCTX   B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE PRINTS REQUEST OR CONTROL BREAK TOTALS                      *         
***********************************************************************         
*                                                                               
PRINTTOT NTR1  ,                                                                
         SR    R2,R2               TOTAL ORDER AMOUNT                           
         IC    R2,OATDISP                                                       
         LA    R2,PSECOND+1(R2)                                                 
         GOTO1 BLDED,DMCB,('ACCULQ',TOTORDT),(12,(R2))                          
         GOTO1 ADEDITOR,DMCB,MYTEMP                                             
*                                                                               
         SR    R2,R2               TOTAL INVOICED AMOUNT                        
         IC    R2,IATDISP                                                       
         LA    R2,PSECOND+1(R2)                                                 
         GOTO1 BLDED,DMCB,('ACCULQ',TOTINVT),(12,(R2))                          
         GOTO1 ADEDITOR,DMCB,MYTEMP                                             
*                                                                               
         SR    R2,R2               DIFFERENCE                                   
         ICM   R2,1,DFTDISP                                                     
         BZ    PRTOT20                                                          
         LA    R2,PSECOND+1(R2)                                                 
         ZAP   PWORK,TOTORDT                                                    
         SP    PWORK,TOTINVT                                                    
         GOTO1 BLDED,DMCB,(L'PWORK,PWORK),(12,(R2))                             
         GOTO1 ADEDITOR,DMCB,MYTEMP                                             
*                                                                               
PRTOT20  SR    R2,R2               PERCENTAGE                                   
         ICM   R2,1,PCTDISP                                                     
         BZ    PRTOT40                                                          
         CP    TOTORDT,=P'0'                                                    
         BE    PRTOT40             IF DIVIDEND=0, SKIP PERCENTAGE               
         LA    R2,PSECOND+1(R2)                                                 
         ZAP   PWORK12,TOTINVT                                                  
         BZ    PRTOT40             IF DIVISOR=0, DON'T BOTHER EITHER            
         SRP   PWORK12,4,0                                                      
         DP    PWORK12,TOTORDT                                                  
         SRP   PWORK12(L'PWORK12-L'TOTORDT),64-1,5                              
         GOTO1 BLDED,DMCB,(L'PWORK12-L'TOTORDT,PWORK12),(12,(R2))               
         MVI   MYTEMP+(EBDECS-EBLOCK),X'01'  1 DP                               
         MVI   MYTEMP+(EBTRAIL-EBLOCK),C'%'  TRAIL WITH PCT SIGN                
         GOTO1 ADEDITOR,DMCB,MYTEMP                                             
*                                                                               
PRTOT40  GOTO1 ACREPORT                                                         
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE READS ACCFILE FOR ACCOUNT, GETS NAME, SQUASHES OUT SPACES,  *         
* CHOPS NAME UP IF REQUIRED (SEE VARMATTR) AND RESETS ACCFILE IF WE   *         
* ARE IN THE MIDDLE OF A REAL ACCOUNT SEQUENCE (NOT IF SORTING).      *         
* NTRY - R1=A(ACCOUNT CODE FOR ACCFILE READ)                          *         
***********************************************************************         
*                                                                               
GETNAME  NTR1  ,                                                                
         MVC   OUTWORK+20(L'BINNAME),SPACES                                     
         MVC   IOB(ACLENGTH-ACKEYD),SPACES                                      
         MVC   IOB(L'ACKEYACC),0(R1)                                            
         GOTO1 BINGET,IOB+1                                                     
         BNE   GETNA00                                                          
         MVC   OUTWORK+20(L'BINNAME),BINNAME                                    
         B     GETNA09             SKIP READ, BINPUT, ETC.                      
*                                                                               
GETNA00  MVC   BINKEY,IOB+1        ENSURE WE'VE GOT BINKEY FOR PUT              
         GOTO1 DATAMGR,DMCB,DMREAD,=C'ACCOUNT',IOB,IOB                          
         CLI   DMCB+8,0                                                         
         BE    GETNA02                                                          
         MVC   OUTWORK+20(L'AC@MSNG),AC@MSNG                                    
         B     GETNA08                                                          
*                                                                               
GETNA02  LA    R3,IOB                                                           
         AH    R3,DATADISP                                                      
         SR    R0,R0                                                            
GETNA04  CLI   0(R3),0                                                          
         BE    GETNAX                                                           
         CLI   0(R3),X'20'                                                      
         BE    GETNA06                                                          
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     GETNA04                                                          
*                                                                               
GETNA06  SR    RF,RF                                                            
         IC    RF,ACNMLEN-ACNAMED(R3)                                           
         SH    RF,=Y(ACNMNAME-ACNAMED+1)                                        
         BNM   *+6                                                              
         DC    H'0'                BAD LENGTH FOR NAME                          
         EX    RF,*+8                                                           
         B     GETNA08                                                          
         MVC   OUTWORK+20(0),ACNMNAME-ACNAMED(R3)                               
GETNA08  GOTO1 BINPUT,OUTWORK+20                                                
*                                                                               
GETNA09  GOTO1 ADSQUASH,DMCB,OUTWORK,64                                         
         L     R4,DMCB+4                                                        
         L     R2,SMUVNTRY                                                      
         TM    VARMATTR-VARMENUD(R2),CHOPPED                                    
         BZ    GETNAX              NOT FOR CHOPPING                             
         GOTO1 CHOPPER,DMCB,((R4),OUTWORK),(26,WORK),2                          
         MVC   OUTWORK,SPACES      CLEAR OUTWORK                                
         MVC   OUTWORK,WORK        PUT BACK DATA                                
         MVI   OUTLEN,25           AMEND OUTPUT LENGTH                          
*                                                                               
GETNAX   B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* BINSRCH INTERFACE ROUTINES                                          *         
***********************************************************************         
*                                                                               
BINPUT   NTR1  ,                   ADD AN ACCOUNT TO BINSRCH BUFFER             
         OC    BINP2,BINP2                                                      
         BZ    BINPUTX                                                          
         MVC   BINNAME,0(R1)                                                    
*                                                                               
BINPUT2  GOTO1 BINSRCH,BINPARMS,(1,BINREC)                                      
         OC    BINP1,BINP1                                                      
         BNZ   BINPUTX                                                          
         XC    BINP3,BINP3                                                      
         B     BINPUT2                                                          
*                                                                               
BINPUTX  B     XIT                                                              
*                                                                               
BINGET   NTR1  ,                   SEEK AN ACCOUNT IN BINSRCH BUFFER            
         MVI   BINP1,1                                                          
         OC    BINP3,BINP3                                                      
         BZ    BINGETX                                                          
         OC    BINP2,BINP2                                                      
         BZ    BINGETX                                                          
         MVC   BINKEY,0(R1)                                                     
         GOTO1 BINSRCH,BINPARMS,BINREC                                          
         CLI   BINP1,0                                                          
         BNE   BINGETX                                                          
         L     RE,0(R1)                                                         
         MVC   BINREC,0(RE)                                                     
*                                                                               
BINGETX  CLI   BINP1,0                                                          
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* SORTER INTERFACE ROUTINES                                           *         
***********************************************************************         
*                                                                               
SETSORT  NTR1  ,                                                                
         LA    R1,SORTKEYL         SORT KEY LENGTH INTO SORTCARD                
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SORTCARD+16(3),DUB                                               
         LA    R1,SORTRECL         SORT RECORD LENGTH INTO RECDCARD             
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  RECDCARD+22(3),DUB                                               
         GOTO1 ADSORTER,DMCB,SORTCARD,RECDCARD                                  
         MVI   SORTACTV,C'N'                                                    
         B     SORTX                                                            
*                                                                               
PUTSORT  NTR1  ,                                                                
         GOTO1 ADSORTER,DMCB,=C'PUT',SORTREC                                    
         MVI   SORTACTV,C'Y'                                                    
         B     SORTX                                                            
*                                                                               
*                                                                               
GETSORT  NTR1  ,                                                                
         GOTO1 ADSORTER,DMCB,=C'GET',0                                          
         XC    SORTKEY,SORTKEY                                                  
         L     RE,DMCB+4           RE=A(SORTED RECORD)                          
         LTR   RE,RE                                                            
         BZ    SORTX                                                            
         LA    R0,SORTREC                                                       
         LA    R1,SORTRECL                                                      
         LR    RF,R1                                                            
         MVCL  R0,RE               MOVE SORTED RECORD BACK INTO W/S             
         B     SORTX                                                            
*                                                                               
*                                                                               
ENDSORT  NTR1  ,                                                                
         GOTO1 ADSORTER,DMCB,=C'END'                                            
         B     SORTX                                                            
*                                                                               
SORTX    B     XIT                                                              
*                                                                               
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(01,000,A),FORMAT=BI,WORK=1'                    
RECDCARD DC    CL80'RECORD TYPE=F,LENGTH=(000,,,,)  '                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE BUILDS EDITOR BLOCK                                         *         
*                                                                     *         
* P1=LENGTH AND ADDRESS OF INPUT                                      *         
* P2=LENGTH AND ADDRESS OF OUTPUT                                     *         
***********************************************************************         
*                                                                               
BLDED    NTR1  ,                                                                
         LM    R2,R3,0(R1)                                                      
         LA    R4,MYTEMP                                                        
         USING EBLOCK,R4                                                        
         LA    RF,STEDBLK                                                       
         MVC   MYTEMP(L'EBLOCK),0(RF)                                           
         STCM  R2,8,EBLIN                                                       
         ST    R2,EBAIN                                                         
         STCM  R3,8,EBLOUT                                                      
         ST    R3,EBAOUT                                                        
         CLI   RCCTRY,CTRYGER                                                   
         BNE   BLDEDX                                                           
         OI    EBOPT,X'01'         SWAP COMMAS/ FULL STOPS                      
         B     BLDEDX                                                           
*                                                                               
BLDEDX   B     XIT                                                              
*                                                                               
* STANDARD EDITOR BLOCK: 2 DECIMAL PLACES, RIGHT-ALIGNED, MINUS=YES             
STEDBLK  DS    0F                                                               
         DC    F'0'                ADDRESS OF INPUT                             
         DC    X'00'               LENGTH                                       
         DC    C'P'                PACKED INPUT                                 
         DC    X'00'               SCALE                                        
         DC    X'00'                                                            
         DC    F'0'                ADDRESS OF OUTPUT                            
         DC    X'00'               LENGTH                                       
         DC    X'02'               DEC PLACES                                   
         DC    X'00'               FILL CHAR                                    
         DC    X'00'               FLOAT CHAR                                   
         DC    X'00'               DIVIDE AND ROUND                             
         DC    X'40'               OPTION BIT (MINUS=YES)                       
         DC    X'80'               ROUND                                        
         DC    C'0'                ALIGN (L=LEFT)                               
         DC    X'00'                                                            
         DC    X'00'               SCALE                                        
         DC    X'0000'                                                          
         EJECT                                                                  
***********************************************************************         
* HOOK ROUTINE                                                        *         
***********************************************************************         
*                                                                               
HOOK     DS    0H                  ROOM FOR EXPANSION HERE, TOO                 
         BR    RE                                                               
         EJECT                                                                  
ACLIST   DC    V(ACLIST)                                                        
*                                                                               
*                                                                               
SORTTAB  DS    0C                  SORT OPTION TABLE                            
*                                                                               
MENU1    DS    0C                                                               
         DC    AL1(MENU1X-MENU1)                                                
         DC    C' '                DEFAULT - ORDER NUMBER SEQUENCE              
         DC    AL1(L'AC@ORDC-1,AC@ORDC-OUTLIST)                                 
         DC    AL1(0,0)                                                         
         DC    AL3(FMORDNO),AL1(PRT,0)                                          
         DC    AL3(FMSUPCN),AL1(PRT+CHOPPED,0)                                  
         DC    AL3(FMJOBEX),AL1(PRT+CHOPPED,TTX)                                
         DC    AL3(FMWCODE),AL1(SJO+PRT,0)                                      
         DC    AL3(FMORDAT),AL1(PRT,0)                                          
         DC    AL3(FMAUTHR),AL1(PRT,0)                                          
         DC    AL3(FMORDAM),AL1(PRT,OAT)                                        
         DC    AL3(FMINVAM),AL1(PRT,IAT)                                        
*&&US                                                                           
         DC    AL3(FMORDUE),AL1(PRT,0)                                          
*&&                                                                             
         DC    AL3(EOT)                                                         
MENU1X   EQU   *                                                                
*                                                                               
MENU2    DS    0C                                                               
         DC    AL1(MENU2X-MENU2)                                                
         DC    C'S'                SORT BY SUPPLIER                             
         DC    AL1(L'AC@SUP-1,AC@SUP-OUTLIST)                                   
         DC    AL1(L'ACORSUPP-1,ACORSUPP-ACORDRD)                               
         DC    AL3(FMSUPCN),AL1(HCB,0)                                          
         DC    AL3(FMORDNO),AL1(PRT,0)                                          
         DC    AL3(FMJOBEX),AL1(PRT,TTX)                                        
         DC    AL3(FMWCODE),AL1(SJO+PRT,0)                                      
         DC    AL3(FMORDAT),AL1(PRT,0)                                          
         DC    AL3(FMAUTHR),AL1(PRT,0)                                          
         DC    AL3(FMORDAM),AL1(PRT,OAT)                                        
         DC    AL3(FMINVAM),AL1(PRT,IAT)                                        
*&&US                                                                           
         DC    AL3(FMORDUE),AL1(PRT,0)                                          
*&&                                                                             
         DC    AL3(EOT)                                                         
MENU2X   EQU   *                                                                
         EJECT                                                                  
MENU3    DS    0C                                                               
         DC    AL1(MENU3X-MENU3)                                                
         DC    C'C'                                                             
         DC    AL1(L'AC@CLI-1,AC@CLI-OUTLIST)                                   
         DC    AL1(L'ACORJOB-1,ACORJOB-ACORDRD)                                 
         DC    AL3(FMCLICN),AL1(HCB,0)                                          
         DC    AL3(FMPRJOB),AL1(PRT+CHOPPED,0)                                  
         DC    AL3(FMORDNO),AL1(PRT,0)                                          
         DC    AL3(FMSUPCN),AL1(PRT+CHOPPED,TTX)                                
         DC    AL3(FMWCODE),AL1(PRT,0)                                          
         DC    AL3(FMORDAT),AL1(PRT,0)                                          
         DC    AL3(FMAUTHR),AL1(PRT,0)                                          
         DC    AL3(FMORDAM),AL1(PRT,OAT)                                        
         DC    AL3(FMINVAM),AL1(PRT,IAT)                                        
*&&US                                                                           
         DC    AL3(FMORDUE),AL1(PRT,0)                                          
*&&                                                                             
         DC    AL3(EOT)                                                         
MENU3X   EQU   *                                                                
*                                                                               
MENU4    DS    0C                                                               
         DC    AL1(MENU4X-MENU4)                                                
         DC    C'E'                                                             
         DC    AL1(L'AC@EXPA-1,AC@EXPA-OUTLIST)                                 
         DC    AL1(L'ACOREXP-1,ACOREXP-ACORDRD)                                 
         DC    AL3(FMJOBEX),AL1(HCB,0)                                          
         DC    AL3(FMORDNO),AL1(PRT,0)                                          
         DC    AL3(FMSUPCN),AL1(PRT,TTX)                                        
         DC    AL3(FMORDAT),AL1(PRT,0)                                          
         DC    AL3(FMAUTHR),AL1(PRT,0)                                          
         DC    AL3(FMORDAM),AL1(PRT,OAT)                                        
         DC    AL3(FMINVAM),AL1(PRT,IAT)                                        
*&&US                                                                           
         DC    AL3(FMORDUE),AL1(PRT,0)                                          
*&&                                                                             
         DC    AL3(EOT)                                                         
MENU4X   EQU   *                                                                
         EJECT                                                                  
MENU5    DS    0C                                                               
         DC    AL1(MENU5X-MENU5)                                                
         DC    C'D'                                                             
         DC    AL1(L'AC@ORDDT-1,AC@ORDDT-OUTLIST)                               
         DC    AL1(L'ACORDATE-1,ACORDATE-ACORDRD)                               
         DC    AL3(FMORDAT),AL1(PRT,0)                                          
         DC    AL3(FMORDNO),AL1(PRT,0)                                          
         DC    AL3(FMSUPCN),AL1(PRT+CHOPPED,0)                                  
         DC    AL3(FMJOBEX),AL1(PRT+CHOPPED,TTX)                                
         DC    AL3(FMWCODE),AL1(SJO+PRT,0)                                      
         DC    AL3(FMAUTHR),AL1(PRT,0)                                          
         DC    AL3(FMORDAM),AL1(PRT,OAT)                                        
         DC    AL3(FMINVAM),AL1(PRT,IAT)                                        
*&&US                                                                           
         DC    AL3(FMORDUE),AL1(PRT,0)                                          
*&&                                                                             
         DC    AL3(EOT)                                                         
MENU5X   EQU   *                                                                
*                                                                               
MENU6    DS    0C                                                               
         DC    AL1(MENU6X-MENU6)                                                
         DC    C'A'                                                             
         DC    AL1(L'AC@ATHR-1,AC@ATHR-OUTLIST)                                 
         DC    AL1(L'ACORAUTH-1,ACORAUTH-ACORDRD)                               
         DC    AL3(FMAUTHR),AL1(HCB,0)                                          
         DC    AL3(FMORDNO),AL1(PRT,0)                                          
         DC    AL3(FMSUPCN),AL1(PRT+CHOPPED,0)                                  
         DC    AL3(FMJOBEX),AL1(PRT+CHOPPED,TTX)                                
         DC    AL3(FMWCODE),AL1(SJO+PRT,0)                                      
         DC    AL3(FMORDAT),AL1(PRT,0)                                          
         DC    AL3(FMORDAM),AL1(PRT,OAT)                                        
         DC    AL3(FMINVAM),AL1(PRT,IAT)                                        
*&&US                                                                           
         DC    AL3(FMORDUE),AL1(PRT,0)                                          
*&&                                                                             
         DC    AL3(EOT)                                                         
MENU6X   EQU   *                                                                
         EJECT                                                                  
MENU7    DS    0C                                                               
         DC    AL1(MENU7X-MENU7)                                                
         DC    C'M'                                                             
         DC    AL1(L'AC@MED-1,AC@MED-OUTLIST)                                   
         DC    AL1(L'ACORMEDC-1,ACORMEDC-ACORDRD)                               
         DC    AL3(FMMEDCN),AL1(HCB,0)                                          
         DC    AL3(FMORDNO),AL1(PRT,0)                                          
         DC    AL3(FMSUPCN),AL1(PRT+CHOPPED,0)                                  
         DC    AL3(FMJOBEX),AL1(PRT+CHOPPED,TTX)                                
         DC    AL3(FMWCODE),AL1(PRT,0)                                          
         DC    AL3(FMORDAT),AL1(PRT,0)                                          
         DC    AL3(FMAUTHR),AL1(PRT,0)                                          
         DC    AL3(FMORDAM),AL1(PRT,OAT)                                        
         DC    AL3(FMINVAM),AL1(PRT,IAT)                                        
*&&US                                                                           
         DC    AL3(FMORDUE),AL1(PRT,0)                                          
*&&                                                                             
         DC    AL3(EOT)                                                         
MENU7X   EQU   *                                                                
*&&US                                                                           
MENU8    DS    0C                                                               
         DC    AL1(MENU8X-MENU8)                                                
         DC    C'U'                                                             
         DC    AL1(L'AC@DUEDT-1,AC@DUEDT-OUTLIST)                               
         DC    AL1(L'ACORDDTE-1,ACORDDTE-ACORDRD)                               
         DC    AL3(FMORDUE),AL1(HCB,0)                                          
         DC    AL3(FMORDNO),AL1(PRT,0)                                          
         DC    AL3(FMSUPCN),AL1(PRT+CHOPPED,0)                                  
         DC    AL3(FMJOBEX),AL1(PRT+CHOPPED,TTX)                                
         DC    AL3(FMWCODE),AL1(SJO+PRT,0)                                      
         DC    AL3(FMORDAT),AL1(PRT,0)                                          
         DC    AL3(FMAUTHR),AL1(PRT,0)                                          
         DC    AL3(FMORDAM),AL1(PRT,OAT)                                        
         DC    AL3(FMINVAM),AL1(PRT,IAT)                                        
         DC    AL3(EOT)                                                         
MENU8X   EQU   *                                                                
*&&                                                                             
         EJECT                                                                  
MENU9    DS    0C                                                               
         DC    AL1(MENU9X-MENU9)                                                
         DC    C'P'                                                             
         DC    AL1(L'AC@CLI-1,AC@CLI-OUTLIST)                                   
         DC    AL1(L'ACORJOB-1,ACORJOB-ACORDRD)                                 
         DC    AL3(FMCLICN),AL1(HCB,0)                                          
         DC    AL3(FMPRJOB),AL1(PRT+CHOPPED,0)                                  
         DC    AL3(FMORDNO),AL1(PRT,0)                                          
         DC    AL3(FMSUPCN),AL1(PRT+CHOPPED,TTX)                                
         DC    AL3(FMWCODE),AL1(PRT,0)                                          
         DC    AL3(FMORDAT),AL1(PRT,0)                                          
         DC    AL3(FMORDAM),AL1(PRT,OAT)                                        
         DC    AL3(FMINVAM),AL1(PRT,IAT)                                        
         DC    AL3(FMOIDIF),AL1(PRT,DFT)                                        
         DC    AL3(FMOIPCT),AL1(PRT,PCT)                                        
         DC    AL3(EOT)                                                         
MENU9X   EQU   *                                                                
*                                                                               
MENU10   DS    0C                                                               
         DC    AL1(MENU10X-MENU10)                                              
         DC    C'X'                                                             
         DC    AL1(L'AC@EXPA-1,AC@EXPA-OUTLIST)                                 
         DC    AL1(L'ACOREXP-1,ACOREXP-ACORDRD)                                 
         DC    AL3(FMJOBEX),AL1(HCB,0)                                          
         DC    AL3(FMORDNO),AL1(PRT,0)                                          
         DC    AL3(FMSUPCN),AL1(PRT+CHOPPED,TTX)                                
         DC    AL3(FMAUTHR),AL1(PRT,0)                                          
         DC    AL3(FMORDAT),AL1(PRT,0)                                          
         DC    AL3(FMORDAM),AL1(PRT,OAT)                                        
         DC    AL3(FMINVAM),AL1(PRT,IAT)                                        
         DC    AL3(FMOIDIF),AL1(PRT,DFT)                                        
         DC    AL3(FMOIPCT),AL1(PRT,PCT)                                        
         DC    AL3(EOT)                                                         
MENU10X  EQU   *                                                                
OPT1TABX DC    AL1(EOT)                                                         
         EJECT                                                                  
STATTAB  DS    0C                  STATUS OPTION TABLE                          
*                                                                               
STAT1    DS    0C                                                               
         DC    AL1(STAT1X-STAT1)                                                
         DC    C'L',C'F'           LOGICALLY DELETE FULLY MATCHED               
         DC    AL1(ZERO,NOP,ONES,0)                                             
*                                                                               
         DC    AL2(AC@FLYMT-OUTLIST),AL1(L'AC@FLYMT-1,SPACE)                    
         DC    AL2(AC@ORDS-OUTLIST),AL1(L'AC@ORDS-1,EOT)                        
STAT1X   EQU   *                                                                
*                                                                               
*                                                                               
STAT2    DS    0C                                                               
         DC    AL1(STAT2X-STAT2)                                                
         DC    C'L',C'P'           LOGICALLY DELETE PARTLY MATCHED              
         DC    AL1(ONES,EQUAL,ONES,0)                                           
*                                                                               
         DC    AL2(AC@PTYMT-OUTLIST),AL1(L'AC@PTYMT-1,SPACE)                    
         DC    AL2(AC@ORDS-OUTLIST),AL1(L'AC@ORDS-1,EOT)                        
STAT2X   EQU   *                                                                
*                                                                               
*                                                                               
STAT3    DS    0C                                                               
         DC    AL1(STAT3X-STAT3)                                                
         DC    C'L',C'U'           LOGICALLY DELETE UNMATCHED                   
         DC    AL1(ONES,NEQUAL,ONES,0)                                          
*                                                                               
         DC    AL2(AC@UMTCD-OUTLIST),AL1(L'AC@UMTCD-1,SPACE)                    
         DC    AL2(AC@ORDS-OUTLIST),AL1(L'AC@ORDS-1,EOT)                        
STAT3X   EQU   *                                                                
*                                                                               
*                                                                               
STAT4    DS    0C                                                               
         DC    AL1(STAT4X-STAT4)                                                
         DC    C'L',C'A'           LOGICALLY DELETE ALL ORDERS                  
         DC    AL1(NOP,NOP,NOP,0)                                               
*                                                                               
         DC    AL2(AC@UMTCD-OUTLIST),AL1(L'AC@UMTCD-1,COMMA)                    
         DC    AL2(AC@PTYMT-OUTLIST),AL1(L'AC@PTYMT-1,SPACE)                    
         DC    AL2(AC@AND-OUTLIST),AL1(L'AC@AND-1,SPACE)                        
         DC    AL2(AC@FLYMT-OUTLIST),AL1(L'AC@FLYMT-1,SPACE)                    
         DC    AL2(AC@ORDS-OUTLIST),AL1(L'AC@ORDS-1,EOT)                        
STAT4X   EQU   *                                                                
*                                                                               
*                                                                               
STAT5    DS    0C                                                               
         DC    AL1(STAT5X-STAT5)                                                
         DC    C'L',C'B'         LOGICALLY DELETE FULLY/PARTLY MATCHED          
         DC    AL1(NOP,EQUAL,ONES,0)                                            
*                                                                               
         DC    AL2(AC@PTYMT-OUTLIST),AL1(L'AC@PTYMT-1,SPACE)                    
         DC    AL2(AC@AND-OUTLIST),AL1(L'AC@AND-1,SPACE)                        
         DC    AL2(AC@FLYMT-OUTLIST),AL1(L'AC@FLYMT-1,SPACE)                    
         DC    AL2(AC@ORDS-OUTLIST),AL1(L'AC@ORDS-1,EOT)                        
STAT5X   EQU   *                                                                
         EJECT                                                                  
STAT6    DS    0C                                                               
         DC    AL1(STAT6X-STAT6)                                                
         DC    C'R',C'F'           RESTORE FULLY MATCHED                        
         DC    AL1(ZERO,NOP,ZERO,0)                                             
*                                                                               
         DC    AL2(AC@DELD-OUTLIST),AL1(L'AC@DELD-1,SPACE)                      
         DC    AL2(AC@FLYMT-OUTLIST),AL1(L'AC@FLYMT-1,SPACE)                    
         DC    AL2(AC@ORDS-OUTLIST),AL1(L'AC@ORDS-1,EOT)                        
STAT6X   EQU   *                                                                
*                                                                               
*                                                                               
STAT7    DS    0C                                                               
         DC    AL1(STAT7X-STAT7)                                                
         DC    C'R',C'P'           RESTORE PARTLY MATCHED                       
         DC    AL1(ONES,EQUAL,ZERO,0)                                           
*                                                                               
         DC    AL2(AC@DELD-OUTLIST),AL1(L'AC@DELD-1,SPACE)                      
         DC    AL2(AC@PTYMT-OUTLIST),AL1(L'AC@PTYMT-1,SPACE)                    
         DC    AL2(AC@ORDS-OUTLIST),AL1(L'AC@ORDS-1,EOT)                        
STAT7X   EQU   *                                                                
*                                                                               
*                                                                               
STAT8    DS    0C                                                               
         DC    AL1(STAT8X-STAT8)                                                
         DC    C'R',C'U'           RESTORE UNMATCHED                            
         DC    AL1(ONES,NEQUAL,ZERO,0)                                          
*                                                                               
         DC    AL2(AC@DELD-OUTLIST),AL1(L'AC@DELD-1,SPACE)                      
         DC    AL2(AC@UMTCD-OUTLIST),AL1(L'AC@UMTCD-1,SPACE)                    
         DC    AL2(AC@ORDS-OUTLIST),AL1(L'AC@ORDS-1,EOT)                        
STAT8X   EQU   *                                                                
*                                                                               
*                                                                               
STAT9    DS    0C                                                               
         DC    AL1(STAT9X-STAT9)                                                
         DC    C'R',C'A'           RESTORE ALL ORDERS                           
         DC    AL1(NOP,NOP,ZERO,0)                                              
*                                                                               
         DC    AL2(AC@DELD-OUTLIST),AL1(L'AC@DELD-1,SPACE)                      
         DC    AL2(AC@UMTCD-OUTLIST),AL1(L'AC@UMTCD-1,COMMA)                    
         DC    AL2(AC@PTYMT-OUTLIST),AL1(L'AC@PTYMT-1,SPACE)                    
         DC    AL2(AC@AND-OUTLIST),AL1(L'AC@AND-1,SPACE)                        
         DC    AL2(AC@FLYMT-OUTLIST),AL1(L'AC@FLYMT-1,SPACE)                    
         DC    AL2(AC@ORDS-OUTLIST),AL1(L'AC@ORDS-1,EOT)                        
STAT9X   EQU   *                                                                
*                                                                               
*                                                                               
STAT10   DS    0C                                                               
         DC    AL1(STAT10X-STAT10)                                              
         DC    C'R',C'B'           RESTORE FULLY/PARTLY MATCHED                 
         DC    AL1(NOP,EQUAL,ZERO,0)                                            
*                                                                               
         DC    AL2(AC@DELD-OUTLIST),AL1(L'AC@DELD-1,SPACE)                      
         DC    AL2(AC@PTYMT-OUTLIST),AL1(L'AC@PTYMT-1,SPACE)                    
         DC    AL2(AC@AND-OUTLIST),AL1(L'AC@AND-1,SPACE)                        
         DC    AL2(AC@FLYMT-OUTLIST),AL1(L'AC@FLYMT-1,SPACE)                    
         DC    AL2(AC@ORDS-OUTLIST),AL1(L'AC@ORDS-1,EOT)                        
STAT10X  EQU   *                                                                
*                                                                               
*                                                                               
STAT11   DS    0C                                                               
         DC    AL1(STAT11X-STAT11)                                              
         DC    C'R',C'D'           RESTORE PHYSICALLY DELETED                   
         DC    AL1(NOP,NOP,NOP,ZERO)                                            
*                                                                               
         DC    AL2(AC@DELD-OUTLIST),AL1(L'AC@DELD-1,SPACE)                      
         DC    AL2(AC@ORDS-OUTLIST),AL1(L'AC@ORDS-1,EOT)                        
STAT11X  EQU   *                                                                
         EJECT                                                                  
STAT12   DS    0C                                                               
         DC    AL1(STAT12X-STAT12)                                              
         DC    C'D',C'L'           PHYSICALLY DELETE LOGICALLY DELETED          
         DC    AL1(NOP,NOP,ZERO,ONES)                                           
*                                                                               
         DC    AL2(AC@LOGDD-OUTLIST),AL1(L'AC@LOGDD-1,SPACE)                    
         DC    AL2(AC@ORDS-OUTLIST),AL1(L'AC@ORDS-1,EOT)                        
STAT12X  EQU   *                                                                
*                                                                               
STATTABX DC    AL1(EOT)                                                         
         EJECT                                                                  
TITLIST  DS    0C                  LIST REPORT TITLE                            
         DC    AL2(AC@ORDER-OUTLIST),AL1(L'AC@ORDER-1,SPACE)                    
         DC    AL2(AC@REC-OUTLIST),AL1(L'AC@REC-1,SPACE)                        
         DC    AL2(AC@LIST-OUTLIST),AL1(L'AC@LIST-1,EOT)                        
*                                                                               
TITLOGC  DS    0C                  LOGICAL DELETE REPORT TITLE                  
         DC    AL2(AC@ORDER-OUTLIST),AL1(L'AC@ORDER-1,SPACE)                    
         DC    AL2(AC@REC-OUTLIST),AL1(L'AC@REC-1,SPACE)                        
         DC    AL2(AC@LOGDL-OUTLIST),AL1(L'AC@LOGDL-1,EOT)                      
*                                                                               
TITDELE  DS    0C                  DELETE REPORT TITLE                          
         DC    AL2(AC@ORDER-OUTLIST),AL1(L'AC@ORDER-1,SPACE)                    
         DC    AL2(AC@REC-OUTLIST),AL1(L'AC@REC-1,SPACE)                        
         DC    AL2(AC@DEL-OUTLIST),AL1(L'AC@DEL-1,EOT)                          
*                                                                               
TITREST  DS    0C                  RESTORE REPORT TITLE                         
         DC    AL2(AC@ORDER-OUTLIST),AL1(L'AC@ORDER-1,SPACE)                    
         DC    AL2(AC@REC-OUTLIST),AL1(L'AC@REC-1,SPACE)                        
         DC    AL2(AC@RSR-OUTLIST),AL1(L'AC@RSR-1,EOT)                          
         EJECT                                                                  
INLIST   DS    0C                  DICTIONARY DC DECLARATIVES                   
         DCDDL AC#MSNG,7,L                                                      
         DCDDL AC#ATHR,10,L                                                     
         DCDDL AC#SUP,10,L                                                      
         DCDDL AC#CLI,10,L                                                      
         DCDDL AC#PRO,10,L                                                      
         DCDDL AC#MED,10,L                                                      
         DCDDL AC#JOBC,10,L                                                     
         DCDDL AC#ORDC,12,L                                                     
         DCDDL AC#JOB,3,L                                                       
         DCDDL AC#LOGDL,16,L                                                    
         DCDDL AC#LOGDD,17,L                                                    
         DCDDL AC#EXPA,11,L                                                     
         DCDDL AC#ORDDT,10,L                                                    
         DCDDL AC#DUEDT,10,L                                                    
         DCDDL AC#TREQ,18,L                                                     
         DCDDL AC#TFOR,10,L                                                     
         DCDDL AC#TOTAL,7,L                                                     
         DCDDL AC#SRTBY,9,L                                                     
         DCDDL AC#SUPCN,22,L                                                    
         DCDDL AC#DUE,6,L                                                       
         DCDDL AC#ORDAM,12,R                                                    
         DCDDL AC#ORDER,6,L                                                     
         DCDDL AC#NUM,6,L                                                       
         DCDDL AC#DATE,6,L                                                      
         DCDDL AC#INVCD,12,R                                                    
         DCDDL AC#TODT,12,R                                                     
         DCDDL AC#PCT,12,R                                                      
         DCDDL AC#DFRNC,12,R                                                    
         DCDDL AC#WORK,4,L                                                      
         DCDDL AC#CODE,4,L                                                      
         DCDDL AC#AND,3,L                                                       
         DCDDL AC#NAME,4,L                                                      
         DCDDL AC#REC,6,L                                                       
         DCDDL AC#LIST,5,L                                                      
         DCDDL AC#DEL,7,L                                                       
         DCDDL AC#RSR,16,L                                                      
         DCDDL AC#LIVE,6,L                                                      
         DCDDL AC#DRAFT,8,L                                                     
         DCDDL AC#UMTCD,20,L                                                    
         DCDDL AC#FLYMT,20,L                                                    
         DCDDL AC#PTYMT,20,L                                                    
         DCDDL AC#ORDS,10,L                                                     
         DCDDL AC#DELD,8,L                                                      
         DCDDL AC#PRD,10,L                                                      
         DCDDL AC#EXP,10,L                                                      
         DCDDL AC#RSRD,12,L                                                     
INLISTX  DC    X'00'                                                            
         EJECT                                                                  
*                                  ********************                         
*                                  LITERALS AND EQUATES                         
*                                  ********************                         
         LTORG                                                                  
*                                                                               
BINBUFF  DC    1500XL(L'BINREC)'00'                                             
BINMAX   EQU   (*-BINBUFF)/L'BINREC                                             
         EJECT                                                                  
HCB      EQU   X'01'               FIELD IS A HEAD CONTROL BREAK                
PRT      EQU   X'02'               FIELD IS IN PRINT LINE                       
CHOPPED  EQU   X'04'               TEXT HAS BEEN CHOPPED                        
SJO      EQU   X'08'               PRODUCTION ONLY                              
*                                                                               
TTX      EQU   X'01'               TOTALS TEXT HERE                             
OAT      EQU   X'02'               ORDER AMOUNT TOTAL HERE                      
IAT      EQU   X'04'               INVOICED AMOUNT TOTAL HERE                   
DFT      EQU   X'08'               DIFFERENCE TOTAL HERE                        
PCT      EQU   X'10'               PERCENTAGE TOTAL HERE                        
*                                                                               
NEQUAL   EQU   X'70'               NEQ FOR BRANCH CC (BNE)                      
EQUAL    EQU   X'80'               EQU FOR BRANCH CC (BE)                       
ZERO     EQU   X'80'               ZERO FOR BRANCH CC (BZ)                      
ONES     EQU   X'10'               ONES FOR BRANCH CC (BO)                      
NOP      EQU   X'00'               NOP FOR BRANCH CC (NOP)                      
SPACE    EQU   C' '                                                             
COMMA    EQU   C','                                                             
SLASH    EQU   C'/'                                                             
FORMHB   EQU   C'H'                FORMAT HEADS AND BOXES - DON'T PRINT         
PRINTP   EQU   C'P'                FORMAT AND PRINT LINE DATA                   
EOT      EQU   X'00'               END-OF-TABLE MARKER                          
CUL      EQU   X'03'               COMPANY/UNIT/LEDGER                          
CTRYGER  EQU   X'03'               COUNTRY IS GERMANY                           
         EJECT                                                                  
*                                  ***********************                      
ACPCD    DSECT                     PROGRAM WORKING STORAGE                      
*                                  ***********************                      
BINPARMS DS    0F                                                               
BINP1    DS    F                                                                
BINP2    DS    F                                                                
BINP3    DS    F                                                                
BINP4    DS    F                                                                
BINP5    DS    F                                                                
BINP6    DS    F                                                                
BINPARML EQU   *-BINPARMS                                                       
SMUFIX   DS    F                   A(SORTTAB MENU FIXED PORTION)                
SMUVNTRY DS    F                   A(SORTTAB MENU VARIABLE LEN. ENTRY)          
*                                                                               
MYTEMP   DS    CL28                FOR EDITOR CALL                              
OFFICE   DS    CL2                 OFFICE                                       
WORKCODE DS    CL(L'ACKEYWRK)      MAJOR WORKCODE                               
LSRTMAJ  DS    CL(L'SRTMAJOR)      LAST MAJOR FIELD IN SORT RECORD              
HCBLEN   DS    X                   L'HCBREAK-1 FOR EXECUTED COMPARE             
OUTWORK  DS    CL(L'SPACES)        OUTPUT TEXT FROM FORMAT ROUTINE              
OUTLEN   DS    X                   L'OUTPUT-1 FROM FORMAT ROUTINE               
*                                                                               
BINREC   DS    0CL50                                                            
BINKEY   DS    CL14                                                             
BINNAME  DS    CL36                                                             
*                                                                               
FULLCC   DS    C                   CC - FOR FULLY MATCHED TEST                  
PARTCC   DS    C                      - FOR PARTLY MATCHED TEST                 
LOGICC   DS    C                      - FOR LOGICALLY DELETED                   
DELECC   DS    C                      - FOR PHYSICALLY DELETED                  
*                                                                               
ACCUMS   DS    0C                  TOTALS ACCUMULATORS                          
TOTORDT  DS    PL6                 CONTROL BREAK TOTAL ORDER AMOUNT             
TOTINVT  DS    PL6                 CONTROL BREAK TOTAL INVOICED AMOUNT          
REQORDT  DS    PL6                 REQUEST TOTAL ORDER AMOUNT                   
REQINVT  DS    PL6                 REQUEST TOTAL INVOICED AMOUNT                
ACCULQ   EQU   *-REQINVT           L'EACH ACCUMULATOR                           
ACCUNUM  EQU   (*-ACCUMS)/ACCULQ   NUMBER OF ACCUMULATORS                       
*                                                                               
PWORK    DS    PL6                 PACKED WORK AREA                             
PWORK12  DS    PL12                PACKED WORK AREA                             
MARKED   DS    PL4                 MARKED RECORDS COUNT                         
PSTART   DS    CL3                 PACKED START YYMMDD                          
PEND     DS    CL3                 PACKED END YYMMDD                            
DTUSED   DS    CL2                 BINARY 'TODAY' FOR USED DATE                 
*                                                                               
DISPS    DS    0X                  PRINT DISPLACEMENTS                          
TTXDISP  DS    X                   PRINT DISPLACEMENT FOR 'TOTAL' TEXT          
OATDISP  DS    X                   ORDER AMOUNT TOTAL DISP                      
IATDISP  DS    X                   INVOICED AMOUNT TOTAL DISP                   
DFTDISP  DS    X                   DIFFERENCE TOTAL DISP                        
PCTDISP  DS    X                   PERCENTAGE TOTAL DISP                        
DISPQ    EQU   *-DISPS             L'PRINT DISPLACEMENTS                        
*                                                                               
PRINTCOM DS    C                   PRINT COMMAND - SEE EQUATES                  
SORTACTV DS    C                   SORTER ACTIVE (Y/N)                          
*                                                                               
ACORSAVE DS    CL256               SAVED ORDER ELEMENT                          
*                                                                               
OUTLIST  DS    0C                  DICTIONARY DS DECLARATIVES                   
         DSDDL PRINT=YES                                                        
*                                                                               
SORTREC  DS    0CL(SORTRECL)       'SORT' RECORD (FOR SORT OR PRINT)            
SORTKEY  DS    0CL(SORTKEYL)                                                    
SRTMAJOR DS    CL15                MAJOR FIELD IN SORT RECORD                   
SRTORDNO DS    CL(L'ACOKNUM)       ORDER NUMBER                                 
SRTSUPP  DS    CL(L'ACORSUPP)      SUPPLIER                                     
SRTJOBEX DS    CL(L'ACORJOB)       JOB/EXPENSE ACCOUNT CODE                     
SRTDUEDT DS    CL(L'ACORDDTE)      DUE DATE (U.S. ONLY)                         
SRTDATE  DS    CL(L'ACORDATE)      ORDER DATE                                   
SRTAUTH  DS    CL(L'ACORAUTH)      AUTHORISER                                   
SRTWCODE DS    CL(L'ACOAWC)        ORDER WORKCODE                               
SORTKEYL EQU   *-SRTMAJOR          KEY LENGTH                                   
*                                                                               
SRTORDAM DS    CL(L'ACOAMT)        ORDER AMOUNT (ESTIMATE)                      
SRTINVAM DS    CL(L'ACOAIVAL)      INVOICED AMOUNT                              
SRTMEDC  DS    CL(L'ACORMEDC)      MEDIA CODE                                   
SORTRECL EQU   *-SORTKEY           RECORD LENGTH                                
*                                                                               
IOB      DS    1000C                                                            
         EJECT                                                                  
*                                  *************                                
MENUD    DSECT                     SORTTAB MENUS                                
*                                  *************                                
MULENGTH DS    AL1                 L'FIXED+VARIABLE MENU PORTIONS               
MUOPTVAL DS    C                   OPTION VALUE                                 
MUTXLEN  DS    AL1                 L'TEXT FOR OPTION DESCRIPTION                
MUTXDISP DS    AL1                 DISP. OF OPTION TEXT INTO OUTLIST            
MUDALEN  DS    AL1                 L'DATA FOR OPTION PRIMARY SORT DATA          
MUDADISP DS    AL1                 A(OPTION PRIMARY SORT DATA)                  
MUFIXLNQ EQU   *-MENUD             L'FIXED MENU PORTION                         
MUVARIA  DS    0CL4                VARIABLE LENGTH PORTION STARTS HERE          
*                                                                               
*                                  ***********************************          
VARMENUD DSECT                     SORTTAB MENU VARIABLE PORTION ENTRY          
*                                  ***********************************          
VARMADDR DS    AL3                 A(FORMAT ROUTINE FOR ENTRY)                  
VARMATTR DS    AL1                 STATUS ATTRIBUTES OF OPTION                  
VARMATT2 DS    AL1                 STATUS ATTRIBUTE BYTE 2                      
VARMLNQ  EQU   *-VARMENUD          L'ENTRY IN VARIABLE LENGTH PORTION           
         EJECT                                                                  
*                                  *************                                
STATD    DSECT                     STATTAB MENUS                                
*                                  *************                                
STLENGTH DS    AL1                 L'FIXED+VARIABLE MENU PORTIONS               
STOPTVAL DS    CL2                 OPTION VALUES                                
STFULLCC DS    C                   FULLY MATCHED CC                             
STPARTCC DS    C                   PART MATCHED CC                              
STLOGICC DS    C                   LOGICALLY DELETED CC                         
STDELECC DS    C                   DELETED CC                                   
STFIXLNQ EQU   *-STATD                                                          
STVARIA  DS    0CL4                VARIABLE LENGTH PORTION STARTS HERE          
*                                                                               
*                                  ***********************************          
VARSTATD DSECT                     STATTAB MENU VARIABLE PORTION ENTRY          
*                                  ***********************************          
VARDISP  DS    AL2                 TEXT DISPLACEMENT INTO OUTLIST               
VARLEN   DS    AL1                 L'TEXT-1                                     
VARSUFF  DS    AL1                 A(SUFFIX CHARACTER)                          
VARSLNQ  EQU   *-VARSTATD          L'VARIABLE PORTION ENTRY                     
*                                                                               
         EJECT                                                                  
* ACGENBOTH                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
ACORDRD  DSECT                                                                  
ACORMEDC DS    C                   MEDIA CODE FROM FCRULMED                     
*                                                                               
* ACREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
* ACGENMODES                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
* DDEBLOCK                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDEBLOCK                                                       
         PRINT ON                                                               
* DDREPXTRAD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDREPXTRAD                                                     
         PRINT ON                                                               
* DDREPMASTD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDREPMASTD                                                     
         PRINT ON                                                               
* DDBIGBOX                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
* DDBOXEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDBOXEQUS                                                      
         PRINT ON                                                               
* DMDTFIS                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMDTFIS                                                        
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'015ACREPPD02 05/01/02'                                      
         END                                                                    
