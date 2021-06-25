*          DATA SET APGHFIDMB  AT LEVEL 037 AS OF 05/01/02                      
*PHASE ACHFDMBA                                                                 
*INCLUDE ADDAY                                                                  
         TITLE 'APG HOOK FOR DOREMUS BRANCH REPORT'                             
ACHFDMB  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACHK**,R9,R8,RR=R5                                           
         L     RA,0(R1)                                                         
         USING MAND,RA                                                          
         L     RC,HOOKAWRK                                                      
         USING ACWORKD,RC                                                       
         ST    R5,HKRELO                                                        
         CLI   ERRORFG,YES                                                      
         BE    XIT                                                              
         CLI   HOOKNUM,1           HOOKS BEFORE SORT                            
         BE    REQF00                                                           
         CLI   HOOKNUM,2           HOOK AFTER SORT                              
         BE    GETREC00                                                         
         BNE   XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*              REQUEST FIRST                                                    
*---------------------------------------------------------------------*         
REQF00   CLI   MODE,REQFRST                                                     
         BNE   PRAC00                                                           
         L     R1,=A(SDATAB)                                                    
         A     R1,HKRELO                                                        
         ST    R1,ASDATAB                                                       
         L     R1,=A(LEVNAMS)                                                   
         A     R1,HKRELO                                                        
         ST    R1,ALEVNMS                                                       
         L     R1,=A(OFCTAB)                                                    
         A     R1,HKRELO                                                        
         ST    R1,AOFCTAB                                                       
         L     R1,=A(COPYREC)                                                   
         A     R1,HKRELO                                                        
         ST    R1,ACOPYREC                                                      
                                                                                
         MVI   RPTOPT,RPTALL                                                    
         CLC   QSELECT(2),SPACES   REQUEST FOR ALL OFFICES                      
         BE    REQF02                                                           
         MVI   RPTOPT,RPTONE                                                    
         CLI   QSELECT+1,C' '      REQUEST FOR ONE OFFICE                       
         BE    REQF02                                                           
         MVI   RPTOPT,RPTGRP       GROUP REQUEST                                
         BAS   RE,SETSTAK                                                       
         TM    RPTOPT,RPTSUP       SET SUPERGROUP SELECTED ON                   
         BZ    REQF02                                                           
         BAS   RE,BLDOFTAB                                                      
                                                                                
REQF02   DS    0H                  SET ON WHICH OFFICES ARE INCLUDED            
         LA    R0,FIRSTOFF         IN LIMITED ACCESS OFFICE LIST (IF            
REQF03   STC   R0,OFFICE           ANY)                                         
         BAS   RE,GETOFF           GET THE OFFICE ENTRY                         
         LTR   RF,RF                                                            
         BZ    REQF04              NOT AN OFFICE                                
         NI    0(RF),OFEX          ASSUME EXCLUDE                               
         TM    0(RF),OFUNK         UNKNOWN OFFICE                               
         BO    REQF04                                                           
         TM    RPTOPT,RPTALL       IS IT A REQUEST FOR ALL OFFICES              
         BNO   REQF04              IF NOT                                       
         BAS   RE,LIMITOF          TEST LIMIT ACCESS                            
                                                                                
REQF04   AH    R0,=H'1'            BUMP TO NEXT OFFICE                          
         CH    R0,=H'250'                                                       
         BL    REQF03                                                           
                                                                                
         TM    RPTOPT,RPTGRP       GROUP OF OFFCIES                             
         BNO   REQF09                                                           
         CLC   QSELECT(2),=C'SS'                                                
         BNE   *+8                                                              
         OI    RPTOPT,RPTSRV       ALL SERVICE DEPARTMENTS                      
                                                                                
         LA    R5,1                ONLY GO THROUGH LOOP ONCE UNLESS             
         TM    RPTOPT,RPTSUP       IT IS SUPERLIST REQUEST                      
         BZ    REQF04A                                                          
         L     R3,ASUPLIST                                                      
         LH    R5,0(R3)            NUMBER OF PASSES TO MAKE                     
         LA    R3,2(R3)            POINT TO FIRST GROUP ENTRY                   
         LR    R4,R3               SAVE THIS ADDRESS FOR BCT                    
         MVC   *+8(2),0(R3)        POINT TO GROUP TABLE AND GO TO               
         LA    R3,0                POINT TO OFFICE LIST                         
         B     REQF07                                                           
                                                                                
REQF04A  LA    R3,GRPNTR                                                        
REQF05   CLC   0(2,R3),QSELECT     MATCH GROUP CODE TO TABLE                    
         BE    REQF06                                                           
         LA    R3,8(R3)            NEXT GROUP POINTER                           
         CLI   0(R3),X'FF'                                                      
         BNE   REQF05                                                           
         MVI   ERRORFG,YES                                                      
         USING BIGPRNTD,R4                                                      
         L     R4,VBIGPRNT                                                      
         MVC   XP(25),=CL25'INVALID GROUP CODE "  "'                            
         MVC   XP+20(2),QSELECT                                                 
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         DROP  R4                                                               
                                                                                
REQF06   DS    0H                                                               
         ICM   R4,15,4(R3)         SAVE ADDRESS OF RULE                         
         BZ    REQF07                                                           
         A     R4,HKRELO                                                        
         ST    R4,ARULE                                                         
*                                  BAL'S DON'T USE REG SAVE CHAIN AND           
REQF07   ST    R5,REGTEMP          ALTER SOME REGISTERS                         
         MVC   *+8(2),2(R3)        BASE AND DISPLACEMENT TO 'LA' INSTR.         
         LA    R3,0                R3 NOW HAS A(GROUP LIST)                     
         MVC   GPNAM,0(R3)         SAVE GROUP NAME                              
         LA    R3,36(R3)           R3 TO OFFICE LIST                            
                                                                                
REQF08   CLI   0(R3),0             END OF LIST                                  
         BE    REQF09A                                                          
         MVC   OFFICE,0(R3)                                                     
         BAS   RE,GETOFF           GET OFFICE ENTRY IN TABLE                    
         LTR   RF,RF                                                            
         BZ    *+8                 NOT AN OFFICE                                
         BAS   RE,LIMITOF          TEST LIMIT ACCESS                            
         BAS   RE,PERCTOF          TEST IF % OF OFFICE                          
         LA    R3,1(R3)            NEXT OFFICE FROM GROUP LIST                  
         B     REQF08                                                           
                                                                                
REQF09   TM    RPTOPT,RPTONE       ONE OFFICE REQUEST                           
         BNO   REQF10                                                           
         MVC   OFFICE,QSELECT      OFFICE CODE                                  
         BAS   RE,GETOFF                                                        
         LTR   RF,RF                                                            
         BZ    REQF10              NOT AN OFFICE                                
         BAS   RE,LIMITOF          TEST LIMIT ACCESS                            
         TM    OFSTAT,OFSRV                                                     
         BNO   *+8                                                              
         OI    RPTOPT,RPTSRV       ONE SERVICE DEPARTMENT                       
         B     REQF10                                                           
                                                                                
REQF09A  LA    R4,2(R4)                                                         
         LR    R3,R4                                                            
         L     R5,REGTEMP                                                       
         BCT   R5,REQF07                                                        
         EJECT                                                                  
*---------------------------------------------------------------------*         
*              GET PERIOD DATES AND NUMBER OF MONTHS                            
*---------------------------------------------------------------------*         
REQF10   GOTO1 DATCON,DMCB,(5,0),(1,WORK)                                       
         MVC   ENDDTE,WORK         REQUEST END (DEFAULT IS TODAY)               
         MVC   WORK(4),QEND                                                     
         MVC   WORK+4(2),=C'01'                                                 
         CLC   WORK(4),SPACES      UNLESS END IS SPECIFIED                      
         BE    REQF11                                                           
         GOTO1 DATCON,DMCB,(0,WORK),(1,WORK+6)                                  
         MVC   ENDDTE,WORK+6       REQUEST END (DEFAULT IS TODAY)               
                                                                                
REQF11   MVC   WORK(4),QSTART      GET START DATE                               
         MVC   WORK+4(2),=C'01'                                                 
                                                                                
*        BUILD TABLE OF DATES BEGINNING WITH ONE YEAR PRIOR TO                  
*        START DATE                                                             
                                                                                
         GOTO1 ADDAY,DMCB,(C'Y',WORK),WORK,F'-1'                                
         GOTO1 DATCON,DMCB,(0,WORK),(1,WORK+6)                                  
         LA    R3,PRIOR                                                         
         LA    R5,24                                                            
                                                                                
REQF13   MVC   0(2,R3),WORK+6      BUILD LIST OF YEAR                           
         LA    R3,2(R3)            NEXT YY/MM AREA                              
         GOTO1 ADDAY,DMCB,(C'M',WORK),WORK,F'1'                                 
         GOTO1 DATCON,DMCB,(0,WORK),(1,WORK+6)                                  
         BCT   R5,REQF13                                                        
                                                                                
         LA    R1,CURRENT          COUNT NUMBER OF MONTHS                       
         ZAP   MNTHINP,=P'1'       FROM QSTART TO QEND                          
         MVI   MNTHINB,1           IN PACKED AND BINARY                         
         LA    R0,1                                                             
                                                                                
REQF14   CLC   0(2,R1),ENDDTE                                                   
         BE    REQF15                                                           
         AP    MNTHINP,=P'1'                                                    
         AH    R0,=H'1'                                                         
         STC   R0,MNTHINB                                                       
         LA    R1,2(R1)                                                         
         B     REQF14                                                           
         EJECT                                                                  
*---------------------------------------------------------------------*         
*              BUILD TABLE OF NAMES FROM SUPELEDGER                             
*---------------------------------------------------------------------*         
REQF15   L     R3,ALEVNMS          A(NAME TABLE)                                
         LA    R0,MXNMS            TABLE MAXIMUM                                
         LA    R2,HKEY             FOR INTERMEDIATE NAMES                       
         USING ACKEYD,R2                                                        
         MVC   HKEY,SPACES                                                      
         MVC   ACKEYACC(1),RCSVCOMP                                             
         MVC   ACKEYACC+1(2),CONLEDG                                            
         BAS   RE,RDHIGH                                                        
         CLC   CONLEDG,ACKEYACC+1                                               
         BE    REQF17              FOUND LEDGER RECORD                          
         DC    H'0'                INVALID SUPERLEDGER                          
                                                                                
REQF17   BAS   RE,SEQ                                                           
         CLC   CONLEDG,ACKEYACC+1  IS THIS OUR SUPERLEDGER                      
         BNE   REQF20              END OF LEDGER                                
         CLI   ACKEYACC+4,C' '     HIGH LEVEL, SKIP IT                          
         BE    REQF17                                                           
         CLI   ACKEYACC+7,C' '                                                  
         BNE   REQF17              LOW (POSTING LEVEL), SKIP IT                 
         MVC   0(4,R3),ACKEYACC+4  SAVE ACCOUNT CODE                            
         MVC   4(36,R3),SPACES                                                  
         LR    R4,R2                                                            
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                NO NAME ELEMENT                              
                                                                                
         USING ACNAMED,R4                                                       
         SR    R1,R1                                                            
         IC    R1,ACNMLEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   4(0,R3),ACNMNAME    NAME TO TABLE                                
         LA    R3,40(R3)                                                        
         MVI   0(R3),X'FF'                                                      
         SH    R0,=H'1'            REDUCE COUNT                                 
         BNZ   REQF17                                                           
         DC    H'0'                THE LEVNAM TABLE IS FULL                     
         EJECT                                                                  
*---------------------------------------------------------------------*         
*              BUILD A TABLE OF SERVICE DEPARTMENT ALLOCATION PERCENTS          
*---------------------------------------------------------------------*         
REQF20   L     R3,ASDATAB          SERVICE DEPARTMENT ALLOCATION                
         MVI   0(R3),X'FF'         END OF TABLE                                 
         USING SDAD,R3                                                          
         LA    R0,MXSDA            MAXIMUM TABLE SIZE                           
         LA    R2,HKEY             FOR INTERMEDIATE NAMES                       
         USING ACKEYD,R2                                                        
         XC    HKEY,HKEY                                                        
         MVI   ACBTKTYP,X'1B'      BUILD A BUDGET KEY                           
         MVC   ACBTKACC,SPACES                                                  
         MVC   ACBTKWRK,SPACES                                                  
         MVC   ACBTKCON,SPACES                                                  
         MVC   ACBTKACC(1),QCOMPANY                                             
         MVC   ACBTKACC+1(2),CONLEDG                                            
         MVI   ACBTKBNO+1,2        BUDGET NUMBER IS HARD                        
         BAS   RE,RDHIGH                                                        
         CLC   ACBTKEY(4),HKSAVE                                                
         BE    *+6                                                              
         DC    H'0'                INVALID BUDGET KEY                           
         CLC   ACBTKBNO,HKSAVE+(ACBTKBNO-ACBTKEY)                               
         BE    REQF23                                                           
         DC    H'0'                INVALID BUDGET KEY                           
                                                                                
REQF22   BAS   RE,SEQ                                                           
         CLC   ACBTKEY(4),HKSAVE                                                
         BNE   REQF38              END OF SERVICE ALLOCATIONS BUDGETS           
         CLC   ACBTKBNO,HKSAVE+(ACBTKBNO-ACBTKEY)                               
         BNE   REQF22                                                           
                                                                                
REQF23   MVC   SDAPRI(SRBLNQ),ZEROS                                             
         MVC   SDACUR(SRBLNQ),ZEROS                                             
         MVI   BYTE,C'N'           ACTIVITY SWITCH                              
         MVI   ELCODE,X'1D'                                                     
         LA    R4,HKEY                                                          
         BAS   RE,GETEL                                                         
         BNE   REQF22                                                           
                                                                                
         USING ACBAD,R4                                                         
REQF24   CLC   ACBAMNTH,PRIOR                                                   
         BL    REQF30              BEFORE PRIOR YEAR START                      
         LA    R5,SDAPRI           R5 TO AMOUNT COLUMNS                         
         LA    R6,PRIOR            R6 TO DATE FIELDS                            
         LA    R1,24                                                            
                                                                                
REQF26   CLC   ACBAMNTH,0(R6)                                                   
         BE    REQF28              MATCH ON YY/MM                               
         LA    R6,2(R6)            NEXT YY/MM                                   
         LA    R5,8(R5)            NEXT ACCUMULATOR                             
         BCT   R1,REQF26                                                        
         B     REQF30                                                           
                                                                                
REQF28   ZAP   0(8,R5),ACBABUDG    BUDGET AMOUNT TO TABLE                       
         CP    0(8,R5),=P'0'                                                    
         BE    REQF30                                                           
         MVI   BYTE,C'Y'                                                        
                                                                                
REQF30   BAS   RE,NEXTEL           GET NEXT ELEMENT                             
         BE    REQF24                                                           
         CLI   BYTE,C'Y'           ANY ACTIVITY                                 
         BNE   REQF22              NO, OK TO READ NEXT                          
         MVC   SDAOFF(2),ACBTKACC+3 OFFICE /DEPARTMENT                          
         LA    R3,SDALNQ(R3)       R3 TO NEXT TABLE AREA                        
         MVI   0(R3),X'FF'         MARK NEW END                                 
         SH    R0,=H'1'            NUMBER REMAINING                             
         BNZ   REQF22                                                           
         DC    H'0'                SERVICE DEPARTMENT TABLE IS FULL             
         EJECT                                                                  
*---------------------------------------------------------------------*         
*              SEED THE FILE WITH ONE RECORD PER ACCOUNT                        
*              TO ENSURE WE PRINT ALL LINES.                                    
*---------------------------------------------------------------------*         
REQF38   DS    0H                                                               
         MVC   SRAMT(SRBLNQ),ZEROS INITIALIZE THE ACCUMS                        
         MVC   SRYTD(24),ZEROS                                                  
         LA    RF,*+10             SWITCH TO 31 BIT MODE                        
         O     RF,=X'80000000'                                                  
         BSM   0,RF                                                             
         L     R2,ASLP             A(SUPERLEDGER)                               
         LA    R3,RATTAB           RATIO LINE ENTRIES                           
                                                                                
         USING SLPD,R2                                                          
REQF40   MVC   POSTACC,SLPTACC     SAVE SUPERLEDGER ACCOUNT                     
         LA    RF,*+6              SWITCH TO 24 BIT MODE                        
         BSM   0,RF                                                             
                                                                                
REQF41   MVC   POSTYPE,POSTACC+6   CURRENT, BUDGET OR PRIOR                     
         CLI   POSTYPE,CURR        SKIP ALL OTHERS                              
         BL    REQF48                                                           
         CLI   POSTYPE,PRIO                                                     
         BH    REQF48                                                           
         MVI   OFFICE,FIRSTOFF     START WITH OFFICE A                          
                                                                                
                                                                                
REQF42   BAS   RE,GETOFF           GET THE OFFICE NAME                          
         TM    OFSTAT,OFIN         IS THIS OFFICE INCLUDED                      
         BNO   REQF46              NO, SKIP IT                                  
         TM    RPTOPT,RPTGRP       IS IT A GROUP REQUEST                        
         BNO   REQF43                                                           
         CLC   POSTACC+3(2),=C'2N' SKIP TOTAL OFFICE EXPENSE                    
         BL    REQF44                                                           
         CLC   POSTACC+3(2),=C'29' SKIP SERVICE DEPT. TOTALS                    
         BH    REQF44              KEEP ALL THE OTHERS                          
         B     REQF46                                                           
                                                                                
REQF43   DS    0H                                                               
         TM    OFSTAT,OFSRV        IS IT A SERVICE DEPARTMENT                   
         BNO   REQF44              FOR SERVICE DEPARTMNETS                      
         CLC   POSTACC+3(2),=C'2N' SKIP  SERV. DEPT. ACCOUNTS ETC.              
         BH    REQF46                                                           
                                                                                
REQF44   BAS   RE,NMES             FILL IN NAMES                                
         BAS   RE,PUTREC           PUT OUT RECORD FOR REPORT 1                  
                                                                                
REQF46   SR    R1,R1               INCREMENT OFFICE CODE                        
         IC    R1,OFFICE                                                        
         AH    R1,=H'1'                                                         
         STC   R1,OFFICE           SAVE CURRENT OFFICE CODE                     
         CLI   OFFICE,C'9'                                                      
         BNH   REQF42              HIGHEST OFFICE CODE                          
         CLC   POSTACC+3(2),=C'28' SKIP  SERV. DEPT. ACCOUNTS                   
         BE    REQF48              FOR COMPANY TOTAL                            
         TM    OFSTAT,OFIN         IS THIS OFFICE INCLUDED                      
         BNO   REQF48              NO, SKIP IT                                  
         BAS   RE,COMPTOT          PUT OUT COMPANY TOTAL RECORD                 
                                                                                
REQF48   LA    RF,*+10             SWITCH TO 31 BIT MODE                        
         O     RF,=X'80000000'                                                  
         BSM   0,RF                                                             
         CLI   0(R2),X'FF'         ALREADY AT END OF SUPERLEDGER                
         BE    REQF49                                                           
         LA    R2,SLPLNQ(R2)       NEXT SUPERLEDGER ENTRY                       
         CLI   0(R2),X'FF'                                                      
         BNE   REQF40                                                           
                                                                                
REQF49   LA    RF,*+6              SWITCH TO 24 BIT MODE                        
         BSM   0,RF                                                             
         CLI   0(R3),X'FF'         END OF OPERATING RATIO TABLE                 
         BE    XIT                                                              
         MVC   POSTACC+2(12),0(R3) OPERATING RATIO ACCOUNT                      
         LA    R3,12(R3)                                                        
         B     REQF41                                                           
         EJECT                                                                  
*---------------------------------------------------------------------*         
*              PROCESS ACCOUNT RECORD                                           
*---------------------------------------------------------------------*         
PRAC00   CLI   MODE,PROCACC                                                     
         BNE   SBAC00                                                           
         MVI   FCRDHIST,C'N'       TURN OFF BUCKET READING                      
         BAS   RE,FLTACC           FILTER ACCOUNTS/GET OFFICE CODE              
                                                                                
PRAC04   BAS   RE,GETOFF           GET OFFICE ENTRY                             
         TM    OFSTAT,OFIN         INCLUDE THIS OFFCIE                          
         BZ    XIT                 THIS WILL SKIP READING OF BUCKETS            
         TM    OFSTAT,OFSRV+OFIN   INCLUDE THIS OFFCIE                          
         BZ    XIT                 THIS WILL SKIP READING OF BUCKETS            
         MVI   FCRDHIST,C'Y'       TELL MONACC TO READ HISTORIES                
         B     XIT                                                              
                                                                                
         EJECT                                                                  
*---------------------------------------------------------------------*         
*              PROCESS SUB-ACCOUNT RECORDS                                      
*---------------------------------------------------------------------*         
SBAC00   CLI   MODE,SBACFRST       OLD APG                                      
         BE    *+8                                                              
         CLI   MODE,PROCSBAC       NEW APG                                      
         BNE   BUD                                                              
         CLI   BUCKTYPE,C' '       NOT MY TYPE                                  
         BH    XIT                                                              
         CLC   ACTACC+1(2),=C'1C'  IGNORE 1C                                    
         BNE   *+14                                                             
         CLC   CURRCON+1(3),=C'128' WITH CONTRA OF 128                          
         BE    XIT                                                              
         MVC   PRICOLS(SRBLNQ),ZEROS INITIALIZE THE ACCUMS                      
         MVC   CURCOLS(SRBLNQ),ZEROS                                            
         L     R4,ADSUBAC          GET THE HISTORY ELEMENTS                     
         MVI   BYTE,C'N'           ACTIVITY SWITCH                              
         MVI   ELCODE,X'45'                                                     
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         MVI   0(R4),0             MAKE MONACC THINK THERE ARE NONE             
                                                                                
         USING TRHISTD,R4                                                       
SBAC05   CLC   TRHSYEAR(2),PRIOR                                                
         BL    SBAC13              BEFORE PRIOR YEAR START                      
         CLC   TRHSYEAR(2),ENDDTE                                               
         BH    SBAC13              AFTER END DATE                               
         LA    R5,PRICOLS          R5 TO AMOUNT COLUMNS                         
         LA    R6,PRIOR            R6 TO DATE FIELDS                            
                                                                                
SBAC09   CLC   TRHSYEAR(2),0(R6)                                                
         BE    SBAC11              MATCH ON YY/MM                               
         LA    R6,2(R6)            NEXT YY/MM                                   
         LA    R5,8(R5)            NEXT ACCUMULATOR                             
         B     SBAC09                                                           
                                                                                
SBAC11   AP    0(8,R5),TRHSDR      ADD THE DEBITS                               
         SP    0(8,R5),TRHSCR      SUBTRACT THE CREDITS                         
         MVI   BYTE,C'Y'                                                        
                                                                                
SBAC13   BAS   RE,NEXTEL           GET NEXT ELEMENT                             
         BE    SBAC05                                                           
         CLI   BYTE,C'Y'           ANY ACTIVITY                                 
         BNE   XIT                 NO, OK TO EXIT                               
         MVI   POSTYPE,CURR                                                     
         BAS   RE,BLDREC           CURRENT ACCUMS TO MERGE                      
         MVI   POSTYPE,PRIO                                                     
         BAS   RE,BLDREC           PRIOR ACCUMS TO MERGE                        
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*              PROCESS BUDGETS RECORDS                                          
*---------------------------------------------------------------------*         
BUD      CLI   MODE,PROCBUD                                                     
         BNE   XIT                                                              
         BAS   RE,FLTACC           FILTER ACCOUNT                               
         CLC   ACTACC+1(2),=C'1C'  IGNORE 1C                                    
         BNE   *+14                                                             
         CLC   CURRCON+1(3),=C'128' WITH CONTRA OF 128                          
         BE    XIT                                                              
         BAS   RE,GETOFF           GET OFFICE CODE                              
         TM    OFSTAT,OFIN         SHOULD IT BE INCLUDED                        
         BZ    XIT                                                              
         TM    OFSTAT,OFSRV+OFIN   SHOULD IT BE INCLUDED                        
         BZ    XIT                                                              
                                                                                
         MVI   BYTE,C'N'           ACTIVITY SWITCH                              
         MVC   BUDCOLS(SRBLNQ),ZEROS                                            
         L     R4,APGIO                                                         
         LA    R4,ACCRFST-ACCRECD(R4)                                           
         MVI   ELCODE,X'1D'                                                     
         BAS   RE,FIRSTEL                                                       
         BNE   XIT                                                              
                                                                                
         USING ACBAD,R4                                                         
BUD06    CLC   ACBAMNTH,CURRENT                                                 
         BL    BUD12               BEFORE PRIOR YEAR START                      
         LA    R5,BUDCOLS          R5 TO AMOUNT COLUMNS                         
         LA    R6,CURRENT          R6 TO DATE FIELDS                            
         LA    R1,12                                                            
                                                                                
BUD08    CLC   ACBAMNTH,0(R6)                                                   
         BE    BUD10               MATCH ON YY/MM                               
         LA    R6,2(R6)            NEXT YY/MM                                   
         LA    R5,8(R5)            NEXT ACCUMULATOR                             
         BCT   R1,BUD08                                                         
         B     BUD12                                                            
                                                                                
BUD10    ZAP   0(8,R5),ACBABUDG    BUDGET AMOUNT TO TABLE                       
         MVI   BYTE,C'Y'                                                        
                                                                                
BUD12    BAS   RE,NEXTEL           GET NEXT ELEMENT                             
         BE    BUD06                                                            
         CLI   BYTE,C'Y'           ANY ACTIVITY                                 
         BNE   XIT                                                              
         MVI   POSTYPE,BUDG                                                     
         BAS   RE,BLDREC           BUDGET ACCUMS TO MERGE                       
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*              ROUTINE TO BUILD SORT RECORD FOR MERGER                          
*---------------------------------------------------------------------*         
BLDREC   NTR1                                                                   
         LA    R6,SRCHARG          SEARCH ARGUMENT FROM BASE                    
         USING ARGD,R6                                                          
         ICM   R2,15,ARGRTN                                                     
         USING SLPD,R2                                                          
         ST    R2,POSTLST          A(FIRST ITEM IN LIST)                        
         MVI   POSTCNT,1           NUMBER OF ITEMS                              
         LA    RF,*+10             SWITCH TO 31 BIT MODE                        
         O     RF,=X'80000000'                                                  
         BSM   0,RF                                                             
         L     R5,SLPLNK                                                        
         USING LNKD,R5                                                          
         LTR   R5,R5                                                            
         BZ    BLDR03                                                           
         L     R3,LNKNUM           NUMBER OF ENTRIES IN THIS LIST               
         LA    R3,1(R3)            COUNT TOTAL NUMBER OF POSTINGS               
         STC   R3,POSTCNT                                                       
         BCTR  R3,0                                                             
         SLL   R3,2                NUMBER OF CHILDREN X 4                       
         BCTR  R3,0                LESS ONE                                     
         EX    R3,*+8                                                           
         B     BLDR03                                                           
         MVC   POSTLST+4(0),LNKCHLD ADD CHILDREN TO LIST                        
                                                                                
BLDR03   LA    R5,POSTLST          R5 TO LIST OF POSTING ENTRIES                
         SR    R0,R0                                                            
         IC    R0,POSTCNT          R0=NUMBER OF ENTRIES                         
                                                                                
BLDR04   LA    RF,*+10             SWITCH TO 31 BIT MODE                        
         O     RF,=X'80000000'                                                  
         BSM   0,RF                                                             
         L     R2,0(R5)            R2 TO SUPERLEDGER ENTRY                      
         MVI   CURRSIGN,C'+'       ASSUME POSTIVE                               
         TM    0(R5),X'80'         IS IT A NEGATIVE                             
         BNO   *+8                                                              
         MVI   CURRSIGN,C'-'       MAKE IT A SUBTRACT                           
                                                                                
         CLC   SLPTACC+6(1),POSTYPE                                             
         BNE   BLDR35                                                           
         LA    RF,*+6              SWITCH TO 24 BIT MODE                        
         BSM   0,RF                                                             
         LA    R4,CURCOLS          R4 TO BUCKETS FOR THIS ACCOUNT               
         CLI   POSTYPE,CURR        1 FOR CURRENT                                
         BE    BLDR07                                                           
         LA    R4,BUDCOLS                                                       
         CLI   POSTYPE,BUDG        2 FOR BUDGET                                 
         BE    BLDR07                                                           
         LA    R4,PRICOLS                                                       
         CLI   POSTYPE,PRIO        3 FOR PRIOR                                  
         BE    BLDR07                                                           
         DC    H'0'                BAD TYPE                                     
                                                                                
BLDR07   MVC   SRAMT(SRBLNQ),0(R4) 12 MONTH ACCUMS                              
         CLC   SRAMT(SRBLNQ),ZEROS                                              
         BE    XIT                                                              
                                                                                
         TM    OFSTAT,OFPCT        ONLY TAKE A % OF LINE                        
         BZ    *+8                                                              
         BAS   RE,SETRULE                                                       
                                                                                
         CLI   CURRSIGN,C'-'       REVERSE THE SIGN                             
         BNE   BLDR08                                                           
         LA    R1,SRAMT                                                         
         LA    RF,12                                                            
         MP    0(8,R1),=P'-1'                                                   
         LA    R1,8(R1)                                                         
         BCT   RF,*-10                                                          
                                                                                
BLDR08   CLC   ACTACC+1(2),=C'12'  REVERSE SIGN FOR UL 12                       
         BNE   BLDR09                                                           
         LA    R1,SRAMT                                                         
         LA    RF,12                                                            
         MP    0(8,R1),=P'-1'                                                   
         LA    R1,8(R1)                                                         
         BCT   RF,*-10                                                          
                                                                                
BLDR09   BAS   RE,CALAN            CALCULATE YTD/ANNUAL                         
                                                                                
BLDR12   LA    RF,*+10             SWITCH TO 31 BIT MODE                        
         O     RF,=X'80000000'                                                  
         BSM   0,RF                                                             
         MVC   POSTACC,SLPTACC     SAVE SUPERLEDGER ACCOUNT CODE                
         LA    RF,*+6              SWITCH TO 24 BIT MODE                        
         BSM   0,RF                                                             
         BAS   RE,NMES             FILL IN NAMES                                
         TM    RPTOPT,RPTGRP       IS IT A GROUP REQUEST                        
         BNO   BLDR13                                                           
         CLC   POSTACC+3(2),=C'2N' SKIP TOTAL OFFICE EXPENSE                    
         BL    BLDR20                                                           
         CLC   POSTACC+3(2),=C'29' SKIP SERVICE DEPT. TOTALS                    
         BH    BLDR20              KEEP ALL THE OTHERS                          
         B     BLDR35                                                           
                                                                                
BLDR13   TM    OFSTAT,OFSRV        IS THIS A SERVICE DEPARTMENT                 
         BNO   BLDR20                                                           
         CLC   POSTACC+3(2),=C'2N' LINES UP TO TOTAL OFFICE EXPENSE             
         BNH   BLDR20              ARE HANDLED NORMALLY                         
         CLC   POSTACC+3(2),=C'33' NO OPERATING EXPSENSE                        
         BNE   BLDR17              FOR SERV. DEPT                               
         BAS   RE,COMPTOT          BUT ADD TO COMPANY RECORD                    
         B     BLDR35                                                           
                                                                                
BLDR17   CLC   POSTACC+3(2),=C'41' OPERATING INCOME - FOR SERVICE DEPT.         
         BNE   BLDR35                                                           
         BAS   RE,COMPTOT          ADD FOR COMPANY TOTAL                        
         BAS   RE,ALLSRV           ALLOCATE SERVICE DEPARTMENT                  
         B     BLDR35                                                           
                                                                                
BLDR20   TM    OFSTAT,OFIN         IS OFFICE INCLUDED FOR PRINTING              
         BNO   BLDR35              MUST BE FOR ALLOCATION - SKIP IT             
         BAS   RE,PUTREC           PUT OUT RECORD FOR REPORT 1                  
         BAS   RE,COMPTOT          RECORD FOR COMPANY TOTALS                    
                                                                                
BLDR35   LA    R5,4(R5)            R5 TO NEXT POINTER                           
         BCT   R0,BLDR04                                                        
         LA    RF,*+6              SWITCH TO 24 BIT MODE                        
         BSM   0,RF                                                             
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*              SET PERCENT OF OFFICE RULES                                      
*---------------------------------------------------------------------*         
SETRULE  NTR1                                                                   
         L     RE,ARULE            GET OFFICE AND ITS RULES                     
SETRULE1 CLI   0(RE),0             END OF TABLE                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   OFFICE,0(RE)                                                     
         BE    SETRULE3                                                         
         LA    RE,RULELNQ(RE)                                                   
         B     SETRULE1                                                         
                                                                                
SETRULE3 ZAP   SMDUB,1(3,RE)       ZAP IN CURRENT YEAR %                        
         CLI   POSTYPE,CURR        1 FOR CURRENT                                
         BE    SETRULE6                                                         
         ZAP   SMDUB,4(3,RE)       ZAP IN PRIOR YEAR %                          
         CLI   POSTYPE,BUDG        2 FOR BUDGET                                 
         BE    SETRULE6                                                         
         ZAP   SMDUB,7(3,RE)       ZAP IN BUDGET YEAR %                         
         CLI   POSTYPE,PRIO        3 FOR PRIOR                                  
         BE    SETRULE6                                                         
         DC    H'0'                                                             
                                                                                
SETRULE6 LA    RE,SRAMT            POINT TO MONTHLY AMOUNTS                     
         LA    RF,12                                                            
SETRULE8 ZAP   DDUB,0(8,RE)        ZAP IN AMOUNT INTO PL16                      
         MP    DDUB,SMDUB          MULTIPLY BY PERCENT                          
         SRP   DDUB,61,5           RE-ADJUST FOR DECIMALS                       
         ZAP   0(8,RE),DDUB        PUT BACK NEW AMOUNT                          
         LA    RE,8(RE)                                                         
         BCT   RF,SETRULE8         NEXT ONE                                     
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*              ALLOCATE SERVICE DEPARTMENT OPERATING INCOME                     
*---------------------------------------------------------------------*         
ALLSRV   NTR1                                                                   
         CLI   POSTYPE,BUDG        IS IT BUDGET - OPERATING INCOME              
         BE    XIT                 DROPPED FOR SERVICE DEPARTMENTS              
         MVC   SRVDPT,OFFICE       SAVE THE SERVICE DEPARTMENT CODE             
         MVC   SRVAMT(SRBLNQ),SRAMT AND ACCUMS                                  
         L     R3,ASDATAB          SER. DEPT. ALLOCATION TABLE                  
         USING SDAD,R3                                                          
                                                                                
ALLSRV1  CLI   0(R3),X'FF'                                                      
         BE    ALLSRV10                                                         
         CLC   SDADPT,SRVDPT       MATCH SERVICE DEPARTMENT                     
         BNE   ALLSRV9                                                          
         LA    R4,SDAPRI           TO PRIOR YEAR PERCENTS                       
         CLI   POSTYPE,PRIO                                                     
         BE    *+8                                                              
         LA    R4,SDACUR           OR CURRENT                                   
         LA    R1,SRVAMT           R1 TO SAVED AMOUNTS                          
         LA    R2,SRAMT            R2 TO OUTPUT RECORD AMOUNTS                  
         LA    RF,12                                                            
                                                                                
ALLSRV3  ZAP   WORK(16),0(8,R1)    AMOUNT                                       
         MP    WORK(16),0(8,R4)    X PERCENT                                    
         SRP   WORK(16),64-6,5     ROUNDED                                      
         ZAP   0(8,R2),WORK(16)    ALLOCATED AMOUNT                             
         LA    R1,8(R1)                                                         
         LA    R2,8(R2)                                                         
         LA    R4,8(R4)                                                         
         BCT   RF,ALLSRV3                                                       
                                                                                
         BAS   RE,CALAN            YTD/ANNUAL                                   
                                                                                
         MVC   OFFICE,SDAOFF       OFFICE CODE                                  
         BAS   RE,GETOFF           GET OFFICE ENTRY                             
         TM    OFSTAT,OFIN         IS OFFICE INCLUDED                           
         BNO   ALLSRV9             SKIP IT                                      
         BAS   RE,NMES             FILL IN NAMES                                
         BAS   RE,PUTREC           OPERATING INC. FOR NEW OFFICE(141X)          
         LA    R0,14                                                            
         LA    R2,SRAMT                                                         
         MP    0(8,R2),=P'-1'      REVERSE SIGN                                 
         LA    R2,8(R2)                                                         
         BCT   R0,*-10                                                          
                                                                                
         MVC   POSTACC+3(3),=C'33 ' OPERATING EXPENSE LINE                      
         BAS   RE,NMES                                                          
         BAS   RE,PUTREC           RECORD FOR (133X1 OR 133X3)                  
                                                                                
         MVC   POSTACC+3(3),=C'291' SERVICE DEPT. TOTAL                         
         BAS   RE,NMES                                                          
         BAS   RE,PUTREC           RECORD FOR (12911 OR 12913)                  
                                                                                
         MVC   POSTACC+3(3),=C'28 ' SERVICE DEPT. ACCOUNT LINE                  
         LA    R2,SRVTAB            GET SUPERLEDGER CODE FROM TABLE             
                                                                                
ALLSRV5  CLC   SRVDPT,0(R2)        MATCH SERVICE DEPT CODE                      
         BE    ALLSRV7                                                          
         LA    R2,2(R2)                                                         
         CLI   0(R2),X'FF'                                                      
         BNE   ALLSRV5                                                          
         DC    H'0'                ERROR IN SERVICE DEPT. TABLE                 
                                                                                
ALLSRV7  MVC   POSTACC+5(1),1(R2)  SUPERLEDGER CODE FOR SERV. DEPT.             
         BAS   RE,NMES                                                          
         BAS   RE,PUTREC                                                        
                                                                                
ALLSRV9  LA    R3,SDALNQ(R3)                                                    
         B     ALLSRV1                                                          
ALLSRV10 MVC   OFFICE,SRVDPT       RESTORE SERVICE DEPT CODE                    
         BAS   RE,GETOFF                                                        
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*              CALCULATE YTD AND ANNUALIZED                                     
*---------------------------------------------------------------------*         
CALAN    ZAP   SRYTD,=P'0'         CLEAR YTD                                    
         ZAP   SRANN,=P'0'               ANNUAL                                 
         ZAP   SRTRG,=P'0'               MONTHLY TO ACHIEVE                     
                                                                                
         LA    R1,SRAMT            GET YTD FOR ACTUAL/BUDGET/PRIOR              
         SR    RF,RF                                                            
         IC    RF,MNTHINB          NUMBERS OF MONTHS                            
         AP    SRYTD,0(8,R1)       ADD TO YEAR-TO-DATE                          
         LA    R1,8(R1)                                                         
         BCT   RF,*-10                                                          
                                                                                
         CLI   POSTYPE,CURR                                                     
         BNE   CALAN3                                                           
         ZAP   WORK(16),SRYTD      GET ANNUALIZED FOR CURRENT                   
         MP    WORK(16),=P'100'    YTD                                          
         DP    WORK(16),MNTHINP    /MONTHS                                      
         MP    WORK(14),=PL2'12'    X 12                                        
         SRP   WORK(14),64-2,5     ROUNDED                                      
         ZAP   SRANN,WORK(14)                                                   
         BR    RE                                                               
                                                                                
CALAN3   LA    R1,SRAMT            BUDGET AND PRIOR                             
         LA    RF,12               ADD ALL 12 MONTHS                            
         AP    SRANN,0(8,R1)       ANNUALIZED                                   
         LA    R1,8(R1)                                                         
         BCT   RF,*-10                                                          
         BR    RE                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
*              FILTER ACCOUNTS - GET OFFICE CODE                                
*---------------------------------------------------------------------*         
FLTACC   CLC   ACTACC+1(2),=C'SE'                                               
         BNE   FLTACC1                                                          
         CLC   ACTACC+3(2),=C'11'   IGNORE SE11                                 
         BE    XIT                                                              
         MVC   OFFICE,ACTACC+9      OFFICE CODE                                 
         BR    RE                                                               
                                                                                
FLTACC1  CLC   ACTACC+1(2),=C'1C'  GET OFFICE FOR 1C ACCOUNTS                   
         BNE   FLTACC2                                                          
         MVC   OFFICE,ACTACC+3                                                  
         BR    RE                                                               
                                                                                
FLTACC2  CLC   ACTACC+1(2),=C'12'  FOR U/L 12                                   
         BNE   XIT                                                              
         CLI   CURRFLT,C' '                                                     
         BE    XIT                                                              
         MVC   OFFICE,CURRFLT      OFFICE IS IN FILTER 1                        
         BR    RE                                                               
                                                                                
*---------------------------------------------------------------------*         
*        GET OFFICE ENTRY FROM OFFICE TABLE                                     
*---------------------------------------------------------------------*         
GETOFF   MVI   OFSTAT,0            DEFAULT IS TO EXCLUDE                        
         SR    RF,RF                                                            
         SR    R1,R1                                                            
         IC    R1,OFFICE                                                        
         SH    R1,=Y(FIRSTOFF)     A = ZERO (THE FIRST ENTRY)                   
         LA    R1,OFPNTR(R1)       R1 TO OFFCIE CODE ENTRY IN TABLE             
         CLI   0(R1),0                                                          
         BER   RE                  INVALID CHARACTER - SKIP IT                  
         IC    RF,0(R1)            RF TO RELATIVE NUMBER IN TABLE               
         BCTR  RF,0                                                             
         MH    RF,=H'37'           X LENGTH OF TABLE ENTRY                      
         LA    RF,OFTAB(RF)        RF TO THIS OFFICE ENTRY                      
         MVC   OFSTAT,0(RF)        SAVE STATUS                                  
         MVC   OFNAM,1(RF)         AND NAME                                     
         BR    RE                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
*              TEST REQUESTED ON LIMITED ACCESS ID.                             
*---------------------------------------------------------------------*         
LIMITOF  EQU   *                                                                
         L     R5,ADOFFLST         GET ADDRESSABLITY TO OFFICE LIST             
         CLI   0(R5),0                                                          
         BE    LIMITOF9            UNLIMITED ACCESS                             
         LA    R1,32                                                            
LIMITOF3 EQU   *                                                                
         CLC   0(1,R5),OFFICE                                                   
         BE    LIMITOF9            VALID OFFICE                                 
         LA    R5,1(R5)                                                         
         CLI   0(R5),0             END OF LIST                                  
         BER   RE                  NOT AUTHORIZED OFFICE                        
         BCT   R1,LIMITOF3                                                      
         BR    RE                  NOT IN OFFICE LIST SO IGNORE                 
LIMITOF9 EQU   *                                                                
         OI    0(RF),OFIN          TURN ON INCLUDE BIT IN OFTAB                 
         BR    RE                                                               
                                                                                
PERCTOF  CLI   QOPT3,C'O'          IS IT % BY OFFICE?                           
         BNER  RE                                                               
         ICM   R5,15,ARULE         IS THERE A RULE FOR SELECT TYPE              
         BZR   RE                                                               
PERCTOF3 CLI   0(R5),0             END OF TABLE                                 
         BER   RE                                                               
         CLC   OFFICE,0(R5)        MATCH ON OFFICE                              
         BE    PERCTOF9                                                         
         LA    R5,RULELNQ(R5)                                                   
         B     PERCTOF3                                                         
PERCTOF9 OI    0(RF),OFPCT         TURN ON % OF OFFICE                          
         BR    RE                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
*              ADD COMPANY RECORD TO SORT FILE                                  
*---------------------------------------------------------------------*         
COMPTOT  NTR1                                                                   
         TM    RPTOPT,RPTALL       IS IT A REQUEST FOR ALL OFFICES              
         BNO   XIT                 IF NOT, SKIP TOTAL RECORD                    
         CLC   POSTACC+3(2),=C'2N' TOTAL OFFICE EXPENSE                         
         BE    XIT                 SKIP FOR COMPANY RECORD                      
         LA    RF,6                                                             
         LA    R1,SRROW1                                                        
         MVI   0(R1),2             SET FOR REPORT 2                             
         LA    R1,16(R1)                                                        
         BCT   RF,*-8                                                           
                                                                                
         MVI   SRACC1,X'90'        ADD COMPANY TOTALS                           
         MVC   SRNAM1(36),=CL36'COMPANY TOTALS'                                 
         BAS   RE,PUTREC                                                        
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*              FILL IN NAME AND PUT TO SORT                                     
*---------------------------------------------------------------------*         
NMES     NTR1  0H                                                               
         LA    RE,5                5 ROWS                                       
         LA    R1,SRROW1                                                        
         MVC   0(16,R1),ROWINIT    INITIALIZE ROWS                              
         LA    R1,16(R1)                                                        
         BCT   RE,*-10                                                          
         MVC   SRROW6(4),=X'01010000'                                           
         MVC   SRACC1(1),OFFICE    OFFICE CODE                                  
         MVC   SRNAM1,OFNAM        OFFICE NAME                                  
         TM    RPTOPT,RPTGRP       IS IT A GROUP REQUEST                        
         BNO   NMES01                                                           
         MVI   SRACC1,X'40'        ADD ALL OFFICES TOGETHER                     
         MVC   SRNAM1,GPNAM        USE GROUP NAME                               
                                                                                
NMES01   MVC   SRACC2(1),POSTACC+3                                              
         MVC   SRACC3(1),POSTACC+4                                              
         MVC   SRACC4(1),POSTACC+5                                              
         MVC   SRACC5(1),POSTACC+6                                              
         LA    R1,=CL15'CURRENT YEAR'                                           
         CLI   SRACC5,CURR                                                      
         BE    NMES04                                                           
         LA    R1,=CL15'BUDGET'                                                 
         CLI   SRACC5,BUDG                                                      
         BE    NMES04                                                           
         LA    R1,=CL15'PRIOR YEAR'                                             
                                                                                
NMES04   MVC   SRNAM5(15),0(R1)   NAME IS CURRENT, BUDGET OR PRIOR              
         L     R1,ALEVNMS          GET NAMES FOR LEVEL 2/3/4                    
         LA    R4,SRNAM2                                                        
         LA    R3,POSTACC+3                                                     
         SR    RF,RF                                                            
                                                                                
NMES06   EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   POSTACC+3(0),0(R1)  MATCH ON 1/2/3 BYTES OF SUPERLEDGER          
         BE    *+12                                                             
         LA    R1,L'LEVNAMS(R1)                                                 
         B     NMES06                                                           
         MVC   0(36,R4),4(R1)      NAME TO SORT RECORD                          
         LA    R4,L'SRNAM1(R4)     R4 TO NEXT LEVEL                             
         LA    RF,1(RF)            INCREASE COMPARE FOR NEXT                    
         CH    RF,=H'2'            ALREAY MATCHED 3 LEVELS                      
         BNH   NMES06                                                           
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*              ROUTINE TO PUT RECORDS TO MERGER                                 
*---------------------------------------------------------------------*         
PUTREC   NTR1                                                                   
         TM    RPTOPT,RPTSUP                                                    
         BZ    PUTREC05                                                         
         BAS   RE,PUTSUPS                                                       
         B     PUTREC08                                                         
                                                                                
PUTREC05 OI    SRTSW,SRTAC                                                      
         GOTO1 MERGER,DMCB,=C'PUT',SREC,ADSORTER                                
PUTREC08 TM    UPSI,TRCMRGE        TRACE PUTS TO MERGE                          
         BNO   XIT                                                              
         CP    TRCMCNT,=PL2'300'   ALREADY PRINTED  THE MAX                     
         BH    XIT                                                              
         USING BOXD,RF                                                          
         L     RF,ADBXAREA                                                      
         MVC   BOXWIDTH,=F'132'                                                 
         MVI   RCSUBPRG,2          TURN OFF BOXES                               
         MVC   P+1(13),=C'MODE=SBACFRST'                                        
         CLI   MODE,SBACFRST                                                    
         BE    PUTREC10                                                         
         MVC   P+1(13),=C'MODE=PROCSBAC'                                        
         CLI   MODE,PROCSBAC       NEW APG                                      
         BE    PUTREC10                                                         
         MVC   P+6(8),=C'PROCBUD '                                              
PUTREC10 MVC   P+16(8),=C'ACCOUNT='                                             
         MVC   P+24(14),ACTACC+1                                                
         MVC   P+40(7),=C'CONTRA='                                              
         MVC   P+47(14),CURRCON+1                                               
         MVC   P+63(8),=C'POSTING='                                             
         MVC   P+71(12),POSTACC+2                                               
         MVC   P+86(7),=C'OFFICE='                                              
         MVC   P+96(1),OFFICE                                                   
         GOTO1 PRINT,DMCB,P,=C'BL01'                                            
                                                                                
         LA    R0,SRLNQ                                                         
         LA    R4,SREC                                                          
         TM    RPTOPT,RPTSUP                                                    
         BZ    *+8                                                              
         L     R4,ACOPYREC                                                      
         GOTO1 PRNTBL,DMCB,0,(R4),C'DUMP',(R0),=C'2D',(C'P',PRINT)              
         AP    TRCMCNT,=P'1'                                                    
         L     RF,ADBXAREA                                                      
         MVC   BOXWIDTH,=F'198'                                                 
         MVI   RCSUBPRG,0                                                       
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*              NOW WE'RE GETTING RECORDS BACK FROM SORTER                       
*              TIME TO CALCULATE - MONTHLY TO ACHIEVE TARGET                    
*              AND OPERATING RATIOS                                             
*---------------------------------------------------------------------*         
GETREC00 DS    0H                                                               
         LA    R2,SREC             MOVE RECORD TO WORKING STORAGE               
         L     R4,HOOKAREC         FROM SORT AREA                               
         LA    R3,SRLNQ                                                         
         LR    R5,R3                                                            
         MVCL  R2,R4                                                            
*                                  BUILD A COMPRESSED KEY                       
         LA    R0,5                                                             
         LA    R1,CMPKEY           COMPRESSED KEY                               
         LA    R2,SRACC1           KEY FIELDS                                   
         MVC   0(1,R1),0(R2)                                                    
         LA    R1,1(R1)                                                         
         LA    R2,16(R2)                                                        
         BCT   R0,*-14                                                          
                                                                                
                                                                                
         CLC   CMPKEY+1(3),=C'51X' IS THIS OPERATING RATIO RECORD               
         BNE   GETR20              IF NOT - MAY NEED TO SAVE DATA               
                                                                                
         CLC   LSTEXP(1),CMPKEY    IS SAVED EXPENSE SAME OFFICE                 
         BNE   GETNO                                                            
         CLC   LSTREV(1),CMPKEY    IS SAVED REVENUE SAME OFFICE                 
         BNE   GETNO                                                            
         LA    R1,LSTEXPC          R1=CURRENT EXPENSE                           
         LA    R2,LSTREVC          R2=CURRENT REVENUE                           
         CLI   CMPKEY+4,CURR                                                    
         BE    GETR07                                                           
         LA    R1,LSTEXPB          R1=BUDGET EXPENSE                            
         LA    R2,LSTREVB          R2=BUDGET REVENUE                            
         CLI   CMPKEY+4,BUDG                                                    
         BE    GETR07                                                           
         LA    R1,LSTEXPP          R1=PRIOR EXPENSE                             
         LA    R2,LSTREVP          R2=PRIOR REVENUE                             
                                                                                
GETR07   LA    R0,14               COMPUTE RATIO FOR 14 COLUMNS                 
         LA    R3,SRAMT            R3=OPERATING RATIO RECORD                    
                                                                                
GETR09   ZAP   0(8,R3),=P'0'       INITIALIZE RATIO                             
         CP    0(8,R1),=P'100'     IF NO EXPENSE                                
         BL    GETR11                                                           
         CP    0(8,R2),=P'100'     OR NO REVENUS                                
         BL    GETR11              DON'T PUT RATIO                              
         ZAP   WORK(16),0(8,R1)    EXPENSE                                      
         MP    WORK(16),=P'10000'                                               
         DP    WORK(16),0(8,R2)    DIVIDED BY REVENUE                           
         CP    WORK(8),=P'10000000' SKIP RATIOS OVER 100,000.00                 
         BH    GETR11              NO NEGATIVES                                 
         ZAP   0(8,R3),WORK(8)     SAVE IN OUTPUT RECORD                        
                                                                                
GETR11   LA    R1,8(R1)                                                         
         LA    R2,8(R2)                                                         
         LA    R3,8(R3)                                                         
         BCT   R0,GETR09                                                        
         B     GETYES                                                           
         EJECT                                                                  
**             COMPUTE MONTHLY TO ACHIEVE BUDGET                                
                                                                                
GETR20   CLI   CMPKEY+4,BUDG       IS CURRENT RECORD A BUDGET                   
         BNE   GETR30              NO COMPUTATION                               
         CLC   CMPKEY(4),LSTCUR    IS IT SAME ACCOUNT AS LAST CURRENT           
         BNE   GETR30                                                           
         ZAP   WORK(16),SRANN      ANNUALIZED BUDGET                            
         SP    WORK(16),CURYTD     LESS CURRENT YTD                             
         ZAP   HALF,=P'12'                                                      
         SP    HALF,MNTHINP        GET MONTHS REMAINING                         
         BZ    GETR30                                                           
         DP    WORK(16),HALF       MONTHLY TO ACHIEVE                           
         ZAP   SRTRG,WORK(14)                                                   
         B     GETR33                                                           
                                                                                
*              SAVE SOME DATA FOR FUTURE COMPUTATIONS                           
                                                                                
GETR30   CLI   CMPKEY+4,CURR                                                    
         BNE   GETR33                                                           
         MVC   LSTCUR,CMPKEY       SAVE CURRENT KEY                             
         ZAP   CURYTD,SRYTD        AND CURRENT YTD                              
                                                                                
GETR33   CLC   CMPKEY+1(3),=C'33X' IS IT OPERATING EXPENSE                      
         BNE   GETR35                                                           
         MVC   LSTEXP,CMPKEY       SAVE EXPENSE KEY                             
         LA    R1,LSTEXPC          R1=CURRENT EXPENSE                           
         B     GETR37                                                           
                                                                                
GETR35   CLC   CMPKEY+1(3),=C'112' IS IT REVENUE                                
         BNE   GETR40                                                           
         MVC   LSTREV,CMPKEY       SAVE REVENUE KEY                             
         LA    R1,LSTREVC          R1=CURRENT REVENUE                           
                                                                                
GETR37   CLI   CMPKEY+4,CURR                                                    
         BE    GETR39                                                           
         LA    R1,LSTLNQ(R1)       R1=BUDGET EXPENSE                            
         CLI   CMPKEY+4,BUDG                                                    
         BE    GETR39                                                           
         LA    R1,LSTLNQ(R1)       R1=PRIOR EXPENSE                             
                                                                                
GETR39   MVC   0(LSTLNQ,R1),SRAMT  SAVE 14 COLUMNS                              
                                                                                
GETR40   DS    0H                                                               
                                                                                
GETYES   LA    R2,SREC             MOVE RECORD BACK                             
         L     R4,HOOKAREC         TO  SORT AREA                                
         LA    R3,SRLNQ                                                         
         LR    R5,R3                                                            
         MVCL  R4,R2                                                            
         SR    R0,R0               TELL CONTROLLER TO PROCESS                   
         B     *+8                                                              
                                                                                
GETNO    LA    R0,1                TELL CONTROLLER TO SKIP IT                   
         LTR   R0,R0                                                            
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*              DATA MANAGER SUPPORT                                             
*---------------------------------------------------------------------*         
RDHIGH   MVC   COMMAND,DMRDHI                                                   
         MVC   HKSAVE,HKEY                                                      
         B     DMALL                                                            
                                                                                
SEQ      MVC   COMMAND,DMRSEQ                                                   
         B     DMALL                                                            
                                                                                
         DC    F'0'                                                             
DMALL    ST    RE,DMALL-4                                                       
         GOTO1 DATAMGR,DMCB,COMMAND,=C'ACCOUNT',HKEY,HKEY,0                     
         L     RE,DMALL-4                                                       
         BR    RE                                                               
                                                                                
         GETEL (R4),DATADISP,ELCODE                                             
                                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        SET STACK FOR NUMBER OF REPORTS NEEDED                                 
*---------------------------------------------------------------------*         
SETSTAK  NTR1                                                                   
         LA    R3,SUPERGR             DOES SELECT CODE INDICATE A               
SETS10   CLI   0(R3),X'FF'            SUPERGROUP SELECTION                      
         BE    SETSXIT                                                          
         CLC   QSELECT(2),0(R3)                                                 
         BE    SETS50                                                           
         LA    R3,SUPERLN(R3)                                                   
         B     SETS10                                                           
                                                                                
SETS50   DS    0H                                                               
         OI    RPTOPT,RPTSUP          SET SUPERGROUP SELECTED ON                
         MVC   *+8(2),2(R3)           BASE AND DISP TO 'LA' INSTR.              
         LA    R4,0                   R4 NOW HAS SUPERGROUP LIST                
         ST    R4,ASUPLIST                                                      
         SR    R0,R0                                                            
         LH    R0,0(R4)               NUMBER OF OFFICES IN SUPERGROUP           
         ST    R0,REPSTKOF            STORE FOR MY PROGRAM                      
         MH    R0,=H'2'                                                         
         ST    R0,NRSTACK             STORE FOR CONTROLLER                      
         L     R0,REPSTKOF            SET FOR BCT LOOP                          
         L     R4,ARSTACK             ADDRESS OF REPORT STACK                   
SETS60   LA    R5,RPTLN               LENGTH OF STACK                           
         MH    R5,=H'2'                                                         
         AR    R4,R5                  NEXT AREA FOR RSTACK                      
         LR    R3,R5                  REPEAT LENGTH FOR MOVE                    
         L     R2,ARSTACK                                                       
         MVCL  R4,R2                  REPEAT STACK                              
         BCT   R0,SETS60                                                        
         OI    RQSW,RQSTK             TELL CONTROLLER TO LEAVE STACK            
                                                                                
SETSXIT  B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        SET TABLE OF OFFICES WITH GROUP CODES                                  
*---------------------------------------------------------------------*         
BLDOFTAB NTR1                                                                   
         MVI   BYTE,1              REPORT COUNTER                               
                                                                                
         USING OFTABD,R3                                                        
         L     R3,AOFCTAB          OFFICE REPORT TABLE                          
         L     R4,ASUPLIST         ADDRESS OF THIS SUPERLIST SELECTION          
         LA    R4,2(R4)            POINT PAST NUMBER OF GROUPS                  
                                                                                
BLDO100  DS    0H                                                               
         LA    R0,1                DUPLICATE THIS OFFICE LIST GROUP             
BLDO110  MVC   *+8(2),0(R4)        IN OFFICE TABLE                              
         LA    R2,0                R2 NOW HAS GROUP LIST                        
         MVC   GRPTEMP,0(R2)                                                    
         MVC   ARULE,4(R2)                                                      
         MVC   *+8(2),2(R2)        IN OFFICE TABLE                              
         LA    R2,0                R2 NOW HAS GROUP LIST                        
         MVC   GPNAM,0(R2)         OFFICE GROUP NAME                            
         LA    R2,36(R2)           POINT PAST NAME                              
BLDO115  CLI   0(R2),0             END OF THIS OFFICE LIST                      
         BE    BLDO150                                                          
         MVC   OFRPTNM,BYTE        THIS REPORT NUMBER                           
         MVC   OFNUM,0(R2)         THIS OFFICE                                  
         MVC   OFGRPNAM,GPNAM      OFFICE GROUP NAME                            
         MVC   OFGRULE,ARULE                                                    
         MVC   OFGRPCOD,GRPTEMP                                                 
         LA    R2,2(R2)                                                         
         LA    R3,OFENTRY(R3)                                                   
         MVI   0(R3),X'FF'         MARK NEW END OF TABLE                        
         B     BLDO115                                                          
                                                                                
BLDO150  DS    0H                                                               
         ZIC   R1,BYTE                                                          
         LA    R1,1(R1)                                                         
         STC   R1,BYTE                                                          
         BCT   R0,BLDO110          REPEAT THIS TABLE FOR REPORT TWO             
                                                                                
         XC    ARULE,ARULE                                                      
         LA    R4,2(R4)                                                         
         CLI   0(R4),X'FF'         END OF SUPER GROUP LIST                      
         BNE   BLDO100                                                          
                                                                                
BLDOFXIT DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        PUT OUT THIS RECORD FOR EACH OFFICE GROUP IT IS IN                     
*---------------------------------------------------------------------*         
PUTSUPS  NTR1                                                                   
         USING OFTABD,R3                                                        
         L     R3,AOFCTAB                                                       
         LA    R4,SREC                                                          
PUTS100  CLC   OFFICE,OFNUM                                                     
         BNE   PUTS300                                                          
         MVC   BYTE,OFRPTNM                                                     
         MVC   SRNAM1,OFGRPNAM     USE GROUP NAME                               
         ST    R3,REGTEMP                                                       
         BAS   RE,COPYPUT                                                       
PUTS300  LA    R3,OFENTRY(R3)                                                   
         CLI   0(R3),X'FF'                                                      
         BNE   PUTS100                                                          
PUTSXIT  B     XIT                                                              
                                                                                
*---------------------------------------------------------------------*         
*        COPY SORTREC, ADJUST REPORT NUMBER AND PUT TO SORTER                   
*---------------------------------------------------------------------*         
COPYPUT  NTR1                                                                   
         LA    R2,SREC                                                          
         L     R4,ACOPYREC                                                      
         LA    R3,SRLNQ                                                         
         LR    R5,R3                                                            
         MVCL  R4,R2                                                            
                                                                                
         CLI   QOPT3,C'O'          IS IT % BY OFFICE?                           
         BNE   COP090                                                           
         L     R3,REGTEMP                                                       
         OC    OFGRULE,OFGRULE                                                  
         BZ    COP090                                                           
         L     RE,OFGRULE                                                       
         A     RE,HKRELO                                                        
COP010   CLI   0(RE),0             END OF TABLE                                 
         BE    COP090                                                           
         CLC   OFFICE,0(RE)                                                     
         BE    COP020                                                           
         LA    RE,RULELNQ(RE)                                                   
         B     COP010                                                           
                                                                                
COP020   ZAP   SMDUB,1(3,RE)       ZAP IN CURRENT YEAR %                        
         CLI   POSTYPE,CURR        1 FOR CURRENT                                
         BE    COP030                                                           
         ZAP   SMDUB,4(3,RE)       ZAP IN PRIOR YEAR %                          
         CLI   POSTYPE,BUDG        2 FOR BUDGET                                 
         BE    COP030                                                           
         ZAP   SMDUB,7(3,RE)       ZAP IN BUDGET YEAR %                         
         CLI   POSTYPE,PRIO        3 FOR PRIOR                                  
         BE    COP030                                                           
         DC    H'0'                                                             
                                                                                
         USING SRECD,R4                                                         
COP030   L     R4,ACOPYREC         POINT TO MONTHLY AMOUNTS                     
         LA    RE,SRAMTD                                                        
         LA    RF,12                                                            
COP040   ZAP   DDUB,0(8,RE)        ZAP IN AMOUNT INTO PL16                      
         MP    DDUB,SMDUB          MULTIPLY BY PERCENT                          
         SRP   DDUB,61,5           RE-ADJUST FOR DECIMALS                       
         ZAP   0(8,RE),DDUB        PUT BACK NEW AMOUNT                          
         LA    RE,8(RE)                                                         
         BCT   RF,COP040           NEXT ONE                                     
                                                                                
         USING SRECD,R4                                                         
COP090   L     R4,ACOPYREC                                                      
         LA    RF,6                                                             
         LA    R1,SRROW1D                                                       
COP100   CLI   0(R1),0                                                          
         BE    *+10                                                             
         MVC   0(1,R1),BYTE          SET FOR REPORT 2                           
         LA    R1,16(R1)                                                        
         BCT   RF,COP100                                                        
                                                                                
         OI    SRTSW,SRTAC                                                      
         L     R4,ACOPYREC                                                      
         GOTO1 MERGER,DMCB,=C'PUT',(R4),ADSORTER                                
COPYXIT  B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        DATA CONSTANTS AND WORKING STORAGE                                     
*---------------------------------------------------------------------*         
RPTOPT   DS    CL1                 REPORT OPTION                                
RPTALL   EQU   X'80'               INCLUDE ALL OFFICES                          
RPTGRP   EQU   X'40'               GROUP REQUEST                                
RPTONE   EQU   X'20'               ONE OFFICE                                   
RPTSUP   EQU   X'10'               SUPER LIST REQUEST                           
RPTSRV   EQU   X'01'               SERVICE DEPARTMENT REQUEST                   
*  OFFICE STATUS EQUATES                                                        
OFSTAT   DS    CL1                                                              
                                                                                
OFALL    EQU   X'FF'                                                            
OFIN     EQU   X'80'               INCLUDE IN THIS REQUEST                      
OFSRV    EQU   X'40'               SERVICE DEPARTMENT                           
OFUNK    EQU   X'08'               UNKNOWN                                      
OFPCT    EQU   X'04'               USE % OF $ FOR OFFICE                        
OFEX     EQU   OFALL-OFIN          EXCLUDE                                      
                                                                                
REPSTKOF DS    F                                                                
ASUPLIST DS    F                                                                
REGTEMP  DS    F                                                                
GRPTEMP  DS    CL2                                                              
                                                                                
OFNAM    DS    CL36                OFFICE NAME                                  
GPNAM    DS    CL36                GROUP NAME (IF GROUP REQUEST)                
                                                                                
PRIOR    DS    12CL2               YYMM(PACKED)                                 
CURRENT  DS    12CL2               YYMM(PACKED)                                 
ENDDTE   DS    CL2                 LAST YYMM FOR CURRENT                        
MNTHINP  DS    PL2                 NUMBER OF MONTHS REQUEST RANGE               
MNTHINB  DS    CL1                 NUMBER OF MONTHS REQUEST RANGE               
HKRELO   DS    F                   HOOK RELO                                    
ALEVNMS  DS    A                   A(INTERMEDIATE LEVEL NAMES)                  
ASDATAB  DS    A                   A(SERVICE DEPARTMENT ALLOCATIONS)            
AOFCTAB  DS    A                   A(OFFICE TABLE/REPORT NUMBERS)               
ACOPYREC DS    A                   A(AREA FOR DUPLICATING SORT RECORDS)         
ARULE    DS    A                   A(RULE TO APPLY TO SELECT GROUP)             
COMMAND  DS    CL6                                                              
ELCODE   DS    CL1                                                              
ROWINIT  DC    X'0101',CL14' '     TO INITIALIZE ROWS                           
ZEROS    DC    12PL8'0'                                                         
PRICOLS  DS    12PL8               AMOUNTS FOR PRIOR                            
CURCOLS  DS    12PL8                           CURRENT                          
BUDCOLS  DS    12PL8                           BUDGET                           
OFFICE   DS    CL1                                                              
POSTYPE  DS    CL1                 POSTING TYPE                                 
POSTACC  DS    CL15                POSTING (SUPERLEDGER) ACCOUNT                
CURR     EQU   C'1'                CURRENT                                      
BUDG     EQU   C'2'                BUDGET                                       
PRIO     EQU   C'3'                PRIOR                                        
POSTCNT  DS    XL1                 NUMBER OF POSTING ACCOUNTS                   
POSTLST  DS    30F                 A(POSTING ACCOUNTS)                          
SRVDPT   DS    CL1                 SERVICE DEPT. CODE                           
SRVAMT   DS    12PL8               SERVICE DEPARTMENT AMOUNTS                   
SMDUB    DS    PL3                 SMALL DUB                                    
DDUB     DS    PL16                DOUBLE DUB                                   
TRCMCNT  DC    PL2'0'                                                           
         EJECT                                                                  
CMPKEY   DS    CL5                 COMPOSITE KEY FROM SORT RECORD               
LSTEXP   DS    CL5                 LAST OPERATING EXPENSE LINE KEY              
LSTREV   DS    CL5                 LAST OPERATING INCOME LINE KEY               
LSTCUR   DS    CL5                 LAST CURRENT(ANY LINE) KEY                   
                                                                                
LSTEXPC  DS    14PL8               LAST OPER. EXP. LINE (CURRENT)               
LSTLNQ   EQU   *-LSTEXPC                                                        
LSTEXPB  DS    14PL8               LAST OPER. EXP. LINE (BUDGET)                
LSTEXPP  DS    14PL8               LAST OPER. EXP. LINE (PRIOR)                 
                                                                                
LSTREVC  DS    14PL8               LAST REVENUE LINE (CURRENT)                  
LSTREVB  DS    14PL8               LAST REVENUE LINE (BUDGET)                   
LSTREVP  DS    14PL8               LAST REVENUE LINE (PRIOR)                    
                                                                                
CURYTD   DS    PL8                 LAST CURRENT YTD                             
                                                                                
RATTAB   DS    0CL12               OPERATING RATIO LINES                        
         DC    CL12'151X1'         CURRENT                                      
         DC    CL12'151X2'         BUDGET                                       
         DC    CL12'151X3'         PRIOR                                        
         DC    X'FF'                                                            
         EJECT                                                                  
*              OFFICE NUMBER EQUATES                                            
                                                                                
FIRSTOFF EQU   C'#'                FIRST OFFICE IN TRANSLATE TABLE              
                                                                                
OFA      EQU   1                                                                
OFB      EQU   2                                                                
OFC      EQU   3                                                                
OFD      EQU   4                                                                
OFE      EQU   5                                                                
OFF      EQU   6                                                                
OFG      EQU   7                                                                
OFH      EQU   8                                                                
OFI      EQU   9                                                                
OFJ      EQU   10                                                               
OFK      EQU   11                                                               
OFL      EQU   12                                                               
OFM      EQU   13                                                               
OFN      EQU   14                                                               
OFO      EQU   15                                                               
OFP      EQU   16                                                               
OFQ      EQU   17                                                               
OFR      EQU   18                                                               
OFS      EQU   19                                                               
OFT      EQU   20                                                               
OFU      EQU   21                                                               
OFV      EQU   22                                                               
OFW      EQU   23                                                               
OFX      EQU   24                                                               
OFY      EQU   25                                                               
OFZ      EQU   26                                                               
OF0      EQU   27                                                               
OF1      EQU   28                                                               
OF2      EQU   29                                                               
OF3      EQU   30                                                               
OF4      EQU   31                                                               
OF5      EQU   32                                                               
OF6      EQU   33                                                               
OF7      EQU   34                                                               
OF8      EQU   35                                                               
OF9      EQU   36                                                               
OF#      EQU   37                                                               
                                                                                
*              OFFICE POINTERS                                                  
                                                                                
OFPNTR   DC    AL1(OF#),(C'A'-C'#'-1)X'00'                                      
         DC    AL1(OFA,OFB,OFC,OFD,OFE,OFF,OFG,OFH,OFI),7X'00'                  
         DC    AL1(OFJ,OFK,OFL,OFM,OFN,OFO,OFP,OFQ,OFR),8X'00'                  
         DC    AL1(OFS,OFT,OFU,OFV,OFW,OFX,OFY,OFZ),6X'00'                      
         DC    AL1(OF0,OF1,OF2,OF3,OF4,OF5,OF6,OF7,OF8,OF9),6X'00'              
         EJECT                                                                  
                                                                                
*               OFFICE STATUS/ NAME TABLE                                       
OFTAB    DS    0CL37                                                            
OFANM    DC    AL1(0),CL36'HEADQUARTERS'                                        
OFBNM    DC    AL1(0),CL36'MIS SERVICES'                                        
OFCNM    DC    AL1(0),CL36'STRATEGIC PLANNING'                                  
OFDNM    DC    AL1(0),CL36'GRAPHICS'                                            
OFENM    DC    AL1(0),CL36'ATLANTA'                                             
OFFNM    DC    AL1(0),CL36'CREATIVE/PRODUCTION'                                 
OFGNM    DC    AL1(0),CL36'MEDIA'                                               
OFHNM    DC    AL1(OFUNK),CL36'UNKNOWN'                                         
OFINM    DC    AL1(0),CL36'ACCT/FIN'                                            
OFJNM    DC    AL1(0),CL36'JAPAN'                                               
OFKNM    DC    AL1(0),CL36'DISTRIB'                                             
OFLNM    DC    AL1(OFUNK),CL36'UNKNOWN'                                         
OFMNM    DC    AL1(0),CL36'OFF, SVCE.'                                          
OFNNM    DC    AL1(0),CL36'PERSONNEL'                                           
OFONM    DC    AL1(0),CL36'DOREMUS FINANCIAL PRINTING'                          
OFPNM    DC    AL1(OFUNK),CL36'UNKNOWN'                                         
OFQNM    DC    AL1(0),CL36'DIGITAL IMAGINING'                                   
OFRNM    DC    AL1(0),CL36'DOR-S.F.'                                            
OFSNM    DC    AL1(OFUNK),CL36'UNKNOWN'                                         
OFTNM    DC    AL1(0),CL36'MEDIA BUYING SERVICE'                                
OFUNM    DC    AL1(0),CL36'TYPOGRAPHY'                                          
OFVNM    DC    AL1(0),CL36'MILAN'                                               
OFWNM    DC    AL1(0),CL36'THE COLLIGAN GROUP'                                  
OFXNM    DC    AL1(0),CL36'HONG KONG'                                           
OFYNM    DC    AL1(OFUNK),CL36'UNKNOWN'                                         
OFZNM    DC    AL1(0),CL36'DOR-K.C.'                                            
OF0NM    DC    AL1(OFUNK),CL36'UNKNOWN'                                         
OF1NM    DC    AL1(0),CL36'FINL'                                                
OF2NM    DC    AL1(0),CL36'GENL'                                                
OF3NM    DC    AL1(0),CL36'L.A.'                                                
OF4NM    DC    AL1(0),CL36'D.R.'                                                
OF5NM    DC    AL1(0),CL36'SPEC. ADV.'                                          
OF6NM    DC    AL1(0),CL36'CHIC.'                                               
OF7NM    DC    AL1(0),CL36'BRAZIL'                                              
OF8NM    DC    AL1(0),CL36'LONDON'                                              
OF9NM    DC    AL1(0),CL36'FINANCIAL CORPORATE COMMUNICATIONS'                  
OF#NM    DC    AL1(OFUNK),CL36'UNKNOWN'                                         
         DC    X'FF'                                                            
         EJECT                                                                  
                                                                                
SUPERLN  EQU   4                   LENGTH OF ONE TABLE ENTRY                    
SUPERGR  DS    0A                  SUPER GROUP CODE POINTERS                    
         DC    C'AX',S(SUPERAX)                                                 
         DC    C'BX',S(SUPERBX)                                                 
         DC    C'CX',S(SUPERCX)                                                 
         DC    C'DX',S(SUPERDX)                                                 
         DC    X'FF'                                                            
                                                                                
SUPERLST DS    0A                  SUPER OFFICE LIST CODES                      
SUPERAX  DC    AL2(2),S(SGDC),S(SGDD),X'FF'                                     
         DS    0A                  SUPER OFFICE LIST CODES                      
SUPERBX  DC    AL2(1),S(SGLC),X'FF'                                             
         DS    0A                  SUPER OFFICE LIST CODES                      
SUPERCX  DC    AL2(4),S(SGFA),S(SGCS),S(SGDO),S(SGDD),X'FF'                     
         DS    0A                  SUPER OFFICE LIST CODES                      
SUPERDX  DC    AL2(4),S(SGFA),S(SGCS),S(SGDO),S(SGDD),X'FF'                     
                                                                                
GRPNTR   DS    0A                  GROUP CODE POINTERS                          
SGCH     DC    C'CH',S(GRPCH),A(0)                                              
SGCS     DC    C'CS',S(GRPCS),A(0)                                              
SGDC     DC    C'DC',S(GRPDC),A(0)                                              
SGDD     DC    C'DD',S(GRPDD),A(0)                                              
SGDO     DC    C'DO',S(GRPDO),A(RULEDO)                                         
SGDR     DC    C'DR',S(GRPDR),A(0)                                              
SGFA     DC    C'FA',S(GRPFA),A(RULEFA)                                         
SGGE     DC    C'GE',S(GRPGE),A(0)                                              
SGIN     DC    C'IN',S(GRPIN),A(0)                                              
SGLC     DC    C'LC',S(GRPLC),A(RULELC)                                         
SGMA     DC    C'MA',S(GRPMA),A(0)                                              
SGMN     DC    C'MN',S(GRPMN),A(0)                                              
SGME     DC    C'ME',S(GRPME),A(0)                                              
         DC    X'FF'                                                            
                                                                                
*              GROUP NAMES / OFFICE LIST                                        
*                                                                               
GRPCH    DC    CL36'CONSUMER HEALTHWORKS'                                       
         DC    C'4',X'0000'                                                     
*                                                                               
GRPCS    DC    CL36'OVERHEAD OFFICES'                                           
         DC    C'A,I,K,Z',X'0000'                                               
                                                                                
GRPDC    DC    CL36'DOREMUS DOMESTIC WITHOUT DFP'                               
         DC    C'A,I,J,K,N,R,T,U,W,Z,1,3,5,6,9',X'0000'                         
                                                                                
GRPDD    DC    CL36'DOMESTIC OFFICES'                                           
         DC    C'A,I,J,K,N,O,R,T,U,W,Z,1,3,5,6,9',X'0000'                       
                                                                                
GRPDO    DC    CL36'REGIONAL OFFICES'                                           
         DC    C'J,R,3,6',X'0000'                                               
                                                                                
GRPDR    DC    CL36'DOMESTIC WITHOUT SAN FRAN'                                  
         DC    C'A,I,J,K,N,O,T,U,W,Z,1,3,5,6,9',X'0000'                         
                                                                                
GRPFA    DC    CL36'FINANCIAL OFFICES'                                          
         DC    C'O,T,U,1,5',X'0000'                                             
                                                                                
GRPGE    DC    CL36'DOREMUS CORPORATE'                                          
         DC    C'N,W,9',X'0000'                                                 
                                                                                
GRPIN    DC    CL36'INTERNATIONAL OFFICES'                                      
         DC    C'V,X,7,8',X'0000'                                               
                                                                                
GRPLC    DC    CL36'MERKLEY OFFICES'                                            
         DC    C'B,C,D,E,F,G,H,M,P,Q,S,Y,2,4',X'0000'                           
                                                                                
GRPMA    DC    CL36'MERKLEY NEWMAN HARTY - ATLANTA'                             
         DC    C'E',X'0000'                                                     
                                                                                
GRPMN    DC    CL36'MERKLEY NEWMAN HARTY - NEW YORK'                            
         DC    C'B,C,D,F,G,H,M,P,Q,S,Y,2',X'0000'                               
                                                                                
GRPME    DC    CL36'MERCEDES'                                                   
         DC    C'M',X'0000'                                                     
                                                                                
*              CL1 = OFFICE                                                     
*              PL3 = % OF ACTUAL YEAR     TO ONE DECIMAL XXX.X                  
*              PL3 = % OF BUDGET                                                
*              PL3 = % OF PRIOR  YEAR                                           
                                                                                
RULEDO   DC    C'U',PL3'0',PL3'0',PL3'20'                                       
RULELNQ  EQU   *-RULEDO                                                         
         DC    AL1(0)                                                           
                                                                                
RULEFA   DC    C'U',PL3'1000',PL3'1000',PL3'850'                                
         DC    C'Q',PL3'0',PL3'0',PL3'250'                                      
         DC    AL1(0)                                                           
                                                                                
RULELC   DC    C'U',PL3'0',PL3'0',PL3'130'                                      
         DC    C'Q',PL3'1000',PL3'1000',PL3'750'                                
         DC    AL1(0)                                                           
         EJECT                                                                  
                                                                                
*              SERVICE DEPARTMENT CONVERSION TABLE                              
*              BYTE 1  SERVICE DEPARTMENT CODE                                  
*              BYTE 2  SUPERLEDGER ACCOUNT CODE                                 
                                                                                
SRVTAB   DC    CL2'D3'             GRAPHICS                                     
         DC    CL2'E4'             FINANCIAL                                    
         DC    CL2'F1'             CREATIVE                                     
         DC    CL2'G2'             MEDIA                                        
         DC    CL2'Q1'             ALSO CREATIVE                                
         DC    X'FF'                                                            
         EJECT                                                                  
                                                                                
*        SORT RECORD                                                            
SREC     DS    0CL(SRLNQ)                                                       
SRBGN    DS    0C                                                               
SRROW1   DS    CL2                 REPORT NUMBER/COPY                           
SRACC1   DS    CL14                ROW 1 OFFICE CODE                            
SRROW2   DS    CL2                 REPORT NUMBER/COPY                           
SRACC2   DS    CL14                ACCOUNT LEVEL 1                              
SRROW3   DS    CL2                 REPORT NUMBER/COPY                           
SRACC3   DS    CL14                ACCOUNT LEVEL 2                              
SRROW4   DS    CL2                 REPORT NUMBER/COPY                           
SRACC4   DS    CL14                ACCOUNT LEVEL 3                              
SRROW5   DS    CL2                 REPORT NUMBER/COPY                           
SRACC5   DS    CL14                ACCOUNT LEVEL 4                              
SRROW6   DS    CL2                 REPORT NUMBER/COPY                           
SRBINZ   DS    XL2                 BINARY ZERO                                  
SRNAM1   DS    CL36                OFFICE CODE NAME                             
SRNAM2   DS    CL36                ROW 2 NAME                                   
SRNAM3   DS    CL36                ROW 3 NAME                                   
SRNAM4   DS    CL36                ROW 4 NAME                                   
SRNAM5   DS    CL36                ROW 5 ACCOUNT NAME                           
SRNLNQ   EQU   *-SRBGN                                                          
SRAMT    DS    12PL8               MONTH BUCKETS                                
SRBLNQ   EQU   *-SRAMT                                                          
SRYTD    DS    PL8                 YTD                                          
SRANN    DS    PL8                 ANNUALIZED                                   
SRTRG    DS    PL8                 MONTHLY TARGET                               
SRLNQ    EQU   *-SRBGN                                                          
                                                                                
HKSAVE   DS    CL42                HOOK SAVE KEY                                
HKEY     DS    CL42                HOOK KEY                                     
HKIO     DS    CL2000              HOOK IO                                      
         EJECT                                                                  
         LTORG                                                                  
ERRORFG  DC    C'N'                                                             
         EJECT                                                                  
MXNMS    EQU   200                                                              
         DS    0D                                                               
LEVNAMS  DS    (MXNMS)CL40        INTERMEDIATE LEVEL NAMES                      
                                                                                
MXSDA    EQU   50                                                               
SDATAB   DS    (MXSDA)CL(SDALNQ)  SERVICE DEPARTMENT ALLOCATION TABLE           
                                                                                
                                                                                
OFCTAB   DS    200CL(OFENTRY)                                                   
         DS    CL1                END OF TABLE                                  
COPYREC  DS    CL(SRLNQ)                                                        
                                                                                
OFTABD   DSECT                                                                  
OFRPTNM  DS    CL1                                                              
OFGRPCOD DS    CL2                                                              
OFNUM    DS    CL1                                                              
OFGRPNAM DS    CL36                                                             
OFGRULE  DS    CL4                                                              
OFENTRY  EQU   *-OFTABD                                                         
                                                                                
SRECD    DSECT                                                                  
SRBGND   DS    0C                                                               
SRROW1D  DS    CL2                 REPORT NUMBER/COPY                           
SRACC1D  DS    CL14                ROW 1 OFFICE CODE                            
SRROW2D  DS    CL2                 REPORT NUMBER/COPY                           
SRACC2D  DS    CL14                ACCOUNT LEVEL 1                              
SRROW3D  DS    CL2                 REPORT NUMBER/COPY                           
SRACC3D  DS    CL14                ACCOUNT LEVEL 2                              
SRROW4D  DS    CL2                 REPORT NUMBER/COPY                           
SRACC4D  DS    CL14                ACCOUNT LEVEL 3                              
SRROW5D  DS    CL2                 REPORT NUMBER/COPY                           
SRACC5D  DS    CL14                ACCOUNT LEVEL 4                              
SRROW6D  DS    CL2                 REPORT NUMBER/COPY                           
SRBINZD  DS    XL2                 BINARY ZERO                                  
SRNAM1D  DS    CL36                OFFICE CODE NAME                             
SRNAM2D  DS    CL36                ROW 2 NAME                                   
SRNAM3D  DS    CL36                ROW 3 NAME                                   
SRNAM4D  DS    CL36                ROW 4 NAME                                   
SRNAM5D  DS    CL36                ROW 5 ACCOUNT NAME                           
SRNLNQD  EQU   *-SRBGND                                                         
SRAMTD   DS    12PL8               MONTH BUCKETS                                
SRBLNQD  EQU   *-SRAMTD                                                         
SRYTDD   DS    PL8                 YTD                                          
SRANND   DS    PL8                 ANNUALIZED                                   
SRTRGD   DS    PL8                 MONTHLY TARGET                               
SRLNQD   EQU   *-SRBGND                                                         
                                                                                
         EJECT                                                                  
*              DSECT TO COVER SERVICE DEPARTMENT ALLOCATION                     
                                                                                
SDAD     DSECT                                                                  
SDAOFF   DS    CL1                 OFFICE CODE                                  
SDADPT   DS    CL1                 DEPARTMENT CODE                              
SDAPRI   DS    12PL8               ALLOCATION PERCENTS (PRIOR MONTHS)           
SDACUR   DS    12PL8               ALLOCATION PERCENTS (CURRENT MONTHS)         
SDALNQ   EQU   *-SDAD                                                           
                                                                                
         EJECT                                                                  
         IHAASCB                                                                
         IHASDWA                                                                
*        ACAPGGEND                                                              
*        ACGENBOTH                                                              
         PRINT OFF                                                              
       ++INCLUDE ACAPGGEND                                                      
       ++INCLUDE ACGENBOTH                                                      
         PRINT   ON                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'037APGHFIDMB 05/01/02'                                      
         END                                                                    
