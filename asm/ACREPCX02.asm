*          DATA SET ACREPCX02  AT LEVEL 005 AS OF 05/01/02                      
*PHASE ACCX02A,*                                                                
*INCLUDE SORTER                                                                 
*INCLUDE UNDERLIN                                                               
         TITLE 'ACCX02 - COKE EXPENDITURE REPORT'                               
ACCX02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,ACCX02,R9                                                      
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACCXD,RC                                                         
         L     R8,VBIGPRNT                                                      
         USING BIGPRNTD,R8                                                      
         ST    R9,BASE2                                                         
         EJECT                                                                  
         CLI   MODE,RUNFRST                                                     
         BNE   CX20                                                             
         L     RF,=A(SAVERC)                                                    
         ST    RC,0(RF)            SAVE REG C                                   
         L     RF,VEXTRAS                                                       
         USING RUNXTRAD,RF                                                      
         L     RF,ADMASTD                                                       
         USING MASTD,RF                                                         
         MVC   ADBOX,MCBXAREA      STORE ADDR OF BOX ROUTINE                    
         L     RF,=A(HOOK)                                                      
         ST    RF,HEADHOOK                                                      
         SPACE 1                                                                
         GOTO1 BUFFALO,DMCB,=C'SET',ADBUFC                                      
         SPACE 1                                                                
         GOTO1 BLDTAB,DMCB,(0,AAREATAB),NAREAS,=C'3A' AREA/REGION/DIST          
         BAS   RE,BLDPROD          BUILD TABLE OF PRODUCTS                      
         B     XIT                                                              
         EJECT                                                                  
CX20     CLI   MODE,REQFRST                                                     
         BNE   CX50                                                             
         MVC   AGYFLT,QSRTAREA     AGENCY FILTER                                
         MVC   MEDFLT,QSRTAREA+3   MEDIA FILTER                                 
         MVC   BUDFLT,QSRTAREA+5   BUDGET CODE FILTER                           
         CLC   MEDFLT,SPACES                                                    
         BE    CX24                NO MEDIA FILTER                              
         LA    R1,MEDTAB           CONVERT COKE MEDIA TO DDS MEDIA              
         LA    R0,NMED                                                          
CX21     CLC   MEDFLT,0(R1)        MATCH ON COKE CODE                           
         BE    CX22                                                             
         CLC   MEDFLT,2(R1)        OR TRY TO MATCH ON DDS CODE                  
         BE    CX22                                                             
         LA    R1,L'MEDTAB(R1)                                                  
         BCT   R0,CX21                                                          
         DC    H'0'                UNKNOWN MEDIA CODE                           
CX22     MVC   MEDFLT,2(R1)        USE DDS MEDIA CODE                           
         SPACE 1                                                                
CX24     GOTO1 SORTER,DMCB,SORTCARD,RECCARD,(40,ASORTC)                         
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,0                                                       
         MVC   PAGE,=H'1'                                                       
         MVI   STATUS,0                                                         
         GOTO1 DATCON,DMCB,(5,0),(1,WORK)                                       
         MVC   YEAR,WORK           DEFAULT YEAR IS RUN YEAR                     
         CLC   QSTART,SPACES                                                    
         BE    CX25                                                             
         GOTO1 DATCON,DMCB,(0,QSTART),(1,STRT1)                                 
         GOTO1 (RF),(R1),,(2,STRT2)                                             
         GOTO1 (RF),(R1),,(8,STRT8)                                             
         GOTO1 (RF),(R1),(0,QEND),(1,END1)                                      
         GOTO1 (RF),(R1),,(2,END2)                                              
         GOTO1 (RF),(R1),,(8,END8)                                              
         MVC   YEAR,STRT1                                                       
*                                                                               
CX25     CLC   QSELECT,SPACES                                                   
         BE    CX30                                                             
         MVC   WORK(2),QSELECT+3                                                
         MVC   WORK+2(4),=C'0101'                                               
         GOTO1 DATCON,DMCB,(0,WORK),(1,WORK+6)                                  
         MVC   YEAR,WORK+6                                                      
*                                                                               
CX30     CLC   QACCOUNT,SPACES     IF FILTERING ON BOTTLER                      
         BE    *+8                                                              
         OI    STATUS,ACCOUNT      SET STATUS BIT FOR PRINTING                  
         CLC   QFILTER1(6),SPACES  IF FILTERING USING FILTERS                   
         BE    *+8                                                              
         OI    STATUS,FILTER       SET STATUS BIT                               
         TM    STATUS,ACCOUNT+FILTER IF FILTERING GET BUDGETS LATER             
         BNZ   *+8                                                              
         BAS   RE,GETBUD           WRITE BUDGETS TO SORTER                      
         B     XIT                                                              
         EJECT                                                                  
         USING RECD,R3                                                          
         USING ACNAMED,R4                                                       
CX50     CLI   MODE,PROCACC                                                     
         BNE   CX75                                                             
         MVI   FCRDTRNS,C'Y'                                                    
         CLC   AGYFLT,SPACES                                                    
         BE    CX53                NOT FILTERING BY AGENCY                      
         L     R2,ADACC                                                         
         CLC   8(3,R2),AGYFLT                                                   
         BE    CX53                                                             
         MVI   FCRDTRNS,C'N'       DON'T NEED TRANSACTIONS                      
         B     XIT                                                              
         SPACE 1                                                                
CX53     TM    STATUS,ACCOUNT+FILTER IF FILTERING GET BUDGETS NOW               
         BZ    *+8                                                              
         BAS   RE,GETBUD           WRITE BUDGETS TO SORTER                      
         GOTO1 BUFFALO,DMCB,=C'CLEAR',ADBUFC,(X'80',1)                          
         LA    R3,RECORD           INITIALIZE KEY                               
         MVI   RECORD,C' '                                                      
         MVC   RECORD+1(L'RECORD-1),RECORD                                      
         L     R2,ADHEIRA                                                       
         MVC   RECBOT,3(R2)         BOTTLER ACN NUMBER                          
         L     R4,ADLVANAM                                                      
         MVC   RECBOTNM,SPACES      FILL WITH SPACES                            
         ZIC   R1,ACNMLEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   RECBOTNM(0),ACNMNAME ACN NAME TO RECORD                          
         MVI   ELCODE,X'39'        GET AREA/REGION/DISTRICT                     
         USING ACEXPD,R5                                                        
         L     R2,ADACC                                                         
         LR    R5,R2                                                            
         BAS   RE,GETEL                                                         
         BNE   *+14                                                             
         MVC   RECAREA(5),ACEXPACC+3 AREA/REGION/DISTRICT                       
         B     *+10                                                             
         MVC   RECAREA(5),=C'99999' DUMMY AREA IF ELEMENT NOT FOUND             
         CLI   QOPT1,C'Y'          IF THIS IS EXCEPTION REPORT                  
         BNE   *+10                                                             
         MVC   RECKAGY,8(R2)       MOVE AGENCY CODE TO KEY                      
         MVC   RECTYPE,=C'BB'      BOTTLER BOUGHT                               
         CLC   8(3,R2),=C'100'                                                  
         BL    XIT                                                              
         MVC   RECTYPE,=C'BA'      AGENCY BOUGHT                                
         CLC   8(3,R2),=C'900'                                                  
         BL    XIT                                                              
         MVC   RECTYPE,=C'CA'      OR MCCANN BOUGHT                             
         B     XIT                                                              
         EJECT                                                                  
         USING RECD,R3                                                          
         USING TRSUBHD,R4                                                       
CX75     CLI   MODE,SBACFRST                                                    
         BNE   CX100                                                            
         LA    R3,RECORD                                                        
         L     R4,ADSUBAC                                                       
         ZIC   R0,NPRODS           CONVERT PRODUCT CODE                         
         L     RF,APRODTAB                                                      
         CLC   TRSBACNT+3(2),2(RF) MATCH ON CHARACTER CODE                      
         BE    *+14                                                             
         LA    RF,L'PRODTAB(RF)                                                 
         BCT   R0,*-14                                                          
         DC    H'0'                PRODUCT NOT FOUND                            
         MVC   RECPRD,0(RF)        NUMERIC PRODUCT CODE                         
         MVC   RECMED,TRSBACNT+5   MEDIA                                        
         LA    R1,RECACCS          INITIALIZE ACCUMULATORS                      
         LA    R0,NRECACCS                                                      
         ZAP   0(8,R1),=P'0'                                                    
         LA    R1,8(R1)                                                         
         BCT   R0,*-10                                                          
         B     XIT                                                              
         EJECT                                                                  
         USING TRANSD,R2                                                        
         USING RECD,R3                                                          
         USING TRSTATD,R4                                                       
         USING TRCASHD,R5                                                       
CX100    CLI   MODE,PROCTRNS                                                    
         BNE   CX125                                                            
         LA    R3,RECORD                                                        
         L     R2,ADTRANS                                                       
         CLC   TRNSDATE(1),YEAR                                                 
         BNE   XIT                 TRANSACTION MUST MATCH YEAR                  
         TM    TRNSSTAT,X'04'      SKIP HELD TRANSACTIONS                       
         BO    XIT                                                              
         LR    R5,R2                                                            
         SH    R5,DATADISP                                                      
         MVI   ELCODE,X'60'                                                     
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         LR    R4,R5                                                            
         OC    TRSTUPDT,TRSTUPDT   ITEM MUST BE APPROVED                        
         BZ    XIT                                                              
         CLC   TRSTUPDT,END2       BEFORE END OF REQUEST PERIOD                 
         BH    XIT                                                              
         LR    R5,R2                                                            
         SH    R5,DATADISP                                                      
         MVI   ELCODE,X'50'        GET THE EXPENDITURE AMOUNT                   
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         ZAP   DOUBLE,TRCSGRS                                                   
         LR    R5,R2                                                            
         SH    R5,DATADISP                                                      
         MVI   ELCODE,X'46'        SUBTRACT CASH DISCOUNT IF AROUND             
         USING TRPAYD,R5                                                        
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         SP    DOUBLE,TRPYCD                                                    
         ZAP   DUB,=P'0'           USE TRANSACTION DATE FOR MONTH COLS.         
         MVO   DUB,TRNSDATE+1(1)   CONVERT PWOS MONTH TO BINARY                 
         CVB   R1,DUB                                                           
         BCTR  R1,0                SUBTRACT ONE                                 
         SLL   R1,3                AND MULTIPLY BY 8                            
         LA    RF,RECMTHS                                                       
         AR    RF,R1               RF=A(ACCUM FOR THIS TRANSACTION)             
         AP    0(8,RF),DOUBLE                                                   
         CLC   TRSTUPDT,STRT2      IF ITEM APPROVED IN PERIOD                   
         BL    XIT                                                              
         CLC   TRSTUPDT,END2                                                    
         BH    XIT                                                              
         AP    RECCUR,DOUBLE       ADD TO CURRENT BUCKET                        
         B     XIT                                                              
         EJECT                                                                  
         USING RECD,R3                                                          
CX125    CLI   MODE,SBACLAST                                                    
         BNE   CX150                                                            
         LA    R3,RECORD                                                        
         CLC   MEDFLT,SPACES                                                    
         BE    *+14                NO MEDIA FILTER                              
         CLC   RECMED,MEDFLT                                                    
         BNE   XIT                 FILTER OUT THIS MEDIA                        
         CLC   BUDFLT,SPACES                                                    
         BE    *+14                NO BUDGET CATEGORY FILTER                    
         CLC   RECPRD,BUDFLT                                                    
         BNE   XIT                 FILTER OUT THIS PRODUCT                      
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUFC,RECORD                               
         B     XIT                                                              
         EJECT                                                                  
CX150    CLI   MODE,ACCLAST                                                     
         BNE   CX200                                                            
         XC    RECORD,RECORD       PUT BUFFALO RECORDS TO SORTER                
         GOTO1 BUFFALO,DMCB,=C'HIGH',ADBUFC,RECORD,1                            
         TM    DMCB+8,X'80'                                                     
         BO    XIT                 NOTHING FOR SORTER                           
         B     CX160                                                            
CX155    GOTO1 BUFFALO,DMCB,=C'SEQ',ADBUFC,RECORD,1                             
         TM    DMCB+8,X'80'                                                     
         BO    XIT                                                              
CX160    DS    0H                                                               
         GOTO1 SORTER,DMCB,=C'PUT',RECORD                                       
         B     CX155                                                            
         EJECT                                                                  
         USING PRINTD,R5                                                        
CX200    CLI   MODE,REQLAST                                                     
         BNE   XIT                                                              
         GOTO1 BUFFALO,DMCB,=C'CLEAR',ADBUFC,(X'80',1)                          
         LA    R1,COUNTS           CLEAR ACTIVITY COUNTERS                      
         LA    R0,NCOUNTS                                                       
         ZAP   0(L'COUNTS,R1),=P'0'                                             
         LA    R1,L'COUNTS(R1)                                                  
         BCT   R0,*-10                                                          
         LA    R1,ACCUMS           CLEAR ACCUMULATORS                           
         LA    R0,NACCUMS*NACCS                                                 
         ZAP   0(8,R1),=P'0'                                                    
         LA    R1,8(R1)                                                         
         BCT   R0,*-10                                                          
         XC    LASTREC,LASTREC                                                  
         OI    STATUS,BOTTLER      SET TO PRINT NAME                            
         LA    R5,XP                                                            
CX210    GOTO1 SORTER,DMCB,=C'GET'                                              
         ICM   R3,15,DMCB+4                                                     
         BZ    CX220                                                            
         MVC   RECORD,0(R3)                                                     
         BAS   RE,REPORT                                                        
         B     CX210                                                            
         SPACE 1                                                                
CX220    OC    LASTREC,LASTREC     IF NO SAVED RECORD                           
         BZ    CX230               THEN NO DATA GENERATED                       
         BAS   RE,CHAAREA          FINISH UP REPORT                             
         MVI   RCSUBPRG,2          SET FOR REQUEST SUMMARY                      
         MVI   DMCB,REQUEST                                                     
         GOTO1 GETBUFF,DMCB                                                     
         BAS   RE,FORCEPRT                                                      
         MVC   PDATA(14),=C'REQUEST TOTALS'                                     
         LA    R2,TOTREQ           PRINT REPORT TOTALS                          
         BAS   RE,FORMAT                                                        
         BAS   RE,PRNTIT                                                        
         MVI   RCSUBPRG,0          RESET HEADLINE CONTROL                       
         MVI   FORCEHED,C'Y'                                                    
CX230    GOTO1 SORTER,DMCB,=C'END'                                              
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO BUILD ACCOUNT TABLES                                  
         SPACE 1                                                                
*                                  P1 BYTE 0 - 0=AREA/REGION/DISTRICT           
*                                  P1 BYTES 1-3=A(TABLE)                        
*                                  P2=MAX. NUMBER OF ENTRIES FOR TABLE          
*                                  P3=A(UNIT/LEDGER)                            
         USING ACKEYD,R4                                                        
BLDTAB   NTR1                                                                   
         LM    R2,R4,0(R1)                                                      
         MVC   BYTE,0(R1)                                                       
         MVC   MYKEY,SPACES        BUILD TABLE OF AREA ACCOUNTS                 
         MVC   MYKEY(1),RCCOMPFL                                                
         MVC   MYKEY+1(2),0(R4)    UNIT/LEDGER                                  
         MVC   IO(L'MYKEY),MYKEY                                                
         LA    R4,IO                                                            
         GOTO1 DATAMGR,DMCB,DMREAD,=C'ACCOUNT',(R4),(R4)                        
         CLI   DMCB+8,0                                                         
         BE    *+6                 MISSING LEDGER RECORD                        
         DC    H'0'                                                             
BLDT4    GOTO1 DATAMGR,DMCB,DMRSEQ,=C'ACCOUNT',(R4),(R4)                        
         CLC   ACKEYACC(3),MYKEY   STILL SAME LEDGER                            
         BNE   BLDTX                                                            
         CLC   ACKEYWRK(ACLENGTH-ACKEYWRK),SPACES ONLY WANT ACCOUNTS            
         BNE   BLDT4                                                            
         MVC   0(15,R2),ACKEYACC   ACCOUNT TO TABLE                             
         LR    R5,R4                                                            
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MISSING NAME ELEMENT                         
         MVC   15(36,R2),SPACES    FILL WITH SPACES                             
         ZIC   R1,1(R5)                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   15(0,R2),2(R5)      NAME TO TABLE                                
         LA    R2,51(R2)           BUMP TO NEXT                                 
         BCT   R3,BLDT4                                                         
         DC    H'0'                NEED TO INCREASE SIZE OF TABLE               
BLDTX    CLI   BYTE,0              IS THIS AREA TABLE                           
         BNE   XIT                                                              
         CH    R3,=H'3'            NEED TO HAVE 3 LEFT                          
         BNL   *+6                                                              
         DC    H'0'                NOT ENOUGH ROOM FOR DUMMIES                  
         MVC   0(L'DUMMIES,R2),DUMMIES  DUMMY AREA FOR NOT-FOUNDS               
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO BUILD PRODUCT TABLE                                   
         SPACE 1                                                                
         USING ACKEYD,R4                                                        
BLDPROD  NTR1                                                                   
         L     R2,APRODTAB                                                      
         MVI   NPRODS,0                                                         
         MVC   MYKEY,SPACES        BUILD TABLE OF AREA ACCOUNTS                 
         MVC   MYKEY(1),RCCOMPFL                                                
         MVC   MYKEY+1(2),=C'3P'   UNIT/LEDGER                                  
         MVC   IO(L'MYKEY),MYKEY                                                
         LA    R4,IO                                                            
         GOTO1 DATAMGR,DMCB,DMREAD,=C'ACCOUNT',(R4),(R4)                        
         CLI   DMCB+8,0                                                         
         BE    *+6                 MISSING LEDGER RECORD                        
         DC    H'0'                                                             
BLDP4    GOTO1 DATAMGR,DMCB,DMRSEQ,=C'ACCOUNT',(R4),(R4)                        
         CLC   ACKEYACC(3),MYKEY   STILL SAME LEDGER                            
         BNE   BLDPX                                                            
         CLC   ACKEYACC+5((ACLENGTH-ACKEYACC)-5),SPACES                         
         BNE   BLDP4                                                            
         MVC   2(2,R2),ACKEYACC+3  PRODUCT CODE                                 
         LR    R5,R4                                                            
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MISSING NAME ELEMENT                         
         MVC   4(36,R2),SPACES     FILL WITH SPACES                             
         ZIC   R1,1(R5)                                                         
         SH    R1,=H'3'                                                         
         BM    BLDP8                                                            
         EX    R1,*+4                                                           
         MVC   4(0,R2),2(R5)      NAME TO TABLE                                 
*                                                                               
         LR    R5,R4                                                            
         MVI   ELCODE,X'23'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MISSING PRODUCT NUMBER                       
         MVC   0(2,R2),2(R5)       NAME TO TABLE                                
*                                                                               
BLDP8    LA    R2,L'PRODTAB(R2)    BUMP TO NEXT                                 
         ZIC   R1,NPRODS                                                        
         LA    R1,1(R1)                                                         
         STC   R1,NPRODS                                                        
         CH    R1,=Y(MAXPRODS)                                                  
         BL    BLDP4                                                            
         DC    H'0'                NEED TO INCREASE SIZE OF TABLE               
BLDPX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO GET BUDGETS                                           
         SPACE 1                                                                
         USING ACBAD,R2                                                         
         USING RECD,R3                                                          
         USING ACBTKEY,R4                                                       
GETBUD   NTR1                                                                   
         LA    R3,RECORD                                                        
         MVI   RECORD,C' '                                                      
         MVC   RECORD+1(L'RECORD-1),RECORD                                      
         LA    R1,RECACCS          INITIALIZE ACCUMULATORS                      
         LA    R0,NRECACCS                                                      
         ZAP   0(8,R1),=P'0'                                                    
         LA    R1,8(R1)                                                         
         BCT   R0,*-10                                                          
         XC    MYKEY,MYKEY                                                      
         LA    R4,MYKEY                                                         
         MVI   ACBTKTYP,ACBTKTEQ   BUILD BUDGET RECORD KEY                      
         MVC   ACBTKACC,SPACES                                                  
         MVC   ACBTKACC(1),RCCOMPFL                                             
         MVC   ACBTKACC+1(2),=C'SE'                                             
         LA    R5,3                R5=L'COMPARE-1                               
         L     RF,ADACC                                                         
         TM    STATUS,ACCOUNT+FILTER IF FILTERING REQUEST                       
         BZ    *+14                                                             
         MVC   ACBTKACC+3(12),3(RF)                                             
         LA    R5,8                INCREASE L'COMPARE FOR BOTTLER               
         LA    R4,IO                                                            
         B     GTBUD2                                                           
GTBUD1   ZIC   R1,MYKEY+41         BUMP KEY TO GET NEXT                         
         LA    R1,1(R1)                                                         
         STC   R1,MYKEY+41                                                      
GTBUD2   MVC   IO(L'MYKEY),MYKEY                                                
         GOTO1 DATAMGR,DMCB,DMRDHI,=C'ACCOUNT',(R4),(R4)                        
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   ACBTKEY(0),MYKEY                                                 
         BNE   GTBUDZ              NO MORE FOR THIS REQUEST                     
         MVC   MYKEY,ACBTKEY       ** BUILD KEY FOR SORTER                      
         SPACE 1                                                                
         CLC   BUDFLT,SPACES                                                    
         BE    *+14                                                             
         CLC   BUDFLT,ACBTKCON+3   DO I WANT THIS BUDGET(PRODUCT)               
         BNE   GTBUD1              IF NOT GET NEXT                              
         CLC   MEDFLT,SPACES                                                    
         BE    *+14                                                             
         CLC   MEDFLT,ACBTKCON+5   DO I WANT THIS MEDIA                         
         BNE   GTBUD1              IF NOT GET NEXT                              
         CLC   AGYFLT,SPACES                                                    
         BE    *+14                                                             
         CLC   AGYFLT,ACBTKACC+8   DO I WANT THIS AGENCY                        
         BNE   GTBUD1              IF NOT GET NEXT                              
         SPACE 1                                                                
         MVC   RECPRD,ACBTKCON+3   PRODUCT CODE                                 
         MVC   RECBOT,ACBTKACC+3   ACN NUMBER (BOTTLER CODE)                    
         MVC   RECMED,ACBTKCON+5   MEDIA CODE                                   
         MVC   RECAGY,ACBTKACC+8   AGENCY CODE (IN CASE NO SE ACCOUNT)          
         CLI   QOPT1,C'Y'          IF THIS IS EXCEPTION REPORT                  
         BNE   *+10                                                             
         MVC   RECKAGY,ACBTKACC+8  AGENCY CODE TO KEY AS WELL                   
         MVC   RECTYPE,=C'BB'      BOTTLER BOUGHT                               
         CLC   RECAGY,=C'100'                                                   
         BL    GTBUD4                                                           
         MVC   RECTYPE,=C'BA'      AGENCY BOUGHT                                
         CLC   RECAGY,=C'900'                                                   
         BL    GTBUD4                                                           
         MVC   RECTYPE,=C'CA'      OR MCCANN BOUGHT                             
GTBUD4   ZAP   RECBUD,=P'0'                                                     
         LA    R2,ACRECORD                                                      
         XR    R1,R1                                                            
GTBUD7   CLI   0(R2),0                                                          
         BE    GTBUDX                                                           
         CLI   0(R2),X'1D'                                                      
         BE    GTBUD9                                                           
GTBUD8   IC    R1,1(R2)                                                         
         AR    R2,R1                                                            
         B     GTBUD7                                                           
GTBUD9   CLC   YEAR,ACBAMNTH       MUST BE FOR THIS YEAR                        
         BNE   GTBUD8                                                           
         CLI   QOPT2,C'Y'          BUDGETS FOR PERIOD                           
         BNE   GTBUD10                                                          
         CLC   ACBAMNTH,STRT1                                                   
         BL    GTBUD8              BEFORE START SKIP IT                         
         CLC   ACBAMNTH,END1                                                    
         BH    GTBUD8              AFTER END SKIP IT                            
GTBUD10  AP    RECBUD,ACBABUDG                                                  
         B     GTBUD8                                                           
         SPACE 1                                                                
GTBUDX   DS    0H                                                               
         BAS   RE,GETACC           GET OTHER INFO FROM SE ACCOUNT               
         GOTO1 SORTER,DMCB,=C'PUT',RECORD    WRITE OUT TO SORTER                
         B     GTBUD1              AND LOOK FOR ANOTHER                         
         SPACE 2                                                                
GTBUDZ   DS    0H                                                               
         CLI   MODE,PROCACC        IF MODE IS PROCACC                           
         BNE   XIT                                                              
         L     RF,ADACC            RE-READ THIS ACCOUNT                         
         GOTO1 DATAMGR,DMCB,DMRDHI,=C'ACCOUNT',(RF),(R4)                        
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO READ SE ACCOUNTS FOR BUDGET                           
         SPACE 1                                                                
         USING RECD,R3                                                          
         USING ACKEYD,R4                                                        
         USING ACEXPD,R5                                                        
GETACC   NTR1                                                                   
         MVC   MYKEY2,SPACES                                                    
         LA    R4,MYKEY2                                                        
         MVC   ACKEYACC(8),MYKEY+1 MOVE IN HIGH LEVEL FROM BUDGET REC.          
         MVC   IO(L'MYKEY2),MYKEY2                                              
         LA    R4,IO                                                            
         GOTO1 DATAMGR,DMCB,DMREAD,=C'ACCOUNT',(R4),(R4)                        
         CLI   DMCB+8,0                                                         
         BE    *+14                                                             
GETACC2  MVC   RECAREA(5),=C'99999' ACCOUNT NOT FOUND - SET TO DUMMIES          
         B     XIT                                                              
         MVI   ELCODE,X'20'        GET ACN NAME                                 
         MVC   RECBOTNM,SPACES     FILL WITH SPACES                             
         LR    R5,R4                                                            
         BAS   RE,GETEL                                                         
         BNE   GETACC4                                                          
         ZIC   R1,1(R5)                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   RECBOTNM(0),2(R5)   ACN NAME TO RECORD                           
GETACC4  LA    R4,MYKEY2                                                        
         MVC   MYKEY2,SPACES                                                    
         MVC   ACKEYACC,MYKEY+1    MOVE IN ACCOUNT FROM BUDGET RECORD           
         MVC   IO(L'MYKEY2),MYKEY2                                              
         LA    R4,IO                                                            
         GOTO1 DATAMGR,DMCB,DMREAD,=C'ACCOUNT',(R4),(R4)                        
         CLI   DMCB+8,0                                                         
         BNE   GETACC2                                                          
         MVI   ELCODE,X'39'        GET AREA/REGION/DISTRICT                     
         LR    R5,R4                                                            
         BAS   RE,GETEL                                                         
         BNE   GETACC2                                                          
         MVC   RECAREA(5),ACEXPACC+3 AREA/REGION/DISTRICT                       
         B     XIT                                                              
         EJECT                                                                  
*              CONTROL PRINTING OF REPORT                                       
         SPACE 1                                                                
         USING RECD,R3                                                          
         USING PRINTD,R5                                                        
REPORT   NTR1                                                                   
         LA    R3,RECORD                                                        
         LA    R4,LASTREC                                                       
         LA    R5,XP                                                            
         OC    LASTREC,LASTREC     IS THIS FIRST TIME                           
         BZ    REPT8               YES - SAVE THIS ONE AND GET OUT              
         CLC   RECAREA,RECAREA-RECD(R4) AREA CHANGE                             
         BE    *+12                                                             
         BAS   RE,CHAAREA                                                       
         B     REPT8                                                            
         CLC   RECREG,RECREG-RECD(R4)   REGION CHANGE                           
         BE    *+12                                                             
         BAS   RE,CHAREG                                                        
         B     REPT8                                                            
         CLC   RECDIST,RECDIST-RECD(R4) DISTRICT CHANGE                         
         BE    *+12                                                             
         BAS   RE,CHADIST                                                       
         B     REPT8                                                            
         CLC   RECPRD,RECPRD-RECD(R4)   PRODUCT CHANGE                          
         BE    *+12                                                             
         BAS   RE,CHAPRD                                                        
         B     REPT8                                                            
         CLC   RECBOT,RECBOT-RECD(R4)   BOTTLER CHANGE                          
         BE    *+12                                                             
         BAS   RE,CHABOT                                                        
         B     REPT8                                                            
         CLC   RECTYPE,RECTYPE-RECD(R4) BUY TYPE CHANGE                         
         BE    *+12                                                             
         BAS   RE,CHATYPE                                                       
         B     REPT8                                                            
         CLC   RECMED,RECMED-RECD(R4)   MEDIA CHANGE                            
         BE    *+12                                                             
         BAS   RE,CHAMED                                                        
         B     REPT8                                                            
         CLC   RECKAGY,RECKAGY-RECD(R4) AGENCY CODE CHANGE (CAN ONLY            
         BE    *+8                      OCCUR ON EXCEPTION REPORT               
         BAS   RE,CHAMED           SIMULATE MEDIA CHANGE                        
REPT8    GOTO1 ADDUP,DMCB,RECACCS,TOTMED  ADD THIS TO HIGHER LEVEL              
         MVC   LASTREC,RECORD      SAVE IT                                      
         B     XIT                                                              
         EJECT                                                                  
*              AREA CHANGE                                                      
         SPACE 1                                                                
         USING PRINTD,R5                                                        
CHAAREA  NTR1                                                                   
         BAS   RE,CHAREG           CHANGE LOWER LEVEL                           
         MVI   RCSUBPRG,1          PRINT AREA SUMMARY                           
         MVI   DMCB,AREA                                                        
         GOTO1 GETBUFF,DMCB                                                     
         BAS   RE,FORCEPRT                                                      
         MVC   PDATA(11),=C'AREA TOTALS'                                        
         LA    R2,TOTAREA                                                       
         GOTO1 ADDUP,DMCB,(1,(R2)),TOTREQ  ADD TO HIGHER LEVEL                  
         BAS   RE,FORMAT                                                        
         BAS   RE,PRNTIT                                                        
         MVI   RCSUBPRG,0          RESET HEADLINE CONTROL                       
         MVI   FORCEHED,C'Y'                                                    
         B     XIT                                                              
         EJECT                                                                  
*              REGION CHANGE                                                    
         SPACE 1                                                                
         USING PRINTD,R5                                                        
CHAREG   NTR1                                                                   
         BAS   RE,CHADIST          CHANGE LOWER LEVEL                           
         CP    CNTDIST,=P'1'       DO I NEED TO SKIP A LINE                     
         BL    CHAREG8                                                          
         MVI   FORCEHED,C'N'                                                    
         BAS   RE,FORCEPRT                                                      
         MVC   PDATA(13),=C'REGION TOTALS'                                      
         LA    R2,TOTREG                                                        
         GOTO1 ADDUP,DMCB,(1,(R2)),TOTAREA ADD TO HIGHER LEVEL                  
         BAS   RE,FORMAT                                                        
         BAS   RE,PRNTIT                                                        
         ZAP   CNTDIST,=P'0'                                                    
CHAREG8  MVI   FORCEHED,C'Y'                                                    
         B     XIT                                                              
         EJECT                                                                  
*              DISTRICT CHANGE                                                  
         SPACE 1                                                                
         USING PRINTD,R5                                                        
CHADIST  NTR1                                                                   
         BAS   RE,CHAPRD           CHANGE LOWER LEVEL                           
         CP    CNTPRD,=P'1'        DO I NEED TO SKIP A LINE                     
         BL    CHADIST8                                                         
         MVI   FORCEHED,C'N'                                                    
         BAS   RE,FORCEPRT                                                      
         MVC   PDATA(15),=C'DISTRICT TOTALS'                                    
         LA    R2,TOTDIST                                                       
         GOTO1 ADDUP,DMCB,(1,(R2)),TOTREG  ADD TO HIGHER LEVEL                  
         BAS   RE,FORMAT                                                        
         BAS   RE,PRNTIT                                                        
         AP    CNTDIST,=P'1'                                                    
         ZAP   CNTPRD,=P'0'                                                     
CHADIST8 MVI   FORCEHED,C'Y'                                                    
         B     XIT                                                              
         EJECT                                                                  
*              PRODUCT CHANGE                                                   
         SPACE 1                                                                
         USING PRINTD,R5                                                        
CHAPRD   NTR1                                                                   
         BAS   RE,CHABOT           CHANGE LOWER LEVEL                           
         MVI   DMCB,PRODUCT        PRINT PRODUCT SUMMARY                        
         GOTO1 GETBUFF,DMCB                                                     
         CP    CNTBOT,=P'1'        DO I NEED TO SKIP A LINE                     
         BL    CHAPRD8                                                          
         BAS   RE,FORCEPRT                                                      
         MVC   PDATA(14),=C'PRODUCT TOTALS'                                     
         LA    R2,TOTPRD                                                        
         GOTO1 ADDUP,DMCB,(1,(R2)),TOTDIST ADD TO HIGHER LEVEL                  
         BAS   RE,FORMAT                                                        
         BAS   RE,PRNTIT                                                        
         AP    CNTPRD,=P'1'                                                     
         ZAP   CNTBOT,=P'0'                                                     
CHAPRD8  MVI   FORCEHED,C'Y'                                                    
         B     XIT                                                              
         EJECT                                                                  
*              BOTTLER CHANGE                                                   
         SPACE 1                                                                
         USING PRINTD,R5                                                        
CHABOT   NTR1                                                                   
         BAS   RE,CHATYPE          CHANGE LOWER LEVELS                          
         LA    R2,TOTBOT                                                        
         GOTO1 ADDUP,DMCB,(1,(R2)),TOTPRD  ADD TO HIGHER LEVEL                  
         CP    CNTTYPE,=P'1'       DO I NEED A TOTAL LINE                       
         BE    CHABOT6                                                          
         BL    CHABOT8                                                          
         MVC   PDATA(14),=C'BOTTLER TOTALS'                                     
         BAS   RE,FORMAT                                                        
         BAS   RE,PRNTIT                                                        
         B     *+8                                                              
CHABOT6  BAS   RE,CLEAR                                                         
         BAS   RE,FORCEPRT                                                      
         ZAP   CNTTYPE,=P'0'     CLEAR BUY TYPE COUNT                           
         AP    CNTBOT,=P'1'                                                     
CHABOT8  OI    STATUS,BOTTLER                                                   
         B     XIT                                                              
         EJECT                                                                  
*              BUY TYPE CHANGE                                                  
         SPACE 1                                                                
         USING PRINTD,R5                                                        
CHATYPE  NTR1                                                                   
         BAS   RE,CHAMED                                                        
         LA    R2,TOTTYPE                                                       
         GOTO1 ADDUP,DMCB,(1,(R2)),TOTBOT  ADD TO HIGHER LEVEL                  
         CP    CNTMED,=P'1'       DO I NEED A TOTAL LINE                        
         BE    CHATYPE6                                                         
         BL    XIT                                                              
         MVC   PDATA(15),=C'BUY TYPE TOTALS'                                    
         BAS   RE,FORMAT                                                        
         BAS   RE,PRNTIT                                                        
         B     *+8                                                              
CHATYPE6 BAS   RE,CLEAR                                                         
         AP    CNTTYPE,=P'1'       ADD TO BUY TYPE COUNT                        
         ZAP   CNTMED,=P'0'        CLEAR MEDIA COUNT                            
         B     XIT                                                              
         EJECT                                                                  
*              MEDIA CHANGE                                                     
         SPACE 1                                                                
         USING PRINTD,R5                                                        
CHAMED   NTR1                                                                   
         LA    R2,TOTMED                                                        
         CLI   QOPT1,C'Y'          IF THIS IS EXCEPTION REPORT                  
         BNE   CHAMED1                                                          
         BAS   RE,CHKREC           CHECK FOR OVER-BUDGET                        
         BH    CHAMED1                                                          
         BAS   RE,CLEAR            IT'S NOT - CLEAR ACCUMS                      
         B     XIT                 AND GET OUT                                  
CHAMED1  LA    R3,LASTREC                                                       
         TM    STATUS,BOTTLER      DO WE NEED TO PRINT BOTTLER NAME             
         BZ    CHAMED2                                                          
         MVC   PDATA(5),RECBOT                                                  
         MVC   PDATA+6(L'PDATA-6),RECBOTNM                                      
         LA    RF,L'PDATA                                                       
         GOTO1 UNDERLIN,DMCB,((RF),PDATA),(X'1C',PDATA+198)                     
         LA    R5,2*198(R5)        BUMP CURRENT LINE                            
CHAMED2  BAS   RE,GETDETS          PRINT BUY TYPE AND MEDIA DETAILS             
         LR    R1,R2               ROUND ALL ACCUMS BEFORE ADDING UP            
         LA    R0,NACCS                                                         
         SRP   0(8,R1),62,5        ROUND OFF PENNIES                            
         LA    R1,8(R1)                                                         
         BCT   R0,*-10                                                          
         GOTO1 ADDUP,DMCB,(1,(R2)),TOTTYPE ADD TO HIGHER LEVEL                  
         BAS   RE,PUTBUFF          ADD TO BUFFALO FOR SUMMARY                   
         BAS   RE,FORMAT                                                        
         BAS   RE,PRNTIT                                                        
         BNE   XIT                 N/E CC MEANS NO LINE PRINTED                 
         TM    STATUS,BOTTLER      IF WE DID AND BOTTLER NAME WAS               
         BZ    *+8                 PRINTED, TURN OFF BOTTLER STATUS             
         NI    STATUS,ALL-BOTTLER                                               
         AP    CNTMED,=P'1'        ADD ONE TO ACTIVITY COUNTER                  
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO CHECK FOR AGENCIES OVER-BUDGET                        
         SPACE 1                                                                
         USING ACCD,R2                                                          
CHKREC   DS    0H                                                               
         ZAP   DUB,=P'0'                                                        
         LA    R1,ACCMTHS          ADD UP MONTHLY BUCKETS                       
         LA    R0,12                                                            
         AP    DUB,0(8,R1)                                                      
         LA    R1,8(R1)                                                         
         BCT   R0,*-10                                                          
         CP    DUB,ACCBUD          IS THIS OVER BUDGET                          
         BR    RE                                                               
         EJECT                                                                  
*              ROUTINE TO ADD ACCUMULATORS TO HIGHER LEVELS                     
         SPACE 1                                                                
*                                  P1 BYTE 0 = 0 RECORD ACCUMS                  
*                                              1 TOTAL ACCUMS                   
*                                  P1 BYTES 1-3= A(1ST ACCUM TO ADD UP)         
*                                  P2 = A(ACCUMULATORS TO ADD TO)               
ADDUP    NTR1                                                                   
         LM    RE,RF,0(R1)                                                      
         LA    R0,NACCS            N'ACCUMS IN TOTALS                           
ADD2     CLI   0(R1),0             IF THIS IS A RECORD                          
         BNE   ADD3                                                             
         CH    R0,=H'15'           Y-T-D                                        
         BE    ADD4                                                             
         CH    R0,=H'13'           AND BALANCE                                  
         BE    ADD4                DON'T HAVE ACCUMS                            
ADD3     AP    0(8,RF),0(8,RE)                                                  
         LA    RE,8(RE)                                                         
ADD4     LA    RF,8(RF)                                                         
         BCT   R0,ADD2                                                          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO MOVE BUY TYPE AND MEDIA DETAILS TO PRINT LINE         
         SPACE 1                                                                
         USING RECD,R3                                                          
         USING PRINTD,R5                                                        
GETDETS  DS    0H                                                               
         ST    RE,FULL                                                          
         LA    R1,BTTAB            GET BUY TYPE                                 
         LA    R0,NBT                                                           
         CLC   RECTYPE,0(R1)       MATCH ON CODE                                
         BE    *+14                                                             
         LA    R1,L'BTTAB(R1)                                                   
         BCT   R0,*-14                                                          
         DC    H'0'                UNKNOWN BUY TYPE                             
         MVC   PDATA(1),2(R1)      BUY TYPE                                     
         LA    RE,24               SET L'MEDIA NAME - 1                         
         LA    RF,PDATA+2          AND DISPLACEMENT INTO P                      
         CLC   RECAREA(5),=C'99999' IF THIS IS A DUMMY                          
         BNE   GETD2                                                            
         MVC   PDATA(3),RECAGY    PRINT AGENCY CODE INSTEAD OF BUY TYPE         
         LA    RE,22                                                            
         LA    RF,PDATA+4                                                       
         B     GETD3                                                            
GETD2    CLI   QOPT1,C'Y'          IF THIS IS EXCEPTION REPORT                  
         BNE   GETD3                                                            
         MVC   PDATA(3),RECKAGY   PRINT AGENCY CODE INSTEAD OF BUY TYPE         
         LA    RE,22                                                            
         LA    RF,PDATA+4                                                       
GETD3    LA    R1,MEDTAB           GET MEDIA NAME                               
         LA    R0,NMED                                                          
GETD4    CLC   RECMED,0(R1)        MATCH ON COKE CODE                           
         BE    GETD6                                                            
         CLC   RECMED,2(R1)        OR TRY TO MATCH ON DDS CODE                  
         BE    GETD6                                                            
         LA    R1,L'MEDTAB(R1)                                                  
         BCT   R0,GETD4                                                         
         DC    H'0'                UNKNOWN MEDIA CODE                           
GETD6    EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),4(R1)                                                    
         L     RE,FULL                                                          
         BR    RE                                                               
         EJECT                                                                  
*              ROUTINE TO PUT RECORDS TO BUFFALO FOR SUMMARIES                  
         SPACE 1                                                                
         USING ACCD,R2                                                          
         USING RECD,R3                                                          
PUTBUFF  NTR1                                                                   
         LA    R3,SUMMREC                                                       
         MVC   SUMMREC,LASTREC                                                  
         MVI   RECLVL,PRODUCT      SET FOR PRODUCT TOTALS                       
         XC    RECBOT,RECBOT       CLEAR BOTTLER                                
         XC    RECMED,RECTYPE      SWITCH MEDIA AND BUY TYPE HIERARCHY          
         XC    RECTYPE,RECMED                                                   
         XC    RECMED,RECTYPE                                                   
         ZAP   RECCUR,ACCCUR       MOVE CURRENT TOTALS TO RECORD                
         ZAP   RECBUD,ACCBUD                                                    
         MVC   RECMTHS(12*8),ACCMTHS                                            
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUFC,(R3)                                 
         MVI   RECLVL,AREA         SET FOR AREA TOTALS                          
         XC    RECREG,RECREG       CLEAR REGION                                 
         XC    RECDIST,RECDIST     AND DISTRICT                                 
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUFC,(R3)                                 
         MVI   RECLVL,REQUEST      SET FOR REQUEST TOTALS                       
         XC    RECAREA,RECAREA     CLEAR AREA                                   
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUFC,(R3)                                 
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PRINT SUMMARIES                                       
         SPACE 1                                                                
*                                  P1 - 0 = PRODUCT LEVEL SUMMARY               
*                                       1 = AREA LEVEL SUMMARY                  
*                                       2 = REQUEST LEVEL SUMMARY               
         USING RECD,R3                                                          
GETBUFF  NTR1                                                                   
         MVC   LEVEL,0(R1)                                                      
         LA    R3,LASTREC                                                       
         CLI   LEVEL,REQUEST                                                    
         BE    *+14                                                             
         CLC   RECAREA(5),=C'99999' NO SUMMARIES FOR DUMMIES                    
         BE    GETBX                                                            
         XC    SUMMREC,SUMMREC                                                  
         XC    SUMMLST,SUMMLST                                                  
         LA    R3,SUMMREC                                                       
         LA    R4,SUMMLST                                                       
         ZAP   CNTTYPE,=P'0'                                                    
         ZAP   CNTMED,=P'0'                                                     
         SPACE 1                                                                
         MVC   RECLVL,LEVEL                                                     
         GOTO1 BUFFALO,DMCB,=C'HIGH',ADBUFC,(R3),1                              
         TM    DMCB+8,X'80'                                                     
         BO    GETBX               NOTHING TO PRINT                             
         CLI   LEVEL,PRODUCT                                                    
         BE    *+12                                                             
         MVI   FORCEHED,C'Y'       SKIP TO NEXT PAGE FOR LARGE SUMMARY          
         B     GETB0                                                            
         BAS   RE,FORCEPRT         ELSE JUST SKIP A LINE                        
         MVC   PDATA(15),=CL15'PRODUCT SUMMARY'                                 
         B     GETB1                                                            
GETB0    MVC   PDATA(15),=CL15'AREA SUMMARY'                                    
         CLI   LEVEL,AREA                                                       
         BE    GETB1                                                            
         MVC   PDATA(15),=CL15'REQUEST SUMMARY'                                 
GETB1    GOTO1 UNDERLIN,DMCB,(15,PDATA),(X'1C',PDATA+198)                       
         BAS   RE,FORCEPRT                                                      
         B     GETB4                                                            
         SPACE 1                                                                
GETB2    GOTO1 BUFFALO,DMCB,=C'SEQ',(LEVEL,ADBUFC),(R3),1                       
         TM    DMCB+8,X'80'                                                     
         BZ    GETB4                                                            
         XC    SUMMREC,SUMMREC     NO MORE RECORDS - CLEAR CURRENT              
         SPACE 1                                                                
GETB4    XC    RECMED,RECTYPE      RETURN MEDIA AND BUY TYPE TO CORRECT         
         XC    RECTYPE,RECMED      POSITIONS                                    
         XC    RECMED,RECTYPE                                                   
         CLI   LEVEL,PRODUCT       IF NOT A PRODUCT SUMMARY                     
         BE    GETB6                                                            
         CLC   RECPRD,RECPRD-RECD(R4) HAS PRODUCT CHANGED                       
         BE    GETB6               NO, SO DON'T PRINT NAME                      
         OI    STATUS,NEWPRD       SET STATUS BYTE                              
         B     GETB7               AND SEE IF MEDIA TOTALS NECESSARY            
         SPACE 1                                                                
GETB5    NI    STATUS,ALL-NEWPRD   RETURN HERE AFTER MEDIA TOTALS               
         LA    R2,TOTPRD                                                        
         CP    CNTMED,=P'1'        DO WE NEED PRODUCT TOTALS                    
         BH    *+12                                                             
         BAS   RE,CLEAR            NO, CLEAR ACCUMS                             
         B     GETB5B                                                           
         MVC   PDATA(14),=C'PRODUCT TOTALS'                                     
         BAS   RE,FORMAT                                                        
         BAS   RE,FORCEPRT                                                      
GETB5B   ZAP   CNTMED,=P'0'                                                     
         OC    SUMMREC,SUMMREC     ARE WE AT END-OF-FILE                        
         BZ    GETBX                                                            
         BAS   RE,FORCEPRT         SKIP A LINE                                  
         ZIC   R0,NPRODS           GET PRODUCT NAME                             
         L     RF,APRODTAB                                                      
         CLC   RECPRD,0(RF)        MATCH ON NUMERIC CODE                        
         BE    *+14                                                             
         LA    RF,L'PRODTAB(RF)                                                 
         BCT   R0,*-14                                                          
         DC    H'0'                PRODUCT NOT FOUND                            
         MVC   PDATA(27),4(RF)     PRODUCT NAME                                 
         GOTO1 UNDERLIN,DMCB,(27,PDATA),(X'1C',PDATA+198)                       
         BAS   RE,FORCEPRT                                                      
GETB6    CLC   RECMED,RECMED-RECD(R4) HAS MEDIA CHANGED                         
         BE    GETB9                                                            
GETB7    LA    R2,TOTMED                                                        
         CLI   LEVEL,PRODUCT       IF HIGH LEVEL SUMMARY                        
         BE    GETB7B                                                           
         AP    CNTMED,=P'1'                                                     
         GOTO1 ADDUP,DMCB,(1,(R2)),TOTPRD SAVE FOR PRODUCT TOTALS               
GETB7B   CP    CNTTYPE,=P'1'       YES - DO WE NEED TOTALS                      
         BH    *+12                                                             
         BAS   RE,CLEAR            NO, CLEAR ACCUMS                             
         B     GETB8                                                            
         MVC   PDATA(12),=C'MEDIA TOTALS'                                       
         BAS   RE,FORMAT                                                        
         BAS   RE,FORCEPRT                                                      
GETB8    ZAP   CNTTYPE,=P'0'                                                    
         TM    STATUS,NEWPRD       IF CHANGING PRODUCTS                         
         BO    GETB5               THEN RETURN FOR PRODUCT TOTALS               
         OC    SUMMREC,SUMMREC     ARE WE AT END-OF-FILE                        
         BZ    GETBX                                                            
GETB9    BAS   RE,GETDETS          GET BUY TYPE AND MEDIA DETAILS               
         LA    R2,TOTTYPE                                                       
         GOTO1 ADDUP,DMCB,RECACCS,(R2)  USE LOW LEVEL ACCUMS FOR PRINT          
         GOTO1 (RF),(R1),(1,TOTTYPE),TOTMED SAVE FOR MEDIA TOTALS               
         BAS   RE,FORMAT                                                        
         BAS   RE,FORCEPRT                                                      
         AP    CNTTYPE,=P'1'       ADD ONE TO BUY TYPE COUNTER                  
         MVC   SUMMLST,SUMMREC     SAVE THIS RECORD                             
         XC    RECMED,RECTYPE      RETURN MEDIA AND BUY TYPE TO SUMMARY         
         XC    RECTYPE,RECMED      POSITIONS FOR SEQUENTIAL READ                
         XC    RECMED,RECTYPE                                                   
         B     GETB2                                                            
GETBX    GOTO1 BUFFALO,DMCB,=C'CLEAR',(LEVEL,ADBUFC),(X'80',1)                  
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO FORMAT ACCUMULATORS TO PRINT LINE                     
         SPACE 1                                                                
         USING ACCD,R2                                                          
         USING PRINTD,R5                                                        
FORMAT   NTR1                                                                   
         ST    R2,FULL                                                          
         LA    R3,NACCS                                                         
         LA    R4,PACCUMS+1                                                     
FMT1     CH    R3,=H'15'           YEAR-TO-DATE - ADD UP MONTHS                 
         BNE   FMT2                                                             
         L     RF,FULL                                                          
         LA    RF,ACCMTHS-ACCD(RF) FIRST MONTHLY BUCKET                         
         LA    R0,12                                                            
         AP    0(8,R2),0(8,RF)                                                  
         LA    RF,8(RF)                                                         
         BCT   R0,*-10                                                          
         ZAP   DOUBLE,0(8,R2)      SAVE FOR BALANCE                             
FMT2     CH    R3,=H'13'           BALANCE = BUDGET - Y-T-D                     
         BNE   FMT4                                                             
         L     RF,FULL                                                          
         ZAP   0(8,R2),ACCBUD-ACCD(8,RF)    BUDGET                              
         SP    0(8,R2),DOUBLE               LESS Y-T-D                          
FMT4     DS    0H                                                               
         MVC   WORK(10),SPACES                                                  
         CP    0(8,R2),=P'0'                                                    
         BE    FMT6                                                             
         EDIT  (P8,0(R2)),(9,WORK),WRK=WORK+10,FLOAT=-                          
FMT6     LA    R1,WORK                                                          
         LA    RE,L'PACCUMS-2      L'MOVE-1                                     
         CH    R3,=H'12'           MONTHS GET SMALLER FIELD                     
         BH    *+12                                                             
         LA    R1,2(R1)                                                         
         LA    RE,L'PACCUMS2-2                                                  
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R1)       MOVE TO PRINT LINE                           
         LA    R4,2(RE,R4)                                                      
         LA    R2,8(R2)                                                         
         BCT   R3,FMT1                                                          
         L     R2,FULL             NOW CLEAR ACCUMULATORS                       
         BAS   RE,CLEAR                                                         
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE CLEARS A ROW OF ACCUMULATORS                             
         SPACE 1                                                                
CLEAR    DS    0H                                                               
         LA    R0,NACCS                                                         
         ZAP   0(8,R2),=P'0'                                                    
         LA    R2,8(R2)                                                         
         BCT   R0,*-10                                                          
         BR    RE                                                               
         EJECT                                                                  
*              PRINT A LINE                                                     
         SPACE 1                                                                
         USING PRINTD,R5                                                        
PRNTIT   NTR1                                                                   
         LA    R0,3                                                             
         LA    R5,XP                                                            
         CLC   PACCUMS(164-L'PDATA),XSPACES IF THERE ARE NO AMOUNTS             
         BNE   PRNT2                                                            
         LA    R5,198(R5)                                                       
         BCT   R0,*-14                                                          
         MVC   XP,XSPACES          DON'T PRINT ANYTHING                         
         MVC   XPSECOND,XSPACES                                                 
         MVC   XPTHIRD,XSPACES                                                  
         B     NO                  TELL CALLER NO DICE                          
FORCEPRT NTR1                                                                   
PRNT2    GOTO1 ACREPORT                                                         
PRNTX    B     YES                 TELL CALLER WE PRINTED ONE                   
         SPACE 2                                                                
         GETEL R5,DATADISP,ELCODE                                               
         ANSR                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              CONSTANTS, TABLES, ETC.                                          
         SPACE 1                                                                
SORTER   DC    V(SORTER)                                                        
UNDERLIN DC    V(UNDERLIN)                                                      
ASORTC   DC    A(SORTC)                                                         
AAREATAB DC    A(AREATAB)                                                       
APRODTAB DC    A(PRODTAB)                                                       
ADBUFC   DC    A(BUFFALOC)                                                      
         SPACE 1                                                                
SORTCARD DC    CL80'SORT FIELDS=(2,19,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=(171,,171)'                            
         SPACE 1                                                                
BTTAB    DS    0CL16                                                            
         DC    C'BA',CL14'AGENCY BOUGHT '                                       
         DC    C'BB',CL14'BOTTLER BOUGHT'                                       
         DC    C'CA',CL14'MCCANN BOUGHT '                                       
NBT      EQU   (*-BTTAB)/L'BTTAB                                                
         SPACE 1                                                                
MEDTAB   DS    0CL29               COKE CODE/DDS CODE/ NAME                     
         DC    CL2'01',CL2'PN',CL25'*** NEWSPAPER ***'                          
         DC    CL2'02',CL2'PO',CL25'*** OUTDOOR ***'                            
         DC    CL2'03',CL2'SR',CL25'*** RADIO ***'                              
         DC    CL2'04',CL2'ST',CL25'*** TELEVISION ***'                         
         DC    CL2'05',CL2'PS',CL25'*** MISCELLANEOUS ***'                      
         DC    CL2'06',CL2'SM',CL25'*** MISCELLANEOUS TV ***'                   
         DC    CL2'07',CL2'PL',CL25'*** LOCAL EXTRA ***'                        
         DC    CL2'08',CL2'SC',CL25'*** CABLE ***'                              
NMED     EQU   (*-MEDTAB)/L'MEDTAB                                              
         SPACE 1                                                                
DUMMIES  DS    0CL(3*51)                                                        
         DC    CL15' 3A9    ',CL36'DUMMY AREA'                                  
         DC    CL15' 3A99   ',CL36'DUMMY REGION'                                
         DC    CL15' 3A99999',CL36'DUMMY DISTRICT'                              
         SPACE 2                                                                
*              EQUATES TO COVER STATUS BYTE                                     
         SPACE 1                                                                
ALL      EQU   X'FF'                                                            
BOTTLER  EQU   X'80'               NEED TO PRINT BOTTLER CODE AND NAME          
ACCOUNT  EQU   X'40'               REQUEST FILTERED BY ACCOUNT                  
FILTER   EQU   X'20'               REQUEST FILTERED BY FILTERS                  
NEWPRD   EQU   X'10'               USED BY SUMMARY ROUTINE                      
         SPACE 2                                                                
*              EQUATES TO COVER BUFFALO RECORD TYPES FOR SUMMARIES              
         SPACE 1                                                                
REQUEST  EQU   1                                                                
AREA     EQU   2                                                                
PRODUCT  EQU   3                                                                
         EJECT                                                                  
*              HEADLINE/BOX ROUTINES (HEADHOOK)                                 
         SPACE 2                                                                
         USING BOXD,R2                                                          
         USING RECD,R3                                                          
         USING PRINTD,R5                                                        
         DS    0D                                                               
HOOK     NMOD1 0,*HOOK*                                                         
         L     RC,SAVERC           RESTORE REG C                                
         L     R9,BASE2            AND SECOND BASE REGISTER                     
         MVC   XHEAD1+70(23),=C'COKE EXPENDITURE REPORT'                        
         MVC   XHEAD2+70(23),=23X'1C'                                           
         SPACE 1                                                                
         LA    R3,XHEAD6+1         DISPLAY FILTER FIELDS                        
         CLC   AGYFLT,SPACES                                                    
         BE    *+20                                                             
         MVC   0(11,R3),=C'AGENCY=XXX,'                                         
         MVC   7(3,R3),AGYFLT                                                   
         LA    R3,11(R3)                                                        
         CLC   MEDFLT,SPACES                                                    
         BE    *+20                                                             
         MVC   0(9,R3),=C'MEDIA=XX,'                                            
         MVC   6(2,R3),MEDFLT                                                   
         LA    R3,9(R3)                                                         
         CLC   BUDFLT,SPACES                                                    
         BE    *+20                                                             
         MVC   0(8,R3),=C'PROD=XX,'                                             
         MVC   5(2,R3),BUDFLT                                                   
         LA    R3,8(R3)                                                         
         BCTR  R3,0                                                             
         CLI   0(R3),C','          REMOVE TRAILING COMMA                        
         BNE   *+8                                                              
         MVI   0(R3),C' '                                                       
         SPACE 1                                                                
         CLI   QOPT1,C'Y'                                                       
         BNE   *+10                                                             
         MVC   XHEAD1+70(23),=C'COKE OVER-BUDGET REPORT'                        
         LA    R3,LASTREC                                                       
         CLI   RCSUBPRG,0          IF NOT A HIGH-LEVEL SUMMARY                  
         BNE   HOOK1                                                            
         ZIC   R0,NPRODS           GET PRODUCT NAME                             
         L     RF,APRODTAB                                                      
         CLC   RECPRD,0(RF)        MATCH ON NUMERIC CODE                        
         BE    *+14                                                             
         LA    RF,L'PRODTAB(RF)                                                 
         BCT   R0,*-14                                                          
         DC    H'0'                PRODUCT NOT FOUND                            
         MVC   XHEAD3+9(36),4(RF)  PRODUCT NAME                                 
HOOK1    CLC   QSTART,SPACES                                                    
         BE    HOOK2                                                            
         MVC   XHEAD5+9(8),STRT8   START DATE                                   
         MVI   XHEAD5+17,C'-'                                                   
         MVC   XHEAD5+18(8),END8   END DATE                                     
HOOK2    CLI   RCSUBPRG,2          IF NOT PRINTING REQUEST SUMMARY              
         BE    HOOK6                                                            
         LA    R0,NAREAS           GET AREA/REGION/DISTRICT NAMES               
         L     RF,AAREATAB                                                      
         LA    R1,XHEAD4+133                                                    
         LA    RE,2                                                             
         MVC   WORK,SPACES                                                      
         MVC   WORK(1),RECAREA                                                  
         CLC   3(12,RF),WORK       MATCH ON CHARACTER CODE                      
         BE    *+14                                                             
HOOK4    LA    RF,L'AREATAB(RF)                                                 
         BCT   R0,*-14                                                          
         DC    H'0'                NAME NOT FOUND                               
         MVC   0(5,R1),3(RF)       CODE                                         
         MVC   6(25,R1),15(RF)     AND NAME TO HEADS                            
         CLI   RCSUBPRG,1          IF PRINTING AREA SUMMARY                     
         BE    HOOK6               THEN FINISHED                                
         CH    RE,=H'0'            NO MORE                                      
         BE    HOOK6                                                            
         BCT   RE,*+14                                                          
         MVC   WORK+2(3),RECDIST   NOW GET DISTRICT (LAST TIME)                 
         B     *+10                                                             
         MVC   WORK+1(1),RECREG    NOW GET REGION (OCCURS BEFORE DIST.)         
         LA    R1,198(R1)                                                       
         B     HOOK4                                                            
         SPACE 1                                                                
HOOK6    L     R2,ADBOX            NOW HANDLE BOXES                             
         MVC   BOXCOLS(198),XSPACES                                             
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+6,C'T'      SET ROWS                                     
         MVI   BOXROWS+9,C'M'                                                   
         MVI   BOXROWS+56,C'B'                                                  
         LA    R5,BOXCOLS          SET COLUMNS                                  
         MVI   BL,C'L'                                                          
         MVI   BR,C'R'                                                          
         LA    R1,PACCUMS                                                       
         LA    R0,4                                                             
         MVI   0(R1),C'C'                                                       
         LA    R1,L'PACCUMS(R1)                                                 
         BCT   R0,*-8                                                           
         LA    R1,PACCUMS2                                                      
         LA    R0,12                                                            
         MVI   0(R1),C'C'                                                       
         LA    R1,L'PACCUMS2(R1)                                                
         BCT   R0,*-8                                                           
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         XMOD1 1                                                                
         SPACE 2                                                                
SAVERC   DC    A(0)                                                             
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
SORTC    DS    0D                                                               
         DS    41000C                                                           
         SPACE 2                                                                
         DS    0D                                                               
AREATAB  DS    (NAREAS)CL(15+36)                                                
         SPACE 1                                                                
NAREAS   EQU   300                 MAXIMUM NUMBER OF U/L=3A ACCOUNTS            
         SPACE 2                                                                
PRODTAB  DS    0CL40                                                            
         DS    CL(MAXPRODS*L'PRODTAB)                                           
         SPACE 2                                                                
         BUFF  LINES=500,ROWS=1,COLUMNS=14,FLAVOR=PACKED,KEYLIST=(20,A)X        
               ,COMMENT=39                                                      
         EJECT                                                                  
*              DSECT TO COVER LOCAL WORKING STORAGE                             
         SPACE 1                                                                
ACCXD    DSECT                                                                  
BASE2    DS    F                   SECOND BASE REGISTER                         
ADBOX    DS    A                   A(BOX CSECT)                                 
STATUS   DS    CL1                 SEE EQUATES                                  
ELCODE   DS    CL1                 FOR GETEL                                    
LEVEL    DS    CL1                 LEVEL FOR SUMMARIES                          
STRT1    DS    CL3                 PACKED START DATE                            
END1     DS    CL3                        END DATE                              
STRT2    DS    CL2                 COMPRESSED START DATE                        
END2     DS    CL2                            END DATE                          
STRT8    DS    CL8                 CHARACTER START DATE                         
END8     DS    CL8                           END DATE                           
YEAR     DS    CL1                 YEAR FOR APPROVED CHECKING                   
AGYFLT   DS    CL3                 AGENCY FILTER                                
MEDFLT   DS    CL2                 MEDIA FILTER                                 
BUDFLT   DS    CL2                 PRODUCT FILTER                               
NPRODS   DS    XL1                 NUMBER OF PRODUCTS IN TABLE                  
MAXPRODS EQU   60                  MAXIMUM NUMBER OF PRODUCTS                   
         SPACE 1                                                                
COUNTS   DS    0PL3                ACTIVITY COUNTERS                            
CNTMED   DS    PL3                                                              
CNTTYPE  DS    PL3                                                              
CNTBOT   DS    PL3                                                              
CNTPRD   DS    PL3                                                              
CNTDIST  DS    PL3                                                              
NCOUNTS  EQU   (*-COUNTS)/L'COUNTS                                              
         SPACE 1                                                                
ACCUMS   DS    0CL(NACCS*8)        ACCUMULATORS FOR ALL LEVELS                  
TOTREQ   DS    (NACCS)PL8                                                       
TOTPRD   DS    (NACCS)PL8                                                       
TOTAREA  DS    (NACCS)PL8                                                       
TOTREG   DS    (NACCS)PL8                                                       
TOTDIST  DS    (NACCS)PL8                                                       
TOTBOT   DS    (NACCS)PL8                                                       
TOTMED   DS    (NACCS)PL8                                                       
TOTTYPE  DS    (NACCS)PL8                                                       
NACCUMS  EQU   (*-ACCUMS)/L'ACCUMS                                              
         SPACE 1                                                                
RECORD   DS    CL(RECLNQ)          SORTER/BUFFALO RECORD                        
LASTREC  DS    CL(RECLNQ)          SAVED RECORD FROM SORTER                     
SUMMREC  DS    CL(RECLNQ)          SUMMARY RECORDS TO BUFFALO                   
SUMMLST  DS    CL(RECLNQ)          SAVED SUMMARY RECORDS                        
         SPACE 1                                                                
MYKEY    DS    CL42                KEY FOR MY READS                             
MYKEY2   DS    CL42                ANOTHER KEY FOR MY READS                     
IO       DS    CL1008              I/O AREA FOR MY READS                        
         EJECT                                                                  
*              DSECT TO COVER BUFFALO/SORTER RECORDS                            
         SPACE 1                                                                
RECD     DSECT                                                                  
RECLVL   DS    CL1                 RECORD TYPE FOR BUFFALO                      
RECAREA  DS    CL1                 AREA                                         
RECREG   DS    CL1                 REGION                                       
RECDIST  DS    CL3                 DISTRICT                                     
RECPRD   DS    CL2                 PRODUCT CODE                                 
RECBOT   DS    CL5                 BOTTLER ACN NUMBER                           
RECTYPE  DS    CL2                 BUY TYPE                                     
RECMED   DS    CL2                 MEDIA CODE                                   
RECKAGY  DS    CL3                 AGENCY CODE FOR EXCEPTION REPORT             
*                                  * END OF KEY                                 
RECAGY   DS    CL3                 AGENCY CODE FOR BUDGETS WITH NO SE           
RECBOTNM DS    CL36                BOTTLER NAME                                 
RECACCS  EQU   *                   * BEGINNING OF ACCUMS                        
RECCUR   DS    PL8                 CURRENT AMOUNT BASED ON APPROVE DATE         
RECBUD   DS    PL8                 BUDGET AMOUNT (YTD)                          
RECMTHS  DS    12PL8               APPROVED BY MONTH BASED ON TRNSDATE          
NRECACCS EQU   (*-RECACCS)/8       N'ACCUMS IN RECORD                           
RECLNQ   EQU   *-RECD              SIZE OF RECORD                               
         SPACE 2                                                                
*              DSECT TO COVER PRINT LINE                                        
         SPACE 1                                                                
PRINTD   DSECT                                                                  
BL       DS    CL1                                                              
PDATA    DS    CL27                                                             
PACCUMS  DS    4CL(1+9)            1ST 4 ACCUMS AND LEFT SIDE BOX               
PACCUMS2 DS    12CL(1+7)           2ND 12 ACCUMS AND LEFT SIDE BOX              
BR       DS    CL1                                                              
         SPACE 2                                                                
*              DSECT TO COVER TOTAL ACCUMULATORS (ACCUMS)                       
         SPACE 1                                                                
ACCD     DSECT                                                                  
ACCCUR   DS    PL8                 CURRENT AMOUNT BASED ON APPROVE DATE         
ACCYTD   DS    PL8                 ALL APPROVED THROUGH END DATE                
ACCBUD   DS    PL8                 BUDGET AMOUNT (YTD)                          
ACCBAL   DS    PL8                 BUDGET LESS YEAR-TO-DATE                     
ACCMTHS  DS    12PL8               APPROVED BY MONTH BASED ON TRNSDATE          
NACCS    EQU   (*-ACCD)/8                                                       
         EJECT                                                                  
* ACGENMODES                                                                    
* ACGENBOTH                                                                     
* ACREPWORKD                                                                    
* DDBUFFALOD                                                                    
* DDBIGBOX                                                                      
* DDREPXTRAD                                                                    
* DDREPMASTD                                                                    
* ACBIGPRNTD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE DDBUFFALOD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDREPXTRAD                                                     
       ++INCLUDE DDREPMASTD                                                     
       ++INCLUDE ACBIGPRNTD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005ACREPCX02 05/01/02'                                      
         END                                                                    
