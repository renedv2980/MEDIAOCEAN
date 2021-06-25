*          DATA SET TAREP19    AT LEVEL 054 AS OF 10/18/13                      
*PHASE T70319C,*                                                                
*======================================================================         
* IF THE BALANCE ELEMENTS HAVE STRANGE DATA, (AMOUNTS HAVE DATE)                
* IT COULD BE THE TABLES HAVE OVERFLOWED.  (*TABLES*)                           
* I'VE CHANGED THE 320CL TO 360CL. (2006-08-26)                                 
* I'VE CHANGED THE 360CL TO 440CL. (2007-08-01)                                 
* I'VE CHANGED THE 440CL TO 520CL. (2011-09-16)                                 
* I'VE CHANGED THE 520CL TO 750CL. (2013-10-18)                                 
*======================================================================         
         TITLE 'T70319 - CREATE BALANCE RECORDS'                                
T70319   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70319,R7,RR=R3                                                
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          SYSTEM SPECIFIC WORK                         
         LA    RA,BUFF                                                          
         LA    RA,8(RA)                                                         
         USING TBALD,RA            BALANCE RECORD INTERFACE DSECT               
         SPACE 3                                                                
         L     R1,=A(LASTABX)      ADDRESS OF LAST TABLE                        
         AR    R1,R3                                                            
         ST    R1,ALASTABX                                                      
*******************************************************************             
* PLEASE NOTE THAT THIS CANNOT BE RUN AS AN UPDATIVE SOON JOB IF THE            
* PERIOD IS MORE THAN ONE DAY BECAUSE THIS PROGRAM ALWAYS DOES A                
* READ HIGH FOR THE PREVIOUS BALANCE.  IF THIS IS THE CASE, GO INTO             
* $REP AND REQUEST THE JOB FOR OVERNIGHT PROCESSING.                            
*******************************************************************             
         EJECT                                                                  
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BNE   MX                                                               
         BAS   RE,INIT             INITIALIZE                                   
*                                                                               
M10      MVI   INVFND,C'N'         DEFAULT - NO INVOICES FOUND                  
         BAS   RE,PRBILL           GET INVOICES - BILL DATE SEQUENCE            
         BAS   RE,PRCHK            GET INVOICES - CHECK DATE SEQUENCE           
         CLI   INVFND,C'N'         IF NO INVOICES FOUND                         
         BE    M15                    GET NET REQUESTED DAY                     
         BAS   RE,PBAL             ELSE GET PREVIOUS DAY'S BALANCE REC          
         BAS   RE,SUMTAB           SUM TABLES                                   
         MVI   SEQNO,0             RESET SEQUENCE NUMBER                        
         BAS   RE,SORTTAB          SORT TABLES                                  
         TM    OPTS,OTRACE                                                      
         BNO   M12                                                              
         BAS   RE,TRACTAB          TRACE TABLES                                 
*                                                                               
M12      BAS   RE,CREATE           ADD/CHANGE THE RECORDS                       
*                                                                               
M15      CLC   PENDDTE,THEPDTE     CHECK IF REACHED LAST REQUESTED DATE         
         BE    M20                                                              
*                                                                               
         GOTO1 DATCON,DMCB,(1,THEPDTE),(0,TEMPEDTE)                             
         GOTO1 ADDAY,DMCB,TEMPEDTE,THEEDTE,1                                    
         GOTO1 DATCON,DMCB,(0,THEEDTE),(1,THEPDTE)                              
         MVC   TGTODAY1,THEPDTE         THEPDTE = PROCESSING DATE PWOS          
         MVC   TGTODAY0,THEEDTE                                                 
*                                                                               
         LA    R2,FHEAD                                                         
         MVI   FHEAD,25            SET UP FAKE FIELD HEADER                     
         MVI   FHEAD+5,8                                                        
         MVC   NPERIOD(8),=C'NEXTBDAY'                                          
         LA    R3,BLOCK                                                         
         USING PERVALD,R3                                                       
         GOTO1 PDVAL,DMCB,(R3)                                                  
         MVC   PCHKDTE,PVALPSTA    SET NEXTBDAY IN CHK DATE                     
         CLC   PENDDTE,THEPDTE     CHECK IF REACHED LAST REQUESTED DATE         
         BL    M20                                                              
         BAS   RE,CLEARTAB                                                      
         B     M10                 GET NEXT DAY & START OVER                    
*                                                                               
M20      BAS   RE,PRTOT            PRINT OUT TOTAL # REC CHANGED/ADDED          
*                                                                               
MX       B     XIT                                                              
         EJECT                                                                  
         SPACE 1                                                                
*                                                                               
*        INITIALIZATION                                                         
*                                                                               
         SPACE 1                                                                
INIT     NTR1                                                                   
         L     R3,ATWA             SAVE ADDRS OF TWA ROUTINES                   
         USING T703FFD,R3                                                       
         L     R2,TWADCONS         SAVE ADDRS OF TWADCON ROUTINES               
         USING TWADCOND,R2                                                      
         MVC   PRNTBL,TPRNTBL                                                   
*                                                                               
         BAS   RE,CLEARTAB         CLEAR TABLES                                 
         LA    R1,MYSPECS          SET UP SPECS FOR REPORT                      
         ST    R1,SPECS                                                         
         ZAP   RECAD,=P'0'         CLEAR COUNTERS                               
         ZAP   RECCHA,=P'0'                                                     
         B     XIT                                                              
         DROP  R2,R3                                                            
         SPACE 2                                                                
*                                                                               
*        CLEAR TABLES                                                           
*                                                                               
         SPACE 1                                                                
CLEARTAB NTR1                                                                   
*                                                                               
         LA    R4,6                CLEAR SIX TABLES                             
         LA    R2,DCEMP1                                                        
*                                                                               
CL10     L     R3,=A(EM1TOTL)      CLEAR 2 TABLES AT A TIME                     
         LA    R1,DCEMP1                                                        
         SR    R3,R1               SIZE OF 2 TABLES                             
         XR    RE,RE                                                            
         LR    RF,RE                                                            
*                                                                               
         MVCL  R2,RE                                                            
         BCT   R4,CL10                                                          
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
         SPACE 1                                                                
*                                                                               
*        READ ALL (X'95') BILL DATE PASSIVE POINTERS FOR REQUESTED DATE         
*                                                                               
         SPACE 1                                                                
PRBILL   NTR1                                                                   
*                                                                               
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING TLINPD,R2           INVOICE X'95' PASSIVE PTRS                   
         MVI   TLINPCD,TLINDCDQ                                                 
         MVC   TLINDDTE,THEPDTE    START DATE                                   
         GOTO1 HIGH                                                             
         B     PR20                                                             
*                                                                               
PR10     GOTO1 SEQ                                                              
*                                                                               
PR20     CLC   KEY(TLINDAGY-TLINPD),KEYSAVE                                     
         BNE   PR50                                                             
*                                                                               
         TM    TLINDINV+5,X'80'    CANCELLED INVOICE                            
         BO    PR25                                                             
         CLI   TLINDINV+5,X'00'    LAST BYTE OF CONVERTED INVOICE <> 0          
         BNE   PR10                - IGNORE CONVERTED INVOICES                  
*                                                                               
PR25     BAS   RE,GETINV           IF INVOICE NOT REQUESTED                     
         BNE   PR10                   GET NEXT ONE                              
*                                                                               
         OC    RECAMT,RECAMT       DON'T BOTHER IF AMOUNT TO POST = 0           
         BZ    PR10                                                             
*                                                                               
         MVI   INVFND,C'Y'         AN INVOICE WAS FOUND                         
         TM    STATUS,TAINSCIN     IF CANCELLED - POST TO 3RD COL               
         BO    PR30                                                             
         GOTO1 POSTIT,DMCB,(RECCURR,RECEMP),(C'D',DUEPDTE),(2,RECAMT)           
         B     PR40                                                             
*                                                                               
PR30     GOTO1 POSTIT,DMCB,(RECCURR,RECEMP),(C'D',DUEPDTE),(3,RECAMT)           
*                                  ELSE POST TO 2ND                             
PR40     GOTO1 POSTIT,DMCB,(RECCURR,RECEMP),(C'I',BILLDATE),(2,RECAMT)          
         B     PR10                POST TO INVOICE TABLE                        
*                                                                               
PR50     DS    0H                                                               
*                                                                               
PRX      B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
         SPACE 1                                                                
*                                                                               
*        READ ALL (X'96') CHECK DATE PASSIVE PTRS FOR REQUESTED DATE            
*                                                                               
PRCHK    NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING TLINPD,R2           INVOICE X'96' PASSIVE PTRS                   
         MVI   TLINPCD,TLINKCDQ                                                 
         MVC   TLINKDTE,THEPDTE    START READING WITH CHKS DATED TODAY          
*                                                                               
         GOTO1 HIGH                                                             
         B     PRD20                                                            
*                                                                               
PRD10    GOTO1 SEQ                                                              
*                                                                               
PRD20    CLI   TLINPKEY,TLINKCDQ   TEST STILL CHECK DATE POINTER                
         BNE   PRDX                                                             
         CLC   TLINKDTE,PCHKDTE    READ THROUGH NEXT BUSINESS DAY               
         BH    PRDX                                                             
         CLI   TLINKINV+5,X'80'    CANCELLED INVOICE                            
         BE    PRD25                                                            
         CLI   TLINKINV+5,X'00'    LAST BYTE OF CONVERTED INVOICE <> 0          
         BNE   PRD10               - IGNORE CONVERTED INVOICES                  
*                                                                               
PRD25    BAS   RE,GETINV           IF INVOICE REQUESTED                         
         BNE   PRD10                                                            
         BAS   RE,RUNDATE          ENSURE CORRECT RUN DATE                      
         BNE   PRD10                                                            
         OC    RECAMT,RECAMT       DON'T BOTHER IF AMOUNT TO POST = 0           
         BZ    PRD10                                                            
*                                                                               
         MVI   INVFND,C'Y'         AN INVOICE WAS FOUND                         
         CLC   BILLDATE,THEPDTE    AND IT WAS BILLED TODAY                      
         BL    PRD30               POST IT TO CURRENT                           
         GOTO1 POSTIT,DMCB,(RECCURR,RECEMP),(C'D',DUEPDTE),(5,RECAMT)           
         B     PRD40               ELSE POST IT TO PREVIOUS                     
*                                                                               
PRD30    GOTO1 POSTIT,DMCB,(RECCURR,RECEMP),(C'D',DUEPDTE),(4,RECAMT)           
*                                                                               
PRD40    TM    STATUS,TAINSCIN     IF CANCELLED POST TO 3RD COL                 
         BO    PRD60                                                            
         CLC   BILLDATE,THEPDTE    ELSE IF BILLED TODAY                         
         BL    PRD50                    POST IT TO CURRENT                      
         GOTO1 POSTIT,DMCB,(RECCURR,RECEMP),(C'I',BILLDATE),(5,RECAMT)          
         B     PRD70                                                            
*                                                                               
PRD50    GOTO1 POSTIT,DMCB,(RECCURR,RECEMP),(C'I',BILLDATE),(4,RECAMT)          
         B     PRD70               ELSE POST IT TO PREVIOUS                     
*                                                                               
PRD60    GOTO1 POSTIT,DMCB,(RECCURR,RECEMP),(C'I',BILLDATE),(3,RECAMT)          
*                                                                               
PRD70    B     PRD10                                                            
*                                                                               
PRDX     B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
         SPACE 1                                                                
*                                                                               
*        GET 1ST CHECK RECORD & COMPARE RUN DATE TO PROCESSING DATE             
*                                                                               
         SPACE 1                                                                
RUNDATE  NTR1                                                                   
*                                                                               
         OC    PRUNDTE,PRUNDTE     IF CHK RUN DATE ALREADY GOTTEN FROM          
         BNZ   RD20                INVOICE - DON'T GET IT FROM CHECK            
         BAS   RE,SETCHK           SET CHKFIL FOR READ/SAVE KEY                 
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING TLCKD,R4                                                         
         LA    R3,SVKEY                                                         
         USING TLINPD,R3                                                        
         MVI   TLCKCD,TLCKCDQ      CHECK RECORD CODE                            
         MVC   TLCKAGY,TLINKAGY    AGENCY                                       
         MVC   TLCKINV,TLINKINV    INVOICE NUMBER                               
         GOTO1 HIGH                                                             
         CLC   KEY(TLCKSORT-TLCKD),KEYSAVE                                      
         BE    RD10                NO CHECKS FOR INVOICE                        
         BAS   RE,SETTAL                                                        
         B     NO                                                               
         DROP  R3,R4                                                            
*                                                                               
RD10     GOTO1 GETREC                                                           
         BAS   RE,SETTAL                                                        
         LA    R4,MYIO                                                          
         USING TACDD,R4                                                         
         MVI   ELCODE,TACDELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   NO                                                               
         MVC   PRUNDTE,TACDRUN     RUN DATE                                     
*                                                                               
RD20     CLC   PRUNDTE,THEPDTE                                                  
         BNE   NO                                                               
         B     YES                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*        GET PREVIOUS DAY'S BALANCE RECORD                                      
*                                                                               
         SPACE 1                                                                
PBAL     NTR1                                                                   
         LA    R5,DCEMP1           ADDRESS OF FIRST TABLE                       
         USING TABD,R5                                                          
*                                                                               
PB05     C     R5,ALASTABX         IF WE REACHED LAST TABLE                     
         BNL   PBX                                                              
         CLC   0(L'CEMP,R5),SPACES    OR NOTHING IN NEXT TABLE                  
         BNH   PBX                 THEN EXIT                                    
*                                                                               
         XC    KEY,KEY             ELSE SET KEY                                 
         LA    R2,KEY                                                           
         USING TLBAD,R2            BALANCE RECORD                               
         MVI   TLBACD,TLBACDQ                                                   
         MVC   TLBADATE,THEPDTE    REQUESTED DATE - COMPLEMENTED                
         XC    TLBADATE,COMPLM                                                  
         MVC   TLBACURR,0(R5)      CURRENCY                                     
         MVC   TLBAEMP,1(R5)       EMPLOYER                                     
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         BAS   RE,CKTRACE1                                                      
         B     PB20                                                             
*                                                                               
PB10     GOTO1 SEQ                 READ TILL REACH THE PREVIOUS (NEXT -         
         BAS   RE,CKTRACE                                                       
*                                       COMPLEMENTED) BALANCE RECORD            
PB20     CLC   TLBAKEY(TLBACURR-TLBAD),KEYSAVE                                  
         BE    PB10                                                             
         CLI   TLBAKEY,TLBACDQ     MAKE SURE IT'S STILL A BALANCE REC           
         BNE   PB60                                                             
*                                  IS IT CORRECT CURRENCY/EMPLOYER              
         CLC   TLBACURR(TLBASEQ-TLBACURR),KEYSAVE+TLBACURR-TLBAD                
         BNE   PB10                                                             
         MVC   KEYSAVE,TLBAKEY     RESET KEYSAVE FOR NEXT 'PHYSICAL'            
*                                  RECORD                                       
         MVC   RECCURR,TLBACURR    SET VALUES FROM KEY                          
         MVC   RECEMP,TLBAEMP                                                   
*                                                                               
PB25     MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
*                                                                               
         TM    OPTS,OTRACE                                                      
         BNO   PB29                                                             
         GOTO1 MYTRACE,DMCB,=C'PREV BAL REC',AIO                                
*                                                                               
PB29     L     R4,AIO             GET EACH ELEMENT IN RECORD                    
         USING TABAD,R4                                                         
         MVI   ELCODE,TABAELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   PB55                                                             
*                                                                               
PB30     L     R2,TABABPRE         A + B + C - D - E = F                        
         A     R2,TABABCUR                                                      
         A     R2,TABABCAN                                                      
         S     R2,TABAPPRE                                                      
         S     R2,TABAPCUR                                                      
         LTR   R2,R2               IF SUM = 0 DON'T USE IT                      
         BZ    PB50                                                             
         ST    R2,TAMT                                                          
         CLI   TABATYPE,C'D'                                                    
         BNE   PB40                                                             
         GOTO1 POSTIT,DMCB,(RECCURR,RECEMP),(C'D',TABADATE),(1,TAMT)            
         B     PB50                                                             
*                                                                               
PB40     GOTO1 POSTIT,DMCB,(RECCURR,RECEMP),(C'I',TABADATE),(1,TAMT)            
*                                                                               
PB50     BAS   RE,NEXTEL           GET NEXT ELEMENT                             
         BE    PB30                                                             
*                                                                               
PB55     GOTO1 SEQ                 CHECK IF ANOTHER 'PHYSICAL' RECORD           
         TM    OPTS,OTRACE                                                      
         BNO   PB59                                                             
         GOTO1 MYTRACE,DMCB,=C'PREV BAL KEY',KEY                                
*                                                                               
PB59     CLC   KEY(TLBASEQ-TLBAD),KEYSAVE                                       
         BE    PB25                                                             
*                                                                               
PB60     L     R3,=A(EM1TOTL)      BUMP TO NEXT TABLE                           
         LA    R1,DCEMP1                                                        
         SR    R3,R1                                                            
         AR    R5,R3                                                            
         B     PB05                                                             
*                                                                               
PBX      MVC   AIO,AIO1                                                         
         B     XIT                                                              
         DROP  R2,R4,R5                                                         
         EJECT                                                                  
*                                                                               
*        CHECK IF NEED TO TRACE RECORDS                                         
*                                                                               
CKTRACE  NTR1                                                                   
         TM    OPTS,OTRACE                                                      
         BNO   CKTX                                                             
         GOTO1 MYTRACE,DMCB,=C'PREV BAL KEY',KEY                                
*                                                                               
CKTX     B     XIT                                                              
         SPACE 2                                                                
*                                                                               
*        CHECK IF NEED TO TRACE RECORDS                                         
*                                                                               
CKTRACE1 NTR1                                                                   
         TM    OPTS,OTRACE                                                      
         BNO   CKTX1                                                            
         GOTO1 MYTRACE,DMCB,=C'CURRENT KEY',KEY                                 
*                                                                               
CKTX1    B     XIT                                                              
         EJECT                                                                  
         SPACE 1                                                                
*                                                                               
*        CREATE BALANCE RECORDS                                                 
*                                                                               
         SPACE 1                                                                
CREATE   NTR1                                                                   
*                                                                               
         USING TABD,R5                                                          
         LA    R5,DCEMP1           ADDRESS OF FIRST TABLE                       
         LR    R6,R5               SAVE ADDRESS OF DUE TABLE                    
*                                                                               
CR10     CLC   0(L'CEMP,R5),SPACES                                              
         BNH   CRX                 IF NO MORE INPUT - EXIT                      
         GOTO1 TRACE,DMCB,(R5),4,=C'CREATE CURR/EMP',15                         
         CLC   SVCEMP,0(R5)        IF CURRENCY OR EMPLOYER CHANGED              
         BE    CR22                                                             
         MVI   SEQNO,0             RESET SEQUENCE NUMBER                        
*                                                                               
CR22     MVC   SVCEMP,0(R5)        SAVE CURRENCY/EMPLOYER                       
*                                                                               
         MVC   AIO,AIO1                                                         
         L     R2,AIO                                                           
         LA    R3,4000             CLEAR IO AREA                                
         XR    RE,RE                                                            
         LR    RF,RE                                                            
         MVCL  R2,RE                                                            
*                                                                               
         BAS   RE,SETKEY           SET KEY                                      
         USING TLBAD,R2            BALANCE RECORD                               
         LA    R2,KEY                                                           
*                                                                               
CR25     L     R4,AIO                                                           
         MVC   0(L'TLRCKEY,R4),0(R2)     SET KEY IN AIO                         
*                                                                               
         LA    R5,EM1DSTR(R5)      SET R5 = A(DUE TABLE)                        
         MVI   ELTYPE,TABATDUE     SET ELEMENT TYPE                             
         BAS   RE,INSELEM          INSERT ELEMENTS IN AIO                       
*                                                                               
         LR    R5,R6               RESET TO START OF DUE TABLE                  
         LH    R1,=AL2(EM1BSTR)                                                 
         AR    R5,R1               SET R5  = A(BILL TABLE)                      
         MVI   ELTYPE,TABATINV     SET ELEMENT TYPE                             
         BAS   RE,INSELEM          INSERT ELEMENTS IN AIO                       
*                                                                               
         GOTO1 ACTVIN,DMCB,0       ADD ACTIVITY ELEMENT                         
         BAS   RE,ADREC            ADD/REWRITE RECORD                           
         BAS   RE,DELETE           DELETE SUBSEQUESENT BALANCE RECS             
*                                                                               
         LR    R5,R6               RESTORE ADDRESS OF DUE TABLE                 
         L     R3,=A(EM1TOTL)      BUMP TO NEXT TABLE                           
         LA    R1,DCEMP1                                                        
         SR    R3,R1                                                            
         AR    R5,R3                                                            
         LR    R6,R5               SAVE A(DUE TABLE)                            
         C     R5,ALASTABX         IF WE REACHED LAST TABLE - EXIT              
         BL    CR10                                                             
*                                                                               
CRX      B     XIT                                                              
         DROP  R2,R5                                                            
         EJECT                                                                  
*                                                                               
*        INSERT ELEMENTS OF TYPE ELTYPE INTO AIO                                
*        R5 - A(TABLE START)    SVCEMP - SAVED CURRENCY/EMPLOYER                
*                                                                               
         USING TABD,R5                                                          
INSELEM  NTR1                                                                   
         USING TABAD,R4            BALANCE ELEMENT                              
         LA    R4,ELEM                                                          
         XC    ELEM,ELEM                                                        
*                                                                               
IN10     OC    TABPREV(20),TABPREV IF THE ELEMENT IS EMPTY                      
         BZ    IN20                DON'T ADD IT                                 
         MVI   TABAEL,TABAELQ      ELEMENT CODE                                 
         MVI   TABALEN,TABALNQ     ELEMENT LENGTH                               
         MVC   TABATYPE,ELTYPE     ELEMENT TYPE                                 
         MVC   TABADATE,TABDATE    DATE PWOS                                    
         MVC   TABABPRE,TABPREV    PREVIOUS BILL AMOUNT                         
         MVC   TABABCUR,TABCURR    CURRENT BILL AMOUNT                          
         MVC   TABABCAN,TABCANC    CANCELLED BILL AMOUNT                        
         MVC   TABAPPRE,TABPPRE    PAYROLL PREVIOUS BILLS                       
         MVC   TABAPCUR,TABPCUR    PAYROLL CURRENT BILLS                        
         GOTO1 ADDELEM                                                          
*                                                                               
IN20     LA    R5,TABLEN(R5)                                                    
         OC    0(TABLEN,R5),0(R5)  END OF INPUT FOR THIS TABLE                  
         BZ    INX                                                              
         L     R2,AIO                                                           
         USING TLBAD,R2                                                         
         LH    R2,TLBALEN          ENSURE RECORD HAS ENOUGH ROOM FOR            
         DROP  R2                                                               
         LA    R3,2000             ANOTHER ELEMENT                              
         LA    R1,TABALNQ+TAACLNQ  AND ROOM FOR ACTIVITY ELEMENT                
         SR    R3,R1                                                            
         CR    R2,R3                                                            
         BL    IN10                                                             
         GOTO1 ACTVIN,DMCB,0       IF NOT - ADD ACTIVITY ELEMENT                
         BAS   RE,ADREC                     ADD/PUT RECORD                      
*                                                                               
         MVC   AIO,AIO1                                                         
         L     R2,AIO                                                           
         LA    R3,4000             CLEAR IO AREA                                
         XR    RE,RE                                                            
         LR    RF,RE                                                            
         MVCL  R2,RE                                                            
*                                                                               
         BAS   RE,SETKEY           SET BALANCE RECORD KEY                       
         USING TLBAD,R2            BALANCE RECORD                               
         LA    R2,KEY                                                           
*                                                                               
IN40     L     R3,AIO                                                           
         MVC   0(L'TLRCKEY,R3),0(R2)     SET KEY IN AIO                         
         B     IN10                                                             
*                                                                               
INX      B     XIT                                                              
         DROP  R4,R5                                                            
         EJECT                                                                  
         SPACE 1                                                                
*                                                                               
*        GET INFO INVOICE RECORD AND INFO FROM IT                               
*                                                                               
         SPACE 1                                                                
GETINV   NTR1                                                                   
         GOTO1 GETREC              GET INVOICE                                  
         L     R4,AIO                                                           
         USING TAPDD,R4                                                         
         MVI   ELCODE,TAPDELQ                                                   
         BAS   RE,GETEL            GET INVOICE PAYMENT DETAILS ELEM             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    TAPDOPT3,TAPDODUM   IF THIS IS A DUMMY INVOICE                   
         BO    NO                                                               
         TM    TAPDOPT3,TAPDOGRY   OR THIS IS A GREY P & G INV                  
         BO    NO                                                               
         TM    TAPDSTA2,TAPDSSUB   OR THIS IS SUBSIDIARY INVOICE                
         BO    NO                                                               
         TM    TAPDPST1,TAPDPBNP   OR THIS IS A BILL NO PAYROLL INV             
         BO    NO                     SKIP IT                                   
         TM    TAPDSTAT,TAPDSCAN   IF THIS IS A US$ PAYMENT                     
         BO    GET10                                                            
         TM    TAPDPST2,TAPDPEUR   IF THIS IS A US$ PAYMENT                     
         BO    GET15                                                            
         MVI   RECCURR,C'U'        SET CURRENCY = US                            
         B     GET20                                                            
*                                                                               
GET10    MVI   RECCURR,C'C'        SET CURRENCY = CAN                           
         B     GET20                                                            
*                                                                               
GET15    MVI   RECCURR,C'E'        SET CURRENCY = EURO                          
         L     R4,AIO                                                           
         MVI   ELCODE,TAEUELQ                                                   
         BAS   RE,GETEL            GET INVOICE PAYMENT DETAILS ELEM             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
GET20    CLI   FILTCURR,C' '       NO FILTER                                    
         BNH   GET25                                                            
         CLC   FILTCURR,RECCURR    FILTER ON CURRENCY, MUST MATCH               
         BNE   NO                                                               
GET25    CLC   FILTEMP,SPACES      IF FILTERING BY EMPLOYER                     
         BNH   GET30                                                            
         CLC   TAPDEOR,FILTEMP     AND DOESN'T MATCH - SKIP RECORD              
         BNE   NO                                                               
*                                                                               
GET30    MVC   RECEMP,TAPDEOR      EMPLOYER OF RECORD                           
         L     R2,TAPDPAYI         INDIVIDUALS PAYMENT AMT                      
         A     R2,TAPDPAYC         + CORP PAYMENT AMT                           
         A     R2,TAPDREXP         + REIMBURSED EXPENSES                        
         ST    R2,RECAMT                                                        
         DROP  R4                                                               
*                                                                               
         L     R4,AIO                                                           
         USING TAIND,R4                                                         
         MVI   ELCODE,TAINELQ                                                   
         BAS   RE,GETEL            GET INVOICE STATUS ELEM                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    TAINSTA2,TAINSADJ   IF THIS IS A PAYROLL ADJUSTMENT              
         BO    NO                     SKIP THE INVOICE                          
         MVC   STATUS,TAINSTAT     RETURN STATUS BYTE                           
         MVC   BILLDATE,TAINBDTE          BILL DATE                             
         MVC   PRUNDTE,TAINCKRN     CHECK RUN DATE                              
         DROP  R4                                                               
*                                                                               
         L     R4,AIO                                                           
         USING TADDD,R4                                                         
         MVI   ELCODE,TADDELQ                                                   
         BAS   RE,GETEL            GET DUE DATE ELEM                            
         BNE   GET40                                                            
         MVC   DUEPDTE,TADDDATE    CURRENT DUE DATE                             
         OC    TADDPREV,TADDPREV   IF THERE WAS A PREVIOUS DUE DATE             
         BZ    GET40                                                            
         MVC   DUEPDTE,TADDPREV    OVERRIDE CURRENT ONE                         
*                                                                               
GET40    TM    STATUS,TAINSCIN     IF CANCELLEE - GET ORIGINAL BILL             
         BNO   GETX                    DATE TO POST TO                          
         L     R4,AIO                                                           
         USING TAOBD,R4                                                         
         MVI   ELCODE,TAOBELQ                                                   
         BAS   RE,GETEL            GET DUE DATE ELEM                            
         BNE   GETX                                                             
         MVC   BILLDATE,TAOBBDTE   ORIGINAL BILL DATE                           
*                                                                               
GETX     B     YES                                                              
*                                                                               
         EJECT                                                                  
         DROP  R4                                                               
         SPACE 1                                                                
*                                                                               
*        POST AMOUNT TO TABLE                                                   
*        P1 - BYTE 0      ('U','C','E' - US, CANADIAN OR EUROS)                 
*        P1 - BYTES 1-3  A(EMPLOYER)                                            
*        P2 - BYTE 0      (C'D', C'I' DUE DATE/INVOICE DATE ANALYSIS)           
*        P2 - BYTES 1-3  A(DATE)                                                
*        P3 - BYTE 0      (COLUMN NUMBER TO POST TO)                            
*        P3 - BYTES 1-3  A(AMOUNT)                                              
*                                                                               
         SPACE 1                                                                
POSTIT   NTR1                                                                   
         MVC   CEMP(1),0(R1)       REQUESTED CURRENCY                           
         L     R3,0(R1)                      EMPLOYER                           
         MVC   CEMP+1(3),0(R3)                                                  
         MVC   TABTYPE,4(R1)                 ANALYSIS TYPE                      
         L     R3,4(R1)                      A(DATE)                            
         MVC   TEMPDATE,0(R3)                                                   
         MVC   BYTE,8(R1)                    COLUMN TO POST TO                  
         L     R3,8(R1)                                                         
         MVC   FULL,0(R3)                    A(AMOUNT)                          
*                                                                               
         LA    R5,DCEMP1           ADDRESS OF FIRST TABLE                       
         USING TABD,R5                                                          
         LR    R6,R5               KEEP A(BEG OF TABLE)                         
*                                                                               
PO10     C     R5,ALASTABX         IF WE REACHED END OF LAST TABLE              
         BL    *+6                                                              
         DC    H'0'                THEN DIE                                     
*                                                                               
         LA    R1,4(R5)                                                         
         ST    R1,TACNTR           A(TABLE COUNTER)                             
*                                                                               
         CLC   CEMP,0(R5)          ELSE IF WE REACHED THE CORRECT ENTRY         
         BE    PO30                        THEN - USE IT                        
         CLC   0(L'DCEMP1,R5),SPACES    OR IF WE REACHED AN EMPTY               
         BNH   PO20                        ENTRY - USE IT                       
         L     R3,=A(EM1TOTL)      BUMP TO NEXT TABLE                           
         LA    R1,DCEMP1                                                        
         SR    R3,R1                                                            
         AR    R5,R3                                                            
         B     PO10                                                             
*                                                                               
PO20     MVC   0(L'CEMP,R5),CEMP   SET CURRENCY & EMPLOYER                      
*                                                                               
PO30     LR    R6,R5                                                            
         LA    R5,EM1DSTR(R5)      START OF TABLE                               
         LH    R1,=AL2(DCEND)                                                   
         AR    R1,R5               START OF INVOICE TABLE                       
         ST    R1,TABEND           A(END OF TABLE)                              
         CLI   TABTYPE,C'D'        DUE DATE ANALYSIS                            
         BE    PO40                                                             
         CLI   TABTYPE,C'I'        INVOICE DATE ANALYSIS                        
         BE    *+6                                                              
         DC    H'0'                MUST BE                                      
         LR    R5,R6                                                            
         LH    R1,=AL2(EM1BSTR)                                                 
         AR    R5,R1               START OF INVOICE TABLE                       
         LR    R6,R5                                                            
         LH    R1,=AL2(DCEND)                                                   
         AR    R1,R5               START OF INVOICE TABLE                       
         ST    R1,TABEND           A(END OF TABLE)                              
*                                                                               
         LR    R1,R5                                                            
         SH    R1,=H'4'                                                         
         ST    R1,TACNTR           A(TABLE COUNTER)                             
*                                                                               
PO40     OC    0(TABLEN,R5),0(R5)  END OF TABLE                                 
         BZ    PO50                                                             
         CLC   TABDATE,TEMPDATE    REACHED CORRECT DATE                         
         BE    PO60                                                             
         LA    R5,TABLEN(R5)       TRY NEXT ENTRY                               
         C     R5,TABEND           IF NOT END OF CURRENT TABLE                  
         BNH   PO40                   TRY NEXT ENTRY                            
         DC    H'0'                                                             
*                                                                               
PO50     MVC   TABDATE,TEMPDATE    SET DATE                                     
         L     R2,TACNTR           R2 = A(ELEMENT COUNTER)                      
         SR    R1,R1                                                            
         ICM   R1,3,0(R2)                                                       
         LA    R1,1(R1)            INCREMENT NUM OF ENTRIES IN TABLE            
         STCM  R1,3,0(R2)                                                       
*                                                                               
PO60     ZIC   R2,BYTE                                                          
         BCTR  R2,0                (COL - 1) * 4 =                              
         SLL   R2,2                     DISPLACEMENT INTO TABLE                 
         AR    R5,R2               A(REQUESTED ACCUMULATOR)                     
         LA    R5,4(R5)             + DATE                                      
         L     R2,0(R5)                                                         
         L     R4,FULL             ADD AMOUNT                                   
         AR    R2,R4                                                            
         ST    R2,0(R5)                                                         
*                                                                               
POX      B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
         SPACE 1                                                                
*                                                                               
*        SUM TABLES ACROSS ROWS                                                 
*                                                                               
         SPACE 1                                                                
SUMTAB   NTR1                                                                   
*                                                                               
         USING TABD,R5                                                          
         LA    R5,DCEMP1           ADDRESS OF FIRST TABLE                       
         LR    R2,R5               SAVE A(DUE TABLE)                            
*                                                                               
ST10     CLC   0(L'CEMP,R5),SPACES                                              
         BNH   STX                 NO MORE INPUT                                
*                                                                               
         LA    R5,EM1DSTR(R5)      START OF TABLE                               
         BAS   RE,STAB1            SUM DUE DATE TABLE                           
         LR    R5,R2                                                            
         LH    R1,=AL2(EM1BSTR)                                                 
         AR    R5,R1               SET R5 = A(BILL TABLE)                       
         BAS   RE,STAB1            SUM BILL TABLE                               
         LR    R5,R2               RESTORE ADDRESS OF DUE TABLE                 
         L     R3,=A(EM1TOTL)      BUMP TO NEXT TABLE                           
         LA    R1,DCEMP1                                                        
         SR    R3,R1                                                            
         AR    R5,R3                                                            
         LR    R2,R5               SAVE A(DUE TABLE)                            
         C     R5,ALASTABX         IF WE REACHED LAST TABLE - EXIT              
         BL    ST10                                                             
*                                                                               
STX      B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
*                                                                               
*        SUM A SINGLE TABLE                                                     
*        R5 - A(TABLE START)                                                    
*                                                                               
         SPACE 1                                                                
         USING TABD,R5                                                          
STAB1    NTR1                                                                   
*                                                                               
S10      L     R2,TABPREV          A +                                          
         A     R2,TABCURR          B +                                          
         A     R2,TABCANC          C -                                          
         S     R2,TABPPRE          D -                                          
         S     R2,TABPCUR          E =                                          
         ST    R2,TABTOTAL         F                                            
*                                                                               
         LA    R5,TABLEN(R5)       NEXT ENTRY IN TABLE                          
         OC    0(TABLEN,R5),0(R5)  END OF INPUT FOR THIS TABLE                  
         BNZ   S10                                                              
*                                                                               
         B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
*                                                                               
*        SET FILE TO TALFILE AND RESTORE KEY                                    
*                                                                               
SETTAL   NTR1                                                                   
         MVC   KEY(L'SVKEY),SVKEY    RE-SET KEY                                 
         MVC   KEYSAVE(L'SVKEY),SVKEY                                           
         MVC   SYSDIR,=CL8'TALDIR'   RE-SET READ FOR TALFILE                    
         MVC   SYSFIL,=CL8'TALFILE'                                             
         MVC   AIO,AIO1                                                         
         B     XIT                                                              
         SPACE 3                                                                
*                                                                               
*        SET FILE TO CHKFILE AND SAVE KEY                                       
*                                                                               
SETCHK   NTR1                                                                   
         MVC   SVKEY,KEY                                                        
         MVC   SYSDIR,=CL8'CHKDIR'   SET READ FOR CHKFILE                       
         MVC   SYSFIL,=CL8'CHKFIL'                                              
         LA    R1,MYIO                                                          
         ST    R1,AIO                                                           
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        SET THE BALANCE RECORD KEY                                             
*                                                                               
SETKEY   NTR1                                                                   
         USING TLBAD,R2            BALANCE RECORD                               
         LA    R2,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   TLBACD,TLBACDQ                                                   
         MVC   TEMPDATE,THEPDTE    REQUESTED DATE - COMPLEMENTED                
         XC    TEMPDATE,COMPLM                                                  
         MVC   TLBADATE,TEMPDATE                                                
         MVC   TLBACURR(L'CEMP),SVCEMP   CURRENCY/EMPLOYER                      
         MVC   TLBASEQ,SEQNO             SEQUENCE NUMBER                        
SETKX    B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        SORT TABLES BY DATE                                                    
*                                                                               
SORTTAB  NTR1                                                                   
         LA    R4,6                SORT SIX TABLES                              
         LA    R2,EM1DUE           1ST TABLE                                    
         LA    R6,DCEMP1           1ST CURR/EMP                                 
         L     R3,=A(EM1TOTL)      R3 = LENGTH OF 2 TABLES                      
         SR    R3,R6                                                            
*                                                                               
SORT10   LR    R1,R2                                                            
         SH    R1,=H'4'            NUMBER OF ELEMENTS FOUND                     
         SR    R5,R5                                                            
         ICM   R5,3,0(R1)          IF NO INPUT - DON'T SORT                     
         BZ    SORT20                                                           
         GOTO1 QSORT,DMCB,(R2),(R5),TABLEN,L'TABDATE,0                          
*                                  DUE DATE TABLE                               
SORT20   LH    R1,=AL2(EM1BSTR)                                                 
         AR    R1,R6               A(2ND TABLE)                                 
         LR    R2,R1                                                            
         LR    R1,R2                                                            
         SH    R1,=H'4'            NUMBER OF ELEMENTS FOUND                     
         SR    R5,R5                                                            
         ICM   R5,3,0(R1)          IF NO INPUT - DON'T SORT                     
         BZ    SORT30                                                           
         GOTO1 QSORT,DMCB,(R2),(R5),TABLEN,L'TABDATE,0                          
*                                                                               
SORT30   AR    R6,R3               NEXT TABLE                                   
         LA    R2,EM1DSTR(R6)                                                   
         BCT   R4,SORT10                                                        
*                                                                               
SORTX    B     XIT                                                              
         EJECT                                                                  
         SPACE 1                                                                
*                                                                               
*        ADD/PUT DIRECTORY POINTER AND RECORD TO FILE                           
*                                                                               
         SPACE 1                                                                
ADREC    NTR1                                                                   
         LA    R2,KEY                                                           
         USING TLDRD,R2                                                         
         MVI   FND,C'N'                  DEFAULT TO RECORD NOT FOUND            
         OI    DMINBTS,X'08'             READ DELETED RECORDS TOO               
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLBAKEY),KEYSAVE    IS RECORD ALREADY ON FILE              
         BE    ADD02                                                            
         MVC   KEY,KEYSAVE               RESET KEY TO WHAT WE WANT              
         B     ADD10                                                            
*                                                                               
ADD02    MVI   FND,C'Y'                  YES - IT'S FOUND                       
         TM    TLDRSTAT,X'80'            IF DIRECTORY MARKED DELETED            
         BZ    ADD04                                                            
         XI    TLDRSTAT,X'80'            UNMARK IT                              
         GOTO1 WRITE                     AND WRITE IT BACK                      
*                                                                               
ADD04    MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                    PREVENT GETREC/PUTREC SYNDROME         
         MVC   AIO,AIO1                                                         
*                                                                               
ADD10    LA    R3,L'TLDRREC              SET LENGTH OF RECORD                   
         CLI   FND,C'Y'                  ADD OR WRITE RECORD                    
         BNE   ADD20                                                            
         GOTO1 MYTRACE,DMCB,=C'CHANGE  BALANCE RECORD',KEY                      
         B     ADD30                                                            
*                                                                               
ADD20    GOTO1 MYTRACE,DMCB,=C'ADD BALANCE RECORD',KEY                          
*                                                                               
ADD30    GOTO1 MYTRACE,DMCB,=C'CREATE BALANCE RECORD',AIO                       
*                                                                               
ADD40    CLI   FND,C'Y'            ADD OR RE-WRITE RECORD                       
         BNE   ADD50                                                            
         GOTO1 PUTREC              RE-WRITE RECORD                              
         AP    RECCHA,=P'1'                                                     
         B     ADDX                                                             
*                                                                               
ADD50    GOTO1 ADDREC              ADD RECORD                                   
         AP    RECAD,=P'1'                                                      
*                                                                               
ADDX     NI    DMINBTS,X'F7'                                                    
         ZIC   R1,SEQNO                                                         
         LA    R1,1(R1)            INCREMENT SEQUENCE NUMBER                    
         STC   R1,SEQNO                                                         
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*              ROUTINE DELETES SUBSEQUENT BALANCE RECORDS                       
*                                                                               
         SPACE 1                                                                
DELETE   NTR1                                                                   
         L     R3,AIO              R3=A(FILE RECORD)                            
         USING TLBAD,R3                                                         
         MVI   RDUPDATE,C'Y'                                                    
DEL2     GOTO1 HIGH                RE-READ RECORD WE JUST WROTE                 
         NI    DMINBTS,X'F7'       TURN OFF READ DELETED                        
         MVI   RDUPDATE,C'Y'       READ FOR UPDATE                              
         GOTO1 SEQ                 GET NEXT                                     
*                                                                               
         CLC   KEY(TLBASEQ-TLBAD),KEYSAVE  TEST STILL SAME BALANCE REC          
         BNE   XIT                                                              
         MVI   RDUPDATE,C'Y'       READ FOR UPDATE                              
         GOTO1 GETREC              GET THE RECORD                               
         OI    TLBASTAT,X'80'      MARK IT DELETED                              
         GOTO1 PUTREC              AND WRITE IT BACK                            
         GOTO1 MYTRACE,DMCB,=C'DELETE  BALANCE RECORD',AIO                      
*                                                                               
         OI    KEY+TLDRSTAT-TLDRD,X'80'  MARK DIRECTORY DELETED                 
         GOTO1 WRITE                     AND WRITE IT BACK                      
         GOTO1 MYTRACE,DMCB,=C'DELETE  BALANCE KEY',KEY                         
         OI    DMINBTS,X'08'       SET TO READ DELETED                          
         B     DEL2                LOOK FOR MORE                                
         EJECT                                                                  
         SPACE 1                                                                
*                                                                               
*        PRINT OUT TOTALS                                                       
*                                                                               
PRTOT    NTR1                                                                   
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         LA    R2,COUNTS           ADDRESS OF COUNTERS                          
         LA    R3,NCOUNTS          NUMBER OF COUNTERS                           
         L     R4,=A(LTRECTAB)                                                  
*                                                                               
PRTOT10  OI    3(R2),X'0F'         TURN SIGN BIT                                
         UNPK  P(7),0(4,R2)        UNPACK THE NUMBER INTO PRINT LINE            
         MVC   P+9(L'LTRECTAB),0(R4)                                            
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R2,L'COUNTS(R2)     BUMP TO NEXT COUNTER                         
         LA    R4,L'LTRECTAB(R4)                                                
         BCT   R3,PRTOT10                                                       
*                                                                               
PRTX     B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        TRACE RECORDS                                                          
*                                                                               
MYTRACE  NTR1                                                                   
         LM    R2,R3,0(R1)         R2=A(LITERAL), R3=A(I/O AREA)                
         ZIC   R4,0(R1)            R4=L'LITERAL                                 
         GOTO1 TRACE,DMCB,(R3),0,(R2),(R4)                                      
MYTX     B     XIT                                                              
         SPACE 2                                                                
*                                                                               
*        TRACE TABLES                                                           
*                                                                               
TRACTAB  NTR1                                                                   
         LA    R4,6                NUMBER OF TABLES                             
*                                                                               
         LA    R5,DCEMP1           1ST CURR/EMP                                 
         L     R6,=A(EM1TOTL)      R6 = LENGTH OF 2 TABLES                      
         SR    R6,R5                                                            
*                                                                               
TR10     GOTO1 TRACE,DMCB,(R5),4,=C'CURR/EMP',8                                 
         LA    R2,EM1DSTR(R5)      A(1ST TABLE)                                 
         LR    R1,R2                                                            
         SH    R1,=H'4'                                                         
         SR    R3,R3                                                            
         ICM   R3,3,0(R1)          NUMBER OF ENTRIES                            
         BZ    TR30                                                             
*                                                                               
TR20     GOTO1 TRACE,DMCB,(R2),TABLEN,=C'DUE TABLE',9                           
         LA    R2,TABLEN(R2)       BUMP TO NEXT TABLE ENTRY                     
         BCT   R3,TR20                                                          
*                                                                               
TR30     LH    R1,=AL2(EM1BSTR)                                                 
         AR    R1,R5               A(INV TABLE)                                 
         LR    R2,R1                                                            
         LR    R1,R2                                                            
         SH    R1,=H'4'                                                         
         SR    R3,R3                                                            
         ICM   R3,3,0(R1)          NUMBER OF ENTRIES                            
         BZ    TR50                                                             
*                                                                               
TR40     GOTO1 TRACE,DMCB,(R2),TABLEN,=C'BILL TABLE',10                         
         LA    R2,TABLEN(R2)       BUMP TO NEXT TABLE ENTRY                     
         BCT   R3,TR40                                                          
*                                                                               
TR50     AR    R5,R6               NEXT TABLE                                   
         BCT   R4,TR10                                                          
*                                                                               
TRX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        SPECS                                                                  
*                                                                               
MYSPECS  DS    0H                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,107,REPORT                                                    
         SSPEC H1,123,PAGE                                                      
         SSPEC H2,107,REQUESTOR                                                 
         DC    X'00'                                                            
         SPACE 5                                                                
*                                                                               
YES      SR    RC,RC               SET CONDITION CODE                           
NO       LTR   RC,RC                                                            
*                                                                               
XIT      XIT1                                                                   
         SPACE 3                                                                
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*                                                                               
*  CONSTANTS                                                                    
*                                                                               
COMPLM   DC    X'FFFFFF'                                                        
*                                                                               
SEQNO    DS    XL1                 SEQUENCE NUMBER                              
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
         SPACE 1                                                                
*                                                                               
*        TABLE OF COUNTERS                                                      
*                                                                               
LTRECTAB DS    0CL15                                                            
LTRECAD  DC    C'RECORDS ADDED  '                                               
LTRECCH  DC    C'RECORDS CHANGED'                                               
         DC    X'FF'                                                            
         SPACE 2                                                                
*                                                                               
*        TABLES FOR CURRENCY/EMPLOYER DUE DATE ANALYSIS                         
*        AND BILL DATE ANALYSIS                                                 
*                                                                               
         DC    CL8'*TABLES*'                                                    
         DS    0D                                                               
DCEMP1   DS    CL4                 CURRENCY/EMPLOYER                            
DCNUM1   DS    XL2                 NUMBER OF ENTRIES IN TABLE                   
         DS    CL2                                                              
EM1DSTR  EQU   *-DCEMP1                                                         
EM1DUE   DS    750CL(TABLEN)       1ST CURR/EMPLOYER DUE DATE ANALYSIS          
DCEND    EQU   *-EM1DUE                                                         
DCEND1   EQU   *-DCEMP1                                                         
*                                                                               
EMNUM1   DS    XL2                 NUMBER OF ENTRIES IN TABLE                   
         DS    CL2                                                              
EM1BSTR  EQU   *-DCEMP1            START OF INVOICE ANALYSIS TABLE              
EM1BIL   DS    750CL(TABLEN)       1ST EMPLOYER BILL DATE ANALYSIS              
EMEND    EQU   *-EMNUM1                                                         
EM1TOTL  EQU   *                   END OF SET OF TABLES                         
*                                                                               
         DS    CL4                                                              
         DS    XL2                 NUMBER OF ENTRIES IN TABLE                   
         DS    CL2                                                              
EM2DUE   DS    750CL(TABLEN)                                                    
         DS    XL2                 NUMBER OF ENTRIES IN TABLE                   
         DS    CL2                                                              
EM2BIL   DS    750CL(TABLEN)                                                    
*                                                                               
         DS    CL4                                                              
         DS    XL2                 NUMBER OF ENTRIES IN TABLE                   
         DS    CL2                                                              
EM3DUE   DS    750CL(TABLEN)                                                    
         DS    XL2                 NUMBER OF ENTRIES IN TABLE                   
         DS    CL2                                                              
EM3BIL   DS    750CL(TABLEN)                                                    
*                                                                               
         DS    CL4                                                              
         DS    XL2                 NUMBER OF ENTRIES IN TABLE                   
         DS    CL2                                                              
EM4DUE   DS    750CL(TABLEN)                                                    
         DS    XL2                 NUMBER OF ENTRIES IN TABLE                   
         DS    CL2                                                              
EM4BIL   DS    750CL(TABLEN)                                                    
*                                                                               
         DS    CL4                                                              
         DS    XL2                 NUMBER OF ENTRIES IN TABLE                   
         DS    CL2                                                              
EM5DUE   DS    750CL(TABLEN)                                                    
         DS    XL2                 NUMBER OF ENTRIES IN TABLE                   
         DS    CL2                                                              
EM5BIL   DS    750CL(TABLEN)                                                    
*                                                                               
         DS    CL4                                                              
         DS    XL2                 NUMBER OF ENTRIES IN TABLE                   
         DS    CL2                                                              
EM6DUE   DS    750CL(TABLEN)                                                    
         DS    XL2                 NUMBER OF ENTRIES IN TABLE                   
         DS    CL2                                                              
EM6BIL   DS    750CL(TABLEN)                                                    
*                                                                               
LASTABX  EQU   *                   END OF LAST TABLE                            
*                                                                               
         EJECT                                                                  
         SPACE 1                                                                
*                                                                               
*        GENERAL DSECT OF THE TABLES THAT WILL BE POSTED TO                     
*                                                                               
TABD     DSECT                                                                  
TABDATE  DS    PL3                 DATE PWOS                                    
         DS    CL1                 SPARE                                        
TABPREV  DS    F                   PREVIOUS AMOUNT                              
TABCURR  DS    F                   CURRENT AMOUNT                               
TABCANC  DS    F                   CANCELLED AMOUNT                             
TABPPRE  DS    F                   PAYROLL PREVIOUS INVOICES                    
TABPCUR  DS    F                   PAYROLL CURRENT INVOICES                     
TABTOTAL DS    F                   BILLED LESS PAID                             
TABLEN   EQU   *-TABD                                                           
*                                                                               
         SPACE 1                                                                
       ++INCLUDE TABALD                                                         
         EJECT                                                                  
         SPACE 1                                                                
*        OTHER DSECTS ARE HIDDEN IN HERE                                        
         SPACE 1                                                                
*TAREPFFD                                                                       
*TAREPF9D                                                                       
*DDGENTWA                                                                       
*DDTWADCOND                                                                     
*DDSPOOLD                                                                       
*DDPERVALD                                                                      
*DDSPLWORKD                                                                     
*TAGENFILE                                                                      
*CTGENFILE                                                                      
*TASYSDSECT                                                                     
*TASYSEQUS                                                                      
*TAREPWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE TAREPFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPF9D                                                       
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDTWADCOND                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAREPWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'054TAREP19   10/18/13'                                      
         END                                                                    
