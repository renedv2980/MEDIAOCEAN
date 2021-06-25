*          DATA SET ACLDXEXP   AT LEVEL 007 AS OF 09/10/20                      
*PHASE ACXEXPA                                                                  
*INCLUDE ACRECTYP                                                               
*INCLUDE SORTER                                                                 
*INCLUDE BINSRCH2                                                               
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE ADDAY                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE CUREDIT                                                                
         TITLE 'ADJUST EXPENSE ROW/CLAIM/POSTED DATE FOR EXPENSE REC'           
                                                                                
*SGAV 005 08AUG19 <DSRD-23471> AMEND DATE ON DIST ACC FOR NEW CIDMDAT           
*SGAV 006 07DEC19 <DSRD-24014> CREATE A NEW DIST ACC FOR NEW CIDMDAT            
*SGAV 007 08SEP20 <DSRD-19949> FIX LIDELD SEQ.NO & DIST-ACC VALIDATIONS         
                                                                                
***********************************************************************         
* THIS CONVERSION ADJUSTS ITEM ROW/CLAIM/POSTED DATE FOR EXPENSE RECORD         
***********************************************************************         
DMLDEXT  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKX-WORKD,DMLDEXT                                              
         USING WORKD,RC                                                         
         B     DMXCTL                                                           
         DC    C'*VERIFY*'                                                      
         DC    X'FC'                                                            
         DC    C'00101'                                                         
         DC    X'FC'                                                            
         DC    C'01231'                                                         
         DC    C'*',X'FF'          FILE = ALL                                   
*        DC    C'QC'               FILE = ACCQC                                 
DMXRTST  DS    0H                                                               
*        B     DMXPURGE            PURGE UNDER TEST                             
         B     DMXKEEP             KEEP WHEN LIVE                               
         EJECT                                                                  
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST(PLISTL),0(R1)                                              
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         CLI   PLIST+8,C'Y'        RETURN CALL AS REQUESTED LAST TIME           
         BE    DMXRET                                                           
         CLI   PLIST,X'00'         FIRST CALL TO INITILISE                      
         BE    DMXINIT                                                          
         CLI   PLIST,X'01'         NORMAL CALL TO PROCESS RECORD                
         BE    DMXREC                                                           
         CLI   PLIST,X'FF'         LAST CALL ON EOF                             
         BE    DMXEOF                                                           
         B     DMXIT                                                            
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         MVI   8(R1),0                                                          
         B     DMXIT                                                            
DMXKERET L     R1,APARM            KEEP RECORD AND RETURN TO ME                 
         MVI   0(R1),0                                                          
         MVI   8(R1),C'R'                                                       
         B     DMXIT                                                            
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         MVI   8(R1),0                                                          
         B     DMXIT                                                            
DMXPGRET L     R1,APARM            PURGE RECORD AND RETURN TO ME                
         MVI   0(R1),X'FF'                                                      
         MVI   8(R1),C'R'                                                       
         B     DMXIT                                                            
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
                                                                                
         EJECT                                                                  
DMXEOF   DS    0H                  END-OF-FILE CONDITION                        
         MVI   P,C' '              INIT PRINTLINE                               
         MVC   P+1(L'P-1),P                                                     
         MVC   P+1(28),=C'CLAIM ITEM DATES MODIFIED = '                         
         CURED CIDCNT#,(12,P+41),0,MINUS=YES                                    
         GOTO1 VPRINTER                                                         
*                                                                               
         MVI   P,C' '              INIT PRINTLINE                               
         MVC   P+1(L'P-1),P                                                     
         MVC   P+1(38),=C'DISTANCE ACCUMULATOR DATES ADDED    = '               
         CURED LIDCNT#,(12,P+41),0,MINUS=YES                                    
         GOTO1 VPRINTER                                                         
         B     DMXIT               EXIT THE PROGRAM                             
*                                                                               
DMXRET   DS    0H                                                               
DMXINIT  DS    0H                                                               
DMXIT    XIT1  ,                                                                
         EJECT                                                                  
DMXREC   GOTO1 =V(DATCON),DMCB,(5,0),(1,DUB)                                    
         MVC   SVYEAR,DUB          SAVE CURRENT YEAR                            
         GOTO1 =V(DATCON),DMCB,(5,0),(0,DUB)                                    
         GOTO1 =V(ADDAY),DMCB,(C'M',DUB),(0,DUB),F'-11'                         
         GOTO1 =V(DATCON),DMCB,(0,DUB),(2,OUTCMPDT) COMPRESSED DATE             
*                                                                               
         GOTO1 =V(DATCON),DMCB,(5,0),(0,DUB)                                    
         GOTO1 =V(ADDAY),DMCB,(C'M',DUB),(0,DUB),F'-12'                         
         GOTO1 =V(DATCON),DMCB,(0,DUB),(1,OUTPCKDT) PACKED DATE                 
         MVC   OUTPCKDT+1(2),=X'0101' SET MM/DD TO 01/01                        
*                                                                               
         USING PERRECD,R2          PERSON RECORD                                
         L     R2,VREC                                                          
         GOTO1 VRECTYP,DMCB,(C'D',PERRECD)                                      
         MVC   RECTYPE,0(R1)                                                    
         MVC   COMPCOD,1(R1)       GET COMPANY-CODE                             
         CLI   COMPCOD,X'96'       QMROD1 ONLY                                  
         BNE   DMXRTST             NO: EXIT THE PROGRAM                         
         CLI   RECTYPE,ACRTPER                                                  
         BE    DMXREC30                                                         
*                                                                               
         USING EXCRECD,R2          EXPENSE RECORD                               
         L     R2,VREC                                                          
         GOTO1 VRECTYP,DMCB,(C'D',EXCRECD)                                      
         MVC   RECTYPE,0(R1)                                                    
         MVC   COMPCOD,1(R1)       GET COMPANY-CODE                             
         CLI   COMPCOD,X'96'       QMROD2 ONLY                                  
         BNE   DMXRTST             NO: EXIT THE PROGRAM                         
         CLI   RECTYPE,ACRTEXPC                                                 
         BNE   DMXRTST                                                          
*                                                                               
         LA    R3,EXCRFST          R3= A(1ST ELEMENT IN EXPENSE RECORD)         
         MVI   RECUPD,0            INIT RECORD UPDATER FLAG                     
*                                                                               
         USING CIDELD,R3           CLAIM ITEM DATA ELEMENT                      
DMXREC10 CLI   CIDEL,0             END OF RECORD?                               
         BE    DMXREC25            YES:CHECK FOR RECUPD & KEEP DATA             
         CLI   CIDEL,CLDELQ        IS CLAIM DATA ELEMENT?                       
         BE    DMXREC20            YES:PROCESS CLDEL ELEMENT                    
         CLI   CIDEL,CIDELQ        IS CLAIM ITEM DATA ELEMENT?                  
         BNE   DMXREC15            NO:CHECK NEXT ELEMENT                        
         CLI   CIDTYPE,CIDTYMQ     ELEMENT TYPE = MAIN ELEMENT?                 
         BNE   DMXREC15            NO:CHECK NEXT ELEMENT                        
*                                                                               
         OC    CIDMDAT,CIDMDAT     IS DATE = NULL?                              
         BZ    DMXREC15            NO:CHECK NEXT ELEMENT                        
*                                  OUTCMPDT = TODAY'S DATE - 11 MONTHS          
         CLC   CIDMDAT,OUTCMPDT    EXPENSE ROW DATE > 11 MONTHS OLD             
         BH    DMXREC15            YES:CHECK NEXT ELEMENT                       
*                                  CIDMDAT = CIDMDAT + 12 MONTHS                
         GOTO1 =V(DATCON),DMCB,(2,CIDMDAT),(0,DUB)                              
         GOTO1 =V(ADDAY),DMCB,(C'M',DUB),(0,DUB),F'12'                          
         GOTO1 =V(DATCON),DMCB,(0,DUB),(2,CIDMDAT)                              
         AP    CIDCNT#,PONE        INCREMENT UPDATED CIDELD COUNTER             
         OI    RECUPD,X'80'        RECORD TO BE UPDATED                         
*                                                                               
DMXREC15 LLC   R0,CIDLN            GET TO NEXT ELEMENT                          
         AR    R3,R0                                                            
         B     DMXREC10                                                         
*                                                                               
         USING CLDELD,R3           CLAIM DATA ELEMENT                           
DMXREC20 CLI   CLDTYPE,CLDTHDRQ    IS TYPE = HEADER INFORMATION?                
         BNE   DMXREC15            NO:CHECK NEXT ELEMENT                        
*                                                                               
         OC    CLDBADD,CLDBADD     IS DATE = NULL?                              
         BZ    DMXREC15            NO:CHECK NEXT ELEMENT                        
*                                  OUTCMPDT = TODAY'S DATE - 11 MONTHS          
         CLC   CLDBADD,OUTCMPDT    DATE BATCH ADDED > 11 MONTHS OLD             
         BH    DMXREC15            YES:CHECK NEXT ELEMENT                       
*                                  CLDBADD = CLDBADD + 12 MONTHS                
         GOTO1 =V(DATCON),DMCB,(2,CLDBADD),(0,DUB)                              
         GOTO1 =V(ADDAY),DMCB,(C'M',DUB),(0,DUB),F'12'                          
         GOTO1 =V(DATCON),DMCB,(0,DUB),(2,CLDBADD)                              
         OI    RECUPD,X'80'        RECORD TO BE UPDATED                         
         B     DMXREC15            GET TO NEXT ELEMENT                          
*                                                                               
DMXREC25 OC    EXCKDATE,EXCKDATE   IS DATE = NULL?                              
         BZ    DMXRTST             YES:EXIT THE PROGRAM                         
*                                                                               
***********************************************************************         
* EXCKDATE IS IN 2ND COMPLEMENT FORM, SO CONVERTING IT INTO COMPRESSED*         
* DATE FORMAT FOR COMPARISON. IF IT IS MATCHING THE CRITERIA THEN     *         
* ADDING 1 YEAR TO EXCKDATE                                           *         
***********************************************************************         
*                                                                               
         XR    RE,RE               INIT RE                                      
         ICM   RE,3,EXCKDATE                                                    
         LNR   RE,RE               CONVERT INTO UNCOMPLEMENTED DATE             
         STCM  RE,3,COMPLDAT       SAVE UNCOMPLEMENTED DATE                     
*                                                                               
         CLC   COMPLDAT,OUTCMPDT   DATE OF CLAIM  > 11 MONTHS OLD               
         BH    DMXRTST             YES:EXIT THE PROGRAM                         
*                                  EXCKDATE = EXCKDATE + 12 MONTHS              
         GOTO1 =V(DATCON),DMCB,(2,COMPLDAT),(0,DUB)                             
         GOTO1 =V(ADDAY),DMCB,(C'M',DUB),(0,DUB),F'12'                          
         GOTO1 =V(DATCON),DMCB,(0,DUB),(2,DUB+4)                                
*                                                                               
         XR    RE,RE               INIT RE                                      
         ICM   RE,3,DUB+4                                                       
         LNR   RE,RE               CONVERT INTO 2ND COMPLEMENT FORMAT           
         STCM  RE,3,EXCKDATE       SAVE 2ND COMPLEMENTED DATE                   
*                                                                               
         L     RE,VISREC           RE=A(EXPENSE DIRECTORY RECORD)               
         MVC   EXCKDATE-EXCRECD(L'EXCKDATE,RE),EXCKDATE                         
         B     DMXKEEP             YES: KEEP MODIFIED EXPENSE RECORDS           
         DROP  R2,R3                                                            
*                                                                               
***********************************************************************         
* READ PERSON AND CHECK IF DISTANCE ACCUMULATOR IS PRESENT FOR CURRENT*         
* YEAR   :                                                            *         
* IF YES : EXIT THE PROGRAM                                           *         
* IF NO  : DISTANCE ACCUMULATOR IS FOUND FOR LAST YEAR, THEN ADD NEW  *         
* DISTANCE ACCUMULATOR FOR CURRENT YEAR WITH SAME DETAILS AS LAST YEAR*         
***********************************************************************         
*                                                                               
         USING PERRECD,R2                                                       
DMXREC30 LA    R3,PERRFST          R3= A(1ST ELEMENT IN PERSON RECORD)          
         MVI   RECUPD,0            INIT FLAG FOR RECORD TO BE UPDATED           
         MVI   LSTYREC,0           INIT FLAG FOR LAST YEAR                      
         LA    RE,DSTNUMTB         DIST-ACC NUMBER TABLE                        
*                                                                               
         USING LIDELD,R3           LIST DATA ELEMENT                            
DMXREC35 CLI   LIDEL,0             END OF RECORD?                               
         BNE   DMXREC40                                                         
         TM    LSTYREC,X'80'       LAST YEAR DIST.ACC FOUND?                    
         BZ    DMREC105            NO:EXIT THE PROGRAM                          
         MVI   0(RE),X'FF'         E-O-T MARKER                                 
         B     DMXREC60            EXIT THE PROGRAM                             
*                                                                               
DMXREC40 CLI   LIDEL,LIDELQ        IS LIST DATA ELEMENT?                        
         BNE   DMXREC45            NO:CHECK NEXT ELEMENT                        
         CLI   LIDTYPE,LIDTDTAC    DISTANCE ACCUMULATOR                         
         BE    DMXREC50            YES:PROCESS FOR DISTANCE ACCUMULATOR         
*                                                                               
DMXREC45 LLC   R0,LIDLN            GET TO NEXT ELEMENT                          
         AR    R3,R0                                                            
         B     DMXREC35                                                         
*                                                                               
DMXREC50 LLC   R4,LIDLN                                                         
         CHI   R4,LIDLNDQ          CHECK ELEMENT IS LONG ENOUGH                 
         BNH   DMXREC45            CHECK NEXT ELEMENT                           
*                                                                               
         LA    R6,LIDDATA                                                       
         LR    R5,R3                                                            
         AR    R5,R4               R5=A(END OF LIDELD ELEMENT)                  
*                                                                               
T        USING LIDDATA,R6                                                       
DMXREC55 CLC   T.LIDDSTDT,OUTPCKDT   ACCUM DATE >= CURRENT YR -  1 YR           
         BL    DMXREC57                                                         
         CLC   T.LIDDSTDT(L'SVYEAR),SVYEAR CURRENT YEAR?                        
         BE    DMXREC58                                                         
         OI    LSTYREC,X'80'       PROCESS REC IF DIST-ACC FOR LAST YR          
*                                                                               
DMXREC57 AHI   R6,LIDDLNQ                                                       
         CR    R6,R5                                                            
         BL    DMXREC55                                                         
         B     DMXREC45            CHECK NEXT ELEMENT                           
*                                                                               
DMXREC58 MVC   0(L'LIDDNUM,RE),T.LIDDNUM  ADD DIST-ACC NUMBER ENTRY             
         AHI   RE,L'LIDDNUM                                                     
         AHI   RF,1                INCREMENT COUNT OF ENTRY                     
         B     DMXREC57            PROCESS FOR NEXT DIST-ACC                    
         DROP  R3,T                                                             
*                                                                               
         USING LIDELD,R3           LIST DATA ELEMENT                            
DMXREC60 LA    R3,PERRFST          R3= A(1ST ELEMENT IN PERSON RECORD)          
*                                                                               
DMXREC65 CLI   LIDEL,0             END OF RECORD?                               
         BE    DMREC105            YES:CHECK FOR RECUPD & KEEP DATA             
         CLI   LIDEL,LIDELQ        IS LIST DATA ELEMENT?                        
         BNE   DMXREC70            NO:CHECK NEXT ELEMENT                        
         CLI   LIDTYPE,LIDTDTAC    DISTANCE ACCUMULATOR                         
         BE    DMXREC75            NO:CHECK NEXT ELEMENT                        
*                                                                               
DMXREC70 LLC   R0,LIDLN            GET TO NEXT ELEMENT                          
         AR    R3,R0                                                            
         B     DMXREC65                                                         
*                                                                               
DMXREC75 LLC   R4,LIDLN                                                         
         CHI   R4,LIDLNDQ          CHECK ELEMENT IS LONG ENOUGH                 
         BNH   DMXREC70            CHECK NEXT ELEMENT                           
*                                                                               
         LA    R6,LIDDATA                                                       
         LR    R5,R3                                                            
         AR    R5,R4               R5=A(END OF LIDELD ELEMENT)                  
         ST    R3,SAVER3                                                        
         XC    ELEMENT,ELEMENT     CLEAR ELEMENT                                
         XC    ELEMENT2,ELEMENT2   CLEAR ELEMENT2                               
         MVC   SVLIDSEQ,LIDSEQ     SAVE CURRENT LIDELD SEQ.NO                   
*                                                                               
         BCTR  R4,0                                                             
         MVC   ELEMENT(0),LIDELD   COPY LIDELD INTO STORAGE                     
         EX    R4,*-6                                                           
*                                                                               
         USING LIDELD,R3                                                        
         LA    R3,ELEMENT          R5=A(START OF LIDELD ELEMENT)                
         LR    R7,R3                                                            
         LLC   R4,LIDLN                                                         
         AR    R7,R4               R7=A(END OF LIDELD IN STORG)                 
*                                                                               
T        USING LIDDATA,R6                                                       
DMXREC80 CLC   T.LIDDSTDT,OUTPCKDT ACCUM DATE >= CURRENT YR -  1 YR             
         BNE   DMXREC85                                                         
         LA    RE,DSTNUMTB         DIST-ACC NUMBER TABLE                        
*                                                                               
DMXREC82 CLI   0(RE),X'FF'         E-O-T?                                       
         BE    DMXREC83            YES: COPY FOR CURRENT YEAR                   
         CLC   0(1,RE),T.LIDDNUM   DISTACC NUM FOR CURRENT YEAR FOUND?          
         BE    DMXREC85            YES: CHECK FOR NEXT DIST-ACC NUMBER          
         AHI   RE,L'LIDDNUM        BUMP TO NEXT ELEMENT                         
         B     DMXREC82                                                         
*                                                                               
DMXREC83 AHI   R4,LIDDLNQ                                                       
         CHI   R4,L'ELEMENT        UPDATED LIDLN > 255?                         
         BNL   DMXREC90                                                         
*                                                                               
***********************************************************************         
* WHEN ELEMENT LENGTH IS <= 255 BYTES, THEN -                         *         
* - OLD DISTANCE ACCUMULATOR TO BE DELETED.                           *         
* - UPDATED DISTANCE ACCUMULATOR TO BE COPIED IN EXISTING LIDELD ELMT *         
***********************************************************************         
*                                                                               
         STC   R4,LIDLN            UPDATE ELEMENT LENGTH                        
         MVC   0(LIDDLNQ,R7),T.LIDDATA COPY DISTANCE ACCMU DATA                 
         DROP  T                                                                
*                                                                               
X        USING LIDDATA,R7                                                       
         GOTO1 =V(DATCON),DMCB,(1,X.LIDDSTDT),(0,DUB)                           
         GOTO1 =V(ADDAY),DMCB,(C'M',DUB),(0,DUB),F'12'                          
         GOTO1 =V(DATCON),DMCB,(0,DUB),(1,X.LIDDSTDT)                           
*                                                                               
         GOTO1 =V(DATCON),DMCB,(1,X.LIDDENDT),(0,DUB)                           
         GOTO1 =V(ADDAY),DMCB,(C'M',DUB),(0,DUB),F'12'                          
         GOTO1 =V(DATCON),DMCB,(0,DUB),(1,X.LIDDENDT)                           
         DROP  X                                                                
*                                                                               
         AP    LIDCNT#,PONE        INCREMENT LIDELD COUNTER                     
         OI    RECUPD,X'80'        RECORD TO BE UPDATED                         
         AHI   R7,LIDDLNQ          GET TO NEXT AVAILABLE STORAGE                
*                                                                               
DMXREC85 AHI   R6,LIDDLNQ          PID - FOR LIDTDTAC                           
         CR    R6,R5                                                            
         BL    DMXREC80                                                         
*                                                                               
         L     R3,SAVER3           RESTORE OLD LIDELD                           
         TM    RECUPD,X'80'        IS RECORD TO BE UPDATED?                     
         BZ    DMXREC70            NO: CHECK NEXT ELEMENT                       
*                                                                               
         MVI   LIDEL,DELETEQ       MARK OLD LIDELD FOR DELETION                 
         GOTO1 VHELLO,DMCB,(C'D',ACCMST),('DELETEQ',PERRECD),0,0                
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                BAD DELETE                                   
*                                                                               
         GOTO1 VHELLO,DMCB,(C'P',ACCMST),PERRECD,ELEMENT,0                      
         CLI   12(R1),0                                                         
         BE    DMXREC70            CHECK NEXT ELEMENT                           
         DC    H'0'                BAD UPDATE                                   
*                                                                               
***********************************************************************         
* WHEN ELEMENT LENGTH IS > 255, THEN -                                *         
* - OLD DISTANCE ACCUMULATOR TO BE DELETED.                           *         
* - UPDATED DISTANCE ACCUMULATOR TO BE COPIED IN EXISTING LIDELD ELMT *         
* - NEW LIDELD CREATED WITH NEXT SEQ.NO FOR DISTANCE ACCUMULATOR      *         
***********************************************************************         
*                                                                               
DMXREC90 SHI   R4,LIDDLNQ                                                       
         STC   R4,LIDLN            UPD ELEMENT LEN FOR EXISTING LIDELD          
         XR    R4,R4               UPD ELEMENT LEN FOR NEW LIDELD               
         LA    R7,ELEMENT2         FOR NEW ELEMENT                              
*                                                                               
T        USING LIDDATA,R6                                                       
DMXREC95 CLC   T.LIDDSTDT,OUTPCKDT ACCUM DATE >= CURRENT YR -  1 YR             
         BL    DMREC100            NO:CHECK FOR NEXT DIST.ACC                   
         CLC   T.LIDDSTDT(L'SVYEAR),SVYEAR CURRENT YEAR?                        
         BE    DMREC100                                                         
         MVC   0(LIDDLNQ,R7),T.LIDDATA COPY DISTANCE ACCMU DATA                 
         DROP  T                                                                
*                                                                               
X        USING LIDDATA,R7                                                       
         GOTO1 =V(DATCON),DMCB,(1,X.LIDDSTDT),(0,DUB)                           
         GOTO1 =V(ADDAY),DMCB,(C'M',DUB),(0,DUB),F'12'                          
         GOTO1 =V(DATCON),DMCB,(0,DUB),(1,X.LIDDSTDT)                           
*                                                                               
         GOTO1 =V(DATCON),DMCB,(1,X.LIDDENDT),(0,DUB)                           
         GOTO1 =V(ADDAY),DMCB,(C'M',DUB),(0,DUB),F'12'                          
         GOTO1 =V(DATCON),DMCB,(0,DUB),(1,X.LIDDENDT)                           
         DROP  X                                                                
*                                                                               
         AP    LIDCNT#,PONE        INCREMENT LIDELD COUNTER                     
         OI    RECUPD,X'80'        RECORD TO BE UPDATED                         
         AHI   R7,LIDDLNQ          GET TO NEXT AVAILABLE STORAGE                
         AHI   R4,LIDDLNQ          SAVE NEW ELEMENT LENGTH                      
*                                                                               
DMREC100 AHI   R6,LIDDLNQ          PID - FOR LIDTDTAC                           
         CR    R6,R5                                                            
         BL    DMXREC95            CHECK FOR LAST YEAR DIST.ACC                 
*                                                                               
         L     R3,SAVER3           RESTORE OLD LIDELD                           
         MVI   LIDEL,DELETEQ       MARK OLD LIDELD FOR DELETION                 
         GOTO1 VHELLO,DMCB,(C'D',ACCMST),('DELETEQ',PERRECD),0,0                
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                BAD DELETE                                   
*                                                                               
         GOTO1 VHELLO,DMCB,(C'P',ACCMST),PERRECD,ELEMENT,0                      
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                BAD UPDATE                                   
*                                                                               
         USING LIDELD,R3                                                        
         XC    ELEMENT,ELEMENT     BUILD NEW ELEMENT                            
         LA    R3,ELEMENT                                                       
         MVI   LIDEL,LIDELQ                                                     
         MVI   LIDTYPE,LIDTDTAC    DISTANCE ACCUMULATOR                         
         MVI   LIDITLN,LIDDLNQ                                                  
         LLC   RE,SVLIDSEQ                                                      
         AHI   RE,1                                                             
         STC   RE,LIDSEQ           INCREMENT SEQ.NO BY 1                        
         STC   RE,SVLIDSEQ                                                      
*                                                                               
         BCTR  R4,0                                                             
         MVC   LIDDATA(0),ELEMENT2  COPY DISTANCE ACCMU DATA                    
         EX    R4,*-6                                                           
*                                                                               
         AHI   R4,LIDLNDQ+1                                                     
         STC   R4,LIDLN            SET ELEMENT LENGTH FOR NEW LIDELD            
*                                                                               
         GOTO1 VHELLO,DMCB,(C'P',ACCMST),PERRECD,ELEMENT,0                      
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R3,SAVER3           RESTORE OLD LIDELD                           
         LLC   R0,LIDLN            SKIP FOR UPDATED LIDELD                      
         AR    R3,R0                                                            
         B     DMXREC70            CHECK NEXT ELEMENT                           
*                                                                               
DMREC105 TM    RECUPD,X'80'        IS RECORD UPDATED?                           
         BZ    DMXRTST             NO:EXIT THE PROGRAM                          
*                                                                               
         B     DMXKEEP             YES: KEEP MODIFIED PERSON RECORDS            
         DROP  R2,R3                                                            
*                                                                               
         LTORG                                                                  
VRECTYP  DC    V(ACRECTYP)                                                      
VBINSRCH DC    V(BINSRCH)                                                       
VSORTER  DC    V(SORTER)                                                        
VHELLO   DC    V(HELLO)                                                         
VLDDEF   DC    V(LDDEFN)                                                        
VDATCON  DC    V(DATCON)                                                        
VHEXOUT  DC    V(HEXOUT)                                                        
CUREDIT  DC    V(CUREDIT)                                                       
ACCMST   DC    C'ACCMST  '                                                      
ACCDIR   DC    C'ACCDIR  '                                                      
LIDCNT#  DC    PL6'0'                                                           
CIDCNT#  DC    PL6'0'                                                           
PONE     DC    P'1'                                                             
DELETEQ  EQU   X'FF'                                                            
SVLIDSEQ DC    X'00'                                                            
RECUPD   DC    X'00'                                                            
LSTYREC  DC    X'00'                                                            
DSTNUMTB DC    255X'00'                TABLE TO STORE DIST-ACC NUMBER           
*                                                                               
         EJECT                                                                  
WORKD    DSECT                                                                  
APARM    DS    A                                                                
SVYEAR   DS    X                       CURRE YEAR IN HEX FORMAT                 
OUTCMPDT DS    XL2                     DATE IN COMPRESSED FORMAT                
OUTPCKDT DS    PL3                     DATE IN PACKED FORMAT                    
COMPLDAT DS    XL2                     DATE AFTER CONVERTING 2ND COMP           
COMPCOD  DS    X                       COMPANY-CODE                             
DUB      DS    D                                                                
DMCB     DS    6F                                                               
WORK     DS    XL64                                                             
         DS    XL1                                                              
*                                                                               
         DS    0D                                                               
PLIST    DS    0X                                                               
VREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
VCARDS   DS    V                                                                
VPEELDT  DS    A                                                                
VISREC   DS    A                                                                
PLISTL   EQU   *-PLIST                                                          
*                                                                               
SAVER3   DS    A                                                                
RECTYPE  DS    X                                                                
ELEMENT  DS    XL256                                                            
ELEMENT2 DS    XL(L'ELEMENT)                                                    
*                                                                               
WORKX    EQU   *                                                                
         EJECT                                                                  
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE ACOPTEQUS                                                      
         PRINT ON                                                               
         SPACE                                                                  
* DMLDDEFN                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMLDDEFN                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* ACRECEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACRECEQUS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* ACGENRAC                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENRAC                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007ACLDXEXP  09/10/20'                                      
         END                                                                    
