*          DATA SET DEDEMAINT  AT LEVEL 020 AS OF 06/02/20                      
*PROCESS USING(WARN(15))                                                        
*PHASE T00ADCE                                                                  
*&&      SET   CSECT=N                                                          
         GBLA  &MAX                                                             
         PRINT OFF                                                              
       ++INCLUDE DEDEMDISP                                                      
         PRINT ON                                                               
         EJECT                                                                  
         TITLE 'DEMAINT - DEMO VALUE/DEMO ELEMENT MAINTENANCE'                  
DEMAINT  RSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKDLQ,**DEMNT                                                  
         USING DEMAINTD,RC         RC=A(W/S)                                    
         ST    R1,APARM                                                         
         LM    R3,R7,0(R1)         R3=A(ACTN),R4=A(DBLOCK),R5=A(WORK)           
*                                  R6=A(OFORMAT BLOCK),R7=MATH FACTOR           
         STCM  R3,8,BYTE1                                                       
*                                                                               
         USING DBLOCKD,R4                                                       
         L     RA,DBCOMFCS                                                      
         USING COMFACSD,RA         RA=A(COMMON FACILITIES LIST)                 
         MVI   DBERROR,0           RE-SET ERROR CODE                            
         MVI   DBERRMOD,EDEMAINT                                                
*                                                                               
         LR    R0,R1               SAVE R1                                      
         ICM   RF,15,CDEMOCON      IS A(DEMOCON) SET IN COMFACS?                
         BNZ   DEM1                YES                                          
         GOTO1 CCALLOV,DMCB,0,X'D9000AE0'  NO: GET A(DEMOCON)                   
         CLI   DMCB+4,X'FF'        WAS A(DEMOCON) RETURNED FROM CALLOV?         
         BNE   *+6                                                              
         DC    H'0'                NO                                           
         MVC   CDEMOCON,DMCB       SET A(DEMOCON) IN COMFACS                    
         L     RF,DMCB                                                          
DEM1     DS    0H                                                               
         GOTO1 (RF),DMCB,(0,0),('DEMOCON_14',APLDTABS),0                        
         LR    R1,R0               RESTORE R1                                   
*                                                                               
*                                  SEARCH ACTION TABLE FOR ACTION               
         LA    RE,ACTNTAB                                                       
DEM2     CLI   0(RE),X'FF'         TEST E-O-T                                   
         BNE   *+12                                                             
         MVI   DBERROR,INVCMND                                                  
         B     DEMX                                                             
         CLC   0(3,RE),0(R3)                                                    
         BE    *+12                                                             
         LA    RE,L'ACTNTAB(RE)                                                 
         B     DEM2                                                             
*                                                                               
         MVC   ACTION,3(RE)        SET ACTION NUMBER FROM TABLE                 
         TM    ACTION,X'80'        TEST IF AN ELEMENT ACTION                    
         BZ    DEM4                                                             
         XC    OFORMAT,OFORMAT                                                  
         LTR   R6,R6               YES - TEST IF FORMAT BLOCK PASSED            
         BNZ   *+12                                                             
         LA    R6,OFORMAT          NO - POINT TO DUMMY FORMAT BLOCK             
         ST    R6,12(R1)           AND RETURN ADDRESS TO CALLER                 
*                                                                               
DEM4     B     4(RE)               GO TO REQUESTED ROUTINE                      
*                                  EXIT FROM MODULE                             
DEMX     XMOD1 1                                                                
         EJECT                                                                  
* ACTION=DSP - GET ADDRESSES OF DISPLACEMENT TABLE HEADERS.                     
*                                                                               
DEMDSP   GOTO1 CDEMEL,DMCB,(C'D',(R6)),DBLOCKD,(R5)                             
         L     RE,APARM                                                         
         MVC   16(4,RE),12(R1)                                                  
         MVC   20(4,RE),16(R1)                                                  
         B     DEMX                                                             
*                                                                               
* ACTION=GET - EXPLODE VALUES INTO WORK AREA.                                   
*                                                                               
DEMGET   GOTO1 CDEMEL,DMCB,(C'E',(R6)),DBLOCKD,(R5)                             
         B     DEMX                                                             
*                                                                               
* ACTION=DEL/REP - DELETE EXISTING DEMO ELEMENTS FROM A RECORD.                 
*                                                                               
DEMDEL   L     RE,DBAQUART         RE=A(QTR HOUR ELEMENT)                       
         USING DREELEM,RE                                                       
         SR    RF,RF                                                            
*                                  FIND QTR HOUR/DEMO ELEMENT                   
DEMDEL2  CLI   DRECODE,0           TEST E-O-R                                   
         BE    DEMDELX                                                          
         CLI   DRECODE,QHCODEQ                                                  
         BNL   *+14                                                             
         IC    RF,1(RE)            BUMP TO NEXT ELEMENT                         
         AR    RE,RF                                                            
         B     DEMDEL2                                                          
*                                  SET ELEMENT CODES TO X'FF'                   
DEMDEL4  CLI   DRECODE,QHCODEQ     TEST END OF QTR HOUR                         
         BL    DEMDEL10            YES - GO DELETE ELEMENTS                     
         CLI   DRECODE,X'30'                                                    
         BL    DEMDEL8                                                          
         CLI   DRECODE,X'5F'       TEST IF END OF QUARTER HOUR                  
         BH    DEMDEL10                                                         
         CLI   OFILTER,0           TEST FOR AN ELEMENT FILTER                   
         BE    DEMDEL6                                                          
         MVC   DUB(1),DRECODE      TES - TEST IF ELEMENT FOUND                  
         OI    DUB,X'01'                                                        
         CLC   OFILTER,DUB                                                      
         BNE   DEMDEL8                                                          
*                                                                               
DEMDEL6  MVI   DRECODE,X'FF'       SET ELEMENT READY FOR DELETION               
DEMDEL8  IC    RF,DRELEN           BUMP TO NEXT ELEMENT                         
         AR    RE,RF                                                            
         B     DEMDEL4                                                          
*                                  DELETE ELEMENTS                              
DEMDEL10 BAS   RE,GETFILE          GET HELLO FILE NAME                          
         GOTO1 CHELLO,DMCB,(C'D',DUB),(X'FF',DBAREC),0,0                        
*                                                                               
DEMDELX  CLI   ACTION,REP          TEST FOR ACTION=REP                          
         BE    DEMPUT              YES - GO ADD NEW ELEMENTS                    
         B     DEMX                                                             
         EJECT                                                                  
* ACTION=PUT/REP - BUILD DEMO ELEMENTS AND ADD TO DEMO RECORD.                  
*                                                                               
DEMPUT   OC    DBNUMVLS,DBNUMVLS   TEST ANY VALUES PRESENT                      
         BZ    DEMPUTX             NO - EXIT                                    
         GOTO1 CDEMEL,DMCB,(C'C',(R6)),DBLOCKD,(R5)                             
         OC    0(2,R5),0(R5)       TEST ANY ELEMENTS CREATED                    
         BZ    DEMPUTX                                                          
         LA    R5,2(R5)            R5=A(FIRST DEMO ELEMENT)                     
         BAS   RE,GETFILE          GET HELLO FILE NAME                          
         SR    R0,R0                                                            
*                                  ADD ONE ELEMENT AT A TIME TO RECORD          
DEMPUT2  CLI   0(R5),0             TEST E-O-L                                   
         BE    DEMPUTX                                                          
         GOTO1 CHELLO,DMCB,(C'P',DUB),DBAREC,(R5),0                             
         CLI   12(R1),0            TEST FOR ERRORS                              
         BE    DEMPUT4                                                          
         CLI   12(R1),5            TEST RECORD TOO BIG                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   DBERROR,RECLONG                                                  
         B     DEMX                                                             
*                                  BUMP TO NEXT ELEMENT TO BE ADDED             
DEMPUT4  IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     DEMPUT2                                                          
*                                                                               
DEMPUTX  B     DEMX                                                             
         EJECT                                                                  
* ACTION=ADD/SUB - ADD OR SUBTRACT RECORD VALUES FROM WORK AREA.                
*                                                                               
DEMSUB   DS    0H                                                               
DEMADD   DS    0H                                                               
         LA    RE,DEMADDX                                                       
*                                                                               
* NOTE:  NTR1 GRABS ENOUGH WORKING STORAGE FOR DEMEL'S WORKAREA.                
*        THIS AREA MUST BE LARGE ENOUGH TO HANDLE THE LARGEST RECORD            
*        DESCRIPTION IN DEDEMDISP. THAT'S WHY THIS MODULE *MUST* BE             
*        RELINKED IF A TABLE IS ADDED TO DEDEMDISP WHICH IS LARGER              
*        THAN THE CURRENT LARGEST. OF COURSE, IT SHOULD THEORETICALLY           
*        BE SAFE TO LINK IT ANYWAY, JUST IN CASE.                               
*                                                                               
         PRINT GEN                                                              
         NTR1  WORK=(R7,&MAX)                                                   
         PRINT NOGEN                                                            
*                                                                               
         GOTO1 CDEMEL,DMCB,(C'E',(R6)),DBLOCKD,(R7)                             
         SR    R0,R0                                                            
         ICM   R0,3,DBNUMVLS       R0=NUMBER OF WORK AREA VALUES                
         BZ    DEMADD6                                                          
         L     R2,16(R1)           A(OUTPUT DISPLACEMENT TABLE)                 
         LA    R2,10(R2)           POINT PAST HEADER TO 1ST ENTRY               
*                                  ADD/SUBTRACT VALUES FROM WORK AREA           
DEMADD2  MVI   DUB,0                                                            
         L     RE,0(R5)                                                         
         CHI   RE,-1               TEST IF ZERO FIELD                           
         BNE   *+10                                                             
         SR    RE,RE                                                            
         OI    DUB,X'80'                                                        
         L     RF,0(R7)                                                         
         CHI   RF,-1               TEST IF ZERO FIELD                           
         BNE   *+10                                                             
         SR    RF,RF                                                            
         OI    DUB,X'80'                                                        
*                                                                               
         LLC   R3,0(R2)                 ENCODED MODIFIER                        
         A     R3,APLMODTB              R3 = A(DECODED MODIFIER)                
         CLI   BYTE1,C'U'               ALSO PROCESS UNIV LIKE OTHER            
         BE    DEMADD3                  DEMOS                                   
         CLI   BYTE1,C'X'               ONLY PROCESS DEMOS CLEAR                
         BE    DEMADD2B                 UNIVERSE VALUES                         
         CLI   BYTE1,C'L'                                                       
         BNE   DEMADD2D                                                         
*                                                                               
         CLI   0(R3),DEMO_MODIFIER_U                                            
         BE    DEMADD3                  OPTION C'L' ONLY PROCESS AND            
         CLI   0(R3),DEMO_MODIFIER_L    KEEP UNIV - IGNORE OTHER DEMOS          
         BE    DEMADD3                  AND KEEP ORGINAL DEMOS                  
         LR    RE,RF                    KEEP PRIOR READS DEMOS                  
         B     DEMADD4                                                          
*                                                                               
DEMADD2B CLI   0(R3),DEMO_MODIFIER_U                                            
         BE    *+8                      OPTION C'X' ONLY PROCESS                
         CLI   0(R3),DEMO_MODIFIER_L    REGULAR DEMOS -CLEAR UNIV               
         BNE   DEMADD3                                                          
         SR    RE,RE                                                            
         B     DEMADD4                                                          
* NORMAL DEFAULT PROCESSING                                                     
DEMADD2D CLI   0(R3),DEMO_MODIFIER_U    BYPASS ADD OR SUBTRACT FOR...           
         BE    DEMADD4                  ...UNIVERSE VALUES                      
         CLI   0(R3),DEMO_MODIFIER_L                                            
         BE    DEMADD4                                                          
*                                                                               
DEMADD3  CLI   ACTION,ADD                                                       
         BNE   *+10                                                             
         AR    RE,RF                                                            
         B     DEMADD4                                                          
         SR    RE,RF                                                            
*                                                                               
DEMADD4  LTR   RE,RE               TEST IF RESULT ZERO                          
         BNZ   *+14                                                             
         TM    DUB,X'80'           YES - TEST IF ZERO FIELD PRESENT             
         BZ    *+6                                                              
         LCR   RE,RE               YES - SET RESULT TO -1                       
         ST    RE,0(R5)            SET RESULT IN WORK AREA                      
         LA    R2,5(R2)            POINT TO NEXT DISP TAB ENTRY                 
         LA    R5,4(R5)            BUMP TO NEXT VALUE                           
         LA    R7,4(R7)                                                         
         BCT   R0,DEMADD2                                                       
*                                                                               
DEMADD6  XIT1                                                                   
*                                                                               
DEMADDX  B     DEMX                                                             
         EJECT                                                                  
* ACTION=MUL/DIV/RDI - PERFORM MATH FUNCTION ON ALL WORK AREA VALUES.           
*                                                                               
DEMMATH  SR    R0,R0                                                            
         ICM   R0,3,DBNUMVLS       R0=NUMBER OF WORK AREA VALUES                
         BZ    DEMMATHX                                                         
         LTR   R7,R7               TEST IF FACTOR/DIVISOR ZERO                  
         BZ    DEMMATHX                                                         
         GOTO1 CDEMEL,DMCB,(C'D',(R6)),DBLOCKD,(R5)                             
         L     R2,16(R1)           A(OUTPUT DISPLACEMENT TABLE)                 
         LA    R2,10(R2)           POINT PAST HEADER TO 1ST ENTRY               
*                                                                               
DEMMATH2 L     RE,0(R5)            RE&RF=WORK AREA VALUE                        
         SRDA  RE,32                                                            
         CHI   RF,-1               TEST IF ZERO FIELD                           
         BE    DEMMATH8            YES - DONT DO OPERATION                      
*                                                                               
         LLC   R3,0(R2)                   ENCODED MODIFIER                      
         A     R3,APLMODTB                R3 = A(DECODED MODIFIER)              
         CLI   BYTE1,C'U'                 TREAT UNIVERSES THE SAME              
         BE    DEMMATH3                   AS OTHER DEMOS                        
         CLI   BYTE1,C'L'                                                       
         BE    DEMMATH3                                                         
         CLI   0(R3),DEMO_MODIFIER_U      SKIP OPERATION FOR...                 
         BE    DEMMATH8                   ...UNIVERSE VALUES                    
         CLI   0(R3),DEMO_MODIFIER_L                                            
         BE    DEMMATH8                                                         
*                                                                               
DEMMATH3 CLI   ACTION,MUL                                                       
         BNE   DEMMATH6                                                         
         CLI   DBSELMED,C'C'       TEST CANADIAN DEMO RECORD                    
         BNE   DEMMATH4                                                         
         CLI   DBSELSRC,C'A'                                                    
         BNE   DEMMATH4                                                         
*                                                                               
         CLI   0(R3),DEMO_MODIFIER_R   RATING OR EXTENDED RATING?               
         BE    *+12                                                             
         CLI   0(R3),DEMO_MODIFIER_E                                            
         BNE   DEMMATH4                                                         
         AHI   RF,5                YES - ROUND DEMO VALUE                       
         D     RE,=F'10'                                                        
         M     RE,=F'10'                                                        
DEMMATH4 MR    RE,R7                                                            
         B     DEMMATH8                                                         
DEMMATH6 CLI   ACTION,DIV                                                       
         BNE   *+10                                                             
         DR    RE,R7                                                            
         B     DEMMATH8                                                         
         SLDA  RE,1                                                             
         DR    RE,R7                                                            
         LTR   RF,RF                                                            
         BM    *+8                                                              
         AHI   RF,1                                                             
         SRA   RF,1                                                             
*                                                                               
DEMMATH8 ST    RF,0(R5)            SET RESULT                                   
         LA    R2,5(R2)            POINT TO NEXT DISPL TAB ENTRY                
         LA    R5,4(R5)            BUMP TO NEXT FIELD                           
         BCT   R0,DEMMATH2                                                      
*                                                                               
DEMMATHX B     DEMX                                                             
         EJECT                                                                  
* GET HELLO FILE NAME FOR DBFILE.                                               
*                                                                               
GETFILE  LA    RF,FILETAB          RF=A(FILE TABLE)                             
         MVC   DUB(3),0(R6)        SET DUB(3) TO FILE                           
         OC    DUB(3),DUB                                                       
         BNZ   *+10                                                             
         MVC   DUB(3),DBFILE                                                    
GETFILE2 CLI   0(RF),X'FF'         TEST E-O-T                                   
         BNE   *+12                                                             
         MVI   DBERROR,INVFM                                                    
         B     DEMX                                                             
         CLC   0(3,RF),DUB                                                      
         BE    *+12                                                             
         LA    RF,L'FILETAB(RF)                                                 
         B     GETFILE2                                                         
         MVC   DUB,3(RF)           RETURN FILE NAME IN DUB                      
         BR    RE                                                               
         EJECT                                                                  
* ACTION=DAD- ADD RECORD VALUES FROM FULL-WORD WORK AREA TO                     
* DOUBLE-WORD ACCUMULATOR AREA. A(ACCUMULATORS) IS IN DBAREC                    
DEMDADD  DS    0H                                                               
*                                                                               
         GOTO1 CDEMEL,DMCB,(C'D',(R6)),DBLOCKD,(R5)  GET A(DISPL TABLE)         
         SR    R0,R0                                                            
         ICM   R0,3,DBNUMVLS       R0=NUMBER OF WORK AREA VALUES                
         BZ    DEMDAD6                                                          
         L     R2,12(R1)           A(START OF DISPLACEMENT TABLE)               
         LA    R2,10(R2)           POINT PAST HEADER TO 1ST ENTRY               
*                                                                               
         ICM   R7,15,DBAREC        DOUBLE WORD ACCUMULATOR AREA                 
*                                                                               
DEMDAD2  LM    RE,RF,0(R7)         DOUBLE-WORD ACCUMULATOR                      
         ICM   R1,15,0(R5)         FULL-WORD VALUE TO BE ADDED                  
         BM    DEMDAD4             SKIP NEGATIVE. SHOULD NEVER HAPPEN           
*                                                                               
         LLC   R3,0(R2)                 ENCODED MODIFIER                        
         A     R3,APLMODTB              R3 = A(DECODED MODIFIER)                
         CLI   BYTE1,C'U'               TREAT UNIVERSES THE SAME                
         BE    DEMDAD3                  AS OTHER DEMOS                          
         CLI   BYTE1,C'X'               DONT WANT UNIV - CLEAR OUT              
         BE    DEMDAD2B                 ONLY WANT OTHER DEMOS                   
         CLI   BYTE1,C'L'               ONLY PROCESS UNIVERSES                  
         BNE   DEMDAD2C                 KEEP PREVIOUS DEMOS                     
         CLI   0(R3),DEMO_MODIFIER_U                                            
         BE    DEMDAD3                                                          
         CLI   0(R3),DEMO_MODIFIER_L    OPTION C'L' ONLY PROCESS                
         BE    DEMDAD3                  UNIVERSES                               
         B     DEMDAD4                  KEEP PREVIOUS DEMOS                     
*                                                                               
DEMDAD2B CLI   0(R3),DEMO_MODIFIER_U                                            
         BE    *+8                      OPTION C'X' ONLY PROCESS                
         CLI   0(R3),DEMO_MODIFIER_L    OTHER DEMOS OTHER THEN UNIV             
         BNE   DEMDAD3                                                          
         SR    RE,RE                    CLEAR OUT UNIVERSE                      
         SR    RF,RF                                                            
         B     DEMDAD4                                                          
* NORMAL PROCESING                                                              
DEMDAD2C CLI   0(R3),DEMO_MODIFIER_U    BYPASS OPERATION FOR UNIVERSES          
         BE    *+12                                                             
         CLI   0(R3),DEMO_MODIFIER_L                                            
         BNE   DEMDAD3                                                          
*                                                                               
DEMDAD2D SR    RE,RE                                                            
         LR    RF,R1               JUST STORE THE VALUE IN DOUBLE WORDS         
         B     DEMDAD4                                                          
*                                                                               
DEMDAD3  ALR   RF,R1               ADD NEW DEMO AND SET CARRY IN CC             
         ALC   RE,=F'0'            ADD CARRY TO HI-ORDER PORTION OF SUM         
*                                                                               
DEMDAD4  STM   RE,RF,0(R7)         SET RESULT IN WORK AREA                      
         LA    R5,4(R5)            BUMP TO NEXT VALUE TO ADD                    
         LA    R7,8(R7)            BUMP TO NEXT ACCUMULATOR SLOT                
         LA    R2,5(R2)            POINT TO NEXT DISP TAB ENTRY                 
         BCT   R0,DEMDAD2                                                       
*                                                                               
DEMDAD6  XIT1                                                                   
*                                                                               
DEMDADX  B     DEMX                                                             
         EJECT                                                                  
* ACTION=DRD - PERFORM ROUNDED DIVISION ON DOUBLE-WORD VALUES                   
* ON INPUT, R5-> AREA OF DOUBLE-WORD VALUES TO BE DIVIDED                       
DEMDRD   SR    R0,R0                                                            
         ICM   R0,3,DBNUMVLS       R0=NUMBER OF WORK AREA VALUES                
         BZ    DEMDRDX                                                          
         LTR   R7,R7               TEST IF FACTOR/DIVISOR ZERO                  
         BZ    DEMDRDX                                                          
         GOTO1 CDEMEL,DMCB,(C'D',(R6)),DBLOCKD,(R5)                             
         L     R2,16(R1)           A(OUTPUT DISPLACEMENT TABLE)                 
         LA    R2,10(R2)           POINT PAST HEADER TO 1ST ENTRY               
*                                                                               
DEMDRD2  LM    RE,RF,0(R5)         DOUBLE-WORD VALUE                            
         LTR   RE,RE               ZERO VALUE?                                  
         BNZ   DEMDRD3                                                          
         LTR   RF,RF                                                            
         BNZ   DEMDRD3                                                          
         B     DEMDRD8             YES - DONT DO OPERATION                      
*                                                                               
DEMDRD3  DS    0H                                                               
         LLC   R3,0(R2)                   ENCODED MODIFIER                      
         A     R3,APLMODTB                R3 = A(DECODED MODIFIER)              
         CLI   BYTE1,C'U'                 TREAT UNIV THE SAME                   
         BE    DEMDRD4                    AS OTHER DEMOS                        
         CLI   BYTE1,C'L'                                                       
         BE    DEMDRD4                                                          
         CLI   0(R3),DEMO_MODIFIER_U      SKIP OPERATION FOR...                 
         BE    DEMDRD8                    ...UNIVERSE VALUES                    
         CLI   0(R3),DEMO_MODIFIER_L                                            
         BE    DEMDRD8                                                          
*                                  ROUNDED DIVISION                             
DEMDRD4  SLDA  RE,1                DEMO VALUE * 2                               
         DR    RE,R7               / DIVISOR                                    
         AHI   RF,1                QUOTIENT + 1                                 
         SRA   RF,1                / 2                                          
         SR    RE,RE                                                            
*                                                                               
DEMDRD8  STM   RE,RF,0(R5)         SET RESULT                                   
         LA    R5,8(R5)            BUMP TO NEXT FIELD                           
         LA    R2,5(R2)            POINT TO NEXT DISPL TAB ENTRY                
         BCT   R0,DEMDRD2                                                       
*                                                                               
DEMDRDX  B     DEMX                                                             
         EJECT                                                                  
* LITERALS ETC.                                                                 
*                                                                               
         LTORG                                                                  
*                                                                               
*                                  TABLE OF DBFILE/HELLO FILE NAMES             
FILETAB  DS    0CL11                                                            
         DC    C'TP ',CL8'DEMFIL'                                               
         DC    C'PAV',CL8'PAVFIL'                                               
         DC    C'NAD',CL8'PAVFIL'                                               
         DC    C'CAB',CL8'PAVFIL'                                               
         DC    C'NTI',CL8'PAVFIL'                                               
         DC    C'MPA',CL8'DEMFIL'                                               
         DC    C'INV',CL8'REPFIL'                                               
         DC    C'EVN',CL8'DEMFIL'                                               
         DC    C'IUN',CL8'PAVFIL'                                               
         DC    C'DPT',CL8'DEMFIL'                                               
         DC    C'RLD',CL8'PAVFIL'                                               
         DC    C'RUA',CL8'DEMFIL'                                               
         DC    X'FF'                                                            
*                                                                               
*                                  TABLE OF VALID ACTIONS                       
         DS    0H                                                               
ACTNTAB  DS    0CL8                                                             
         DC    C'DSP',AL1(DSP)                                                  
         B     DEMDSP                                                           
         DC    C'GET',AL1(GET)                                                  
         B     DEMGET                                                           
         DC    C'ADD',AL1(ADD)                                                  
         B     DEMADD                                                           
         DC    C'SUB',AL1(SUB)                                                  
         B     DEMSUB                                                           
         DC    C'DEL',AL1(DEL)                                                  
         B     DEMDEL                                                           
         DC    C'PUT',AL1(PUT)                                                  
         B     DEMPUT                                                           
         DC    C'REP',AL1(REP)                                                  
         B     DEMDEL                                                           
         DC    C'MUL',AL1(MUL)                                                  
         B     DEMMATH                                                          
         DC    C'DIV',AL1(DIV)                                                  
         B     DEMMATH                                                          
         DC    C'RDI',AL1(RDI)                                                  
         B     DEMMATH                                                          
         DC    C'DAD',AL1(DAD)     DOUBLE-WORD ADD                              
         B     DEMDADD                                                          
         DC    C'DRD',AL1(DRD)     DOUBLE-WORD ROUNDED DIVISION                 
         B     DEMDRD                                                           
         DC    X'FF'                                                            
         EJECT                                                                  
* DSECT TO COVER DEMAINT W/S                                                    
*                                                                               
DEMAINTD DSECT                                                                  
BYTE1    DS    C                                                                
DUB      DS    D                                                                
DMCB     DS    6F                                                               
APARM    DS    A                                                                
ACTION   DS    X                                                                
*                                  OUTPUT BOOK FORMAT BLOCK                     
OFORMAT  DS    0XL10                                                            
OFILE    DS    CL3                                                              
OMED     DS    CL1                                                              
OINTFIL  DS    CL1                                                              
OINTMED  DS    CL1                                                              
OSOURCE  DS    CL1                                                              
OBOOK    DS    XL2                                                              
OFILTER  DS    XL1                                                              
*                                                                               
         DS    0V                                                               
APLDTABS DS    0XL(6*4)            6 ADDRESSES                                  
APLMODTB DS    V                   A(MODIFIER TABLE)                            
APLPLDTB DS    V                   A(PERSONAL LANG. ATTRIBUTE TABLE)            
APLENCTB DS    V                   A(MODIFIER BYTE ENCODING TABLE)              
APLPRNTB DS    V                   A(PERSONAL LANG. PRINTABLE DESCRIPS)         
         DS    V                   SPARE                                        
         DS    V                   SPARE                                        
*                                                                               
WORKDLQ  EQU   *-DEMAINTD                                                       
*                                                                               
*                                  ACTION EQUATES                               
DSP      EQU   X'80'                                                            
GET      EQU   X'81'                                                            
ADD      EQU   X'82'                                                            
SUB      EQU   X'83'                                                            
DEL      EQU   X'84'                                                            
PUT      EQU   X'85'                                                            
REP      EQU   X'86'                                                            
MUL      EQU   X'87'                                                            
DIV      EQU   X'88'                                                            
RDI      EQU   X'89'                                                            
DAD      EQU   X'8A'               DOUBLE-WORD ADDITION                         
DRD      EQU   X'8B'               DOUBLE-WORD ROUNDED DIVISION                 
         EJECT                                                                  
* DEDBLOCK                                                                      
         PRINT OFF                                                              
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* DEDEMEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DEDEMEQUS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DEDEMEQUS2                                                                    
         PRINT OFF                                                              
       ++INCLUDE DEDEMEQUS2                                                     
         PRINT ON                                                               
         SPACE 1                                                                
* DEDEMFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE DEDEMFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'020DEDEMAINT 06/02/20'                                      
         END                                                                    
