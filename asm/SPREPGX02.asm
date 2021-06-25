*          DATA SET SPREPGX02  AT LEVEL 039 AS OF 12/16/02                      
*PHASE SPGX02A                                                                  
*INCLUDE SORTER                                                                 
*INCLUDE RECUP                                                                  
*INCLUDE DATCON                                                                 
************************* HIPO ****************************************         
*  NUMBER:                                                            *         
*  TITLE: SPREPGL02 - TRANSFER GOALS FROM TAPE                        *         
*                                                                     *         
*  COMMENTS: THIS PROGRAM WILL CREATE A WORKER FILE OF GOAL RECORDS   *         
*    FROM AN AGENCY SUPPLIED GOAL TAPE.                               *         
*                                                                     *         
*  CALLED FROM: SPONSOR                                               *         
*                                                                     *         
*                                                                     *         
*  CALLS TO: SORTER,BINSRCH,WORKER                                    *         
*                                                                     *         
*                                                                     *         
*  INPUTS: SPWORKD                                                    *         
*          AGENCY SUPPLIED GOAL TAPE                                  *         
*                                                                     *         
*                                                                     *         
*  GLOBAL OUTPUTS: WORKER FILE                                        *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*  LOCAL OUTPUTS:                                                     *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*  LOCALS:                                                            *         
*   RA,RC   - SPWORKD                                                 *         
*   R2      - SPGX WORK AREA                                          *         
*   R3      - SORT RECORD                                             *         
*   R4      - GOAL RECORD                                             *         
***********************                                               *         
*  LOGIC:                                                             *         
*  SELECT AGENCY CONVERSION CONTROL TABLE                             *         
*  WHILE INPUT TAPE NOT EOF                                           *         
*      GET INPUT RECORD                                               *         
*      CONVERT TO SORT RECORD BASED ON SELECTED AGENCY CONTROL TABLE  *         
*      PUT CONVERTED RECORD TO SORT                                   *         
*                                                                     *         
*  WHILE SORT NOT EOF                                                 *         
*      GET SORT RECORD                                                *         
*      IF CURRENT SORTKEY = PREVIOUS SORTKEY                          *         
*      '  THEN ADD UP POINTS AND DOLLARS                              *         
*      '  ELSE IF SORT KEY NE PREVIOUS                                *         
*      '  '    ' IF PREVIOUS GOAL RECORD                            *           
*      '  '    ' ' ATTACH SORTKEY TO GOAL RECORD                    *           
*      '  '    ' ' WRITE GOAL TO WORKFILE                           *           
*      '  '    ' MOVE CURRENT KEY TO PREVIOUS KEY                     *         
*      '  '    ' READ FOR CURRENT GOAL KEY                            *         
*      '  '    ' IF CURRENT GOAL KEY NOT FOUND                        *         
*      '  '    ' '  THEN BUILD SKELETON GOAL RECORD IN IO AREA        *         
*      '  '    ' '  ELSE READ RECORD INTO IO AREA                     *         
*      '  '    ' '                                                    *         
*      '  ' BUILD GOAL ELEMENT FROM SORT RECORD *                               
*      '  ' INSERT GOAL ELEMENT INTO RECORD *                                   
***********************************************************************         
         PRINT NOGEN                                                            
SPGX02   CSECT                                                                  
         NMOD1 0,**GX02**                                                       
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         L     R2,=V(SPGXWK)                                                    
         USING SPGXWK,R2                                                        
         LA    R3,SORTIO                                                        
         USING SORTREC,R3                                                       
         L     R4,ADGOAL                                                        
         USING GOALREC,R4                                                       
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BNE   *+8                                                              
         MVI   RCREQREP,C'N'       SUPPRESS REQUEST PRINTING                    
*                                                                               
         CLI   MODE,RUNLAST                                                     
         BNE   *+16                                                             
         MVI   GXMODE,CLOSEWK                                                   
         BAS   RE,PUTWRKR                                                       
         B     EXIT                                                             
*                                                                               
         CLI   MODE,REQFRST                                                     
         BNE   EXIT                                                             
*                                                                               
*                                                                               
         MVI   RCSUBPRG,2                                                       
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 =A(BLDCATBL),DMCB,(RC)                                           
         MVI   RCSUBPRG,1                                                       
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 =V(SORTER),DMCB,SORT,RECCARD,0                                   
         MVI   GOALSW,0                                                         
         BAS   RE,GETCL            GET THE CLIENT/PROD TABLE                    
         BAS   RE,SETIN            SET INPUT PHASE                              
PROCIN   GOTO1 AINPUT,DMCB,(RA),(R2) GET THE INPUT                              
         B     PROCIN1     *******FOR TESTING***********                        
         MVC   P(1),GXMODE                                                      
         OI    P,X'F0'                                                          
         MVC   P+2(20),SORTIO                                                   
         GOTO1 REPORT                                                           
         SPACE 1                                                                
PROCIN1  CLI   GXMODE,EOF          END OF INPUT                                 
         BE    PROCOUT                                                          
         CLI   GXMODE,PUTSORT                                                   
         BNE   PROCIN                                                           
         GOTO1 =V(SORTER),DMCB,=C'PUT',SORTIO                                   
         B     PROCIN                                                           
         EJECT                                                                  
PROCOUT  XC    PREVKY1,PREVKY1     CLEAR PREVIOUS KE                            
         MVI   BYPGET,0            SET TO GET FIRST RECORD                      
         MVI   GXMODE,0                                                         
         XC    SORTIO,SORTIO       INIT SORT RECORD                             
         LR    RE,R4               CLEAR GOAL RECORD                            
         LA    RF,2100                                                          
         XCEF                                                                   
PROCOUT2 BAS   RE,SUMSRT           READ AND SUMMARIZE SORT RECORDS              
         CLI   GXMODE,EOF                                                       
         BE    PRO2A                                                            
         CLC   PREVKY1,SORTREC     WILL BE EQUAL IF EOF ON FIRST READ           
         BE    PROCOUT3                                                         
PRO2A    CLI   GOALSW,1                                                         
         BNE   *+8                                                              
         BAS   RE,PUTWRKR          PUT GOAL ONTO WORKER FILE                    
         MVI   GOALSW,0                                                         
         CLI   GXMODE,EOF                                                       
         BE    ENDJOB                                                           
         MVC   PREVKY1,SORTREC                                                  
         BAS   RE,READGL           GET A GOAL RECORD                            
         L     RE,ADGOAL                                                        
         CLI   0(RE),2             DEAL WITH ERRORS                             
         BNE   *+8                                                              
         MVI   GOALSW,1                                                         
         SPACE 2                                                                
PROCOUT3 BAS   RE,BGE              BUILD A GOAL ELEMENT                         
         B     PROCOUT2                                                         
         SPACE 2                                                                
ENDJOB   GOTO1 =V(SORTER),DMCB,=C'END'                                          
EXIT     XMOD1 1                                                                
         LTORG                                                                  
         EJECT                                                                  
************************* HIPO ****************************************         
*  NUMBER:                                                            *         
*  TITLE: GETCL                                                       *         
*                                                                     *         
*  COMMENTS:    FILL CLIENT/PRD TABLE FROM CLIENT HDR                 *         
*                                                                     *         
*                                                                     *         
*  CALLED FROM:                                                       *         
*                                                                     *         
*                                                                     *         
*  CALLS TO:   CLPACK,GETCLT,BINSRCH                                  *         
*                                                                     *         
*                                                                     *         
*  INPUTS: QCLT                                                       *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*  GLOBAL OUTPUTS: CPTAB                                              *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*  LOCAL OUTPUTS:                                                     *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*  LOCALS:                                                            *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
***********************                                               *         
*  LOGIC: PACK THE CLIENT                                             *         
*         SET UP CLIENT KEY AND READ THE CLIENT                       *         
*         INSERT ALPHA AND NUMERIC CLT/PRD IN TABLE                   *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
GETCL    NTR1  BASE=VSPGX02                                                     
         GOTO1 CLPACK,DMCB,QCLT,BCLT                                            
         XC    KEY,KEY             READ THE CLIENT HEADER                       
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
         GOTO1 HIGH                                                             
         GOTO1 GETCLT                                                           
         L     R5,ADCLT                                                         
         USING CLTHDR,R5                                                        
         LA    R6,CLIST            POINT TO PRODUCT LIST                        
         XC    WORK,WORK                                                        
         LA    R7,WORK                                                          
         USING CPTABD,R7                                                        
GETCL2   CLI   3(R6),0                                                          
         BE    GETCLX              END OF LIST                                  
         MVC   CPACLT,QCLT         BUILD TABLE ENTRY                            
         MVC   CPAPRD,0(R6)                                                     
         MVC   CPBCLT,BCLT                                                      
         MVC   CPBPRD,3(R6)                                                     
         GOTO1 BINSRCH,DMCB,(X'01',WORK),CPTAB,CPTABCNT,CPTABLN,       X        
               CPTKLN,CPTABMAX                                                  
         OC    0(4,R1),0(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                TABLE FULL                                   
         MVC   CPTABCNT,8(R1)                                                   
         LA    R6,4(R6)            NEXT PRODUCT                                 
         B     GETCL2                                                           
         SPACE 2                                                                
GETCLX   XIT1                                                                   
         DROP  R7                                                               
         EJECT                                                                  
************************* HIPO ****************************************         
*  NUMBER:                                                            *         
*  TITLE: SETIN                                                       *         
*                                                                     *         
*  COMMENTS: SCAN FOR AGENCY INPUT ROUTINE                            *         
*                                                                     *         
*                                                                     *         
*  CALLED FROM:                                                       *         
*                                                                     *         
*                                                                     *         
*  CALLS TO:                                                          *         
*                                                                     *         
*                                                                     *         
*  INPUTS: QAGY                                                       *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*  GLOBAL OUTPUTS: AINPUT                                             *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*  LOCAL OUTPUTS:                                                     *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*  LOCALS:                                                            *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
***********************                                               *         
*  LOGIC: COMPARE AGYTAB TO QAGY                                      *         
*            IF EQUAL SET AINPUT AND EXIT                             *         
*               ELSE IF END OF TABLE BOMB                             *         
*               ELSE NEXT ENTRY AND TRY AGAIN                         *         
*                                                                     *         
***********************************************************************         
SETIN    NTR1                                                                   
         LA    RE,AGYTAB                                                        
SETIN2   CLC   QAGY,0(RE)          SAME AS REQUESTED AGENCY                     
         B     SETIN3               SET UP INPUT ADDRESS                        
         CLI   0(RE),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                INVALID AGENCY                               
         LA    RE,LNAGYTAB(RE)     TRY NEXT AGENCY                              
         B     SETIN2                                                           
         SPACE 1                                                                
SETIN3   L     RF,4(RE)                                                         
         ST    RF,AINPUT                                                        
         XIT1                                                                   
         EJECT                                                                  
************************* HIPO ****************************************         
*  NUMBER:                                                            *         
*  TITLE: SUMSRT                                                     *          
*                                                                     *         
*  COMMENTS: GET RECORDS FROM SORT AND SUMMARIZE THEM                 *         
*                                                                     *         
*                                                                     *         
*  CALLED FROM:                                                       *         
*                                                                     *         
*                                                                     *         
*  CALLS TO:                                                          *         
*                                                                     *         
*                                                                     *         
*  INPUTS:                                                            *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*  GLOBAL OUTPUTS:                                                    *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*  LOCAL OUTPUTS:                                                     *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*  LOCALS:                                                            *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
***********************                                               *         
*  LOGIC: IF BYPASS READ = 0 THEN GET SORT RECORD                     *         
*         RESET BYPASS READ                                           *         
*         IF INPUT MODE = EOF EXIT                                    *         
*         IF CURR SORT KEY = PRV SORT KEY THEN                        *         
*              ADD SORT RECORDS TOGETHER                              *         
*            ELSE SET BYPASS READ TO 1                                *         
*                 SET MODE TO PROCSORT                                *         
*                                                                     *         
***********************************************************************         
         EJECT                                                                  
SUMSRT   NTR1                                                                   
         CLI   BYPGET,2            PREVIOUS EOF                                 
         BNE   SUMSRT1                                                          
         MVI   GXMODE,EOF          SET EOF                                      
         XC    SORTIO,SORTIO                                                    
         B     SUMSRTX                                                          
         SPACE 2                                                                
SUMSRT1  CLI   BYPGET,1                                                         
         BE    SUMSRT1A                                                         
         GOTO1 =V(SORTER),SSDMCB,=C'GET'                                        
SUMSRT1A OC    SSDMCB+4(4),SSDMCB+4                                             
         BNZ   *+16                                                             
         MVI   GXMODE,PROCSORT                                                  
         MVI   BYPGET,2                                                         
         B     SUMSRTX                                                          
         L     R3,SSDMCB+4         GET SORT RECORD                              
         CLI   BYPGET,0            RECORD READ THIS TIME                        
         BE    SUMSRT1B             PROCESS IT                                  
         MVC   SORTIO,0(R3)        MOVE SORT RECORD TO IO AREA                  
         MVI   BYPGET,0                                                         
         B     SUMSRT1                                                          
SUMSRT1B CLI   SORTIO,0            FIRST TIME                                   
         BNE   *+14                                                             
         MVC   SORTIO,0(R3)        SET SORT RECORD IN IO                        
         B     SUMSRT1                                                          
         SPACE 2                                                                
         CLC   0(SRKLN,R3),SORTIO  RELEASE RECORD IN NOT THE SAME KEY           
         BNE   SUMSRT3                                                          
         LA    R3,SORTIO                                                        
         LA    R5,SRDATA           POINT TO DATA                                
         L     R3,SSDMCB+4                                                      
         LA    R6,(SREND-SRDATA)/4                                              
         LA    R9,SRDATA                                                        
         SPACE 2                                                                
SUMSRT2  ICM   RF,15,0(R9)         ADD DATA TO SORT IO                          
         ICM   RE,15,0(R5)                                                      
         AR    RE,RF                                                            
         STCM  RE,15,0(R5)                                                      
         LA    R9,4(R9)            SET TO NEXT SLOT                             
         LA    R5,4(R5)                                                         
         BCT   R6,SUMSRT2                                                       
         B     SUMSRT1                                                          
SUMSRT3  MVI   GXMODE,PROCSORT                                                  
         MVI   BYPGET,1                                                         
SUMSRTX  XIT                                                                    
SSDMCB   DS    6F                                                               
         EJECT                                                                  
************************* HIPO ****************************************         
*  NUMBER:                                                            *         
*  TITLE:       PUTWRKR                                               *         
*                                                                     *         
*  COMMENT:     PUT GOAL RECORD TO WORKER FILE                        *         
*                                                                     *         
*                                                                     *         
*  CALLED FROM:                                                       *         
*                                                                     *         
*                                                                     *         
*  CALLS TO:                                                          *         
*                                                                     *         
*                                                                     *         
*  INPUTS:                                                            *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*  GLOBAL OUTPUTS:                                                    *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*  LOCAL OUTPUTS:                                                     *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*  LOCALS:                                                            *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
***********************                                               *         
*  LOGIC:IF RCWRITE NE. Y THEN EXIT                                   *         
*        ATTACH SORT KEY TO GOAL RECORD                               *         
*        IF MODE EQ. CLOSE THEN *                                               
*            CLOSE WORKER FILE                                        *         
*        IF FIRST TIME THEN OPEN WORKER FILE                          *         
*            ELSE ADD RECORD TO WORKER FILE                           *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
PUTWRKR  NTR1  0,**PWRK**                                                       
         CLI   RCWRITE,C'Y'        BYPASS IF WRITE=NO                           
         BNE   WRKRXIT                                                          
         CLI   GXMODE,CLOSEWK      CLOSE THE WORKER FILE                        
         BE    WRKRCLS                                                          
         CLC   WRKCNT,=F'65000'                                                 
         BH    WRKRESET                                                         
WRKRET   L     RE,WRKCNT                                                        
         LA    RE,1(RE)                                                         
         ST    RE,WRKCNT                                                        
         SPACE 1                                                                
         MVC   WRKREC+4(6),PRVCLPR                                              
         SR    R1,R1                                                            
         ICM   R1,3,GLENGTH                                                     
         LA    R6,10(R1)                                                        
         STCM  R6,3,WRKREC                                                      
         CH    R6,=H'1540'                                                      
         BL    *+6                                                              
         DC    H'0'                                                             
         XC    WRKREC+2(2),WRKREC+2                                             
         LA    RF,WRKREC+10                                                     
         L     RE,ADGOAL                                                        
         MOVE  ((RF),(R1)),(RE)                                                 
         CLI   WRKOPNSW,1                                                       
         BE    WRKADD                                                           
         XC    WRKRINDX,WRKRINDX                                                
         LA    RE,WRKRINDX         SET UP TO OPEN WORKER FILE                   
         USING UKRECD,RE                                                        
         MVC   UKUSRID,RCORIGID                                                 
         MVC   UKSYSPRG,=C'SGX'                                                 
         MVC   UKSUBPRG,QAGY                                                    
         PACK  DUB,TODAY                                                        
         NI    DUB+6,X'0F'                                                      
         LH    RF,DUB+6                                                         
         SRL   RF,4                                                             
         STC   RF,UKDAY                                                         
         B     *+8                                                              
         MVI   UKDAY,21                                                         
*                                                                               
         ZIC   RF,UKDAY            ALLOW DAY OVERRIDE AND INC                   
         AH    RF,WRKINCR                                                       
         STC   RF,UKDAY                                                         
*                                                                               
         MVC   UKCLASS,QMED                                                     
         GOTO1 WORKER,DMCB,=C'INDEX',AWRKR4K,WRKRINDX                           
         MVI   WRKOPNSW,1                                                       
WRKADD   GOTO1 WORKER,DMCB,=C'ADD',AWRKR4K,WRKRINDX,WRKREC                      
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         B     WRKRXIT                                                          
         MVC   P(20),WRKREC                                                     
         GOTO1 REPORT                                                           
         B     WRKRXIT                                                          
         SPACE 2                                                                
WRKRESET CLI   WRKOPNSW,1          RESET FOR LARGE FILES                        
         BNE   WRKRSET1                                                         
         GOTO1 WORKER,DMCB,=C'CLOSE',AWRKR4K,WRKRINDX                           
WRKRSET1 XC    WRKCNT,WRKCNT                                                    
         LH    RF,WRKINCR                                                       
         LA    RF,1(RF)                                                         
         STH   RF,WRKINCR                                                       
         MVI   WRKOPNSW,0                                                       
         B     WRKRET                                                           
         SPACE 2                                                                
WRKRCLS  CLI   WRKOPNSW,1                                                       
         BNE   WRKRXIT                                                          
         GOTO1 WORKER,DMCB,=C'CLOSE',AWRKR4K,WRKRINDX                           
         SPACE 2                                                                
WRKRXIT  XIT1                                                                   
AWRKR4K  DC    A(WRKR4K)                                                        
WRKCNT   DC    F'0'                                                             
WRKINCR  DC    H'0'                                                             
         EJECT                                                                  
************************* HIPO ****************************************         
*  NUMBER:                                                            *         
*  TITLE:      READGL                                                 *         
*                                                                     *         
*  COMMENTS:   READ OR CREATE GOAL RECORD                             *         
*                                                                     *         
*                                                                     *         
*  CALLED FROM:                                                       *         
*                                                                     *         
*                                                                     *         
*  CALLS TO: BGS,BINSRCH,HIGH,GETGOAL                                 *         
*                                                                     *         
*                                                                     *         
*  INPUTS:         SPWORKD                                            *         
*                  SORT RECORD                                        *         
*                                                                     *         
*                                                                     *         
*  GLOBAL OUTPUTS: GOAL RECORD IN ADGOAL                              *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*  LOCAL OUTPUTS:                                                     *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*  LOCALS:                                                            *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
***********************                                               *         
*  LOGIC:      CONVERT CLIENT/PRODUCT TO BINARY                       *         
*              BUILD GOAL KEY IN KEY                                  *         
*              READ FOR GOAL                                          *         
*              IF FOUND THEN                                          *         
*                   READ GOAL RECORD                                  *         
*                   ELSE BUILD SKELETON GOAL RECORD                   *         
*              SET SWITCH TO HAVE GOAL                                *         
*                                                                     *         
***********************************************************************         
READGL   NTR1                                                                   
         XC    KEY,KEY                                                          
         CLC   PRVCLPR(6),SRCLT    DO WE HAVE BINARY CLT/PRD                    
         BE    READGL2              YES - BUILD GOAL KEY                        
*                                  NO - GET IT FROM TABLE                       
         SPACE 2                                                                
* FIND BINARY CLIENT/PRD                                                        
         MVC   PRVCLPR,SRCLT       SET SEARCH KEY                               
         GOTO1 BINSRCH,DMCB,(X'01',PRVCLPR),CPTAB,CPTABCNT,CPTABLN,    X        
               CPTKLN,CPTABMAX                                                  
         CLI   0(R1),0                                                          
         BE    READGL1                                                          
         L     RE,ADGOAL           IF A PRD HEADER IS MISSING                   
         MVI   0(RE),0             JUST BYPASS THE DATA                         
         B     READGLX                                                          
         DC    H'0'                CLT/PRD NOT FOUND IN TABLE                   
*                                                                               
READGL1  L     RE,0(R1)            SET TO TABLE SLOT                            
         USING CPTABD,RE                                                        
         MVC   BINCLT,CPBCLT       SET BINARY CLT/PRD                           
         MVC   BINPRD,CPBPRD                                                    
READGL2  LA    R4,KEY              BUILD GOAL KEY                               
         BAS   R9,RGFILKY          FILL THE KEY                                 
         MVC   KEYSAVE,KEY                                                      
         B     READGL3             IGNORE PREVIOUS RECORDS                      
         GOTO1 HIGH                SEE IF RECORD EXISTS                         
         CLC   KEY(13),KEYSAVE                                                  
         BNE   READGL3              NO- BUILD SKELETON                          
         GOTO1 GETGOAL              YES - READ THE RECORD                       
         B     READGLX                                                          
*                                                                               
READGL3  L     R4,ADGOAL                                                        
*        BUILD THE SKELETON RECORD                                              
         BAS   RE,BGS                                                           
         BAS   R9,RGFILKY                                                       
*                                                                               
READGLX  XIT1                                                                   
         SPACE 2                                                                
RGFILKY  MVI   GKEYTYPE,X'02'      BUILD GOAL KEY                               
         MVC   GKEYAM,BAGYMD                                                    
         MVC   GKEYCLT,BINCLT                                                   
         MVC   GKEYPRD,BINPRD                                                   
         MVC   GKEYMKT,SRMKT                                                    
         MVC   GKEYEST,SREST                                                    
         MVC   GKEYDPT,SRDPT                                                    
         MVC   GKEYSLN,SRSLN                                                    
         MVC   GKEYSEC,SRSLN                                                    
         BR    R9                                                               
         EJECT                                                                  
************************* HIPO ****************************************         
*  NUMBER:                                                            *         
*  TITLE: BGS - BUILD SKELETON GOAL RECORD                                      
*                                                                               
*  COMMENTS:                                                                    
*                                                                               
*                                                                               
*  CALLED FROM:                                                                 
*                                                                               
*                                                                               
*  CALLS TO:                                                                    
*                                                                               
*                                                                               
*  INPUTS: ADGOAL TODAYP                                                        
*                                                                               
*                                                                               
*                                                                               
*  GLOBAL OUTPUTS: SKELETON GOAL RECORD IN AREA POINTED TO BY ADGOAL            
*                                                                               
*                                                                               
*                                                                               
*  LOCAL OUTPUTS:                                                               
*                                                                               
*                                                                               
*                                                                               
*  LOCALS:                                                                      
*                                                                               
*                                                                               
*                                                                               
*                                                                               
***********************                                                         
*  LOGIC:                                                                       
*       CLEAR GOAL IO AREA                                                      
*       MOVE CONSTANTS INTO GOAL RECORD                                         
*                                                                               
***********************************************************************         
BGS      NTR1                                                                   
         LR    RE,R4               CLEAR GOAL RECORD                            
         LA    RF,2100                                                          
         XCEF                                                                   
         SPACE 1                                                                
         MVI   GKEYTYPE,X'02'      BUILD SKELETON RECORD                        
         MVC   GKEYAM,BAGYMD                                                    
         MVC   GAGYALPH,QAGY                                                    
         MVC   GLENGTH,=AL2(LNGLREC+LNGLCC)                                     
         MVI   GOCODE,X'20'                                                     
         MVI   GOLEN,LNGDELEM                                                   
         MVC   GBUYNAME,=CL12'TAPE'                                             
         MVC   GREDATE,TODAYP                                                   
         MVC   GACTDATE,TODAYP                                                  
         LA    RE,LNGLREC(R4)                                                   
         USING GLCCEL,RE                                                        
         MVI   GLCCEL,X'43'                                                     
         MVI   GLCCLEN,LNGLCC                                                   
         MVC   GLCCBTLR,SRBTLRCC                                                
         MVC   GLCCAGY,SRAGYCC                                                  
         MVC   GLCCPRD,SRPRDCC                                                  
         MVC   GLCCCAT,SRCATCC                                                  
         MVC   GLCCDPT,SRDPT                                                    
         DROP  RE                                                               
         XIT1                                                                   
         EJECT                                                                  
************************* HIPO ****************************************         
*  NUMBER:                                                            *         
*  TITLE: BGE - BUILD GOAL ELEMENT FROM SORT RECORD AND INSERT INTO  *          
*           RECORD                                                    *         
*  COMMENTS:                                                          *         
*                                                                     *         
*                                                                     *         
*  CALLED FROM:                                                       *         
*                                                                     *         
*                                                                     *         
*  CALLS TO: RECUP                                                    *         
*                                                                     *         
*                                                                     *         
*  INPUTS: SORT RECORD,ADGOAL                                         *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*  GLOBAL OUTPUTS: UPDATED GOAL RECORD AT ADGOAL                      *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*  LOCAL OUTPUTS:                                                     *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*  LOCALS:                                                            *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
***********************                                               *         
*  LOGIC: SET UP NEW GOAL ELEMENT                                     *         
*         SCAN RECORD FOR UNTIL GOAL ELEMENT GTE NEW ELEMENT     *              
*         IF GOAL ELEMENT = NEW ELEMENT                        *                
*              DELETE GOAL ELEMENT                                    *         
*         INSERT GOAL ELEMENT AT PROPER PLACE USING RECUP             *         
*         RETURN                                                      *         
*                                                                     *         
***********************************************************************         
BGE      NTR1                                                                   
         L     R4,ADGOAL                                                        
         MVC   GDCATCC,SRCATCC     MOVE IN CATAGORY CODE                        
         LA    RE,GDSKL                                                         
         USING GLEMENT,RE                                                       
DTOK     MVC   GLWEEK,SRWKOF       SET VARIABLE DATA IN GOAL ELEMENT            
         MVC   GLGRP,SRGRP                                                      
         MVC   GLBUDGET,SRDOL                                                   
         DROP  RE                                                               
         BAS   R9,SLOTELM          SET UP FOR INSERT                            
         OC    SRGRP(8),SRGRP      ANY DATA                                     
         BZ    BGEXIT               NO - EXIT                                   
         GOTO1 =V(RECUP),DMCB,ADGOAL,GDSKL,(R8)                                 
BGEXIT   XIT1                                                                   
         SPACE 2                                                                
SLOTELM  LA    R8,GLEMENT          SET UP INSERT POSITION                       
SLOTELM1 CLI   0(R8),0             END OF RECORD                                
         BER   R9                   RETURN                                      
         CLC   0(4,R8),GDSKL       CURR ELM GT. RECORD ELM                      
         BHR   R9                   RETURN                                      
         BE    SLOTELM2            DELETE IF DUPLICATE                          
         SPACE 1                                                                
         ZIC   R7,1(R8)            TRY NEXT ELM                                 
         AR    R8,R7                                                            
         B     SLOTELM1                                                         
         SPACE 2                                                                
SLOTELM2 GOTO1 =V(RECUP),DMCB,ADGOAL,(R8)                                       
         BR    R9                                                               
         EJECT                                                                  
         EJECT                                                                  
SORT     DC    C'SORT FIELDS=(1,14,BI,A),WORK=1 '                               
RECCARD  DC    C'RECORD TYPE=F,LENGTH=38 '                                      
         LTORG                                                                  
         EJECT                                                                  
EOF      EQU   255                                                              
PUTSORT  EQU   1                                                                
PROCSORT EQU   2                                                                
CLOSEWK  EQU   240                                                              
SPGXWK   CSECT                                                                  
AINPUT   DC    F'0'                                                             
AGETCL   DC    A(GETCL)                                                         
VSPGX02  DC    V(SPGX02)                                                        
INCOUNT  DC    F'0'                                                             
GDSKL    DS    0C                  SKELETON GOAL ELEMENT                        
         DC    X'21'                                                            
         DC    AL1(12)                                                          
         DS    XL2                                                              
         DS    XL4                                                              
         DS    XL4                                                              
         DC    4X'00'                                                           
WRKOPNSW DC    X'00'               WORKER OPEN SWITCH                           
GXMODE   DS    C                   MODE RETURNED BY INPUT                       
BYPGET   DS    X                   BYPASS SORT GET                              
GOALSW   DS    X                   GOAL RECORD IN PROCESS                       
BINCLT   DS    XL2                 PACKED CLIENT                                
BINPRD   DS    C                   BINARY PRODUCT                               
PREVKY1  DS    CL11                PREVIOUS SORT KEY                            
*                                                                               
CPTAB    DC    A(CPTABC)                                                        
CPTABCNT DC    A(0)                                                             
CPTABMAX DC    A(CPTABCLN/CPTABLN)                                              
*                                                                               
         DS    0F                                                               
AGYTAB   DC    C'CC',H'0',V(SPGXCC)                                             
         DC    C'CK',H'0',V(SPGXCC)                                             
         DC    X'FF'                                                            
LNAGYTAB EQU   8                                                                
*                                                                               
PRVCLPR  DS    CL6                 PREVIOUS ALPHA CLIENT/PRD                    
SORTIO   DS    CL60                                                             
WRKRINDX DS    16C                                                              
WRKREC   DS    2200C                                                            
         EJECT                                                                  
************************* HIPO ****************************************         
*  NUMBER:                                                            *         
*  TITLE: SPGXCC                                                      *         
*                                                                     *         
*  COMMENTS: COKE GOALS TRANSFER INPUT ROUTINE                        *         
*                                                                     *         
*                                                                     *         
*  CALLED FROM: SPGX02                                                *         
*                                                                     *         
*                                                                     *         
*  CALLS TO:                                                          *         
*                                                                     *         
*                                                                     *         
*  INPUTS: SPWORKD                                                    *         
*          SPGXWK                                                     *         
*                                                                     *         
*                                                                     *         
*  GLOBAL OUTPUTS: GXMODE SORT-RECORD                                 *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*  LOCAL OUTPUTS:                                                     *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*  LOCALS:                                                            *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
***********************                                               *         
*  LOGIC:                                                             *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
SPGXCC   CSECT                                                                  
         NMOD1 0,**GXCC**                                                       
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         L     R2,=V(SPGXWK)                                                    
         USING SPGXWK,R2                                                        
         SPACE 2                                                                
         LA    R3,SORTIO                                                        
         USING SORTREC,R3                                                       
         L     R4,ADGOAL                                                        
         USING GOALREC,R4                                                       
         L     R5,=A(INREC)                                                     
         USING COKETAPE,R5                                                      
         SPACE 2                                                                
         CLI   CCFIRST,2           DUPLICATE UNDER COKE CLIENT                  
         BNE   CCGENX                                                           
         MVC   SRCLT,=C'CC '                                                    
         MVI   GXMODE,PUTSORT                                                   
         MVI   CCFIRST,0                                                        
         B     CCXIT                                                            
CCGENX   DS    0H                                                               
         SPACE 1                                                                
         CLI   CCFIRST,X'01'                                                    
         BNE   RDCLTSX                                                          
         OPEN  (CCINPUT,(INPUT))                                                
         MVI   CCFIRST,X'00'                                                    
         XC    INCOUNT,INCOUNT                                                  
         MVI   INDEX,X'FF'                                                      
         BAS   R9,CCBLDDT          BUILD THE QUARTER DATES                      
         EJECT                                                                  
         L     R6,=A(INTRLIST)     SET CLIENT INTERFACE PTR.                    
         MVI   0(R6),X'FF'                                                      
         XC    KEY,KEY             CLEAR THE KEY                                
         MVC   KEY+1(1),BAGYMD     SET FOR FIRST CLTHDR                         
RDCLTS   GOTO1 HIGH                READ ALL CLIENTS AND BUILD                   
*                                  CLIENT/PRODUCT TABLE                         
         CLC   KEY+1(1),BAGYMD     SAME AGENCY MEDIA                            
         BNE   RDCLTSX             NO - EXIT                                    
         GOTO1 CLUNPK,DMCB,KEY+2,QCLT                                           
         L     RF,AGETCL                                                        
         BASR  RE,RF               BUILD TABLE OF PROUCT NUMBERS                
         SPACE 1                                                                
         L     R7,ADCLT            BUILD TABLE OF INTERFACE NUMBERS             
         USING CLTHDR,R7                                                        
         LA    RE,CCLTIFC                                                       
         CLI   0(RE),X'F0'         QUICK TEST FOR NUMERIC                       
         BL    *+12                                                             
         LA    RE,1(RE)            COUNT THE CHARACTERS                         
         B     *-12                                                             
         LA    RF,CCLTIFC                                                       
         SR    RE,RF               DETERMINE THE NUMERIC LENCTH                 
         LTR   RE,RE                                                            
         BZ    RDCLTS2             EXIT IF NO NUMERICS                          
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB(8),CCLTIFC(0)                                                
         CVB   RF,DUB                                                           
         STH   RF,HALF                                                          
         CH    RF,=H'9999'                                                      
         BE    RDCLTS2             EXIT IF NO NUMERICS                          
         L     R1,=A(INTRLIST)                                                  
RDCDUP   CLI   0(R1),X'FF'                                                      
         BE    RDCNODUP                                                         
         CLC   0(2,R1),HALF                                                     
         BNE   RDCDUP1                                                          
         EDIT  (B2,HALF),(3,P)                                                  
         MVC   P+4(3),QCLT                                                      
         MVC   P+8(3),2(R1)                                                     
         MVC   P+14(24),=C'DUPLICATE INTERFACE CODE'                            
         GOTO1 REPORT                                                           
         LH    RF,HALF                                                          
         B     RDCNODUP                                                         
RDCDUP1  LA    R1,5(R1)                                                         
         B     RDCDUP                                                           
*                                                                               
RDCNODUP STCM  RF,3,0(R6)                                                       
*                                                                               
RDCLTS1  MVC   2(3,R6),QCLT                                                     
         LA    R6,5(R6)                                                         
RDCLTS2  MVI   0(R6),X'FF'         SET END OF LIST                              
         MVI   KEY+4,X'FF'         SET TO READ NEXT CLIENT                      
         MVC   KEY+5(7),KEY+4                                                   
         B     RDCLTS                                                           
RDCLTSX  DS    0H                                                               
         DROP  R7                                                               
         SPACE 2                                                                
GETCOKE  CLI   INDEX,14                                                         
         BL    GETCOKE2                                                         
GETCOKE1 L     R0,=A(INREC)                                                     
         GET   CCINPUT,(R0)                                                     
*        RECORD FILTERS - JUST BYPASS THE RECORD                                
         CLI   CCRECTYP,C'1'       IS IT A GOAL                                 
         BNE   GETCOKE                                                          
         CLI   CCMEDIA,C' '        DEFAULT MEDIA SETUP                          
         BH    *+8                                                              
         MVI   CCMEDIA,C'T'                                                     
         CLI   CCMEDIA,C'C'        CABLE GOES UNDER MEDIA T                     
         BNE   *+8                                                              
         MVI   CCMEDIA,C'T'                                                     
         CLC   CCMEDIA,QMED        ONLY DO REQUESTED MEDIA                      
         BNE   GETCOKE                                                          
**       CLC   QAGY,=C'CC'                                                      
**       BNE   *+14                                                             
**       CLC   CCYEAR+2(2),=C'95'                                               
**       BH    GETCOKE                                                          
**       CLC   QAGY,=C'CK'                                                      
**       BNE   *+14                                                             
**       CLC   CCYEAR+2(2),=C'96'                                               
**       BL    GETCOKE                                                          
************************2/05/93 PUT THEM BACK*******************                
*        CLI   CCDPT,C'+'          BYPASS '+' AND '-' DAYPARTS                  
*        BE    GETCOKE1                                                         
*        CLI   CCDPT,C'-'                                                       
*        BE    GETCOKE1                                                         
****************************************************************                
*        RECORD OK - PROCESS IT                                                 
         MVI   INDEX,0                                                          
         CLC   CCCLT,=C'666'      PER S PEEPLES 4/16/96                         
         BE    GETCOKE1                                                         
         OC    CCMKT,=C'0000'                                                   
         CLC   CCMKT,=C'0000'                                                   
         BE    GETCOKE1                                                         
         B     GETCOKE2                                                         
         CLC   CCMKT,=C'1001'                                                   
         BNE   GETCOKE                                                          
         L     RE,=A(INREC)                                                     
         MVC   P(132),0(RE)                                                     
         GOTO1 REPORT                                                           
GETCOKE2 L     RE,INCOUNT          COUNT THE INPUT RECORDS                      
         LA    RE,1(RE)                                                         
         ST    RE,INCOUNT                                                       
         B     *+12                NOP TESTING LIMITS                           
         C     RE,=F'5111'                                                      
         BH    CCEOF                                                            
         CLI   CCADJDOL,C'*'                                                    
         BNE   *+10                                                             
         MVC   CCADJDOL,=C'000000000'                                           
         OC    CCADJDOL,=C'000000000'                                           
         PACK  DUB,CCADJDOL                                                     
         CVB   R9,DUB                                                           
         MH    R9,=H'100'          CONVERT TO PENNIES                           
         ST    R9,CCSPCDOL                                                      
         SPACE 2                                                                
         MVC   SRBTLRCC,CCBTLR                                                  
         MVC   SRAGYCC,CCCLT                                                    
         MVC   SRPRDCC,CCPRD                                                    
         MVC   SRDPT,CCDPT         DAYPART                                      
         MVI   SRSLN,30            SPOT LENGTH                                  
         MVI   SRMEDIA,C'T'        ONLY ALLOW MEDIA R OR T                      
         CLI   CCMEDIA,C'R'                                                     
         BNE   *+12                                                             
         MVI   SRMEDIA,C'R'        SET MEDIA TO RADIO                           
         MVI   SRSLN,60            SPOT LENGTH (60 FOR RADIO)                   
         SPACE 2                                                                
         BAS   R9,CCPCNV           PRODUCT                                      
         BAS   R9,CNVMKT           MARKET                                       
         BAS   R9,CCGCNV           GRP                                          
         BAS   R9,CCDCNV           CPP                                          
         BAS   R9,CCCNVAG          AGENCY                                       
         BAS   R9,CCCNVDT          DATE AND ESTIMATE                            
         SPACE 1                                                                
         ICM   RF,15,SRGRP         CONVERT CPP TO DOLLARS                       
         ICM   RE,15,SRDOL                                                      
         MR    RE,RE                                                            
         SLDA  RE,1                                                             
         D     RE,=F'10'                                                        
         SRA   RF,1                                                             
         STCM  RF,15,SRDOL                                                      
         OC    CCSPCDOL,CCSPCDOL                                                
         BZ    GETCOKE3                                                         
         XC    SRGRP,SRGRP                                                      
         SR    RE,RE                                                            
         L     RF,CCSPCDOL                                                      
         D     RE,QTNUMWKS                                                      
         STCM  RF,15,SRDOL                                                      
*                                                                               
         CLI   INDEX,0                                                          
         BNE   BALOK                                                            
         L     RE,QTNUMWKS         ADJUST WEEK 1 FOR BALANCING                  
         MR    RE,RE                                                            
         L     RE,CCSPCDOL                                                      
         SR    RE,RF                                                            
         ICM   RF,15,SRDOL                                                      
         AR    RE,RF                                                            
         STCM  RE,15,SRDOL                                                      
         LR    RF,RE                                                            
*                                                                               
BALOK    CLI   INDEX,1                                                          
         BH    GETCOKE3                                                         
         ICM   RF,15,SRDOL                                                      
*                            SET NEGATIVE DOLLARS FOR "-" DAYPART               
GETCOKE3 CLI   SRDPT,C'-'                                                       
         BNE   NOTMINUS                                                         
         LNR   RF,RF                                                            
         STCM  RF,15,SRDOL                                                      
NOTMINUS ZIC   RE,INDEX                                                         
         LA    RE,1(RE)                                                         
         STC   RE,INDEX                                                         
         OC    SRWKOF,SRWKOF                                                    
         BZ    GETCOKE                                                          
         MVI   GXMODE,PUTSORT                                                   
         MVI   CCFIRST,2           SET TO DUPLICATE UNDER COKE                  
         B     CCXIT                                                            
         SPACE 2                                                                
CCEOF    MVI   GXMODE,EOF                                                       
         CLOSE (CCINPUT,REWIND)                                                 
         SPACE 2                                                                
CCXIT    XMOD1 1                                                                
         EJECT                                                                  
CCPCNV   LA    R8,CCPTAB           CONVERT PROD TO DDS                          
         XC    ERROR,ERROR         SET THE ERROR                                
         MVC   ERROR(13),=C'PRODUCT ERROR'                                      
         MVC   ERROR+14(2),CCPRD                                                
CCPCNV1  CLI   0(R8),X'FF'         END OF TABLE                                 
         BE    ERRORP                                                           
         CLC   CCPRD,0(R8)         IS THIS THE ONE                              
         BE    CCPCNV2                                                          
         LA    R8,5(R8)            NO - TRY NEXT                                
         B     CCPCNV1                                                          
         SPACE 1                                                                
CCPCNV2  MVC   SRPRD,2(R8)                                                      
         BR    R9                                                               
         SPACE 2                                                                
CNVMKT   PACK  DUB,CCMKT           CONVERT MARKET NUMBER                        
         CVB   R8,DUB                                                           
         STCM  R8,3,SRMKT                                                       
         BR    R9                                                               
         SPACE 2                                                                
CCGCNV   ZIC   RE,INDEX            CONVERT THE GRPS                             
         SLL   RE,2                                                             
         LA    RF,CCWKGRP(RE)                                                   
         PACK  DUB,0(4,RF)                                                      
         CVB   R8,DUB                                                           
         MH    R8,=H'10'           ADJUST PRECISION                             
         STCM  R8,15,SRGRP                                                      
         BR    R9                                                               
         SPACE 2                                                                
CCDCNV   TM    CCCPP,X'F0'         HAVE HAD GARBAGE HERE                        
         BO    *+10                                                             
         MVC   CCCPP(6),=C'000000' SO ZAP IT IF IT OCCURS                       
*                                                                               
         PACK  DUB,CCCPP           CONVERT THE CPP                              
         CVB   R8,DUB                                                           
         STCM  R8,15,SRDOL                                                      
         BR    R9                                                               
         SPACE 2                                                                
* CONVERT AGENCY TO CLIENT CODE                                                 
CCCNVAG  CLC   CCCLT,=C'900'       FORCE MCCANN OFFICES TO MCCANN               
         BL    CCCNVAG1                                                         
         MVC   SRCLT,=C'MC '                                                    
         BR    R9                                                               
         SPACE 1                                                                
CCCNVAG1 XC    ERROR,ERROR                                                      
         MVC   ERROR(14),=C'UNKNOWN AGENCY'                                     
         MVC   ERROR+15(3),CCCLT                                                
         PACK  DUB,CCCLT           CONVERT AGENCY TO BINARY                     
         CVB   RE,DUB                                                           
         STCM  RE,3,BNCLT                                                       
         SPACE 1                                                                
         L     R6,=A(INTRLIST)                                                  
CCCNVAG2 CLI   0(R6),X'FF'         FIND EQUIVALENCE                             
         BE    ERRORP                                                           
         CLC   BNCLT,0(R6)                                                      
         BE    *+12                                                             
         LA    R6,5(R6)                                                         
         B     CCCNVAG2                                                         
         MVC   SRCLT,2(R6)                                                      
         BR    R9                                                               
         EJECT                                                                  
CCBLDDT  L     R6,=A(CATTBLE)      BUILD THE WEEKLY START DATES                 
         USING CCQTRSD,R6                                                       
         L     R8,MEDBUFF                                                       
         USING MEDBLOCK,R8                                                      
         MVC   MEDNUMWK,=F'15'                                                  
         MVC   MEDNUMMO,=F'0'                                                   
         MVC   MEDNUMPE,=F'1'                                                   
         MVC   MEDNUMQT,=F'0'                                                   
CCBLDDT1 MVC   QSTART(12),CCQSTDT                                               
         GOTO1 MEDDATE,DMCB,(RA)                                                
         L     RF,MEDAFRST                                                      
         LA    R1,15               BUILD WEEKLY LIST                            
         LA    R7,CCQWKS                                                        
         XC    QTNUMWKS,QTNUMWKS                                                
CCBLDDT2 MVC   0(2,R7),0(RF)       MOVE TO INDEX SLOT                           
*                                                                               
         OC    0(2,R7),0(R7)       COUNT NUMBER OF ACTIVE WEEKS                 
         BZ    CCBLDDT3            IN QUARTER FOR +,- DAYPART ALLOCAT           
         L     RE,QTNUMWKS                                                      
         LA    RE,1(RE)                                                         
         ST    RE,QTNUMWKS                                                      
*                                                                               
CCBLDDT3 LA    R7,2(R7)                                                         
         LA    RF,12(RF)                                                        
         BCT   R1,CCBLDDT2                                                      
*                                                                               
         MVC   CCNUMWKS,QTNUMWKS+3 SAVE NUMBER OF WEEKS IN QTR                  
*                                                                               
         LA    R6,CCCLEN(R6)       GET NEXT QUARTER                             
         CLI   0(R6),X'FF'                                                      
         BNE   CCBLDDT1                                                         
         BR    R9                                                               
         EJECT                                                                  
* CONVERT SLOTS TO DATES AND QUARTERS TO ESIMATES                               
CCCNVDT  XC    ERROR,ERROR                                                      
         MVC   ERROR(12),=C'INVALID DATE'                                       
         MVC   ERROR+13(4),CCYEAR                                               
         MVI   ERROR+17,C'/'                                                    
         MVC   ERROR+18(1),CCQTR                                                
         MVC   ERROR+20(9),=C'CATAGORY='                                        
         MVC   ERROR+29(2),CCCATCC                                              
         MVC   ERROR+32(8),=C'PRODUCT='                                         
         MVC   ERROR+40(2),CCPRD                                                
         L     R6,=A(CATTBLE)      POINT TO QUARTER TABLE                       
         USING CCQTRSD,R6                                                       
CCCNVDT1 CLI   0(R6),X'FF'         NOT IN TABLE                                 
         BE    ERRORP                                                           
         MVC   FULL(2),CCYEAR+2    BUILD SEARCH YEAR/QTR                        
         MVC   FULL+2(1),CCQTR                                                  
         SPACE 1                                                                
CCCNVDTA CLC   FULL(3),CCQYY       YEAR/QTR FOUND                               
         BE    *+12                                                             
         LA    R6,CCCLEN(R6)       TRY NEXT YEAR/QTR                            
         B     CCCNVDT1                                                         
         CLC   CCCATCC,CCCAT       IS IT THE CORRECT CATEGORY?                  
         BE    CCCNVDTB                                                         
         LA    R6,CCCLEN(R6)       BUMP TABLE                                   
         CLI   0(R6),X'FF'         REACHED THE END?                             
         BE    ERRORP                                                           
         B     CCCNVDTA                                                         
         SPACE 1                                                                
CCCNVDTB ZIC   RE,INDEX            INDEX FOR PROPER WEEK                        
         SLL   RE,1                                                             
         LA    R7,CCQWKS(RE)                                                    
         MVC   SRWKOF,0(R7)        SET THE WEEK                                 
         MVC   SREST,CCQEST        SET THE ESTIMATE                             
         XC    QTNUMWKS,QTNUMWKS                                                
         MVC   QTNUMWKS+3(1),CCNUMWKS RESET WKS IN QTR                          
         BR    R9                                                               
         EJECT                                                                  
ERRORP   MVC   P1(L'ERROR),ERROR   PRINT THE ERROR MESSAGE                      
         L     RE,=A(INREC)                                                     
         MVC   P2(132),0(RE)                                                    
         GOTO1 REPORT                                                           
         MVI   INDEX,X'FF'         FORCE TO READ NEXT RECORD                    
         B     GETCOKE                                                          
         LTORG                                                                  
         EJECT                                                                  
CCFIRST  DC    X'01'                                                            
CCPTAB   DC    C'01',C'CL '                                                     
         DC    C'02',C'FR '                                                     
         DC    C'03',C'TB '                                                     
         DC    C'04',C'SP '                                                     
         DC    C'05',C'PB '                                                     
         DC    C'06',C'MY '                                                     
         DC    C'07',C'FA '                                                     
         DC    C'08',C'CC '                                                     
         DC    C'11',C'MM '                                                     
         DC    C'12',C'DC '                                                     
         DC    C'13',C'CY '                                                     
         DC    C'14',C'DS '                                                     
         DC    C'15',C'PA '                                                     
         DC    C'16',C'TC '                                                     
         DC    C'17',C'OK '                                                     
         DC    C'18',C'CD '                                                     
         DC    C'20',C'BQ '                                                     
         DC    C'22',C'SG '        PROJECT VINILLA                              
         DC    C'23',C'CT '        KINGFISHER                                   
         DC    C'25',C'PX '        MR PIBB EXTRA                                
         DC    C'34',C'PR '                                                     
         DC    C'58',C'MJ '                                                     
         DC    C'59',C'FT '                                                     
         DC    C'69',C'PJ '        PLANET JAVA                                  
         DC    C'70',C'NT '                                                     
         DC    C'71',C'NC '                                                     
         DC    C'72',C'MR '        MAD RIVER JUICES - ADDED 9/21/01             
         DC    C'73',C'MT '        MAD RIVER TEAS - ADDED 9/21/01               
         DC    C'75',C'DA '                                                     
         DC    C'76',C'KX '                                                     
         DC    C'96',C'CO '                                                     
         DC    X'FF'                                                            
LNCCPTAB EQU   5                                                                
*                                                                               
         SPACE 2                                                                
INDEX    DS    C                                                                
BNCLT    DS    CL2                                                              
QTNUMWKS DS    F                                                                
CCSPCDOL DS    F                                                                
ERROR    DS    CL50                                                             
         PRINT NOGEN                                                            
CCINPUT  DCB   DDNAME=CCINPUT,DSORG=PS,RECFM=FB,LRECL=132,             X        
               MACRF=GM,EODAD=CCEOF                                             
*              BLKSIZE=32736,MACRF=GM,EODAD=CCEOF                               
         LTORG                                                                  
INTRLIST DS    CL1320                                                           
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -           
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -           
*BUILD CATEGORY TABLE                                                           
BLDCATBL DS    0D                                                               
         NMOD1 0,**CATB**                                                       
         L     RC,0(R1)                                                         
         USING SPWORKD,RA,RC                                                    
*                                                                               
         LA    R6,CATTBLE          COKE CATEGORY TABLE                          
         USING CCQTRSD,R6                                                       
         XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING ESTHDR,R5                                                        
         MVI   EKEYTYPE,X'00'      RECORD TYPE                                  
         MVC   EKEYAM,SVAGYMD      AGENCY/MEDIA                                 
         MVC   EKEYCLT,=X'885F'    CLIENT 'CC'                                  
         MVC   EKEYPRD,=C'POL'     PRODUCT                                      
         GOTO1 HIGH                                                             
*                                                                               
CCAT1    CLC   KEY(7),KEYSAVE                                                   
         BNE   CCATX                                                            
         OC    KEY+7(1),KEY+7      IS THIS AN ESTIMATE REC?                     
         BZ    CCATNXRC                                                         
         GOTO1 GETEST                                                           
         DROP  R5                                                               
*                                                                               
         L     R5,ADEST                                                         
         USING ESTHDR,R5                                                        
*                                                                               
         GOTO1 DATCON,DMCB,(0,ESTART),(3,SVBESTRT)                              
         GOTO1 DATCON,DMCB,(0,EEND),(3,SVBEEND)                                 
*                                                                               
         MVC   SVSTYEAR,SVBESTRT   START YEAR                                   
         MVC   SVSTMO,SVBESTRT+1   START MONTH                                  
*                                                                               
CCAT3    CLI   SVSTMO,3            IS IT THE FIRST QUARTER?                     
         BH    CCAT4                                                            
         MVC   CCQQTR,=C'1'                                                     
         MVC   SVBQSTRT(1),SVSTYEAR                                             
         MVI   SVBQSTRT+1,01                                                    
         MVI   SVBQSTRT+2,15                                                    
         MVC   SVBQEND(1),SVSTYEAR                                              
         MVI   SVBQEND+1,03                                                     
         MVI   SVBQEND+2,15                                                     
         B     CCAT7                                                            
*                                                                               
CCAT4    CLI   SVSTMO,6            IS IT THE SECOND QUARTER?                    
         BH    CCAT5                                                            
         MVC   CCQQTR,=C'2'                                                     
         MVC   SVBQSTRT(1),SVSTYEAR                                             
         MVI   SVBQSTRT+1,04                                                    
         MVI   SVBQSTRT+2,15                                                    
         MVC   SVBQEND(1),SVSTYEAR                                              
         MVI   SVBQEND+1,06                                                     
         MVI   SVBQEND+2,15                                                     
         B     CCAT7                                                            
*                                                                               
CCAT5    CLI   SVSTMO,9            IS IT THE THIRD QUARTER?                     
         BH    CCAT6                                                            
         MVC   CCQQTR,=C'3'                                                     
         MVC   SVBQSTRT(1),SVSTYEAR                                             
         MVI   SVBQSTRT+1,07                                                    
         MVI   SVBQSTRT+2,15                                                    
         MVC   SVBQEND(1),SVSTYEAR                                              
         MVI   SVBQEND+1,09                                                     
         MVI   SVBQEND+2,15                                                     
         B     CCAT7                                                            
*                                                                               
CCAT6    MVC   CCQQTR,=C'4'        MUST BE THE FOURTH!                          
         MVC   SVBQSTRT(1),SVSTYEAR                                             
         MVI   SVBQSTRT+1,10                                                    
         MVI   SVBQSTRT+2,15                                                    
         MVC   SVBQEND(1),SVSTYEAR                                              
         MVI   SVBQEND+1,12                                                     
         MVI   SVBQEND+2,15                                                     
*                                                                               
CCAT7    GOTO1 DATCON,DMCB,(3,SVBQSTRT),(0,SVCQSTRT)                            
         GOTO1 DATCON,DMCB,(3,SVBQEND),(0,SVCQEND)                              
         GOTO1 GETBROAD,DMCB,SVCQSTRT,BLOCK,=V(GETDAY),=V(ADDAY)                
         GOTO1 GETBROAD,DMCB,SVCQEND,BLOCK+12,=V(GGETDAY),=V(ADDAY)             
         GOTO1 DATCON,DMCB,(0,BLOCK+18),(3,WORK)                                
*                                                                               
         ZIC   R0,WORK                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  CCQYY,DUB           YEAR FROM QUARTER END DATE                   
         MVC   CCQSTDT,BLOCK       START DATE                                   
         MVC   CCQENDT,BLOCK+18    END DATE                                     
         MVC   CCCAT,EUSER1        CATEGORY                                     
         CLI   CCCAT+1,C' '                                                     
         BH    *+14                                                             
         MVC   CCCAT+1(1),CCCAT                                                 
         MVI   CCCAT,C'0'                                                       
         MVC   CCQEST,EKEYEST      ESTIMATE                                     
*                                                                               
         EDIT  CCQQTR,(1,P+1)                                                   
         MVI   P+2,C'/'                                                         
         MVC   P+3(2),CCQYY                                                     
         EDIT  CCQEST,(4,P+7)                                                   
         MVC   P+13(6),CCQSTDT                                                  
         MVC   P+23(6),CCQENDT                                                  
         MVC   P+33(6),ESTART                                                   
         MVC   P+42(6),EEND                                                     
         MVC   P+50(2),CCCAT                                                    
*                                                                               
         CLC   CCQENDT,ESTART      TEST QTR ENDS BEFORE EST START               
         BNL   CCAT10              NO                                           
         XC    0(CCCLEN,R6),0(R6)  CLEAR THE CURRENT ENTRY                      
         MVC   P,SPACES                                                         
         B     CCAT20                                                           
*                                                                               
CCAT10   CLC   CCQSTDT,EEND        EST END TO QUARTER START                     
         BNH   CCAT12                                                           
         XC    0(CCCLEN,R6),0(R6)  CLEAR THE CURRENT ENTRY                      
         MVC   P,SPACES                                                         
         B     CCATNXRC                                                         
*                                                                               
CCAT12   GOTO1 REPORT              PRINT THE LINE                               
         LA    R6,CCCLEN(R6)       BUMP TABLE                                   
         L     R7,=A(EOTABLE)                                                   
         CR    R7,R6               HAVE WE REACHED THE END OF TABLE?            
         BH    *+6                 NO                                           
         DC    H'0'                                                             
*                                                                               
CCAT20   ZIC   R8,SVSTMO                                                        
         LA    R8,3(R8)                                                         
         STC   R8,SVSTMO                                                        
         CLI   SVSTMO,12           HAS THE END OF YEAR BEEN REACHED             
         BNH   CCAT3                                                            
         SH    R8,=H'12'           BACK UP THE MONTH                            
         STC   R8,SVSTMO                                                        
         IC    RE,SVSTYEAR         AND ADD 1 TO THE YEAR                        
         LA    RE,1(RE)                                                         
         STC   RE,SVSTYEAR                                                      
         B     CCAT3                                                            
*                                                                               
CCATNXRC DS    0H                                                               
         GOTO1 SEQ                                                              
         B     CCAT1                                                            
*                                                                               
CCATX    DS    0H                                                               
         MVI   0(R6),X'FF'                                                      
         DROP  R5,R6                                                            
         XMOD1                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -           
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -           
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
BLOCK    DS    CL24                FOR GETBROAD OUTPUT                          
         DS    0H                                                               
SVSTMO   DS    XL1                 STARTING MONTH                               
         DS    0H                                                               
SVSTYEAR DS    XL1                 STARTING YEAR                                
SVBESTRT DS    XL3                 BINARY ESTIMATE START DATE                   
SVBEEND  DS    XL3                 BINARY ESTIMATE END DATE                     
SVBQSTRT DS    XL3                 BINARY QRTR START DATE                       
SVBQEND  DS    XL3                 BINARY QRTR END DATE                         
SVCQSTRT DS    CL6                 QUARTER START DATE (YYMMDD)                  
SVCQEND  DS    CL6                 QUARTER END DATE   (YYMMDD)                  
CATTBLE  DS    750CL48             COKE CATEGORY TABLE                          
EOTABLE  DS    0C                                                               
         EJECT                                                                  
WRKR4K   DS    4096C                                                            
INREC    EQU   WRKR4K                                                           
CPTABC   DS    0C                                                               
CPTABCST DS    0C                                                               
         DS    80000C                                                           
CPTABCEN DS    0C                                                               
CPTABCLN EQU   CPTABCEN-CPTABCST                                                
         EJECT                                                                  
SORTREC  DSECT                                                                  
SRSTART  DS    0C                                                               
SRMEDIA  DS    CL1                                                              
SRCLT    DS    CL3                 CLIENT                                       
SRPRD    DS    CL3                 PRODUCT                                      
SRMKT    DS    XL2                 MARKET                                       
SREST    DS    XL1                 ESTIMATE                                     
SRDPT    DS    CL1                 DAYPART                                      
SRSLN    DS    XL1                 SPOT LENGTH                                  
SRWKOF   DS    XL2                 WEEK OF (MONDAY START DATE)                  
SRDATA   DS    0C                                                               
SRGRP    DS    XL4                 RATING POINTS                                
SRDOL    DS    XL4                 DOLLARS                                      
SRGRP2   DS    XL4                 SECONDARY GRPS                               
SRCATCC  DS    CL2                 COKE CATAGORY CODE                           
SRBTLRCC DS    CL5                 COKE BOTTLER CODE                            
SRAGYCC  DS    CL3                 COKE AGENCY CODE                             
SRPRDCC  DS    CL2                 COKE PRODUCT CODE                            
SREND    DS    0C                                                               
SRLN     EQU   SREND-SRSTART                                                    
SRKLN    EQU   SRDATA-SRSTART                                                   
         SPACE 2                                                                
CPTABD   DSECT                                                                  
CPACLT   DS    CL3                                                              
CPAPRD   DS    CL3                                                              
CPBCLT   DS    CL2                                                              
CPBPRD   DS    CL1                                                              
CPTKLN   EQU   6                                                                
CPTABLN  EQU   9                                                                
         EJECT                                                                  
COKETAPE DSECT                     COKE GOAL TAPE                               
CCRECTYP DS    CL1                 C'1'                                         
CCBTLR   DS    CL5                                                              
CCPRD    DS    CL2                                                              
CCYEAR   DS    CL4                 YYYY                                         
CCCLT    DS    CL3                 (AGENCY)                                     
CCMKT    DS    CL4                 ADI MARKET NUMBER                            
CCQTR    DS    CL1                 QUARTER                                      
CCDPT    DS    CL1                 DAYPART                                      
CCCPP    DS    CL6                 9999.99                                      
CCWKGRP  DS    14CL4               WEEKLY GRPS (9999)                           
CCTOTGRP DS    CL5                 TOTAL GRP                                    
CCADJDOL DS    CL9                 DOLLARS FOR "+", "-" DAYPARTS                
         DS    CL5                                                              
CCCATCC  DS    CL2                 CATEGORY CODE                                
CCMEDIA  DS    CL1                 MEDIA                                        
         DS    CL27                SPARE                                        
         SPACE 2                                                                
CCQTRSD  DSECT                     COKE CATEGORY TABLE                          
CCQST    DS    0C                                                               
CCQYY    DS    CL2                 YEAR                                         
CCQQTR   DS    CL1                 QUARTER(1-4)                                 
CCQSTDT  DS    CL6                 START DATE                                   
CCQENDT  DS    CL6                 END DATE                                     
CCQEST   DS    XL1                 ESTIMATE                                     
CCCAT    DS    CL2                 CATEGORY                                     
CCQWKS   DS    15CL2               15 WEEKLY START DATES                        
         DS    CL6                                                              
CCNUMWKS DS    CL1                 NUMBER OF WEEKS IN QUARTER                   
CCQEN    DS    0C                                                               
CCCLEN   EQU   CCQEN-CCQST                                                      
         SPACE 2                                                                
         EJECT                                                                  
       ++INCLUDE SPGENGOAL                                                      
LNGDELEM EQU   GLEMENT-GDELEM      LENGTH OF DESC. ELEMENT                      
LNGLHDRR EQU   GDELEM-GDESC        LENGTH OF RECORD HEADER                      
LNGLELEM EQU   GLKELEM-GLEMENT     LENGTH OF GOAL DATA ELEMENT                  
LNGLCC   EQU   19                  LENGTH OF GOAL COKE ELEMENT                  
LNGLREC  EQU   LNGLHDRR+LNGDELEM   LENGTH OF SKELETON RECORD                    
         EJECT                                                                  
       ++INCLUDE DMWRKRK                                                        
         EJECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
       ++INCLUDE SPREPMODES                                                     
         EJECT                                                                  
       ++INCLUDE SPMEDBLOCK                                                     
         EJECT                                                                  
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'039SPREPGX02 12/16/02'                                      
         END                                                                    
