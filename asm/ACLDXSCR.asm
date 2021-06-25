*          DATA SET ACLDXSCR   AT LEVEL 011 AS OF 05/01/02                      
*PHASE ACLDXSCR,*                                                               
*INCLUDE DATCON                                                                 
*INCLUDE DICTATE                                                                
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'CONVERT OR FIX SCRIBE ELEMENTS'                                 
***********************************************************************         
*                                                                     *         
*  PARAMETER LIST:                                                    *         
*                                                                     *         
*    P1=A(RECORD)     PASS FIRST BYTE X'00'= INITIALIZE               *         
*                                     X'01'= RECORD IN CORE           *         
*                                     X'FF'= END OF FILE              *         
*                     RETURN VALUE    X'00'= KEEP RECORD              *         
*                                     X'FF'= PURGE RECORD             *         
*                                     X'FF'/C'EOJ'=PURGE & CAUSE EOJ  *         
*    P2=A(TAPEOUT)    PASS FIRST BYTE X'80'= TAPE INPUT               *         
*                                     X'40'= TAPE OUTPUT              *         
*                                     X'20'= RECORD IS I/S FILE RECORD*         
*    P3=A(PARAM CARD) PASS FIRST BYTE C'Y' = YOU ASKED ME TO RETURN   *         
*                     RETURN          C'R' = RETURN BACK TO EXTERNAL  *         
*    P4=A(FILE DEFN)                                                  *         
*    P5=A(PRINTER)                                                    *         
*    P6=A(CPRINT)                                                     *         
*                                                                     *         
*  TABLES TO BE UPDATED:                                              *         
*    KEYWRDS  - THE KEYWORDS TO BE SCANNED FOR                        *         
*    KEYWRDS2 - THE KEYWORDS TO EXCLUDE REPORTING ON                  *         
*    FILTERS  - THE FILTER TYPES TO BE SEARCHED FOR                   *         
*    RPTTYPES - THE REPORT TYPES TO BE CONSIDERED (NULL MEANS ALL)    *         
*    PARMTBL  - THE PARAMETERS TO BE TRANSLATED                       *         
*                                                                     *         
*  CODE TO BE UPDATED:                                                *         
*    GEN50    - ROW AND COLUMN GENERAL UPDATES                        *         
*                                                                     *         
*  CONSTANTS TO BE UPDATED:                                           *         
*    CPYSEL   - HEXADECIMAL CODE OF COMPANY TO BE SELECTED            *         
*               X'00' MEANS SELECT ALL COMPANIES                      *         
*    RQSTART  - START DATE IN PACKED YYMMDD                           *         
*    RQEND    - END   DATE IN PACKED YYMMDD                           *         
*    UPSI     - DEBUGGING REQUESTS                                    *         
*                 BIT 0 = DUMP GET REQUESTS                           *         
*                 BIT 1 = DUMP PUT REQUESTS                           *         
*                 BIT 2 = DUMP UPDATED ELEMENTS                       *         
*    KWSCNFOR - LIMIT KEYWORD SCAN:                                   *         
*                 C     = CHARACTER  FORMAT                           *         
*                 T     = TRANSLATED FORMAT                           *         
*                 BLANK = ALL        FORMATS                          *         
*    RCWRITE  - WRITE REQUEST FROM WRITE= JOB INPUT CARD              *         
*                 N     = NO                                          *         
*                 Y     = YES                                         *         
*                 BLANK = YES                                         *         
*                                                                     *         
*  CURRENT ITEMS BEING UPDATED:                                       *         
*    KEYWORDS - F1, F2, F3, F4, F5 AND OF                             *         
*                                                                     *         
***********************************************************************         
         EJECT ,                                                                
         SPACE 1                                                                
         PRINT NOGEN                                                            
ACLDXSCR CSECT                                                                  
         NMOD1 ACLDSCRX-ACLDSCRD,DMLDSCR                                        
         USING ACLDSCRD,RC                                                      
         SPACE 1                                                                
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
*                                                                               
         CLI   PLIST+8,C'Y'        RETURN CALL AS REQUESTED LAST TIME           
         BE    DMXRET                                                           
         CLI   PLIST,X'00'         FIRST CALL TO INITILIZE                      
         BE    DMXINIT                                                          
         CLI   PLIST,X'01'         NORMAL CALL TO PROCESS RECORD                
         BE    DMXREC                                                           
         CLI   PLIST,X'FF'         LAST CALL ON EOF                             
         BE    DMXEOF                                                           
         B     DMXIT                                                            
*                                                                               
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         MVI   8(R1),0                                                          
         B     DMXIT                                                            
*                                                                               
DMXKERET L     R1,APARM            KEEP RECORD AND RETURN TO ME                 
         MVI   0(R1),0                                                          
         MVI   8(R1),C'R'                                                       
         B     DMXIT                                                            
*                                                                               
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         MVI   8(R1),0                                                          
         B     DMXIT                                                            
*                                                                               
DMXPGRET L     R1,APARM            PURGE RECORD AND RETURN TO ME                
         MVI   0(R1),X'FF'                                                      
         MVI   8(R1),C'R'                                                       
         B     DMXIT                                                            
*                                                                               
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
*                                                                               
DMXIT    XMOD1 1                                                                
EXIT     XIT1                                                                   
         EJECT ,                                                                
         SPACE 1                                                                
***********************************************************************         
*  INITIALIZE LOGIC - FIRST CALL HERE - NO RETURN ALLOWED             *         
***********************************************************************         
         SPACE 1                                                                
DMXINIT  DS    0H                                                               
         ZAP   TOTRECS,=P'0'       SYSTEM     TOTAL     RECORDS                 
         ZAP   TOTFNDS,=P'0'       SYSTEM     TOTAL     FOUND                   
         ZAP   TOTCHGS,=P'0'       SYSTEM     TOTAL     CHANGES                 
         ZAP   CPYRECS,=P'0'       COMPANY    TOTAL     RECORDS                 
         ZAP   CPYFNDS,=P'0'       COMPANY    TOTAL     FOUND                   
         ZAP   CPYCHGS,=P'0'       COMPANY    TOTAL     CHANGES                 
         MVI   PREVCPY,X'00'       PREVIOUS   COMPANY                           
         MVC   REQDATED,SPACES     REQUEST    DATE                              
         MVC   REQUEST#,SPACES     REQUEST    NUMBER                            
         ZAP   DUMPCNT,=P'0'                                                    
         ZAP   DUMPECNT,=P'0'                                                   
         ZAP   PDUMP,=P'0'                                                      
         MVI   PRTOTALS,NO         PRINTED    TOTALS                            
         EJECT ,                                                                
         SPACE 1                                                                
*                                                                               
         USING KEYWRDSD,R2         KEYWORDS  DSECT                              
*                                                                               
DT020    DS    0H                                                               
         L     R2,=A(KEYWRDS)      ->   KEYWORD   TABLE                         
*                                                                               
DT030    DS    0H                                                               
         CLI   0(R2),EOT           END  OF   TABLE ?                            
         BE    DT040               YES, DONE                                    
*                                  TRANSLATE KEYWORD                            
         GOTO1 EXPDNTRY,DMCB,(L'KEYWDIC,KEYWDIC),KEYWTEXT                       
         LA    R2,KEYWRDLQ(,R2)    ->   NEXT KEYWORD   ENTRY                    
         B     DT030               TRANSLATE NEXT KEYWORD                       
*                                                                               
         DROP  R2                  KEEP IT   CLEAN                              
*                                                                               
*                                                                               
         USING RPTTYPED,R2         REPORT    TYPES     DSECT                    
*                                                                               
DT040    DS    0H                                                               
         MVI   LIMRTYPS,NO         LIMIT     REPORT    TYPES                    
         L     R2,=A(RPTTYPES)     ->   KEYWORD   TABLE                         
*                                                                               
DT050    DS    0H                                                               
         CLI   0(R2),EOT           END  OF   TABLE ?                            
         BE    DT060               YES, DONE                                    
         MVI   LIMRTYPS,YES        LIMIT     REPORT    TYPES                    
*                                  TRANSLATE REPORT    TYPE                     
         GOTO1 EXPDNTRY,DMCB,(L'RPTTDIC,RPTTDIC),RPTTTEXT                       
         LA    R2,RPTTYPLQ(,R2)    ->   NEXT REPORT    TYPE ENTRY               
         B     DT050               TRANSLATE NEXT KEYWORD                       
*                                                                               
         DROP  R2                  KEEP IT   CLEAN                              
*                                                                               
*                                                                               
         USING KEYWRDSD,R2         KEYWORDS  DSECT                              
*                                                                               
DT060    DS    0H                                                               
         L     R2,=A(KEYWRDS2)     ->   KEYWORD   EXCLUDE   TABLE               
*                                                                               
DT070    DS    0H                                                               
         CLI   0(R2),EOT           END  OF   TABLE ?                            
         BE    DT080               YES, DONE                                    
*                                  TRANSLATE KEYWORD                            
         GOTO1 EXPDNTRY,DMCB,(L'KEYWDIC,KEYWDIC),KEYWTEXT                       
         LA    R2,KEYWRDLQ(,R2)    ->   NEXT KEYWORD   ENTRY                    
         B     DT070               TRANSLATE NEXT KEYWORD                       
*                                                                               
         DROP  R2                  KEEP IT   CLEAN                              
*                                                                               
*                                                                               
         USING PARMSD,R2           PARAMETERS     DSECT                         
*                                                                               
DT080    DS    0H                                                               
         L     R2,=A(PARMTBL)      ->   PARAMETER TABLE                         
*                                                                               
DT090    DS    0H                                                               
         CLI   0(R2),EOT           END  OF   TABLE ?                            
         BE    DT100               YES, DONE                                    
*                                  TRANSLATE PARAMETER                          
         GOTO1 EXPDNTRY,DMCB,(L'PARMDIC,PARMDIC),PARMTEXT                       
         LA    R2,PARMENLQ(,R2)    ->   NEXT PARAMETER ENTRY                    
         B     DT090               TRANSLATE NEXT PARAMETER                     
*                                                                               
         DROP  R2                  KEEP IT   CLEAN                              
*                                                                               
         USING RPTTYPED,R2         REPORT    TYPES     DSECT                    
*                                                                               
DT100    DS    0H                                                               
         L     R2,=A(RPTNAMES)     ->   REPORT    NAMES     TABLE               
*                                                                               
DT110    DS    0H                                                               
         CLI   0(R2),EOT           END  OF   TABLE ?                            
         BE    DT120               YES, DONE                                    
*                                  TRANSLATE REPORT    TYPE NAME                
         GOTO1 EXPDNTRY,DMCB,(L'RPTTDIC,RPTTDIC),RPTTTEXT                       
         LA    R2,RPTTYPLQ(,R2)    ->   NEXT REPORT    TYPE ENTRY               
         B     DT110               TRANSLATE NEXT KEYWORD                       
*                                                                               
         DROP  R2                  KEEP IT   CLEAN                              
*                                                                               
DT120    DS    0H                                                               
         ZAP   LINE,=P'99'         NEW  PAGE                                    
         MVI   P,C' '              SKIP A    LINE                               
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
         EJECT ,                                                                
***********************************************************************         
*  PROCESS RECORD LOGIC - RECORD IN AREC - RETURN ALLOWED             *         
***********************************************************************         
         SPACE 1                                                                
         USING RESRECD,R3                                                       
         SPACE 1                                                                
DMXREC   L     R3,AREC             ->    RECORD                                 
         LR    R4,R3               SAVE  ADDRESS    OF   RECORD                 
         CLI   RESKTYP,RESKTYPQ    X'2D'                                        
         BL    DMXKEEP                                                          
         BNE   OUTOTALS                                                         
         CLI   RESKSUB,RESKSUBQ    X'02'                                        
         BL    DMXKEEP                                                          
         BNE   OUTOTALS                                                         
*                                                                               
         MVC   DBGRCD,RESKEY       SAVE  RCD   HDR  FOR  DEBUGGING              
*                                                                               
         MVC   CURRCPY,RESKCPY     COMPANY                                      
         MVC   CURRFORM,RESKFORM   FORMAT                                       
*                                                                               
         CLI   CPYSEL,X'00'        LIMIT COMPANIES ?                            
         BE    RS10                NO,   SKIP                                   
         CLC   CURRCPY,CPYSEL      REQUESTED   COMPANY ?                        
         BNE   DMXKEEP                                                          
*                                                                               
RS10     DS    0H                                                               
         CLC   CURRCPY,PREVCPY     DIFFERENT   COMPANY ?                        
         BE    RS20                NO,   SKIP                                   
         BAS   RE,PRTCPYTO         PRINT COMPANY    TOTALS                      
*                                                                               
RS20     DS    0H                                                               
         SR    R5,R5               COPY  THE   RECORD                           
         LR    R4,R3               ->    RECORD                                 
         ICM   R5,3,RESRLEN        GET   RECORD     LENGTH                      
         L     R0,AIO              ->    WORK  AREA                             
         LR    R1,R5               GET   WORK  LENGTH                           
         MVCL  R0,R4               COPY  THE   RECORD                           
*                                                                               
         L     R3,AIO              USE   WORK  AREA                             
         LA    R4,RESRFST          ->    1ST   ELEMENT                          
         MVC   PREVCPY,CURRCPY     SAVE  COMPANY                                
         MVI   WRTSW,NO            SET   WRITE SWITCH                           
         MVI   FOUND,NO                                                         
         MVI   FOUND2,NO                                                        
         MVI   DMPSW,YES           SET   DUMP  SWITCH                           
         MVI   PURGESW,NO          SET   PURGE SWITCH                           
         MVC   REPTCODE,SPACES                                                  
         MVC   REPTNAME,SPACES                                                  
         MVC   FNDKEYWD,SPACES                                                  
         MVC   FNDFILTR,SPACES                                                  
         MVC   FNDRPTYP,SPACES                                                  
         MVC   REQDATED,SPACES                                                  
         MVC   REQUEST#,SPACES                                                  
*                                                                               
         DROP  R3                  KEEP  IT    CLEAN                            
*                                                                               
         USING RESRECD,R3          SCRIBE      RECORD                           
*                                                                               
         MVC   FORMNAME,RESKFORM   SAVE   FORMAT    NAME                        
         OC    FORMNAME,FORMNAME   IS     FORMAT    NULL ?                      
         BNZ   RS30                NO,    SKIP                                  
         MVI   FOUND,YES           YES,   INDICATE  FOUND                       
         MVC   FNDRPTYP,=CL6'*NULL*'                                            
         MVC   FORMNAME,=CL8'*-NULL-*'                                          
         MVI   PURGESW,YES         PURGE  THIS RECORD                           
*                                                                               
         DROP  R3                  KEEP  IT    CLEAN                            
*                                                                               
RS30     DS    0H                                                               
         CLI   0(R4),0                                                          
         BE    RS70                                                             
         MVI   TRANSLAT,NO         ASSUME NOT  TRANSLATED                       
         MVI   CHGELMNT,NO         ASSUME NOT  CHANGED       ELEMENT            
*                                                                               
         XC    DBGELMNT,DBGELMNT   CLEAR  DBG  ELEMNET                          
         SR    RF,RF               CLEAR  REGISTER                              
         IC    RF,1(R4)            GET    EL   LENGTH                           
         BCTR  RF,0                GET    EX    EL  LENGTH                      
         EXMVC RF,DBGELMNT,0(R4)   MOVE   ELEMENT   FOR  DEBUGGING              
*                                                                               
         CLI   0(R4),FFNELQ        X'25'  FREE FORM SCRIBE  (STYELQ)            
         BE    RP10                                                             
         CLI   0(R4),PACELQ        X'A1'  PERSON    ACTIVITY ELEMENT            
         BE    A110                                                             
         CLI   0(R4),RRWELQ        X'C2'  ROW       ELEMENT                     
         BE    RW10                                                             
         CLI   0(R4),RCLELQ        X'C3'  COLUMN    ELEMENT                     
         BE    CL10                                                             
         CLI   0(R4),RPFELQ        X'C4'  PROFILE   ELEMENT                     
         BE    PF10                                                             
         CLI   0(R4),RFLELQ        X'C5'  FILTER    ELEMENT                     
         BE    FL10                                                             
         CLI   0(R4),PTRELQ        X'FA'  POINTER   ELEMENT                     
         BE    FA10                                                             
         CLI   0(R4),DTSELQ        X'FB'  DATE/TIME STAMP                       
         BE    FB10                                                             
*                                                                               
RS35     DS    0H                                                               
         SR    R1,R1                                                            
         IC    R1,1(,R4)                                                        
         AR    R4,R1                                                            
         B     RS30                                                             
*                                                                               
         EJECT ,                                                                
         SPACE 1                                                                
         USING PACELD,R4           PERSON ACTIVITY    ELEMENT                   
         SPACE 1                                                                
A110     DS    0H                                                               
         LA    RF,PACDATE          LAST   ACTIVITY    DATE                      
         GOTO1 DATCON,DMCB,(1,(RF)),(21,LASTACTD)                               
         B     RS35                                                             
*                                                                               
         DROP  R4                  KEEP   IT   CLEAN                            
         SPACE 3                                                                
         USING PTRELD,R4           POINTER     ELEMENT                          
         SPACE 1                                                                
FA10     DS    0H                                                               
         ZIC   R1,PTRLN            ELEMENT     LENGTH                           
         SH    R1,=Y(PTRLN1Q)                                                   
         SRL   R1,2                DIVIDE BY   4                                
         CVD   R1,DUB              NUMBER OF   REQUESTS                         
         OI    DUB+7,X'0F'                                                      
         UNPK  REQUEST#,DUB        SAVE   NUMBER                                
         B     RS35                                                             
*                                                                               
         DROP  R4                  KEEP   IT   CLEAN                            
         SPACE 3                                                                
         USING DTSELD,R4           DATE/TIME   STAMP     ELEMENT                
         SPACE 1                                                                
FB10     DS    0H                  REQUEST     DATE                             
         GOTO1 DATCON,DMCB,(2,DTSDATE),(1,REQDATE)                              
         CLC   REQDATE,RQSTART                                                  
         BL    FB95                                                             
         CLC   REQDATE,RQEND                                                    
         BH    FB95                                                             
         GOTO1 DATCON,DMCB,(2,DTSDATE),(21,REQDATED)                            
         B     RS35                CONTINUE                                     
*                                                                               
FB95     DS    0H                  SKIP   ELEMENT                               
         MVI   FOUND2,YES          IGNORE THIS RECORD                           
         B     RS70                                                             
*                                                                               
         DROP  R4                  KEEP   IT   CLEAN                            
         SPACE 3                                                                
         EJECT ,                                                                
         SPACE 1                                                                
         USING STYELD,R4           FREE   FORM   SCRIBE     ELEMENT             
         SPACE 1                                                                
RP10     DS    0H                  REPORT TYPE                                  
         MVC   REPTCODE,STYCODE    SAVE   REPORT TYPE  CODE                     
         MVC   REPTNAME,STYNAME    SAVE   REPORT TYPE  NAME                     
*                                  INVALID       FORMAT ?                       
         CLC   FNDRPTYP,=CL6'*NULL*'                                            
         BNE   *+10                NO,    SKIP                                  
         MVC   FNDRPTYP,REPTNAME   YES,   INSERT REPORT TYPE NAME               
         CLI   LIMRTYPS,YES        LIMIT  REPORT TYPES                          
         BNE   RP30                NO,    SKIP                                  
         L     RE,=A(RPTTYPES)     ->     REPORT TYPES TABLE                    
*                                                                               
         USING RPTTYPED,RE         REPORT TYPE   ENTRY                          
*                                                                               
RP20     DS    0H                                                               
         CLI   0(RE),EOT           END    OF     TABLE ?                        
         BE    RP95                YES,   SKIP   RECORD                         
         CLC   RPTTDIC,REPTNAME    MATCH  DICTIONARY   ENTRY ?                  
         BE    RP30                YES,   GOOD   ENTRY                          
         CLC   RPTTTEXT,REPTNAME   MATCH  TEXT   ENTRY ?                        
         BE    RP30                YES,   GOOD   ENTRY                          
         LA    RE,RPTTYPLQ(,RE)    TRY    NEXT   ENTRY                          
         B     RP20                                                             
*                                                                               
         USING RPTTYPED,RE         REPORT TYPE   ENTRY                          
*                                                                               
RP30     DS    0H                  GOOD   ENTRY                                 
         L     RE,=A(RPTNAMES)     ->     REPORT TYPE  NAMES  TABLE             
*                                                                               
         CLI   REPTNAME,ESCHIGHQ   ABOVE  ESCAPE SEQUENCE ?                     
         BH    RP60                YES,   CONTINUE                              
*                                                                               
*                                  PROCESS       TRANSLATED   ENTRIES           
         OC    REPTNAME,REPTNAME   NULL   REPORT TYPE  ?                        
         BNZ   RP40                NO,    SKIP                                  
         MVC   REPTNAME,=CL8'**NULL**'                                          
         MVC   FNDRPTYP,REPTNAME   INDICATE      ERROR ON     REPORT            
         MVI   FOUND,YES           FOUND  INVALID      REPORT TYPE              
         MVI   PURGESW,YES         PURGE  THIS   RECORD                         
         B     RP95                CONTINUE                                     
*                                                                               
RP40     DS    0H                  FIND   TRANSLATED   REPORT NAME              
         CLI   0(RE),EOT           END    OF     TABLE ?                        
         BNE   *+6                 NO,    CONTINUE                              
         DC    H'0'                DUMP - SHOULD NOT   OCCUR                    
*                                                                               
         CLC   RPTTDIC,REPTNAME    MATCH  DICTIONARY   ENTRY ?                  
         BE    RP50                YES,   GET    REPORT       NAME              
         LA    RE,RPTTYPLQ(,RE)    TRY    NEXT   ENTRY                          
         B     RP40                                                             
*                                                                               
RP50     DS    0H                  DICTIONARY    ENTRY FOUND                    
         MVC   REPTNAME,RPTTTEXT   GET    REPORT TYPE  NAME                     
         B     RP90                CONTINUE                                     
*                                                                               
RP60     DS    0H                  CHARACTER     REPORT       TYPE              
         CLI   0(RE),EOT           END    OF     TABLE ?                        
         BNE   *+6                 NO,    CONTINUE                              
         DC    H'0'                DUMP - SHOULD NOT   OCCUR                    
*                                                                               
         CLC   RPTTTEXT,REPTNAME   MATCH  TEXT   ENTRY ?                        
         BE    RP70                YES,   UPDATE REPORT       NAME              
         LA    RE,RPTTYPLQ(,RE)    TRY    NEXT   ENTRY                          
         B     RP60                                                             
*                                                                               
RP70     DS    0H                  TEXT   ENTRY  FOUND                          
         MVI   FOUND,YES           FORMAT WILL   BE    UPDATED                  
         MVI   CHGELMNT,YES        UPDATE THE    ELEMENT                        
         MVI   WRTSW,YES           UPDATE THE    RECORD                         
         MVC   STYNAME,RPTTDIC     INSERT DICTIONARY   VALUE                    
         MVC   FNDRPTYP,REPTNAME   FOUND  REPORT TYPE  NAME                     
         BAS   RE,DMPELMNT         DUMP   ELEMENT                               
         B     RP90                CONTINUE                                     
*                                                                               
*                                                                               
RP90     DS    0H                  GOOD   ENTRY                                 
         B     RS35                CONTINUE      TO    NEXT   ELEMENT           
*                                                                               
RP95     DS    0H                  SKIP   RECORD                                
         B     RS70                SKIP   IT                                    
*                                                                               
         DROP  R4,RE               KEEP   IT     CLEAN                          
         EJECT ,                                                                
         SPACE 1                                                                
         USING RRWELD,R4                                                        
         SPACE 1                                                                
RW10     DS    0H                  ROW    ELEMENT                               
         LA    RE,RRWDATA                                                       
         LA    R1,L'RRWDATA                                                     
         TM    RRWOPT2,RRWDICT     TRANSLATED ROW ?                             
         BZ    *+8                 NO,    SKIP                                  
         MVI   TRANSLAT,YES        SAY    TRANSLATED                            
         B     GEN00                                                            
         SPACE 3                                                                
         USING RCLELD,R4                                                        
         SPACE 1                                                                
CL10     DS    0H                  COLUMN ELEMENT                               
         LA    RE,RCLDATA                                                       
         LA    R1,L'RCLDATA                                                     
         TM    RCLOPT2,RCLDICT     TRANSLATED COLUMN ?                          
         BZ    *+8                 NO,    SKIP                                  
         MVI   TRANSLAT,YES        SAY    TRANSLATED                            
         B     GEN00                                                            
*                                                                               
         DROP  R4                  KEEP IT   CLEAN                              
         EJECT ,                                                                
         SPACE 1                                                                
GEN00    DS    0H                                                               
         MVC   WORK,SPACES                                                      
         LA    RF,WORK                                                          
         CLI   TRANSLAT,YES        TRANSLATED     DATA ?                        
         BE    GEN10               YES, SKIP                                    
*                                                                               
GEN05    DS    0H                  FIND THE  KEYWORD   WHEN WE   HAVE           
         CLI   0(RE),C' '               CHARACTER DATA                          
         BE    GEN08                                                            
         CLI   0(RE),C','                                                       
         BE    GEN08                                                            
         CLI   0(RE),X'00'                                                      
         BE    GEN08                                                            
         CLI   0(RE),X'01'                                                      
         BE    GEN08                                                            
         MVC   0(1,RF),0(RE)                                                    
         LA    RF,1(,RF)                                                        
         LA    RE,1(,RE)                                                        
         BCT   R1,GEN05                                                         
*                                                                               
GEN08    DS    0H                  FOUND      CHARACTER     KEYWORD             
         ST    RE,ADELIM           SAVE  ADDR OF   DELIMITER                    
         STH   R1,CHARLEFT         SAVE  NUM  OF   CHARACTERS     LEFT          
*                                                                               
GEN10    DS    0H                  IS    THE  KEYWORD   IN   TABLE ?            
         L     RF,=A(KEYWRDS)                                                   
*                                                                               
         USING KEYWRDSD,RF                                                      
*                                                                               
GEN20    DS    0H                                                               
         CLI   0(RF),EOT                                                        
         BE    GEN60                                                            
         CLI   TRANSLAT,YES        TRANSLATED     DATA ?                        
         BE    GEN30               YES, SKIP                                    
         CLI   KWSCNFOR,KWSCNTRN   TRANSLATED     ONLY REQUESTED ?              
         BE    GEN60               YES, SKIP                                    
         CLC   WORK(6),KEYWTEXT    TEXT MATCH ?                                 
         BE    GEN50               YES, FOUND                                   
         B     GEN40               NO,  CONTINUE                                
*                                                                               
GEN30    DS    0H                                                               
         CLI   KWSCNFOR,KWSCNCHR   CHARACTER      ONLY REQUESTED ?              
         BE    GEN60               YES, SKIP                                    
         CLC   0(2,RE),KEYWDIC+1   DICTIONARY     ENTRY     MATCH ?             
         BE    GEN50               YES, FOUND                                   
*                                                                               
GEN40    DS    0H                                                               
         LA    RF,KEYWRDLQ(,RF)    NEXT KEYWORD   ENTRY                         
         B     GEN20                                                            
*                                                                               
GEN50    DS    0H                  KEYWORD   IS   IN   TABLE                    
         MVI   FOUND,YES           KEYWORD   FOUND                              
         MVC   FNDKEYWD,KEYWTEXT   SAVE TEXT                                    
         CLI   KEYWUPD,YES         UPDATE    REQUEST ?                          
         BNE   GEN90               NO,  DONE                                    
*                                                                               
         MVI   WRTSW,YES           UPDATE    REQUESTED                          
         MVC   NEWDATA,KEYWTXTN    SAVE NEW  TEXT DATA                          
         MVI   CHGELMNT,YES        UPDATE    ELEMENT                            
*                                                                               
         DROP  RF                  KEEP IT   CLEAN                              
*                                                                               
         CLI   TRANSLAT,YES        TRANSLATED     DATA                          
         BNE   GEN50PRM            NO,  PROCESS   CHARACTER PARAMETERS          
*                                  USE  OLD  PARAMETERS                         
         MVC   NEWDATA+2(L'NEWDATA-2),2(RE)                                     
         B     GEN50UPD            READY     TO   UPDATE                        
*                                                                               
*                                  ***  TRANSLATE THE  PARAMETERS               
GEN50PRM DS    0H                  PROCESS   NEXT PARAMETER                     
         MVC   WORK,SPACES                                                      
         L     RE,ADELIM           ->   DELIMITER                               
         LA    RF,WORK             ADDR OF   NEXT AVAILABLE CHARACTER           
         SR    R5,R5               LENGTH    OF   PARAMETER                     
         SR    R1,R1                                                            
         ICM   R1,3,CHARLEFT       NUM  OF   CHARACTERS LEFT                    
         BZ    GEN50P50            0,   ALMOST    READY     TO   UPDATE         
         CLI   0(RE),C' '          END  OF   PARAMETERS ?                       
         BE    GEN50P50            YES, ALMOST    READY     TO   UPDATE         
         CLI   0(RE),X'00'         END  OF   PARAMETERS ?                       
         BE    GEN50P50            YES, ALMOST    READY     TO   UPDATE         
         LA    RE,1(,RE)           ->   PARM 1ST  CHARACTER                     
         BCTR  R1,0                SKIP THE  C',' OR   X'01'                    
*                                                                               
GEN50P10 DS    0H                  FIND NEXT PARAMETER                          
         CLI   0(RE),C' '                                                       
         BE    GEN50P20                                                         
         CLI   0(RE),C','                                                       
         BE    GEN50P20                                                         
         CLI   0(RE),X'00'                                                      
         BE    GEN50P20                                                         
         CLI   0(RE),X'01'                                                      
         BE    GEN50P20                                                         
         MVC   0(1,RF),0(RE)                                                    
         LA    R5,1(,R5)                                                        
         LA    RF,1(,RF)                                                        
         LA    RE,1(,RE)                                                        
         BCT   R1,GEN50P10                                                      
*                                                                               
GEN50P20 DS    0H                  FOUND     THE  PARAMETER                     
         ST    RE,ADELIM           SAVE ADDR OF   DELIMITER                     
         STH   R1,CHARLEFT         SAVE NUM  OF   CHARACTERS     LEFT           
         BCTR  R5,0                MINUS     ONE  FOR  EXECUTE   LENGTH         
         L     RF,=A(PARMTBL)      ->   PARAMETER TABLE                         
*                                                                               
         USING PARMSD,RF           PARAMETER TABLE     DSECT                    
*                                                                               
GEN50P30 DS    0H                                                               
         CLI   0(RF),EOT           IN   TABLE ?                                 
         BNE   *+6                 YES, OKAY                                    
         DC    H'0'                NO,  DUMP                                    
*                                                                               
         EXCLC R5,WORK,PARMTEXT    TEXT MATCH ?                                 
         BE    GEN50P40            YES, FOUND                                   
         LA    RF,PARMENLQ(,RF)    NEXT PARAMETER ENTRY                         
         B     GEN50P30            TEST NEXT PARAMETER ENTRY                    
*                                                                               
GEN50P40 DS    0H                  PARAMETER FOUND                              
         ZIC   R1,PARMGNUM         GET  PARAMETER NUMBER                        
         SLL   R1,1                FIND INDEX    INTO NEWDATA                   
         LA    R1,NEWDATA(R1)      ->   UPDATE   FIELD                          
         MVC   0(2,R1),PARMDNUM    INSERT    DICTIONARY    NUMBER               
         SLL   R5,6                MOVE INPUT     LENGTH-1 (0-3)     TO         
         STC   R5,BYTE                  HIGH ORDER    BITS OF   BYTE            
         OC    0(1,R1),BYTE        TURN ON   HIGH ORDER    BITS                 
         B     GEN50PRM            CONVERT   NEXT USER PARAMETER                
*                                                                               
*                                  PROCESSED ALL  PARAMETERS                    
GEN50P50 DS    0H                  ***  MAKE SURE WE   LEFT NO  HOLES           
         LA    RE,NEWDATA+2        ->   PARM LIST -    FROM AREA                
         LA    RF,NEWDATA+2        ->   PARM LIST -    TO   AREA                
         LA    R5,MAX#PARM         MAX  NUM  OF   KEYWORD   PARAMETERS          
*                                                                               
GEN50P60 DS    0H                                                               
         OC    0(2,RE),0(RE)       ANY  FROM PARM ?                             
         BZ    GEN50P80            NO,  SKIP                                    
         CR    RE,RF               FROM ADDR =    TO   ADDR                     
         BE    GEN50P70            YES, GET  NEXT TO   AND  FROM ADDR           
         MVC   0(2,RF),0(RE)       MOVE FROM PARM ->   TO   PARM                
         XC    0(2,RE),0(RE)       CLEAR     FROM PARM                          
*                                                                               
GEN50P70 DS    0H                                                               
         LA    RF,2(,RF)           NEXT TO PARM                                 
*                                                                               
GEN50P80 DS    0H                                                               
         LA    RE,2(,RE)           NEXT FROM PARM                               
         BCT   R5,GEN50P60         TEST FROM PARM                               
*                                                                               
GEN50UPD DS    0H                  ***  READY     TO   UPDATE                   
         CLI   0(R4),RRWELQ        ROW  ELEMENT ?                               
         BE    GEN50U10            YES, UPDATE    ROW                           
*                                                                               
         USING RCLELD,R4           COLUMN    ELEMENT                            
*                                                                               
         MVC   RCLDATA,NEWDATA     NEW  TEXT                                    
         OI    RCLOPT2,RCLDICT     TURN ON   TRANSLATE                          
*                                  TURN OFF  NAME REQUESTED                     
         NI    RCLOPT2,TURNOFF-RCLNAME                                          
         B     GEN50U30                                                         
*                                                                               
         DROP  R4                                                               
*                                                                               
         USING RRWELD,R4           ROW       ELEMENT                            
*                                                                               
GEN50U10 DS    0H                                                               
         MVC   RRWDATA,NEWDATA     NEW  TEXT                                    
         OI    RRWOPT2,RRWDICT     TURN ON   TRANSLATE                          
*                                  TURN OFF  NAME REQUESTED                     
         NI    RRWOPT,TURNOFF-RRWNAME                                           
*                                                                               
GEN50U30 DS    0H                                                               
         B     GEN90                                                            
*                                                                               
         DROP  R4                                                               
         EJECT ,                                                                
         SPACE 1                                                                
*                                                                               
         USING KEYWRDSD,RF                                                      
*                                                                               
GEN60    DS    0H                                                               
         L     RF,=A(KEYWRDS2)     ->   EXCLUDE   TABLE                         
*                                                                               
GEN70    DS    0H                                                               
         CLI   0(RF),EOT                                                        
         BE    GEN90                                                            
         CLI   TRANSLAT,YES        TRANSLATED     DATA ?                        
         BE    GEN75               YES, SKIP                                    
         CLI   KWSCNFOR,KWSCNTRN   TRANSLATED     ONLY REQUESTED ?              
         BE    GEN90               YES, SKIP                                    
         CLC   WORK(6),KEYWTEXT    TEXT MATCH ?                                 
         BE    GEN85               YES, FOUND                                   
         B     GEN80               NO,  CONTINUE                                
*                                                                               
GEN75    DS    0H                                                               
         CLI   KWSCNFOR,KWSCNCHR   CHARACTER      ONLY REQUESTED ?              
         BE    GEN90               YES, SKIP                                    
         CLC   0(2,RE),KEYWDIC+1   DICTIONARY     ENTRY     MATCH ?             
         BE    GEN85               YES, FOUND                                   
*                                                                               
GEN80    DS    0H                                                               
         LA    RF,KEYWRDLQ(,RF)    NEXT KEYWORD   ENTRY                         
         B     GEN70                                                            
*                                                                               
GEN85    DS    0H                                                               
         MVI   FOUND2,YES                                                       
         B     GEN90                                                            
*                                                                               
*                                                                               
         DROP  RF                  KEEP IT   CLEAN                              
*                                                                               
GEN90    CLI   FOUND2,YES          SUPPRESS  WRITE ?                            
         BE    RS35                                                             
         BAS   RE,DMPELMNT         DUMP ELEMENT                                 
         B     RS35                RETURN                                       
         EJECT ,                                                                
         SPACE 1                                                                
         USING RPFELD,R4           PROFILE       DATA ELEMENT                   
         SPACE 1                                                                
PF10     DS    0H                  PROFILE   ELEMENT                            
         B     RS35                                                             
*                                                                               
         DROP  R4                  KEEP   IT     CLEAN                          
         EJECT ,                                                                
         SPACE 1                                                                
         USING RFLELD,R4                                                        
         USING FILTERSD,RF                                                      
         SPACE 1                                                                
FL10     DS    0H                  FILTER    ELEMENT                            
         L     RF,=A(FILTERS)                                                   
*                                                                               
FL20     CLI   0(RF),EOT           END  OF   TABLE ?                            
         BE    FL90                YES, EXIT                                    
         CLC   RFLTYPE,FILTTYPE    FILTER    MATCH ?                            
         BE    FL30                YES, GOOD                                    
         LA    RF,FILTERLQ(,RF)    NEXT FILTER    ENTRY                         
         B     FL20                                                             
*                                                                               
FL30     MVI   FOUND,YES           FOUND                                        
         MVC   FNDFILTR,FILTDESC                                                
*                                                                               
FL90     B     RS35                                                             
*                                                                               
         DROP  R4,RF               KEEP IT   CLEAN                              
         EJECT ,                                                                
         SPACE 1                                                                
         USING RESRECD,R3                                                       
         SPACE 1                                                                
RS70     DS    0H                                                               
         CLI   FOUND,YES                                                        
         BNE   RS80                                                             
         CLI   FOUND2,YES          EXCLUDE   FOUND ?                            
         BE    RS80                                                             
         AP    CPYFNDS,=P'1'       KEEP COUNT                                   
         AP    TOTFNDS,=P'1'       KEEP COUNT                                   
*                                                                               
         USING PDLD,R4             DETAIL    OUTPUT    LINE                     
*                                                                               
         LA    R4,P                                                             
         GOTO1 HEXOUT,DMCB,RESKCPY,PDLCPYCD,1     COMPANY   CODE                
         MVC   PDLFORM,FORMNAME                   FORMAT                        
         MVC   PDLRPTTC,REPTCODE                  REPORT    TYPE CODE           
         MVC   PDLRPTYP,REPTNAME                  REPORT    TYPE NAME           
         CLC   FNDRPTYP,SPACES                                                  
         BE    RS70B                                                            
         MVC   PDLFNDWH,=CL15'FND REPORT TYPE'                                  
         MVC   PDLFNDKW(L'FNDRPTYP),FNDRPTYP      REPORT    TYPE                
         MVC   PDLLREQD,REQDATED             LAST REQUEST   DATE                
         MVC   PDLLREQX,REQUEST#             NUM  OF        REQUESTS            
         GOTO1 VPRINTER                                                         
*                                                                               
         CLC   FNDKEYWD,SPACES                                                  
         BNE   RS70A                                                            
         CLC   FNDFILTR,SPACES                                                  
         BE    RS75                                                             
*                                                                               
RS70A    DS    0H                  FOUND     KEYWORD AND/OR FILTER ALSO         
         GOTO1 HEXOUT,DMCB,RESKCPY,PDLCPYCD,1     COMPANY   CODE                
         MVC   PDLFORM,FORMNAME                   FORMAT                        
         MVC   PDLRPTTC,REPTCODE                  REPORT    TYPE CODE           
         MVC   PDLRPTYP,REPTNAME                  REPORT    TYPE NAME           
*                                                                               
RS70B    DS    0H                  FOUND     KEYWORD AND/OR FILTER ?            
         CLC   FNDKEYWD,SPACES                                                  
         BE    RS72                                                             
         CLC   FNDFILTR,SPACES                                                  
         BNE   RS71                                                             
         MVC   PDLFNDWH,=CL15'FOUND KEYWORD'                                    
         MVC   PDLFNDKW,FNDKEYWD                  KEYWORD                       
         B     RS74                                                             
*                                                                               
RS71     DS    0H                  FOUND     KEYWORD   AND  FILTER              
         MVC   PDLFNDWH,=CL15'FND KW + FILTER'                                  
         MVC   PDLFNDKW,FNDKEYWD                  KEYWORD                       
         MVC   PDLFNDFL,FNDFILTR                  FILTER                        
         B     RS74                                                             
*                                                                               
RS72     DS    0H                  FOUND     FILTER                             
         MVC   PDLFNDWH,=CL15'FOUND FILTER'                                     
         MVC   PDLFNDFL,FNDFILTR                  FILTER                        
*                                                                               
RS74     DS    0H                                                               
         MVC   PDLLREQD,REQDATED             LAST REQUEST   DATE                
         MVC   PDLLREQX,REQUEST#             NUM  OF        REQUESTS            
         GOTO1 VPRINTER                                                         
*                                                                               
         DROP  R4                  KEEP IT   CLEAN                              
*                                                                               
RS75     DS    0H                                                               
         CLI   WRTSW,YES                                                        
         BNE   RS80                                                             
         AP    CPYCHGS,=P'1'                                                    
         AP    TOTCHGS,=P'1'                                                    
*                                                                               
         SR    R5,R5               MOVE THE  UPDATES   TO   THE  RECORD         
         LR    R4,R3               ->   WORK AREA                               
         ICM   R5,3,RESRLEN        GET  RECORD    LENGTH                        
         L     R0,AREC             ->   RECORD                                  
         LR    R1,R5               GET  RCD  LENGTH                             
         MVCL  R0,R4               COPY THE  UPDATES                            
         BAS   RE,DMPPUT           DUMP RECORD    AFTER     FIXES               
*                                                                               
RS80     DS    0H                                                               
         AP    CPYRECS,=P'1'       UPDATE    COMPANY   RECORDS                  
         AP    TOTRECS,=P'1'       UPDATE    TOTAL     RECORDS                  
         CLI   PURGESW,YES         PURGE     THIS RECORD ?                      
         BNE   DMXKEEP             NO,       WRITE     THE  RECORD              
         B     DMXPURGE            YES,      PURGE     THE  RECORD              
         B     RS10                                                             
*                                                                               
         EJECT ,                                                                
***********************************************************************         
*  OUTPUT FINAL TOTALS                                                *         
***********************************************************************         
         SPACE 1                                                                
OUTOTALS DS    0H                                                               
         BAS   RE,FINALPRT         PRINT     FINAL     TOTALS                   
         B     DMXKEEP             EXIT                                         
         EJECT ,                                                                
***********************************************************************         
*  REQUESTED RETURN - DATA IN AREC AS LEFT PREVIOUSLY                 *         
***********************************************************************         
         SPACE 1                                                                
DMXRET   DS    0H                                                               
         EJECT ,                                                                
***********************************************************************         
*  END-OF-FILE LOGIC - LAST CALL HERE - RETURN ALLOWED                *         
***********************************************************************         
         SPACE 1                                                                
DMXEOF   DS    0H                                                               
         BAS   RE,FINALPRT         PRINT     FINAL     TOTALS                   
         B     DMXIT               EXIT                                         
         EJECT ,                                                                
***********************************************************************         
*  PRINT FINAL SUMMARY STATISTICS                                     *         
***********************************************************************         
         SPACE 1                                                                
FINALPRT NTR1                                                                   
         CLI   PRTOTALS,YES        FINAL     TOTALS    PRINTED ?                
         BE    FINALPEX            YES, SKIP                                    
         MVI   PRTOTALS,YES        SAY  FINAL     TOTALS    PRINTED             
*                                                                               
         BAS   RE,PRTCPYTO         PRINT     COMPANY   TOTALS                   
*                                                                               
         USING PSD,R4              SUMMARY   OUTPUT    LINE                     
*                                                                               
         LA    R4,P                                                             
         MVC   PSDTFORH,=C'TOTAL FOR'                                           
         MVC   PSDTFALL(9),=C'** ALL **'                                        
         ZAP   WORK(8),TOTFNDS          GET  NUMBER    OF   MATCHES             
         OI    WORK+7,X'0F'                                                     
         MVC   PSDRMATH,=C'RECORDS MATCHED='                                    
         UNPK  PSDRMAT,WORK(8)                                                  
         ZAP   WORK(8),TOTCHGS          GET  NUMBER    OF   CHANGES             
         OI    WORK+7,X'0F'                                                     
         MVC   PSDRCHGD,=C'RECORDS CHANGED='                                    
         UNPK  PSDRCHG,WORK(8)                                                  
         ZAP   WORK(8),TOTRECS          GET  NUMBER    OF   RECORDS             
         OI    WORK+7,X'0F'                                                     
         MVC   PSDTRECH,=C'TOTAL RECORDS='                                      
         UNPK  PSDTREC,WORK(8)                                                  
         GOTO1 VPRINTER                                                         
*                                                                               
         ZAP   TOTRECS,=P'0'       SYSTEM    TOTAL     RECORDS                  
         ZAP   TOTFNDS,=P'0'       SYSTEM    TOTAL     FOUND                    
         ZAP   TOTCHGS,=P'0'       SYSTEM    TOTAL     CHANGES                  
*                                                                               
         MVI   P,C' '              SKIP A    LINE                               
         GOTO1 VPRINTER                                                         
*                                                                               
FINALPEX DS    0H                  EXIT                                         
         B     EXIT                                                             
*                                                                               
         DROP  R4                  KEEP IT   CLEAN                              
         EJECT ,                                                                
***********************************************************************         
*  ROUTINE TO GET TABLE DICTIONARY ENTRIES                            *         
*                                                                     *         
*    ON INPUT:                                                        *         
*       PARM LIST:                                                    *         
*          P1  BYTE  0   = LENGTH  OF THE DICTIONARY ENTRY            *         
*              BYTES 1-3 = ADDRESS OF THE DICTIONARY ENTRY            *         
*                                                                     *         
*          P2  BYTES 0-3 = ADDRESS OF THE TEXT       ENTRY            *         
***********************************************************************         
         SPACE 1                                                                
EXPDNTRY NTR1                                                                   
         L     R2,0(,R1)           GET  DICTIONARY     ENTRY                    
         LA    R2,0(,R2)           CLEAR     HIGH ORDER     BYTE                
         L     R3,4(,R1)           GET  TEXT           ENTRY                    
         LA    R3,0(,R3)           CLEAR     HIGH ORDER     BYTE                
         ZIC   R4,0(,R1)           GET  DICTIONARY     ENTRY     LENGTH         
         BCTR  R4,0                MINUS     ONE  FOR  EXECUTE                  
         EXCLC R4,0(R3),SPACES     WAS  TEXT DEFINED ?                          
         BNE   EXIT                YES, RETURN                                  
*                                                                               
         EXMVC R4,0(R3),0(R2)      MOVE DICTIONARY     VALUE TO  TEXT           
         CLI   0(R3),ESCHIGHQ      TEST FOR  ESCAPE    SEQUENCE                 
         BNL   EXIT                                                             
*                                  TRANSLATE                                    
         GOTO1 DICTATE,DMCB,C'SU  ',(R3),0                                      
         B     EXIT                RETURN                                       
         EJECT ,                                                                
***********************************************************************         
*  PRINT TOTALS FOR COMPANY                                           *         
***********************************************************************         
         SPACE 1                                                                
         USING PSD,R4              SUMMARY   OUTPUT    LINE                     
         SPACE 1                                                                
PRTCPYTO NTR1                                                                   
         CLI   PREVCPY,X'00'       ANY  COMPANY ?                               
         BE    PRTCPYTX            NO,  EXIT                                    
         LA    R4,P                                                             
         MVC   PSDTFORH,=C'TOTAL FOR'                                           
         MVC   PSDTFCPY,=C'COMPANY'                                             
         MVC   CPYCDE,PREVCPY           GET  COMPANY   CODE                     
         GOTO1 HEXOUT,DMCB,CPYCDE,PSDCPYCD,1                                    
         ZAP   WORK(8),CPYFNDS          GET  NUMBER    OF   MATCHES             
         OI    WORK+7,X'0F'                                                     
         MVC   PSDRMATH,=C'RECORDS MATCHED='                                    
         UNPK  PSDRMAT,WORK(8)                                                  
         ZAP   WORK(8),CPYCHGS          GET  NUMBER    OF   CHANGES             
         OI    WORK+7,X'0F'                                                     
         MVC   PSDRCHGD,=C'RECORDS CHANGED='                                    
         UNPK  PSDRCHG,WORK(8)                                                  
         ZAP   WORK(8),CPYRECS          GET  NUMBER OF RECORDS                  
         OI    WORK+7,X'0F'                                                     
         MVC   PSDTRECH,=C'TOTAL RECORDS='                                      
         UNPK  PSDTREC,WORK(8)                                                  
         GOTO1 VPRINTER                                                         
         CP    CPYFNDS,=P'0'            DID  WE   FIND DATA ?                   
         BE    PRTCPYTX                 NO,  SKIP                               
         MVI   P,C' '                   YES, SKIP A    LINE                     
         GOTO1 VPRINTER                                                         
*                                                                               
PRTCPYTX DS    0H                                                               
         ZAP   CPYRECS,=P'0'       COMPANY   TOTAL     RECORDS                  
         ZAP   CPYFNDS,=P'0'       COMPANY   TOTAL     FOUND                    
         ZAP   CPYCHGS,=P'0'       COMPANY   TOTAL     CHANGES                  
         B     EXIT                RETURN                                       
*                                                                               
         DROP  R4                  KEEP  IT  CLEAN                              
         EJECT ,                                                                
***********************************************************************         
*  ROUTINE TO DELETE AN ELEMENT                                       *         
*                                                                     *         
*        P1    BYTE 0    ELEMENT CODE                                 *         
*              BYTE 1-3  A(RECORD)                                    *         
*        P2    BYTE 0    LENGTH OF SEARCH ARGUMET                     *         
*              BYTE 1-3  A(SEARCH ARGUMENT)                           *         
*                                                                     *         
*  E.G.  GOTO1 DELEL,DMCB,('PTRELQ',AREC),0                           *         
***********************************************************************         
         SPACE 1                                                                
DELEL    NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         ZIC   R4,0(R1)                                                         
         ZIC   R5,4(R1)                                                         
         GOTO1 HELLO,DMCB,(C'D',=C'ACCMST  '),((R4),(R2)),((R5),(R3))           
         B     EXIT                                                             
         EJECT ,                                                                
***********************************************************************         
*  ROUTINE TO GET AN ELEMENT                                          *         
***********************************************************************         
         SPACE 1                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT ,                                                                
***********************************************************************         
*  ROUTINE TO ADD AN ELEMENT                                          *         
*                                                                     *         
*        P1    A(RECORD)                                              *         
*        P2    A(ELEMENT)                                             *         
***********************************************************************         
         SPACE 1                                                                
ADDEL    NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         GOTO1 HELLO,DMCB,(C'P',=C'ACCMST  '),(R2),(R3)                         
         CLI   DMCB+12,0                                                        
         BE    EXIT                                                             
         TM    DMCB+12,X'05'                                                    
         BNZ   *+6                                                              
         DC    H'0'                CAN'T ADD THE ELEMENT                        
         MVC   P(30),=CL30'RECORD TOO BIG TO ADD TO'                            
         GOTO1 VPRINTER                                                         
         MVI   WRTSW,NO                                                         
         B     EXIT                                                             
         EJECT ,                                                                
***********************************************************************         
*  ROUTINE TO FIND AN ELEMENT                                         *         
*                                                                     *         
*        P1    BYTE 0    ELEMENT CODE                                 *         
*              BYTE 1-3  A(RECORD)                                    *         
*        P2    BYTE 0    LENGTH OF SEARCH ARGUMENT                    *         
*              BYTE 1-3  A(SEARCH ARGUMENT)                           *         
***********************************************************************         
         SPACE 1                                                                
FNDEL    NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         ZIC   R4,0(R1)                                                         
         ZIC   R5,4(R1)                                                         
         GOTO1 HELLO,DMCB,(C'G',=C'ACCBIG  '),((R4),(R2)),((R5),(R3))           
         B     EXIT                                                             
         EJECT ,                                                                
***********************************************************************         
*  DUMP ROUTINES                                                      *         
***********************************************************************         
         SPACE 1                                                                
         USING ACCRECD,R3          GENERAL   RECORD    DSECT                    
         SPACE 1                                                                
DMPGET   TM    UPSI,UPSIGET        MUST REQUEST   DUMP                          
         BZR   RE                                                               
         CLI   DMPSW,YES           ALREADY   DUMPED    THIS RECORD ?            
         BNER  RE                                                               
         MVI   DMPSW,NO                                                         
         NTR1  ,                                                                
         AP    DUMPCNT,=P'1'                                                    
         ZAP   DUB,DUMPCNT                                                      
         DP    DUB,EVERY                                                        
         CP    DUB+4(4),=P'0'                                                   
         BNE   EXIT                                                             
         AP    PDUMP,=P'1'                                                      
         CP    PDUMP,MAXDUMP                                                    
         BH    EXIT                                                             
         LA    R6,TEXTMSG                                                       
         ICM   R6,8,=AL1(L'TEXTMSG)                                             
         CLC   TEXTMSG,SPACES                                                   
         BH    DUMP                                                             
         LA    R6,=C'GET'                                                       
         ICM   R6,8,=AL1(3)                                                     
         B     DUMP                                                             
*                                                                               
DMPPUT   TM    UPSI,UPSIPUT        MUST REQUEST   DUMP                          
         BZR   RE                                                               
         CLI   WRTSW,YES           WILL WE   WRITE/PUT THIS RECORD ?            
         BNER  RE                  NO,  SO   DO   NOT  SHOW PUT                 
         MVI   DMPSW,NO                                                         
         NTR1  ,                                                                
         AP    DUMPCNT,=P'1'                                                    
         ZAP   DUB,DUMPCNT                                                      
         DP    DUB,EVERY                                                        
         CP    DUB+4(4),=P'0'                                                   
         BNE   EXIT                                                             
         AP    PDUMP,=P'1'                                                      
         CP    PDUMP,MAXDUMP                                                    
         BH    EXIT                                                             
         L     R3,AREC             ->   RECORD                                  
         LA    R6,TEXTMSG                                                       
         ICM   R6,8,=AL1(L'TEXTMSG)                                             
         CLC   TEXTMSG,SPACES                                                   
         BH    DUMP                                                             
         LA    R6,=C'PUT'                                                       
         ICM   R6,8,=AL1(3)                                                     
         SPACE 1                                                                
DUMP     SR    R8,R8                                                            
         ICM   R8,3,ACCRLEN        RECORD    LENGTH                             
         B     DUMPNOW                                                          
*                                                                               
DMPELMNT TM    UPSI,UPSIELMT       MUST REQUEST   DUMP                          
         BZR   RE                                                               
         CLI   CHGELMNT,YES        DID  WE   CHANGE    THIS ELEMENT ?           
         BNER  RE                  NO,  SO   DO   NOT  SHOW ELEMENT             
*        MVI   DMPSW,NO                                                         
         NTR1  ,                                                                
         AP    DUMPECNT,=P'1'                                                   
         ZAP   DUB,DUMPECNT                                                     
         DP    DUB,EVERY                                                        
         CP    DUB+4(4),=P'0'                                                   
         BNE   EXIT                                                             
         AP    PDUMP,=P'1'                                                      
         CP    PDUMP,MAXDUMP                                                    
         BH    EXIT                                                             
         LR    R3,R4               DUMP THE   ELEMENT                           
         SR    R8,R8                                                            
         IC    R8,1(,R3)           ELEMENT    LENGTH                            
         LA    R6,TEXTMSG                                                       
         ICM   R6,8,=AL1(L'TEXTMSG)                                             
         CLC   TEXTMSG,SPACES                                                   
         BH    DUMPNOW                                                          
         LA    R6,=C'ELEMENT'                                                   
         ICM   R6,8,=AL1(7)                                                     
*        B     DUMPNOW                                                          
*                                                                               
DUMPNOW  GOTO1 PRNTBL,DMCB,(R6),(R3),C'DUMP',(R8),=C'2D'                        
         XC    TEXTMSG,TEXTMSG                                                  
         B     EXIT                                                             
*                                                                               
         DROP  R3                  KEEP IT   CLEAN                              
         EJECT ,                                                                
***********************************************************************         
*  EQUATES                                                            *         
***********************************************************************         
         SPACE 1                                                                
TURNOFF  EQU   X'FF'                                                            
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
EOT      EQU   X'FF'               END OF TABLE                                 
ESCHIGHQ EQU   48                  X'30' ABOVE ESCAPE SEQUENCE                  
MXRLNQ   EQU   2100                MAX   LENGTH    OF   RECORD                  
MAX#PARM EQU   4                   MAX   NUMBER    OF   KEYWORD PARMS           
MAX#PRS  EQU   6                   MAX   NUMBER    OF   PARSE   ELEM.S          
         SPACE 3                                                                
*        SUBDIVISION OF BATCH TYPES                                             
         SPACE 1                                                                
TY30DI   EQU   229                 TYPE 30 DIFFERENCE (FOREIGN CURR)            
TY06MN   EQU   230                 TYPE 06 MANUAL BILLING                       
TY30CH   EQU   231                 TYPE 30 CHECK                                
TY30OF   EQU   232                 TYPE 30 OFFSET                               
TY30WO   EQU   233                 TYPE 30 WRITE OFF                            
TY30TT   EQU   234                 TYPE 30 TRANSFER TO                          
TY30TF   EQU   235                 TYPE 30 TRANSFER FROM                        
*                                                                               
         EJECT ,                                                                
***********************************************************************         
*  CONSTANTS                                                          *         
***********************************************************************         
         SPACE 1                                                                
PRNTBL   DC    V(PRNTBL)           ->    PRNTBL                                 
HELLO    DC    V(HELLO)            ->    HELLO                                  
HEXIN    DC    V(HEXIN)            ->    HEXIN                                  
HEXOUT   DC    V(HEXOUT)           ->    HEXOUT                                 
DATCON   DC    V(DATCON)           ->    DATCON                                 
DICTATE  DC    V(DICTATE)          ->    DICTATE                                
*                                                                               
AIO      DC    A(IO)               ADDR  I/O  AREA FOR  ACCOUNT     REC         
*                                                                               
EVERY    DC    PL4'1'                                                           
MAXDUMP  DC    PL4'500'                                                         
*                                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*  CONSTANTS TO BE UPDATED                                            *         
***********************************************************************         
         SPACE 1                                                                
CPYSEL   DC    XL1'00'             HEX   CODE OF COMPANY TO BE SELECTED         
*                                        X'00'     MEANS ALL COMPANIES          
*                                                                               
RQSTART  DC    XL3'000000'         START DATE PACKED                            
RQEND    DC    XL3'FFFFFF'         END   DATE PACKED                            
*                                                                               
UPSI     DC    XL1'00'             RUN   UPSI CODE                              
UPSIGET  EQU   X'80'               .     DUMP GET  REQUESTS                     
UPSIPUT  EQU   X'40'               .     DUMP PUT  REQUESTS                     
UPSIELMT EQU   X'20'               .     DUMP UPDATED   ELEMENTS                
*                                                                               
KWSCNFOR DC    CL1'C'              LIMIT KEYWORD   SCAN                         
KWSCNCHR EQU   C'C'                .     CHARACTER      FORMAT                  
KWSCNTRN EQU   C'T'                .     TRANSLATED     FORMAT                  
KWSCNALL EQU   C' '                .     ALL            FORMATS                 
         EJECT ,                                                                
***********************************************************************         
*  VARIABLES THAT MUST BE PART OF THIS MODULE                         *         
***********************************************************************         
         SPACE 1                                                                
CPYRECS  DS    PL8                 COMPANY   RECORDS                            
CPYFNDS  DS    PL8                 COMPANY   FOUND                              
CPYCHGS  DS    PL8                 COMPANY   CHANGES                            
*                                                                               
TOTRECS  DS    PL8                 TOTAL     RECORDS                            
TOTFNDS  DS    PL8                 TOTAL     FOUND                              
TOTCHGS  DS    PL8                 TOTAL     CHANGES                            
*                                                                               
DUMPCNT  DS    PL4                 RECORDS   DUMPED                             
DUMPECNT DS    PL4                 ELEMENTS  DUMPED                             
PDUMP    DS    PL4                 TOTAL     DUMPED                             
*                                                                               
PREVCPY  DS    CL(L'RESKCPY)       PREVIOUS  COMPANY                            
*                                                                               
LIMRTYPS DS    CL1                 LIMIT     REPORT    TYPES                    
*                                                                               
PRTOTALS DS    CL1                 PRINTED   TOTALS                             
*                                                                               
CHARECS  DC    PL4'0'                                                           
ERRRECS  DC    PL4'0'                                                           
DATADISP DC    H'56'                                                            
SAVENAME DS    XL64                                                             
         EJECT ,                                                                
***********************************************************************         
*  KEYWORD TABLES                                                     *         
***********************************************************************         
         SPACE 1                                                                
KEYWRDS  DS    0F                  KEYWORDS INCLUDE TABLE                       
         DCDD  AC#RSAF1,6          F1                                           
         DC    CL6'F1'                                                          
         DC    CL1'Y'                                                           
         DC    XL12'0592C5BE0000000000000000'                                   
*                                                                               
         DCDD  AC#RSAF2,6          F2                                           
         DC    CL6'F2'                                                          
         DC    CL1'Y'                                                           
         DC    XL12'0593C5BE0000000000000000'                                   
*                                                                               
         DCDD  AC#RSAF3,6          F3                                           
         DC    CL6'F3'                                                          
         DC    CL1'Y'                                                           
         DC    XL12'0594C5BE0000000000000000'                                   
*                                                                               
         DCDD  AC#RSAF4,6          F4                                           
         DC    CL6'F4'                                                          
         DC    CL1'Y'                                                           
         DC    XL12'0595C5BE0000000000000000'                                   
*                                                                               
         DCDD  AC#RSAF5,6          F5                                           
         DC    CL6'F5'                                                          
         DC    CL1'Y'                                                           
         DC    XL12'08E3C5BE0000000000000000'                                   
*                                                                               
         DCDD  AC#RSOFF,6          OF = OFFICE                                  
         DC    CL6'OF'                                                          
         DC    CL1'Y'                                                           
         DC    XL12'05FE00000000000000000000'                                   
*                                                                               
         DCDD  AC#RSCNO,6          CKNO                                         
         DC    CL6'CKNO'                                                        
         DC    CL1'Y'                                                           
         DC    XL12'058C00000000000000000000'                                   
*&&DO                                                                           
         DCDD  AC#RSBAL,6                                                       
         DC    CL6'  '                                                          
         DC    CL1'N'                                                           
         DC    XL12'00'                                                         
*                                                                               
         DCDD  AC#RSBLC,6                                                       
         DC    CL6'  '                                                          
         DC    CL1'N'                                                           
         DC    XL12'00'                                                         
*                                                                               
         DCDD  AC#RSGRA,6                                                       
         DC    CL6'  '                                                          
         DC    CL1'N'                                                           
         DC    XL12'00'                                                         
*                                                                               
         DCDD  AC#RSGCD,6                                                       
         DC    CL6'  '                                                          
         DC    CL1'N'                                                           
         DC    XL12'00'                                                         
*                                                                               
         DCDD  AC#RSNTA,6                                                       
         DC    CL6'  '                                                          
         DC    CL1'N'                                                           
         DC    XL12'00'                                                         
*                                                                               
         DCDD  AC#RSNCD,6                                                       
         DC    CL6'  '                                                          
         DC    CL1'N'                                                           
         DC    XL12'00'                                                         
*                                                                               
         DCDD  AC#RSCOM,6                                                       
         DC    CL6'  '                                                          
         DC    CL1'N'                                                           
         DC    XL12'00'                                                         
*                                                                               
         DCDD  AC#RSCDA,6                                                       
         DC    CL6'  '                                                          
         DC    CL1'N'                                                           
         DC    XL12'00'                                                         
*&&                                                                             
*                                                                               
         DC    AL1(EOT)                                                         
*                                                                               
KEYWRDS2 DS    0F                  KEYWORDS EXCLUDE TABLE                       
*&&DO                                                                           
         DCDD  AC#RSWC,6                                                        
         DC    CL6'  '                                                          
         DC    CL1'N'                                                           
         DC    XL12'00'                                                         
*                                                                               
         DCDD  AC#RSWCN,6                                                       
         DC    CL6'  '                                                          
         DC    CL1'N'                                                           
         DC    XL12'00'                                                         
*                                                                               
         DCDD  AC#RSTSK,6                                                       
         DC    CL6'  '                                                          
         DC    CL1'N'                                                           
         DC    XL12'00'                                                         
*                                                                               
         DCDD  AC#RSGPT,6                                                       
         DC    CL6'  '                                                          
         DC    CL1'N'                                                           
         DC    XL12'00'                                                         
*&&                                                                             
*                                                                               
         DC    AL1(EOT)                                                         
*                                                                               
         EJECT ,                                                                
***********************************************************************         
*  PARAMETERS TABLE                                                   *         
*                                                                     *         
*  NOTE; MAINTAIN IN SAME ORDER AS PARMTAB ENTRIES IN ACSCR01         *         
***********************************************************************         
         SPACE 1                                                                
PARMTBL  DS    0F                                                               
         DCDD  AC#RSADD,4          ADR                                          
         DC    CL4'ADR'                                                         
         DC    AL1(2)                                                           
*                                                                               
         DCDD  AC#RSCOD,4          CODE                                         
         DC    CL4'CODE'                                                        
         DC    AL1(1)                                                           
*                                                                               
         DCDD  AC#RSNAM,4          NAME                                         
         DC    CL4'NAME'                                                        
         DC    AL1(1)                                                           
*                                                                               
         DCDD  AC#RSNAM,4          NAME - FRENCH                                
         DC    CL4'NOM '                                                        
         DC    AL1(1)                                                           
*                                                                               
         DCDD  AC#RSNAM,4          NAME - DUTCH                                 
         DC    CL4'NAAM'                                                        
         DC    AL1(1)                                                           
*                                                                               
         DCDD  AC#RSCAN,4          BOTH                                         
         DC    CL4'BOTH'                                                        
         DC    AL1(1)                                                           
*                                                                               
         DCDD  AC#RSCAN,4          BOTH - GERMAN AND DUTCH                      
         DC    CL4'BEID'                                                        
         DC    AL1(1)                                                           
*                                                                               
         DCDD  AC#RSCAN,4          BOTH - FRENCH                                
         DC    CL4'DEUX'                                                        
         DC    AL1(1)                                                           
*                                                                               
         DC    AL1(EOT)                                                         
         EJECT ,                                                                
***********************************************************************         
*  FILTER TYPE TABLE                                                  *         
***********************************************************************         
         SPACE 1                                                                
FILTERS  DS    0F                                                               
*        DC    AL1(RFLCLI)                                                      
*        DC    CL19'CLIENT'                                                     
*                                                                               
         DC    AL1(EOT)                                                         
         EJECT ,                                                                
***********************************************************************         
*  REPORT TYPES TABLE (IF EMPTY, THEN ALL REPORT TYPES)               *         
***********************************************************************         
         SPACE 1                                                                
RPTTYPES DS    0F                                                               
*&&DO                                                                           
         DCDD  AC#RSRCV,6          RCV                                          
         DC    CL6'RCV'                                                         
         DCDD  AC#RSRCV,6          DEB                                          
         DC    CL6'DEB'                                                         
         DCDD  AC#RSINC,6          INC                                          
         DC    CL6'INC'                                                         
         DCDD  AC#RSPAY,6          PAY                                          
         DC    CL6'PAY'                                                         
         DCDD  AC#RSPAY,6          PAY                                          
         DC    CL6'CRD'                                                         
         DCDD  AC#RSEXP,6          EXP                                          
         DC    CL6'EXP'                                                         
         DCDD  AC#RS497,6          PROD                                         
         DC    CL6'PROD'                                                        
         DCDD  AC#RS498,6          PERSON                                       
         DC    CL6'PERSON'                                                      
         DCDD  AC#RS540,6          CASH                                         
         DC    CL6'CASH'                                                        
         DCDD  AC#RS544,6          P&L                                          
         DC    CL6'P&&L'                                                        
         DCDD  AC#GLG,6            G/L                                          
         DC    CL6'G/L'                                                         
         DCDD  AC#GLG,6            GL                                           
         DC    CL6'GL'                                                          
         DC    CL6'FI'             FINANCIAL                                    
         DC    CL6'  '                                                          
         DC    CL6'M2'             MANPOWER                                     
         DC    CL6'  '                                                          
         DC    CL6'IV'             INVOICE                                      
         DC    CL6'  '                                                          
*&&                                                                             
*                                                                               
         DC    AL1(EOT)                                                         
         EJECT ,                                                                
***********************************************************************         
*  REPORT TYPE NAMES TABLE - CONTAINS LIST OF ALL REPORT TYPES        *         
***********************************************************************         
         SPACE 1                                                                
RPTNAMES DS    0F                                                               
         DCDD  AC#RSRCV,6          RCV    - US                                  
         DC    CL6'RCV'                                                         
         DCDD  AC#RSRCV,6          DEB    - UK                                  
         DC    CL6'DEB'                                                         
         DCDD  AC#RSRCV,6          CREAN. - FRENCH                              
         DC    CL6'CREAN.'                                                      
*                                                                               
         DCDD  AC#RSINC,6          INC    - US                                  
         DC    CL6'INC'                                                         
         DCDD  AC#RSINC,6          BENEF  - FRENCH                              
         DC    CL6'BENEF'                                                       
         DCDD  AC#RSINC,6          INK    - DUTCH                               
         DC    CL6'INK'                                                         
*                                                                               
         DCDD  AC#RSPAY,6          PAY    - US                                  
         DC    CL6'PAY'                                                         
         DCDD  AC#RSPAY,6          CRD    - UK                                  
         DC    CL6'CRD'                                                         
         DCDD  AC#RSPAY,6          APAYER - FRENCH                              
         DC    CL6'APAYER'                                                      
         DCDD  AC#RSPAY,6          KRED   - GERMAN                              
         DC    CL6'KRED'                                                        
         DCDD  AC#RSPAY,6          TEBET  - DUTCH                               
         DC    CL6'TEBET'                                                       
*                                                                               
         DCDD  AC#RSEXP,6          EXP    - US                                  
         DC    CL6'EXP'                                                         
         DCDD  AC#RSEXP,6          FRAIS  - FRENCH                              
         DC    CL6'FRAIS'                                                       
         DCDD  AC#RSEXP,6          AUF    - GERMAN                              
         DC    CL6'AUF'                                                         
         DCDD  AC#RSEXP,6          ONK    - DUTCH                               
         DC    CL6'ONK'                                                         
*                                                                               
         DCDD  AC#RS497,6          PROD   - US                                  
         DC    CL6'PROD'                                                        
*                                                                               
         DCDD  AC#RS498,6          PERSON - US                                  
         DC    CL6'PERSON'                                                      
         DCDD  AC#RS498,6          COST   - FRENCH                              
         DC    CL6'COST'                                                        
         DCDD  AC#RS498,6          PERSOO - DUTCH                               
         DC    CL6'PERSOO'                                                      
*                                                                               
         DCDD  AC#RS540,6          CASH   - US                                  
         DC    CL6'CASH'                                                        
         DCDD  AC#RS540,6          ESP}CE - FRENCH                              
         DC    CL6'ESP}CE'                                                      
         DCDD  AC#RS540,6          KOSTEN - GERMAN (US ONLY)                    
         DC    CL6'KOSTEN'                                                      
         DCDD  AC#RS540,6          GELD   - DUTCH                               
         DC    CL6'GELD'                                                        
*                                                                               
         DCDD  AC#RS544,6          P&L    - US                                  
         DC    CL6'P&&L'                                                        
         DCDD  AC#RS544,6          P+P    - FRENCH                              
         DC    CL6'P+P'                                                         
         DCDD  AC#RS544,6          G&V    - GERMAN                              
         DC    CL6'G&&V'                                                        
         DCDD  AC#RS544,6          W/V    - DUTCH                               
         DC    CL6'W/V'                                                         
*                                                                               
         DCDD  AC#GLG,6            G/L    - US AND UK                           
         DC    CL6'G/L'                                                         
         DCDD  AC#GLG,6            GL     - OLD IN UK                           
         DC    CL6'GL'                                                          
         DCDD  AC#GLG,6            GLG    - FRENCH                              
         DC    CL6'GLG'                                                         
         DCDD  AC#GLG,6            BKT    - GERMAN                              
         DC    CL6'BKT'                                                         
         DCDD  AC#GLG,6            AG     - DUTCH                               
         DC    CL6'AG'                                                          
*                                                                               
         DC    CL6'FI'             FINANCIAL - US ONLY                          
         DC    CL6'  '                                                          
         DC    CL6'M2'             MANPOWER                                     
         DC    CL6'  '                                                          
         DC    CL6'IV'             INVOICE                                      
         DC    CL6'  '                                                          
*                                                                               
         DC    AL1(EOT)                                                         
         EJECT ,                                                                
***********************************************************************         
*  TRANSACTION TYPE TABLE                                             *         
***********************************************************************         
         SPACE 1                                                                
TTYPETAB DS    0C                                                               
         DC    AL1(TY30DI)        DIFFERENCE TYPE 30                            
         DC    CL4'DI'                                                          
         DC    AL1(TY30CH)        CHECK TYPE 30                                 
         DCDD  AC#RSTCH,4                                                       
         DC    AL1(TY30OF)        OFFSET TYPE 30                                
         DCDD  AC#RSTOF,4                                                       
         DC    AL1(TY30WO)        WRITE-OFF TYPE 30                             
         DCDD  AC#RSTWO,4                                                       
         DC    AL1(TY30TT)        TRANSFERED FROM TYPE 30                       
         DCDD  AC#RSTTT,4                                                       
         DC    AL1(TY30TF)        TRANSFERED TO TYPE 30                         
         DCDD  AC#RSTTF,4                                                       
         DC    AL1(TY06MN)        MANUAL BILLING TYPE 06                        
         DC    CL4'M'                                                           
         DC    AL1(EOT)                                                         
         EJECT ,                                                                
***********************************************************************         
*        BUFFERS                                                      *         
***********************************************************************         
         SPACE 1                                                                
         DC    F'0'                IOAREA     #1 - ACCOUNT   RECORD             
IO       DC    (MXRLNQ)X'00'                                                    
*                                                                               
         EJECT ,                                                                
***********************************************************************         
*  VARIABLES THAT ARE NOT PART OF THIS MODULE                         *         
***********************************************************************         
         SPACE 1                                                                
ACLDSCRD DSECT                                                                  
DUB      DS    D                   DOUBLE     WORD                              
*                                                                               
DMCB     DS    6F                  PARM  LIST TO   CALL SUBROUTINES             
*                                                                               
ADELIM   DS    A                   ->    DELIMITER FOR  KEYWORD                 
APARM    DS    A                   ->    PARM LIST FROM CALLER                  
*                                                                               
         DS    0F                                                               
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
*                                                                               
HALF     DS    H                   HALF  WORD WORK AREA                         
CHARLEFT DS    H                   NUM   OF   CHARACTERS     LEFT               
*                                                                               
WORK     DS    XL64                WORK  AREA                                   
*                                                                               
BYTE     DS    CL1                 ONE   BYTE WORK AREA                         
*                                                                               
CURRCPY  DS    CL(L'RESKCPY)       CURRENT    COMPANY                           
CURRFORM DS    CL(L'RESKFORM)      CURRENT    FORMAT                            
CPYCDE   DS    XL1                 COMPANY    CODE                              
*                                                                               
FORMNAME DS    CL(L'RESKFORM)      FORMAT     NAME                              
REPTCODE DS    CL(L'STYCODE)       REPORT     TYPE CODE                         
REPTNAME DS    CL(L'STYNAME)       REPORT     TYPE NAME                         
FNDRPTYP DS    CL(L'STYNAME)       REPORT     TYPE NAME THAT WAS  FOUND         
*                                                                               
LASTACTD DS    CL10                LAST  ACTIVITY  DATE (MMMDD/YYYY)            
REQDATE  DS    PL3                 LAST  REQUEST   DATE (PACKED)                
REQDATED DS    CL10                LAST  REQUEST   DATE (MMMDD/YY)              
REQUEST# DS    CL3                 NUM   OF   OVERNIGHT REQUESTS                
*                                                                               
NEWDATA  DS    CL(L'KEYWTXTN)      NEW   DATA TEXT VALUE                        
*                                                                               
TEXTMSG  DS    CL12                                                             
*                                                                               
FOUND    DS    CL1                                                              
FOUND2   DS    CL1                 EXCLUDE INDICATOR FOUND                      
CHGELMNT DS    CL1                 THIS  ELEMENT   WAS  CHANGED  (Y/N)          
TRANSLAT DS    CL1                 TRANSLATED BIT  ON  (Y/N)                    
FNDKEYWD DS    CL6                 LAST  KEYWORD   THAT WAS  FOUND              
FNDFILTR DS    CL19                LAST  FILTER    THAT WAS  FOUND              
WRTSW    DS    CL1                 UPDATE     REQUESTED                         
PURGESW  DS    CL1                 PURGE      REQUESTED                         
DMPSW    DS    CL1                 OKAY  TO   DUMP THIS RECORD                  
*                                                                               
NPARMS   DS    XL1                 #     OF   PARAMETERS                        
BLOCK    DS    (MAX#PRS)CL32       PARSE BLOCK                                  
*                                                                               
ELCODE   DS    CL1                                                              
ELEMENT  DS    CL255                                                            
DBGRCD   DS    CL(RESRFST-RESRECD) DEBUG   RCD  ID                              
DBGELMNT DS    CL256               DEBUG   ELEMENT                              
ACLDSCRX EQU   *                                                                
         EJECT ,                                                                
***********************************************************************         
*  FREE FORM SCRIBE ELEMENT                                           *         
***********************************************************************         
         SPACE 1                                                                
STYELD   DSECT                     X'25'   FREE FORM SCRIBE    ELEMENT          
STYEL    DS    X                                                                
STYELQ   EQU   X'25'                                                            
STYLN    DS    X                   ELEMENT LENGTH                               
STYCODE  DS    CL1                 REPORT  TYPE CODE                            
STYNAME  DS    CL6                 REPORT  TYPE NAME                            
STYSEC#1 DS    XL1                 KEYWORD SECURITY                             
STYSSAL  EQU   X'01'                 01 =  SALARY                               
STYSART  EQU   X'02'                 02 =  ARATES                               
STYSBRT  EQU   X'04'                 03 =  BRATE                                
STYSCRT  EQU   X'08'                 04 =  CRATE                                
STYSTAT  DS    XL1                                                              
STYSTXT  EQU   X'80'               NARRATIVE    RECORD    PRESENT               
STYSSTO  EQU   X'40'               AMENDED THROUGH   STEREO                     
STYSWRN  EQU   X'20'               WARNING MESSAGE   ATTACHMENT                 
STYSEC#5 DS    XL1                 PROFILE SECURITY                             
STYSOFF  EQU   X'01'                 33 =  OFFICE REPORTING                     
STYLNQ   EQU   *-STYELD                                                         
         EJECT ,                                                                
***********************************************************************         
*  KEYWORDS DSECT                                                     *         
***********************************************************************         
         SPACE 1                                                                
KEYWRDSD DSECT                                                                  
*                                  KEYWORD   ENTRY                              
KEYWDIC  DS    XL6                 KEYWORD - DICTIONARY ENTRY                   
KEYWTEXT DS    CL6                 KEYWORD - TEXT                               
KEYWUPD  DS    CL1                 KEYWORD - UPDATE REQUESTED Y/N               
KEYWTXTN DS    XL12                KEYWORD - REPLACEMENT ENTRY                  
KEYWRDLQ EQU   *-KEYWRDSD          LENGTH OF A KEYWORD ENTRY                    
         EJECT ,                                                                
***********************************************************************         
*  PARAMETERS DSECT                                                   *         
***********************************************************************         
         SPACE 1                                                                
PARMSD   DSECT                                                                  
*                                  PARAMETER  ENTRY                             
PARMDIC  DS    0XL4                PARAMETER  DICTIONARY     ENTRY              
PARMDESC DS    XL1                 DATA  DICTIONARY     ESCAPE SEQUENCE         
PARMDNUM DS    XL2                 DATA  DICTIONARY     NUMBER                  
PARMDLEN DS    XL1                 DATA  DICTIONARY     LENGTH                  
PARMTEXT DS    CL4                 PARAMETER  TEXT                              
PARMGNUM DS    AL1                 PARAMETER  GROUP     NUMBER                  
PARMENLQ EQU   *-PARMSD            LENGTH OF  A    PARAMETER ENTRY              
         EJECT ,                                                                
***********************************************************************         
*  FILTERS DSECT                                                      *         
***********************************************************************         
         SPACE 1                                                                
FILTERSD DSECT                                                                  
*                                  FILTER   ENTRY                               
FILTTYPE DS    XL1                 FILTER - TYPE                                
FILTDESC DS    CL19                FILTER - DESCRIPTION                         
FILTERLQ EQU   *-FILTERSD          LENGTH OF A FILTER ENTRY                     
         EJECT ,                                                                
***********************************************************************         
*  REPORT TYPES TO BE CONSIDERED                                      *         
*                                                                     *         
*  IF THIS TABLE IS EMPTY, THEN ALL REPORT TYPES ARE VALID            *         
***********************************************************************         
         SPACE 1                                                                
RPTTYPED DSECT                                                                  
*                                  REPORT TYPE ENTRY                            
RPTTDIC  DS    XL6                 REPORT TYPE - DICTIONARY ENTRY               
RPTTTEXT DS    CL6                 REPORT TYPE - TEXT                           
RPTTYPLQ EQU   *-RPTTYPED          LENGTH OF A KEYWORD ENTRY                    
         EJECT ,                                                                
***********************************************************************         
*  SUMMARY OUTPUT LINE                                                *         
***********************************************************************         
         SPACE 1                                                                
PSD      DSECT                                                                  
*                                  SUMMARY LINE                                 
PSDTFORH DS    CL9                 TOTAL FOR                                    
         DS    CL1                                                              
PSDTFCPY DS    CL7                 COMPANY                                      
         DS    CL1                                                              
PSDCPYCD DS    CL2                 COMPANY CODE                                 
         ORG   PSDTFCPY                                                         
PSDTFALL DS    CL10                ** ALL **                                    
         DS    CL3                                                              
PSDRMATH DS    CL16                RECORDS MATCHED=                             
PSDRMAT  DS    CL8                 NUMBER                                       
         DS    CL3                                                              
PSDRCHGD DS    CL16                RECORDS CHANGED=                             
PSDRCHG  DS    CL8                 NUMBER                                       
         DS    CL3                                                              
PSDTRECH DS    CL14                TOTAL RECORDS=                               
PSDTREC  DS    CL8                 NUMBER                                       
PSDLNQ   EQU   *-PSD               LENGTH OF A SUMMARY OUTPUT LINE              
         EJECT ,                                                                
***********************************************************************         
*  DETAIL OUTPUT LINE                                                 *         
***********************************************************************         
         SPACE 1                                                                
PDLD     DSECT                                                                  
*                                  DETAIL  LINE                                 
         DS    CL2                                                              
PDLCPYCD DS    CL2                 COMPANY CODE                                 
         DS    CL1                                                              
PDLFORM  DS    CL8                 FORMAT  NAME                                 
         DS    CL1                                                              
PDLRPTTC DS    CL1                 REPORT  TYPE CODE                            
         DS    CL1                                                              
PDLRPTYP DS    CL6                 REPORT  TYPE                                 
         DS    CL1                                                              
PDLFNDWH DS    CL15                FOUND   WHAT                                 
         DS    CL1                                                              
PDLFNDKW DS    CL6                 FOUND   KEYWORD                              
         DS    CL1                                                              
PDLFNDFL DS    CL19                FOUND   FILTER                               
         DS    CL1                                                              
PDLLREQD DS    CL10                LAST    REQUEST DATE                         
         DS    CL1                                                              
PDLLREQX DS    CL3                 LAST    REQUEST TIMES                        
PDLLNQ   EQU   *-PDLD              LENGTH OF A DETAIL OUTPUT LINE               
         EJECT ,                                                                
         SPACE 1                                                                
* DMLDDEFN                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMLDDEFN                                                       
         PRINT ON                                                               
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
*ACDDEQUS                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
* ACRECEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACRECEQUS                                                      
         PRINT ON                                                               
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011ACLDXSCR  05/01/02'                                      
         END                                                                    
