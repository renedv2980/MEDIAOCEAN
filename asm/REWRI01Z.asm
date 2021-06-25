*          DATA SET REWRI01Z   AT LEVEL 063 AS OF 05/01/02                      
*          DATA SET REWRI01X   AT LEVEL 007 AS OF 01/07/97                      
*PHASE T82101C,*                                                                
*INCLUDE COVAIL                                                                 
         TITLE 'T82101 - REP WRITER APPLICATION'                                
***********************************************************************         
*                                                                     *         
*  LOCALS: REGISTER USAGE                                             *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR     *         
*          R3 - WORK REG                                              *         
*          R4 - GLOBALD                                                         
*          R5 - WORK REG                                              *         
*          R6 - GETEL REG                                                       
*          R7 - SECOND BASE                                           *         
*          R8 - POINTER TO SPOOLD                                     *         
*          R9 - POINTER TO SYSD                                       *         
*          RA - POINTER TO ATWA                                       *         
*          RB - FIRST BASE                                            *         
*          RC - POINTER TO GEND                                       *         
* AIO USAGE - AIO1 - VALI RTNS IN CONTROLLER (REWRI00-T23000)         *         
*                  - IN AIO FROM HYPER CONTROLLER (T00A30)            *         
*             AIO2 -                                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
***********************************************************************         
*                                                                     *         
*  08NOV95 (BU ) --- CHANGE REGENALL TO REGENALL1 (2K BFR/CONTRACT)   *         
*                                                                               
***********************************************************************         
         TITLE 'T82101 - REP WRITER APPLICATION'                                
T82101   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T82101,R7                                                      
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          SYSTEM SPECIFIC WORK                         
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
**                                                                              
         MVI   NRGREAD,C'Y'                                                     
**                                                                              
         SPACE                                                                  
         CLI   MODE,VALREC                                                      
         BNE   MODE2                                                            
         BAS   RE,VREC                                                          
         B     XIT                                                              
         SPACE                                                                  
MODE2    CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
         BAS   RE,PREP                                                          
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE RECORD                                                  
         SPACE 2                                                                
VREC     NTR1                                                                   
         SPACE                                                                  
         GOTO1 USERVAL             GET REP NAME & ADDRESS                       
         SPACE                                                                  
         LA    R2,REWOPTH          VALIDATE OPTIONS BEFORE OTHERS               
         GOTO1 VALOPTS                                                          
         SPACE                                                                  
**       CLI   DOWNOPT,C'Y'        IF WE ARE DOWNLOADING                        
**       BNE   VREC10                                                           
**       CLI   CONOUT,C' '            AND OUTPUT TYPE NOT REQUESTED             
**       BH    VREC10                                                           
**       MVC   CONOUT(8),=CL8'DOWN'   DEFAULT TO OUTPUT OF 'DOWN'               
**       OI    CONOUTH+6,X'80'                                                  
**       MVC   TWAOUT,CONOUT                                                    
         SPACE                                                                  
VREC10   GOTO1 INITDRON            INITIALIZE DRONE                             
         SPACE                                                                  
*                                  OPTIONAL FIELDS                              
         SPACE                                                                  
         LA    R2,REWREGH          REGION                                       
         MVI   BYTE,QLREG          RGON EQUATE                                  
         BAS   R5,MAXNUM                                                        
         BE    *+10                IF RGON AND "ALL" ENTERED                    
         GOTO1 VALIREG                                                          
                                                                                
         LA    R2,REWOFFH          OFFICE                                       
         MVI   BYTE,QLOFF          RGON EQUATE                                  
         BAS   R5,MAXNUM                                                        
         BE    *+10                IF RGON AND "ALL" ENTERED                    
         GOTO1 VALIOFF                                                          
                                                                                
         LA    R2,REWPERH          PERIOD DATES                                 
         GOTO1 VALIPDT                                                          
                                                                                
* - GROUP+SUBGROUP - NOTE 2 EQUATES IN RGON                                     
         LA    R2,REWGRPH          GROUP/SUBGROUP                               
                                                                                
         CLI   NRGREAD,C'Y'        ,,IF RGON READ                               
         BNE   *+12                                                             
         CLI   5(R2),0             ,,MUST HAVE GROUP/SUB                        
         BE    INVLDERR                                                         
                                                                                
         MVI   BYTE,QLGRGRP        RGON EQUATE FOR GROUP                        
         CLI   5(R2),1                                                          
         BE    *+8                                                              
         MVI   BYTE,QLGRP          RGON EQUATE FOR GROUP+SUBGROUP               
         BAS   R5,MAXNUM                                                        
         GOTO1 VALIGS                                                           
                                                                                
         LA    R2,REWSTAH          STATION                                      
         MVI   BYTE,QLSTA          RGON EQUATE                                  
         BAS   R5,MAXNUM                                                        
         BE    *+10                IF RGON AND "ALL" ENTERED                    
         GOTO1 VALISTA                                                          
                                                                                
         LA    R2,REWADTEH        ACTIVITY DATES                                
         GOTO1 VALIACT                                                          
                                                                                
         LA    R2,REWSALH          SALESPERSON                                  
         CLI   NRGREAD,C'Y'        ,,IF RGON READ                               
         BNE   *+12                                                             
         CLI   5(R2),0                                                          
         BNE   INVLDERR            ,,SALESPERSON IS INVALID ENTRY               
         GOTO1 VALISAL                                                          
                                                                                
                                                                                
         LA    R2,REWTEMH          DIV/TEAM                                     
         MVI   BYTE,QLTEM          RGON EQUATE                                  
         BAS   R5,MAXNUM                                                        
         BE    *+10                IF RGON AND "ALL" ENTERED                    
         GOTO1 VALIDT                                                           
                                                                                
         LA    R2,REWADVH          ADVERTISER                                   
         MVI   BYTE,QLADV          RGON EQUATE                                  
         BAS   R5,MAXNUM                                                        
         BE    *+10                IF RGON AND "ALL" ENTERED                    
         GOTO1 VALIADV                                                          
                                                                                
         LA    R2,REWAGYH          AGENCY                                       
         MVI   BYTE,QLAGY          RGON EQUATE                                  
         BAS   R5,MAXNUM                                                        
         BE    *+10                IF RGON AND "ALL" ENTERED                    
         GOTO1 VALIAGY                                                          
                                                                                
         LA    R2,REWCLSH          CONTRACT CLASS                               
         MVI   BYTE,QLCLS          RGON EQUATE                                  
         BAS   R5,MAXNUM                                                        
         BE    *+10                IF RGON AND "ALL" ENTERED                    
         GOTO1 VALICLS                                                          
                                                                                
         LA    R2,REWCATH          CATEGORY                                     
         MVI   BYTE,QLCAT          RGON EQUATE                                  
         BAS   R5,MAXNUM                                                        
         BE    *+10                IF RGON AND "ALL" ENTERED                    
         GOTO1 VALICAT                                                          
                                                                                
         LA    R2,REWPRDH          PRODUCT                                      
         GOTO1 VALIPRD                                                          
         CLI   NRGREAD,C'Y'        ,,IF RGON                                    
         BNE   *+12                                                             
         CLI   5(R2),0                                                          
         BNE   INVLDERR            ,,NO PRODUCT FILTER                          
                                                                                
         LA    R2,REWBOKH          BOOK                                         
         GOTO1 VALIBOK                                                          
         CLI   NRGREAD,C'Y'        ,,IF RGON                                    
         BNE   *+12                                                             
         CLI   5(R2),0                                                          
         BNE   INVLDERR            ,,NO BOOK FILTER                             
                                                                                
********************************************************************            
* FOR RGON READ,  IF STATION IS NOT SELECTED AS FILTER, THEN 3 IS MAX           
* NUMBER OF FIELDS ALLOWED.                                                     
                                                                                
         CLI   NRGREAD,C'Y'        IF ARGON READ                                
         BNE   VREC50                                                           
         CLI   NRGMAX,3               CHECK MAX                                 
         BNH   VREC50                                                           
         CLI   REWSTAH+5,0         IS STATION INPUT?                            
         BE    RGNMXERR            NO-RGON ERROR                                
*********************************************************************           
                                                                                
VREC50   LA    R2,REWFILTH         OPTIONAL FILTERS                             
         GOTO1 VALFILT                                                          
                                                                                
         LA    R2,REWLEFTH         LEFT HEADERS                                 
         MVI   MAX,4                                                            
         GOTO1 VALLEFT                                                          
                                                                                
         LA    R2,REWRGHTH         RIGHT HEADERS                                
         MVI   MAX,3                                                            
         GOTO1 VALRIGHT                                                         
                                                                                
         LA    R2,REWMIDH          MID LINE                                     
         MVI   MAX,1                                                            
         GOTO1 VALMID                                                           
                                                                                
         LA    R2,REWROWSH         ROWS                                         
         MVI   MAX,8                                                            
         CLI   5(R2),0                                                          
         BE    NEED1R                                                           
         GOTO1 VALROWS                                                          
                                                                                
*                                                                               
         CLI   NRGREAD,C'Y'        IF RGON READ?                                
         BNE   *+12                                                             
         BAS   RE,CHKSET           CHECK IF SET EXIST FOR REQUEST               
         BNE   SETERR                                                           
                                                                                
         LA    R2,REWCOLSH         COLUMNS                                      
         CLI   5(R2),0                                                          
         BE    NEED1C                                                           
         MVI   MAX,16                                                           
         GOTO1 VALCOLS                                                          
                                                                                
         LA    R2,REWTITLH         USER TITLES                                  
         GOTO1 VALTITS                                                          
         GOTO1 WRAPDRON                                                         
         SPACE                                                                  
         B     XIT                                                              
                                                                                
                                                                                
*                                                                               
INVLDERR MVI   ERROR,INVALID                                                    
         GOTO1 ERREX,DMCB                                                       
                                                                                
RGNMXERR XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(37),=C'ERROR - EXCEEDS MAXIMUM FILTER NUMBER'            
         GOTO1 ERREX2                                                           
*                                                                               
SETERR   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(27),=C'ERROR - REQUEST SET INVALID'                      
         GOTO1 ERREX2                                                           
*                                                                               
         EJECT                                                                  
* TRACKS NUMER OF REQUESTED FILTER FIELDS FOR NRGON READ                        
* INPUT                                                                         
* R2 ->   SCREEN FIELD HEADER                                                   
* BYTE -> EQUATED SET TYPE FOR FIELD                                            
*                                                                               
* RETURNS CONDITION CODE OF "=" IF RGON READ AND 'ALL' ENTERED                  
* IN THAT CASE WE MUST SKIP VALIDATION ROUTINE                                  
*                                                                               
* OUPUT   SAVES REQUESTED DATA TYPES IN NRGTYPE TABLE                           
*         IF OFFLINE, FILLS NRGTBL WITH DATA TYPE + SCREEN FIELD DATA           
*                                                                               
MAXNUM   CLI   NRGREAD,C'Y'        RGON READ?                                   
         BNER  R5                  NO                                           
                                                                                
         CLI   5(R2),0             ANY INPUT ?                                  
         BE    MXN50               NO                                           
                                                                                
         ZIC   R1,NRGMAX           NUMBER OF REQUESTED FIELDS                   
         LA    R1,1(R1)                                                         
         STC   R1,NRGMAX                                                        
                                                                                
         LA    RE,NRGTYPE          SAVE DATA TYPE IN TABLE                      
         LA    RF,10               MAX NUMBER OF TYPES                          
MXN10    CLI   0(RE),0                                                          
         BE    MXN20                                                            
         LA    RE,1(RE)                                                         
         BCT   RF,MXN10                                                         
         B     RGNMXERR                                                         
MXN20    MVC   0(1,RE),BYTE        SET DATA TYPE                                
*                                                                               
         CLI   OFFLINE,C'Y'        ,,IF OFFLINE                                 
         BNER  R5                                                               
         CLC   =C'ALL',8(R2)       ,,AND NOT 'ALL'                              
         BER   R5                                                               
         LA    RF,NRGTBL           ,,SET DATA TYPE/SCREEN DATA TABLE            
         CLI   0(RF),0                                                          
         BE    *+12                                                             
         LA    RF,9(RF)                                                         
         B     *-12                                                             
         MVC   0(1,RF),BYTE        PASS SET TYPE                                
         XC    1(8,RF),1(RF)       CLEAR DATA FIELD                             
         LA    R1,7                MAX LENGTH                                   
         ZIC   R0,5(R2)            GET LENGTH OF SREEN FIELD INPUT DATA         
         CR    R0,R1               USE WHICHEVER LENGTH IS LESS                 
         BH    *+4                                                              
         LR    R1,R0                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   1(0,RF),8(R2)       AND DATA OF SCREEN FIELD                     
                                                                                
MXN50    LTR   R5,R5               SET CC -> "NOT ="                            
         BR    R5                                                               
*                                                                               
         EJECT                                                                  
*              PRINT REPORT                                                     
         SPACE 2                                                                
PREP     NTR1                                                                   
         GOTO1 INITDRIV            INITIALIZE DRIVER                            
         L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         LA    R1,HOOK             APPLICATION HOOK                             
         ST    R1,GLAHOOK                                                       
         MVI   GLMODE,GLINIT                                                    
         GOTO1 DRIVER,DMCB,(R4)                                                 
         SPACE                                                                  
         MVC   REACOMFC,ACOMFACS   SET UP FOR REWRIIO                           
         LA    R1,IOHOOK                                                        
         ST    R1,REHOOK                                                        
         MVC   REACCS,TWAACCS      PASS THROUGH LIMIT ACCESS                    
         MVC   REAUTH,TWAAUTH                   AND AUTHORIZATION               
         MVC   REUSERID,TWAORIG                 AND REQUESTING ID#              
         MVC   REREP,AGENCY                                                     
         MVC   RADDAY,ADDAY                                                     
*                                                                               
         GOTO1 =V(COVAIL),DMCB,C'GET',30000,30000                               
         OC    DMCB+4(4),DMCB+4    WORK AREA RETRIEVED?                         
         BNZ   *+6                 YES                                          
         DC    H'0'                NO  - CAN'T GO ON                            
         MVC   REANWMON,DMCB+4                                                  
*                                                                               
         SPACE                                                                  
* READ CATEGORY RECS TO BUILD CLASS TABLE *                                     
         SPACE                                                                  
         TM    REQTABLE,REQCTCL    IS CLASS PART OF REQUEST                     
         BZ    PREP10                                                           
         SPACE                                                                  
         MVI   REREAD,RCTGCLST     READ CATEGORY RECS, SAVING CAT/CLS           
         SPACE                                                                  
         GOTO1 REWRIIO,DMCB,REWRIIOD                                            
         SPACE                                                                  
* READ OFFICE RECS TO BUILD REGION TABLE *                                      
         SPACE                                                                  
PREP10   TM    REQTABLE,REQOFRG    IS REGION PART OF REQUEST                    
         BZ    PREP14                                                           
         SPACE                                                                  
         MVI   REREAD,ROFFREGT     READ OFFICE RECS, SAVING OFF & REG           
         SPACE                                                                  
         GOTO1 REWRIIO,DMCB,REWRIIOD                                            
         SPACE                                                                  
* READ STATION RECS TO BUILD OWNER TABLE *                                      
         SPACE                                                                  
PREP14   TM    REQTABLE,REQSTOW    IS STATION OWNER PART OF REQUEST             
         BZ    PREP20                                                           
         SPACE                                                                  
         MVI   REREAD,RSTAOWNT     READ STATION RECS, SAVING STA & OWN          
         SPACE                                                                  
         GOTO1 REWRIIO,DMCB,REWRIIOD                                            
         SPACE                                                                  
PREP20   EQU   *                                                                
         CLI   NRGREAD,C'Y'        RGON READ?                                   
         BNE   PREP22                                                           
         BAS   RE,NRGRRD          READ RGON                                     
         B     PREP30              DONE                                         
                                                                                
PREP22   MVI   READRECS,RCONKTYQ                                                
         MVC   REREAD,READRECS     RECORDS TO BE READ                           
         GOTO1 REWRIIO,DMCB,REWRIIOD                                            
         SPACE                                                                  
PREP30   MVI   GLMODE,GLOUTPUT     THEN PRINT THE REPORTS                       
         GOTO1 DRIVER,DMCB,(R4)                                                 
                                                                                
* FREE REQUESTED STORAGE                                                        
         GOTO1 =V(COVAIL),DMCB,C'FREE',REANWMON,30000                           
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
* IOHOOK FOR CONTRACT AND OPTIONAL PWC RECS *                                   
         SPACE                                                                  
IOHOOK   NTR1                                                                   
         CLI   REMODE,PROCREC      IF THERE IS SOMETHING INTERESTING            
         BE    IOHOOK2                                                          
         B     XIT                                                              
         SPACE                                                                  
IOHOOK2  CLI   TESTOPT,0           TEST OPTION                                  
         BE    IOHOOK4                                                          
         BAS   RE,TEST                                                          
         CLI   TESTOPT,C'B'                                                     
         BNE   XIT                                                              
         SPACE                                                                  
IOHOOK4  BAS   RE,SUBCON           MAY CONTROL AT SUB RECORD LEVEL              
         B     XIT                                                              
         SPACE 3                                                                
*              SUB RECORD CONTROL                                               
         SPACE                                                                  
SUBCON   NTR1                                                                   
         L     R6,REAREC           MUST MATCH ON RECORD                         
         CLC   READRECS,0(R6)                                                   
         BE    SUBCON2                                                          
         CLC   READRECS,REKEY      OR KEY                                       
         BNE   XIT                                                              
         SPACE                                                                  
SUBCON2  DS    0H                                                               
         MVI   GLMODE,GLINPUT                                                   
         XC    ATHISEL,ATHISEL     (NO SUB CONTROL)                             
         GOTO1 DRIVER,DMCB,(R4)                                                 
         B     XIT                                                              
         EJECT                                                                  
* TEST OPTION - PRINT REC CODES FROM CONTRACT RECS *                            
         SPACE 3                                                                
TEST     NTR1                                                                   
         MVC   P(21),REKEY+2                                                    
         MVI   BLOCK,C' '                                                       
         MVC   BLOCK+1(254),BLOCK                                               
         LA    R2,TESTTAB                                                       
         LA    R3,BLOCK                                                         
         SPACE                                                                  
TEST2    CLI   0(R2),X'FF'                                                      
         BNE   TEST4                                                            
         GOTO1 SQUASHER,DMCB,BLOCK,250                                          
         GOTO1 CHOPPER,DMCB,(250,BLOCK),(90,P+30),4                             
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         SPACE                                                                  
TEST4    ZIC   R1,10(R2)           L'DATA                                       
         BCTR  R1,0                                                             
         ZIC   RE,11(R2)           DISPLACEMENT INTO RECODES                    
         LA    RE,RECODES(RE)                                                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         OC    0(0,RE),0(RE)       CHECK IF ANY DATA                            
         BZ    TEST6                                                            
         ZIC   R1,0(R2)            L'LITERAL                                    
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),1(R2)                                                    
         AR    R3,R1                                                            
         MVI   0(R3),C'='                                                       
         LA    R3,1(R3)                                                         
         ZIC   R1,10(R2)           L'DATA                                       
         BCTR  R1,0                                                             
         ZIC   RE,11(R2)           DISPLACEMENT INTO RECODES                    
         LA    RE,RECODES(RE)                                                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(RE)                                                    
         LA    R3,2(R1,R3)                                                      
         SPACE                                                                  
TEST6    LA    R2,L'TESTTAB(R2)                                                 
         B     TEST2                                                            
         EJECT                                                                  
**************************************************************                  
*  READ RGON X'0002' RECORD TO FIND ONE THAT MATCHES THE REQUEST                
*                                                                               
*  NRGTYPE = REQUESTED TYPES                                                    
*                                                                               
CHKSET   NTR1                                                                   
*                                                                               
         MVI   BYTE,0              SET BYTE=2 IF STATION IS DATA TYPE           
         LA    R1,NRGTYPE                                                       
CHK01    CLI   0(R1),2             2=STA DATA TYPE                              
         BE    CHK02                                                            
         LA    R1,1(R1)                                                         
         CLI   0(R1),0                                                          
         BE    CHK03                                                            
         B     CHK01                                                            
CHK02    MVI   BYTE,2              STATION IS REQUESTED                         
*                                                                               
CHK03    XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         MVC   0(2,R3),AGENCY                                                   
         MVC   2(1,R3),DOPTSV      SET AS DEFAULT 1ST $ TYPE                    
         MVI   KEY+4,2                                                          
         MVC   FILENAME,=C'RRGNEW  '                                            
         MVC   LKEY,=H'48'                                                      
         MVI   USEIO,C'Y'                                                       
* SAVE AS POSSIBLE SET TYPE KEYS IN KEYTPTST                                    
         LA    R2,KEYTPTST                                                      
         LA    R1,WORK                                                          
         MVC   WORK(3),NRGTYPE          ABC                                     
         BAS   RE,MOVNSWAP                                                      
         MVC   WORK(1),NRGTYPE+2                                                
         MVC   WORK+1(2),NRGTYPE        CAB                                     
         MVC   WORK+3(3),WORK      SAVE CAB                                     
         BAS   RE,MOVNSWAP                                                      
         MVC   WORK(1),WORK+5                                                   
         MVC   WORK+1(2),WORK+3         BCA                                     
         BAS   RE,MOVNSWAP                                                      
* ALL POSSIBLE 6 KEY COMBINATIONS ARE IN KEYTPTST                               
* READ THROUGH KEYTPTS TO SEE IF ANY MATCH AN 02 SET TYPE REC                   
         LA    R1,KEYTPTST                                                      
         MVC   WORK(5),KEY         SAVE STANDARD KEY                            
CHK05    MVC   KEY(5),WORK         STANDARD KEY                                 
         MVC   KEY+5(3),0(R1)      SETTYPES                                     
         GOTO1 HIGH                                                             
*                                                                               
CHK05B   L     R3,AIO                                                           
         CLC   0(2,R3),AGENCY                                                   
         BNE   CHK07                GET NEXT KEYTYPE SET                        
         CLC   3(2,R3),=X'0002'                                                 
         BNE   CHK07                GET NEXT KEYTYPE SET                        
         CLC   5(3,R3),0(R1)        SET TYPES MATCH?                            
         BE    CHKYES               YES                                         
*                                                                               
CHK06    GOTO1 SEQ                  NO -TRY SEQ                                 
         B     CHK05B                                                           
                                                                                
CHK07    LA    R1,3(R1)             NO/BUMP KEYTPTST                            
         OC    0(3,R1),0(R1)                                                    
         BZ    CHKNO                                                            
         B     CHK05                                                            
*                                                                               
*                                                                               
CHKYES   LA    R1,RGNWRK           ,,,SET KEY TYPES                             
         USING RRGWRI,R1                                                        
         MVC   RRGWDT1(3),5(R3)    ,,,TO DSECT FOR LATER RGON READ              
         DROP  R1                                                               
         SR    RE,RE               CLEAR FOR CC CODE                            
                                                                                
*                                                                               
CHKNO    DS    0H                                                               
*                                                                               
         XC    FILENAME,FILENAME                                                
         MVI   USEIO,0                                                          
         MVC   LKEY,=H'27'         RESET TO REP                                 
*                                                                               
         LTR   RE,RE               SET CONDITION CODE                           
         B     CHKX                                                             
                                                                                
CHKX     B     XIT                                                              
                                                                                
                                                                                
* EXPECTS R1 -> WORK   R2 -> KEYTPTST                                           
MOVNSWAP NTR1                                                                   
         MVC   0(3,R2),0(R1)                                                    
         XC    1(1,R1),2(R1)                                                    
         XC    2(1,R1),1(R1)                                                    
         XC    1(1,R1),2(R1)                                                    
         MVC   3(3,R2),0(R1)                                                    
         LA    R2,6(R2)                                                         
         XIT1  REGS=(R2)                                                        
                                                                                
         EJECT                                                                  
***************************************************************                 
*  CALL RGON INTERFACE  - USE RENRGWRI DSECT                                    
*  RRGWDT1,2,3 KEY TYPES ALREAD SET BY CHKSET                                   
*                                                                               
         DROP  R4                                                               
NRGRRD   NTR1                                                                   
         LA    R3,RGNWRK           R3->RGON DSECT                               
         USING RRGWRI,R3                                                        
                                                                                
* KEY TYPES ALREADY IN RRGWDT1,2,3 (SET BY CHKSET)                              
* NOW PASS ANY DATA ASSOCIATED WITH KEY TYPES TO RGON READ                      
* NRGTBL HAS KEY TYPE (CL1) + DATA (CL8)                                        
*                                                                               
         LA    R0,3                3 KEY TYPES                                  
         LA    R4,10               NRGTBL BCT                                   
         LA    RE,RRGWDT1          KEY TYPE                                     
         LA    RF,RRGWDA1          KEY TYPE DATA AREA                           
                                                                                
NRG03    DS    0H                                                               
         LA    R1,NRGTBL           TBL OF REQ KEY TYPES + DATA IF ANY           
                                                                                
NRG05    CLC   0(1,RE),0(R1)       MATCH KEY WITH TBL OF KEY TYPES              
         BE    NRG07                                                            
         LA    R1,9(R1)            BUMP TBL OF KEY TYPES+DATA                   
         BCT   R4,NRG05                                                         
         DC    H'0'                SHOULD NEVER GET HERE                        
*                                                                               
NRG07    MVC   0(8,RF),1(R1)       MATCH-SET DATA TO KEY TYPE DATA AREA         
         LA    RF,8(RF)            BUMP TO NEXT DATA OUT AREA                   
         LA    RE,1(RE)            BUMP TO NEXT KEY TYPE                        
         LA    R4,10               NRGTBL BCT                                   
         BCT   R0,NRG03                                                         
*                                                                               
         LA    R4,10                                                            
         LA    R1,NRGTBL           FILTERING ON REGION?                         
NRG10    CLI   0(R1),3             REGION EQUATE=3                              
         BE    NRG12                                                            
         LA    R1,9(R1)                                                         
         BCT   R4,NRG10                                                         
         B     *+10                NO                                           
NRG12    MVC   RRGWREG,1(R1)       YES - SET REGION DATA                        
                                                                                
************************************************************                    
*        IN FUTURE ADD FILTER CODE FOR AFFILIATION, TVB,                        
*        RANK AND OWNER HERE.                                                   
                                                                                
************************************************************                    
* SET OTHER REQUIRED FIELDS                                                     
         MVC   RRGWID,=C'**WRIT**'   CONSTANT TO IDENTIFY CALLER                
         MVC   RRGWREP,AGENCY        REP                                        
         MVC   RRGWDTY,DOPTSV        DOLLAR TYPE                                
         MVC   RRGWSTDT,YMSTRP       Y/M START DATE                             
         MVC   RRGWEDDT,YMENDP       Y/M END DATE                               
                                                                                
**********************************************************                      
*  CALL RGON READER - PASS R3 -> RGON INTERFACE DSECT                           
         L     RF,CALLOV                                                        
         ST    RF,VCALLOV                                                       
         MVC   VADDAY,ADDAY                                                     
         MVC   VCALLOV,CALLOV                                                   
         MVC   VDATAMGR,DATAMGR                                                 
         MVC   VDATCON,DATCON                                                   
         MVC   VHEXOUT,HEXOUT                                                   
         MVC   VSPOOL,SPOOL                                                     
         MVC   VSPOOLD,ASPOOLD                                                  
         XC    DMCB(7),DMCB                                                     
         MVC   DMCB+4(4),=X'D9081504'                                           
         GOTO1 (RF),DMCB                                                        
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     R6,0(R1)                                                         
NRG20    GOTO1 (R6),DMCB,(R3)                                                   
*        GOTO1 NRGMOD,DMCB,(R3)                                                 
***********************************************************                     
         CLI   RRGWSTAT,0          SUCCESSFUL READ                              
         BE    NRG30                                                            
         CLI   RRGWSTAT,X'FF'      EOF                                          
         BE    NRGX                                                             
         DC    H'0'                BLOW UP ON ERROR FOR NOW                     
                                                                                
*  POST DATA                                                                    
NRG30    DS    0H                                                               
* POST DATA TYPES FROM KEY                                                      
         LA    R2,3                3 DATA TYPES                                 
         LA    RE,RRGWDT1          DATA TYPE IN KEY                             
         LA    R5,RRGWDAT1         DATA FIELD FROM RGON READ                    
NRG32    LA    RF,NRGTAB                                                        
         LA    R4,RECODES                                                       
*                                                                               
NRG33    CLI   0(RE),0             IF KEY TYPE=0                                
         BE    NRG45               BUMP TO NEXT TYPE I KEY                      
         CLC   0(1,RE),0(RF)       MATCH ON TYPE EQUATE?                        
         BE    NRG40                                                            
         LA    RF,4(RF)            BUMP CODE TABLE                              
         CLI   0(RF),X'FF'         HIT EOF?                                     
         BNE   NRG33                                                            
         DC    H'0'                SHOULD NEVER GET HERE                        
*                                                                               
NRG40    ZIC   R1,1(RF)            GET LENGTH                                   
         ZICM  R0,2(RF),2          GET DISPLACEMENT                             
         AR    R4,R0               ADD TO START OF TABLE                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R5)                                                    
NRG45    LA    RE,1(RE)            BUMP KEY DATA TYPE                           
         LA    R5,8(R5)            BUMP DATA AREA FROM RGON READ                
         BCT   R2,NRG32                                                         
                                                                                
* POST ANY ADDITIONAL DATA ( IN CASES WHEN STATION OR REGION IN KEY)            
*                                                                               
* FOLLOWING SENT IF STATION IS ONE OF KEY TYPES                                 
         OC    RRGWSAFF,RRGWSAFF   AFFILIATION                                  
         BZ    *+10                                                             
         MVC   NRGAFFIL,RRGWSAFF                                                
         OC    RRGWTVB,RRGWTVB    TVB                                           
         BZ    *+10                                                             
         MVC   NRGTVB,RRGWTVB                                                   
         OC    RRGWSRNK,RRGWSRNK   RANK                                         
         BZ    *+10                                                             
         MVC   NRGRANK,RRGWSRNK                                                 
         OC    RRGWSOWN,RRGWSOWN   OWNER                                        
         BZ    *+10                                                             
         MVC   NRGOWN,RRGWSOWN                                                  
                                                                                
* FOLLOWING SENT IF OFFICE IS ONE OF KEY TYPES                                  
         OC    RRGWOREG,RRGWOREG   REGION                                       
         BZ    *+10                                                             
         MVC   NRGREG,RRGWOREG                                                  
                                                                                
                                                                                
* DATE ALWAYS SENT                                                              
         MVC   NRGYRMON,RRGWYRMO   YEAR/MONTH                                   
                                                                                
* $ DATA                                                                        
         MVC   NRGWPRBL,RRGWPRBL   PRIOR BILLED                                 
         MVC   NRGWCRBL,RRGWCRBL   CURRENT BILLED                               
         MVC   NRGWPFIN,RRGWPFIN   PRIOR FINAL                                  
         MVC   NRGWCBUD,RRGWCBUD   CURR BUDGET                                  
         MVC   NRGWCBK,RRGWCBK     CURRENT BOOKED THIS YEAR                     
         MVC   NRGWPBK,RRGWPBK     BOOKED THIS WEEK 1 YEAR AGO                  
         MVC   NRGWCPAC,RRGWCPAC   CURRENT PACING                               
         MVC   NRGWPCTF,RRGWPCTF   PERCENT TO FINAL                             
         MVC   NRGWPCTB,RRGWPCTB   PERCENT TO BUDGET                            
*                                                                               
                                                                                
                                                                                
* SEND RECORD TO DRIVER *                                                       
                                                                                
         L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         MVI   GLMODE,GLINPUT                                                   
         GOTO1 DRIVER,DMCB,(R4)                                                 
         B     NRG20                                                            
         SPACE                                                                  
NRGX     B     XIT                                                              
         EJECT                                                                  
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
         SPACE                                                                  
HOOK     NTR1                                                                   
         CLI   GLHOOK,GLHEAD                                                    
         BNE   XIT                                                              
         GOTO1 GENHEAD                                                          
         B     XIT                                                              
         SPACE                                                                  
XIT      XIT1                                                                   
         SPACE                                                                  
NEED1R   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(18),=C'ERROR - NEED 1 ROW'                               
         GOTO1 ERREX2                                                           
NEED1C   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(21),=C'ERROR - NEED 1 COLUMN'                            
         GOTO1 ERREX2                                                           
         SPACE                                                                  
*                                                                               
* TABLE CONTAINS 6 POSSIBLE SET TYPE KEYS                                       
KEYTPTST DS    CL18                                                             
                                                                                
*                                                                               
* TABLE CONTAINS DATA TYPE (CL1) AND DATA FROM REQ SCREEN (CL8)                 
*                                                                               
NRGTBL   DS    CL1                 DATA TYPE                                    
         DS    CL8                 DATA FROM REQUEST SCREEN FIELD               
         DS    CL81                FOR 9 MORE TYPES                             
         DC    X'FF'                                                            
                                                                                
*                                                                               
TESTTAB  DS    0CL12                                                            
         DC    AL1(6),C'GRPSUB   ',AL1(L'REGRS),AL1(REGRS-RECODES)              
         DC    AL1(3),C'STA      ',AL1(L'RESTA),AL1(RESTA-RECODES)              
         DC    AL1(3),C'OFF      ',AL1(L'REOFF),AL1(REOFF-RECODES)              
         DC    AL1(3),C'AGY      ',AL1(L'REAGY),AL1(REAGY-RECODES)              
         DC    AL1(3),C'AOF      ',AL1(L'REAOF),AL1(REAOF-RECODES)              
         DC    AL1(3),C'ADV      ',AL1(L'READV),AL1(READV-RECODES)              
         DC    AL1(3),C'CON      ',AL1(L'RECONZD),AL1(RECONZD-RECODES)          
         DC    AL1(8),C'SALESMAN ',AL1(L'RESAL),AL1(RESAL-RECODES)              
         DC    AL1(7),C'CONTYPE  ',AL1(L'RECTY),AL1(RECTY-RECODES)              
         DC    AL1(4),C'RANK     ',AL1(L'RERNK),AL1(RERNK-RECODES)              
         DC    AL1(6),C'REGION   ',AL1(L'REREG),AL1(REREG-RECODES)              
         DC    AL1(7),C'DIVTEAM  ',AL1(L'REDVT),AL1(REDVT-RECODES)              
         DC    X'FF'                                                            
                                                                                
* NRGON READ WORK AREA COVERD BY RENRGWRI DSECT                                 
*                                                                               
RGNWRK   DS    CL(RRGWRILE)                                                     
         EJECT                                                                  
*              LTORG AND TABLES                                                 
         LTORG                                                                  
*                                                                               
NRGTAB   DS    0F                                                               
*  TYPE EQUATE  LENGTH  DISPLACEMENT                                            
                                                                                
         DC    AL1(02),AL1(L'NRGSTAT-1),AL2(NRGSTAT-RECODES)                    
         DC    AL1(03),AL1(L'NRGREG-1),AL2(NRGREG-RECODES)                      
         DC    AL1(04),AL1(L'NRGOFF-1),AL2(NRGOFF-RECODES)                      
         DC    AL1(25),AL1(L'NRGGRP-1),AL2(NRGGRP-RECODES)   (1 CHAR)           
         DC    AL1(07),AL1(L'NRGGRP),AL2(NRGGRP-RECODES)     (2 CHAR)           
         DC    AL1(22),AL1(L'NRGSTYP-1),AL2(NRGSTYP-RECODES)                    
         DC    AL1(23),AL1(L'NRGTVB-1),AL2(NRGTVB-RECODES)                      
         DC    AL1(24),AL1(L'NRGOWN-1),AL2(NRGOWN-RECODES)                      
         DC    AL1(05),AL1(L'NRGTEAM-1),AL2(NRGTEAM-RECODES)                    
         DC    AL1(15),AL1(L'NRGCTGY-1),AL2(NRGCTGY-RECODES)                    
         DC    AL1(33),AL1(L'NRGCNTP-1),AL2(NRGCNTP-RECODES)                    
         DC    AL1(39),AL1(L'NRGMKT-1),AL2(NRGMKT-RECODES)                      
         DC    AL1(30),AL1(L'NRGRANK-1),AL2(NRGRANK-RECODES)                    
         DC    AL1(10),AL1(L'NRGAGY-1),AL2(NRGAGY-RECODES)                      
         DC    AL1(08),AL1(L'NRGADV-1),AL2(NRGADV-RECODES)                      
         DC    AL1(45),AL1(L'NRGDVTP-1),AL2(NRGDVTP-RECODES)                    
         DC    AL1(13),AL1(L'NRGCLASS-1),AL2(NRGCLASS-RECODES)                  
         DC    AL1(11),AL1(L'NRGAFFIL-1),AL2(NRGAFFIL-RECODES)                  
         DC    X'FF'                                                            
*                                                                               
         EJECT                                                                  
       ++INCLUDE REWRIWORKZ                                                     
         EJECT                                                                  
*              OTHER DSECTS ARE HIDDEN IN HERE                                  
         SPACE 3                                                                
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*REGENALL                                                                       
*REGENPWC                                                                       
*DRGLOBAL                                                                       
*FAFACTS                                                                        
*FATIOB                                                                         
*REWRIFFD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE REGENALL1                                                      
       ++INCLUDE REGENPWC                                                       
       ++INCLUDE DRGLOBAL                                                       
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FATIOB                                                         
       ++INCLUDE REWRIFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE REWRIF1XD                                                      
         SPACE                                                                  
READRECS DS    XL1                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE RENRGWRI                                                       
         SPACE                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'063REWRI01Z  05/01/02'                                      
         END                                                                    
