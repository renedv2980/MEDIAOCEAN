*          DATA SET ACREP5602S AT LEVEL 044 AS OF 05/17/00                      
*PHASE AC5602A,*                                                                
*INCLUDE POSTWRK                                                                
         TITLE 'ACCPAK CHECK REGISTER PROGRAM'                                  
AC5602   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*AC5602*,R9,R8                                                 
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING AC56D,RC                                                         
         CLI   MODE,RUNFRST                                                     
         BE    RNF00                                                            
         CLI   MODE,LEDGFRST                                                    
         BE    INT00                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* INITIALIZATION FOR RUN                                              *         
***********************************************************************         
                                                                                
RNF00    MVI   RNSW,0              INIT RUN CONTROL SWITCH                      
         CLC   4(4,R1),=C'AC55'    CALLED FROM CHECKS                           
         BNE   RNF03                                                            
         L     RF,8(R1)                                                         
         MVC   WKIDP,0(RF)         WORKER FILE POSTING ID                       
         MVC   AWRKBP,12(R1)       A(WORKER FILE BUFFER)                        
         OI    RNSW,CHECK          SET CHECK RUN                                
*                                                                               
RNF03    LA    RE,RELOTAB          RELOCATE A AND V TYPES                       
         LA    R1,ATYPES                                                        
*                                                                               
RNL05    L     RF,0(RE)                                                         
         ST    RF,0(R1)                                                         
         LA    R1,4(R1)                                                         
         LA    RE,4(RE)                                                         
         CLI   0(RE),X'FF'                                                      
         BNE   RNL05                                                            
*                                                                               
         LA    R2,COMLINS              CLEAR OUT COMMENT LINES PASSED           
         LA    R3,COMENTS              FROM JCL - WILL BE PRINTED ON            
RNL10    XC    0(L'COMLIN1,R2),0(R2)   REPORT                                   
         LA    R2,L'COMLIN1(R2)                                                 
         BCT   R3,RNL10                                                         
*                                                                               
         GOTO1 DATCON,DMCB,(4,RCDATE),(0,TODAY6)                                
         GOTO1 DATCON,DMCB,(0,TODAY6),(1,TODAY3)                                
         L     RF,ADMASTC                                                       
         USING MASTD,RF                                                         
         MVC   PQID,MCREMPQK                                                    
         CLI   MCRECOVR,C'W'                                                    
         BNE   *+8                                                              
         OI    RNSW,SOON                                                        
         OC    MCREMPQK,MCREMPQK   TEST SOON RUN                                
         BZ    *+8                                                              
         OI    RNSW,SOON           RUNNING SOON                                 
         CLI   MCTSTRUN,X'FF'      RUN=TEST                                     
         BNE   *+8                                                              
         OI    RNSW,RUNTST                                                      
         MVC   VSSB,MCSSB                                                       
         L     RF,ADBXAREA                                                      
         USING BOXD,RF                                                          
         MVC   BOXWIDTH,=F'132'                                                 
         BAS   RE,IDNM             GET ID DATA                                  
         B     XIT                                                              
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* INITIALIZATION FOR LEDGER                                           *         
***********************************************************************         
                                                                                
INT00    LA    R0,NPACUM                                                        
         LA    R1,PACUM                                                         
         ZAP   0(L'PACUM,R1),=P'0' CLEAR SOME ACCUMS                            
         LA    R1,L'PACUM(R1)                                                   
         BCT   R0,*-10                                                          
*                                                                               
         CLC   QSTART,SPACES                                                    
         BE    INT03                                                            
         MVC   TODAY6,QSTART                                                    
         GOTO1 DATCON,DMCB,(0,TODAY6),(1,TODAY3)                                
*                                                                               
INT03    OI    RNSW,FIRST          SET FIRST TIME SWITCH                        
         XC    FILNO,FILNO                                                      
         CLC   QSELECT,SPACES                                                   
         BE    INT07                                                            
         PACK  DUB,QSELECT(3)      KEEP FILE SEQUENCE NUMBER                    
         CVB   R1,DUB                                                           
         STCM  R1,3,FILNO          SAVE FILE SEQUENCE                           
*                                                                               
INT07    MVC   BANKACC,SPACES                                                   
         MVC   BANKCUL(1),RCCOMPFL                                              
         L     R4,ADCMPEL                                                       
         USING CPYELD,R4                                                        
         MVC   BANKCUL+1(2),CPYBANK  SAVE U/L OF CASH POSTINGS                  
*                                                                               
***********************************************************************         
*        MVC   DIR,ACCFIL          ASSUME ACCOUNT FILE                *         
*        L     RF,AMONACC                                             *         
*        USING ACMD,RF                                                *         
*        TM    ACMINDS,ACMIEMUD    IS FILE EMULATED                   *         
*        BZ    *+10                                                   *         
*        MVC   DIR,ACCDIR          SET FOR DIRECTORY                  *         
*        GOTO1 DATAMGR,DMCB,DMDTFA,DIR                                *         
*        L     RE,12(R1)           RE=A(ACCFIL DTF) OR A(ACCDIR DTF)  *         
*        L     RE,ISPDKEY-ISDTF(RE)                                   *         
*        MVC   MNACKEY,0(RE)       SAVE LAST MONACC KEY               *         
***********************************************************************         
                                                                                
         LA    RF,SYSTAB           FIND SYSTEM ENTRY                            
*                                                                               
INT09    CLC   QLEDGER,0(RF)       MATCH LEDGER TO SYSTEM TABLE                 
         BE    INT11                                                            
         LA    RF,L'SYSTAB(RF)                                                  
         CLI   0(RF),X'FF'                                                      
         BNE   INT09                                                            
         DC    H'0'                NO ENTRY FOR THIS LEDGER                     
*                                                                               
INT11    MVC   SYSENT,0(RF)        SAVE CURRENT SYSTEM ENTRY                    
         L     R4,ADLDGEL          GET THE LEDGER ELEMENT                       
         USING LDGELD,R4                                                        
         MVI   LGSW,0              LEDGER OPTIONS                               
         MVI   LSRSW,0             LASER OPTIONS                                
         LA    RF,ETXT1            US TEXT FIELDS                               
         TM    LDGSTAT,X'20'       TEST CANADIAN LEDGER                         
         BNO   *+12                                                             
         LA    RF,CTXT1            CANADIAN TEXT FIELDS                         
         OI    LGSW,LGCAN          SET CANADIAN OPTION                          
         MVC   TXT1(TXTLQ),0(RF)   MOVE TEXT FIELDS                             
         TM    LDGSTAT,X'08'       IS IT EXPENDITURE                            
         BNO   *+14                                                             
         MVC   SYSNME,=CL16'EXPENDITURE'                                        
         OI    LGSW,LGCKE          SET COKE OPTION                              
*                                                                               
         L     R4,ADLDGNAM         GET LEDGER NAME                              
         USING NAMELD,R4                                                        
         MVC   LDGNME,SPACES                                                    
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LDGNME(0),NAMEREC                                                
*                                                                               
         L     R4,ADLDGHIR         GET LENGTH OF LEVEL A                        
         USING ACLELD,R4                                                        
         MVC   LVALN,ACLVALS                                                    
*                                                                               
*MN      L     R4,ADLEDGER         IF X'10' CHECK AUTH RECORD PRESENT           
         BAS   RE,READCKAU         THEN USE INSTEAD OF LEDGER RECORD            
*MN      BNE   *+8                                                              
         L     R4,AIO2                                                          
         LA    R4,ACCORFST(R4)                                                  
         SR    R0,R0                                                            
         B     INT16                                                            
*                                                                               
INT15    IC    R0,1(R4)            BUMP TO NEXT CHECK ELEMENT                   
         AR    R4,R0                                                            
*                                                                               
         USING OCNELD,R4           NEW DSECT IS OCNELD                          
INT16    CLI   0(R4),0             EOF?                                         
         BE    ERR9                                                             
         CLI   0(R4),OCNELQ        X'54' CHECK ELEMENT                          
         BNE   INT15                                                            
         CLC   OCNOFFID,ORIGINUM   FIND ELEMENT FOR THIS ID                     
         BNE   INT15                                                            
         TM    OCNSTAT,OCNSPRTQ+OCNSDREG DIRECT OR DIRECT REGISTER              
         BZ    *+8                                                              
         OI    LGSW,LGDIR                                                       
         TM    OCNSTAT,OCNSSHUT    TEST SHUTTLE                                 
         BNO   *+8                                                              
         OI    LGSW,LGSHU                                                       
         CLI   1(R4),OCNLN2Q                                                    
         BL    INT16E                                                           
         TM    OCNSTAT2,OCNSMICR   SET MICRO PRINTING INDICATOR                 
         BZ    *+8                                                              
         OI    LGSW,LGMIC                                                       
         TM    OCNSTAT2,OCNSLBLT   SET MICRO FOR BOTTOM LINE                    
         BZ    *+8                                                              
         OI    LGSW,LGMIC+LGUPD                                                 
         CLI   OCNLASR,0           SET MICRO FOR LASER(XXX)                     
         BE    INT16C                                                           
         OI    LGSW,LGMIC+LGUPD                                                 
         OI    LSRSW,LSRCOLR       LASER CHECKS PRINTING AT DDS                 
INT16C   TM    OCNSTAT2,OCNSFTP    IF FILE TRANSFER                             
         BZ    *+8                                                              
         OI    LGSW,LGFTP                                                       
*                                                                               
INT16E   TM    RNSW,SOON           TEST SOON                                    
         BO    INT17                                                            
         MVI   TYPCHK,TYPDDS       DEFAULT IS DDS                               
         TM    OCNSTAT,OCNSLOCL    LOCAL                                        
         BZ    INT18                                                            
         MVI   TYPCHK,TYPLOCL      MAKE IT LOCAL                                
         TM    LGSW,LGMIC+LGFTP    TEST IF MICR OR FTP                          
         BNZ   INT18               YES, IT'S LIVE                               
         OI    RNSW,DRAFT          DRAFT                                        
         B     INT18                                                            
*                                                                               
INT17    MVC   TYPCHK,QOPT2        OPTION 2 IS TYPE - LOCAL OR SOON             
         CLI   QOPT1,C'D'          DRAFT                                        
         BNE   *+8                                                              
         OI    RNSW,DRAFT                                                       
*                                                                               
INT18    MVC   BANKACC,OCNBANK                                                  
         CLC   BANKCUL+1(2),=C'SC' BANK ACCOUNT                                 
         BE    INT19                                                            
         MVC   BANKCUL(1),RCCOMPFL                                              
         MVC   BANKCUL+1(2),=C'SC'                                              
         MVC   BANKCODE,OCNBANK                                                 
*                                                                               
INT19    MVC   CHRC,OCNNTYP        LEADING CHARACTER                            
         PACK  FIRSTR,OCNAFT       IF MICROPRINTING GET FIRST CHECK             
         TM    RNSW,SOON                                                        
         BNO   INT20                                                            
         TM    LGSW,LGMIC+LGFTP    NUMBER FROM AFTER NOT BEFORE                 
         BNZ   *+10                                                             
*                                                                               
INT20    PACK  FIRSTR,OCNBEF       SET FIRST FOR RUN(BEFORE)                    
         ZAP   FIRSTC,FIRSTR       FIRST CASH                                   
         LA    R3,=P'0'                                                         
         TM    LGSW,LGFTP          NO VOIDS FOR FTP                             
         BO    INT21                                                            
         TM    LGSW,LGMIC          NO VOIDS FOR MICR                            
         BO    INT21                                                            
         LA    R3,=P'3'            A FEW FOR THE LINEUP                         
         TM    LGSW,LGSHU          IS IT THE SHUTTLE                            
         BO    INT21               THEN 3 IS ENOUGH                             
         LA    R3,=P'5'                                                         
         CLI   TYPCHK,TYPDDS       FOR DDS SHUTTLE OR DIRECT                    
         BE    INT21               IGNORE PROFILE                               
         LA    R3,=P'0'            DEFAULT IS ZERO                              
         BAS   RE,CKP              GET THE CHECK PROFILE                        
         CLI   CKPROF+6,0                                                       
         BE    INT21                                                            
         SR    R3,R3                                                            
         IC    R3,CKPROF+6         USE A55 PROFILE VALUE                        
         CVD   R3,DUB                                                           
         LA    R3,DUB+7                                                         
*                                                                               
INT21    AP    FIRSTC,0(1,R3)                                                   
         PACK  LASTR,OCNAFT        GET NEXT STARTING NUMBER                     
         SP    LASTR,ONE           LAST FOR RUN                                 
         ZAP   LASTC,LASTR                                                      
         TM    LGSW,LGMIC+LGFTP                                                 
         BNZ   *+10                                                             
         SP    LASTC,ONE           LAST CASH                                    
         EJECT                                                                  
***********************************************************************         
* READ AND VALIDATE INPUT CARDS                                       *         
***********************************************************************         
                                                                                
VAL01    XC    NVOD,NVOD           NUMBER OF VOIDS                              
         XC    NSKP,NSKP           NUMBER OF SKIPS                              
         CLC   RCFFPARM(5),=C'CARDS' INPUT CARDS                                
         BNE   VAL15                                                            
*                                                                               
VAL03    GOTO1 CARDS,DMCB,CARDIO,=C'RE00'                                       
         CLC   CARDIO(2),=C'/*'                                                 
         BE    VAL15                                                            
*                                                                               
VAL09    LA    R2,OPTTAB           LIST OF VALID INPUT FIELDS                   
         SR    R1,R1                                                            
*                                                                               
VAL11    IC    R1,0(R2)            LENGTH FOR COMPARE                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   CARDIO(0),1(R2)     MATCH CARD FIELD TO TABLE                    
         BE    VAL13                                                            
         LA    R2,L'OPTTAB(R2)                                                  
         CLI   0(R2),X'FF'                                                      
         BNE   VAL11                                                            
         B     ERR2                INVALID INPUT OPTION                         
*                                                                               
VAL13    SR    RF,RF               GET VALIDATION ROUTINE                       
         ICM   RF,3,11(R2)                                                      
         AR    RF,RB                                                            
         BASR  RE,RF               VALIDATE INPUT OPTION                        
         B     VAL03               GET NEXT CARD                                
*                                                                               
VAL15    CLC   BANKCODE,SPACES     WAS A BANK ACCOUNT ENTERED                   
         BNE   VAL17               OK, USE INPUT BANK                           
         LA    R3,SYSBANK-22       USE DEFAULT BANK                             
         BAS   RE,VALBANK                                                       
*                                                                               
VAL17    TM    RNSW,ERRS           ANY ERRORS                                   
         BO    XIT                                                              
         B     REG00               OK TO PROCESS REGISTER                       
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE BANK=                                                  *         
***********************************************************************         
                                                                                
VALBANK  NTR1  ,                                                                
         MVC   BANKCODE,CARDIO+5    BANK CODE                                   
         MVC   REGKEY,SPACES                                                    
         MVC   REGKEY(L'BANKACC),BANKACC                                        
         BAS   RE,HIGH                                                          
         BNE   ERR3                 INVALID ACCOUNT                             
         B     XIT                                                              
         SPACE 2                                                                
***********************************************************************         
* VALIDATE FIRST=                                                     *         
***********************************************************************         
                                                                                
VALFRST  NTR1  ,                                                                
         BAS   RE,RPLCOM           REPLACE THE , WITH A -                       
         LA    R2,CARDIO+6         R2 = START OF NUMBER                         
         LA    R6,FIRSTR           R6 = FIRST FOR RUN                           
         LA    R7,FIRSTC           R7 = FIRST CASH                              
         BAS   RE,VALNUM           VALIDATE - SAVE NUMBERS                      
         BNE   ERR4                                                             
         CP    FIRSTR,FIRSTC       FIRST FOR RUN CAN'T BE HIGHER                
         BH    ERR4                THAN FIRST WITH CASH                         
         B     XIT                                                              
         SPACE 2                                                                
***********************************************************************         
* VALIDATE LAST=                                                      *         
***********************************************************************         
                                                                                
VALLAST  NTR1  ,                                                                
         BAS   RE,RPLCOM           REPLACE THE , WITH A -                       
         LA    R2,CARDIO+5         R2 = START OF NUMBER                         
         LA    R6,LASTR            R6 = LAST FOR RUN                            
         LA    R7,LASTC            R7 = LAST CASH                               
         BAS   RE,VALNUM           VALIDATE - SAVE NUMBERS                      
         BNE   ERR4                                                             
         TM    LGSW,LGMIC+LGFTP    MICRO CHECKS WON'T HAVE A POSTING            
         BZ    VALLAST5            FOR THE TRAILING VOID SO THESE               
         CP    LASTC,LASTR         NUMBERS CAN BE EQUAL                         
         BH    ERR4                                                             
         B     XIT                                                              
VALLAST5 CP    LASTC,LASTR         LAST CASH MUST BE                            
         BNL   ERR4                LESS THAN LAST FOR RUN                       
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE SKIP=                                                      *         
***********************************************************************         
                                                                                
VALSKIP  NTR1  ,                                                                
         MVC   SKIPCRD,CARDIO      SAVE SKIP CARD                               
         LA    R2,CARDIO+5         R2 = START OF NUMBERS                        
VALSKIP3 LA    R6,SKIP1            R6 = SKIP FROM                               
         LA    R7,SKIP2            R7 = SKIP TO                                 
         BAS   RE,VALNUM           VALIDATE - SAVE NUMBERS                      
         BNE   ERR4                                                             
         CP    SKIP1,SKIP2         SKIP "FROM" CAN'T BE                         
         BH    ERR4                GT SKIP "TO"                                 
         L     R4,ASTAB            R2 = SKIP TABLE                              
         L     R3,NSKP             R3 = NUMBER OF SKIPS                         
         MH    R3,=Y(L'STAB*2)                                                  
         AR    R4,R3               R2 = NEXT AVAILABLE ENTRY                    
         LA    R0,MXSKIP           MAXIMUM NUMBER OF SKIPS                      
         L     R3,NSKP             R3 = NUMBER OF SKIPS                         
*                                                                               
         ZAP   0(L'STAB,R4),SKIP1  SKIP TO TABLE                                
         ZAP   L'STAB(L'STAB,R4),SKIP2                                          
         AH    R3,=H'1'            COUNT NUMBER OF VOIDS                        
         ST    R3,NSKP                                                          
         CR    R3,R0                                                            
         BH    ERR5                                                             
         CLI   0(R2),C','          ANOTHER SET                                  
         BNE   XIT                                                              
         LA    R2,1(R2)            R2 TO NEXT NUMERIC                           
         B     VALSKIP3                                                         
*                                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE VOID=                                                      *         
***********************************************************************         
                                                                                
VALVOID  NTR1  ,                                                                
         LA    R2,CARDIO+5         R2 = START OF NUMBERS                        
VALVOID3 LA    R6,VOID1            R6 = START VOID                              
         LA    R7,VOID2            R7 = END VOID                                
         BAS   RE,VALNUM           VALIDATE - SAVE NUMBERS                      
         BNE   ERR4                                                             
         CP    VOID2,VOID1         END VOID CAN'T BE LT START                   
         BL    ERR4                                                             
         L     R4,AVTAB            R2 = VOID TABLE                              
         L     R3,NVOD             R3 = NUMBER OF VOIDS                         
         MH    R3,=Y(L'VTAB)                                                    
         AR    R4,R3               R2 = NEXT AVAILABLE ENTRY                    
         LA    R0,MXVOID           MAXIMUM NUMBER OF VOIDS                      
         L     R3,NVOD             R3 = NUMBER OF VOIDS                         
*                                                                               
VALVOID5 ZAP   0(L'VTAB,R4),VOID1  VOID NUMBER TO TABLE                         
         AH    R3,=H'1'            COUNT NUMBER OF VOIDS                        
         ST    R3,NVOD                                                          
         CR    R3,R0                                                            
         BH    ERR5                TOO MANY VOIDS                               
         AP    VOID1,ONE                                                        
         LA    R4,L'VTAB(R4)                                                    
         CP    VOID1,VOID2         HAVE ALL BEEN ADDED                          
         BNH   VALVOID5                                                         
         CLI   0(R2),C','          ANOTHER SET                                  
         BNE   XIT                                                              
         LA    R2,1(R2)                                                         
         B     VALVOID3                                                         
         EJECT                                                                  
***********************************************************************         
* VALIDATE COMMENT=                                                   *         
***********************************************************************         
                                                                                
VALCOMM  NTR1  ,                                                                
         LA    R2,COMLINS                                                       
         LA    R3,COMENTS                                                       
VALCOM3  CLI   0(R2),C' '          IS THERE A SKIP CARD                         
         BNH   VALCOM5                                                          
         LA    R2,L'COMLIN1(R2)                                                 
         BCT   R3,VALCOM3          ALLOW MAX OF 3 COMMENT CARDS -               
         B     XIT                 SKIP THE REST TO AVOID ERROR                 
VALCOM5  MVC   0(L'COMLIN1,R2),CARDIO+8                                         
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE CHECK NUMBERS                                              *         
*   R2 = A(START OF SCAN)                                             *         
*   R6 = A(START NUMBER)                                              *         
*   R7 = A(END NUMBER)                                                *         
***********************************************************************         
                                                                                
VALNUM   LR    R5,R6               SAVE THE START                               
*                                                                               
VALNUM1  SR    R1,R1                                                            
         LR    RF,R2               R2/RF = START OF SCAN                        
*                                                                               
VALNUM3  CLI   0(R2),C' '          BLANK                                        
         BE    VALNUM5                                                          
         CLI   0(R2),C','          OR COMMA IS END OF SCAN                      
         BE    VALNUM5                                                          
         CLI   0(R2),C'-'          FOUND THE DASH                               
         BE    VALNUM5                                                          
         CLI   0(R2),C'0'          TEST NUMMERIC                                
         BL    VALNO                                                            
         CLI   0(R2),C'9'                                                       
         BH    VALNO                                                            
         LA    R1,1(R1)            COUNT NUMBER OF NUMERICS                     
         CH    R1,=H'6'            CAN'T BE MORE THAN SIX CHARACTERS            
         BH    VALNO                                                            
         LA    R2,1(R2)            SCAN NEXT CHARACTER                          
         B     VALNUM3                                                          
*                                                                               
VALNUM5  LTR   R1,R1               TEST ANY NUMERICS                            
         BZ    VALNO                                                            
         BCTR  R1,0                LENGTH OF DATA                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  0(6,R5),0(0,RF)                                                  
         CR    R5,R7               WAS THAT THE SECOND NUMBER                   
         BE    VALYES              ALL DONE                                     
         ZAP   0(6,R7),0(6,R5)     SECOND = FIRST                               
         CLI   0(R2),C' '          BLANK                                        
         BE    VALYES              ALL DONE                                     
         CLI   0(R2),C','          OR COMMA IS END OF SCAN                      
         BE    VALYES                                                           
         LR    R5,R7               NOW GET SECOND                               
         LA    R2,1(R2)            R2 PAST THE -                                
         B     VALNUM1                                                          
*                                                                               
VALNO    LTR   RB,RB                                                            
         BR    RE                                                               
VALYES   CR    RB,RB                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* REPLACE THE COMMA "," WITH A DASH "-"   ON SOME CARDS               *         
***********************************************************************         
                                                                                
RPLCOM   LA    R0,L'CARDIO-1       CHANGE NNN,NNN TO NNN-NNN                    
         LA    R1,CARDIO                                                        
*                                                                               
RPLCOM3  CLI   0(R1),C','          FIND A COMMA                                 
         BNE   *+16                                                             
         TM    1(R1),X'F0'         FOLLOWED BY NUMERIC                          
         BNO   *+8                                                              
         MVI   0(R1),C'-'          CHANGE THE COMMA TO A DASH                   
         LA    R1,1(R1)                                                         
         BCT   R0,RPLCOM3                                                       
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* READ SKELETON FILE & CREATE POSTING FILE                            *         
***********************************************************************         
                                                                                
REG00    L     RF,REMOTEC          SET UP REPORT TITLE                          
         USING REMOTED,RF                                                       
         OC    REMOTKEY,REMOTKEY   TEST OUTPUT IS REMOTE                        
         BZ    *+10                                                             
         MVC   REMOTKEY+4(1),QLEDGER  AC56X                                     
*                                                                               
         TM    LSRSW,LSRCOLR       NO NEED TO CHECK FOR DUP POSTING             
         BZ    REG01               FILE IF NOT DDS LASER CHECKS                 
         CLI   RCPOSTNG,C'N'                                                    
         BE    REG01                                                            
         BAS   RE,CKDUPWK                                                       
         TM    RNSW,ERRS                                                        
         BO    XIT                                                              
*                                                                               
REG01    MVI   RCSUBPRG,0                                                       
         GOTO1 ADSORTER,DMCB,SORTCARD,RECCARD                                   
         OI    RNSW,SORT           TURN ON SORT OPENED                          
         L     R5,ACHKF            CHECK DETAIL FILE                            
         OPEN  ((R5),OUTPUT)                                                    
         L     R4,ATRNF            TRANSACTION FILE                             
         OPEN  ((R4),OUTPUT)                                                    
         ZAP   CURRENT,FIRSTR                                                   
         L     RE,AWRKB            R3 = WORK FILE BUFFER                        
         LH    RF,=Y(L'WRKB)       R4 = LENGTH OF BUFFER                        
         XCEFL (RE),(RF)           CLEAR IT                                     
         XC    WKID,WKID           BUILD INPUT WKFILE KEY                       
         LA    R7,WKID                                                          
         USING UKRECD,R7                                                        
         MVC   UKUSRID,ORIGINUM                                                 
         MVC   UKSYSPRG,=C'A55'                                                 
         MVC   UKSUBPRG,QLEDGER    LEDGER TO SUB-USER KEY FIELD                 
         MVC   UKDAY,TODAY3+2      SET DATE IN ID'S                             
         MVC   UKCLASS,TYPCHK      CAN BE S(OON) OR L(OCAL)  OR C(DDS)          
         BAS   RE,WRKINDX          GET INDEX RECORD                             
         TM    RNSW,ERRS                                                        
         BO    XIT                 EXIT IF ERROR                                
         LA    R6,VTRNL            GET AND SAVE FIRST VOID                      
         B     *+8                                                              
*                                                                               
REG03    LA    R6,RTRNL                                                         
REG04    BAS   RE,WRKREAD          READ THE CHECK FILE                          
         TM    RNSW,ERRS           ANY ERRORS                                   
         BO    XIT                                                              
         LA    R2,L'VTRNL(R6)      R2 = A(FIRST ELEMENT)                        
         CLI   0(R2),CRDELQ        CHECK REGISTER DETAIL ELEMENT                
         BE    REG04               SKIP RECORD                                  
         TM    RNSW,FIRST          IS IT FIRST TIME                             
         BNO   REG07                                                            
         NI    RNSW,ALL-FIRST                                                   
         ZAP   DUB,FIRSTC          VOID CHECKS AT START OF RUN                  
         SP    DUB,FIRSTR          GET THE VOIDS AT START                       
         BZ    REG05                                                            
         CVB   R3,DUB                                                           
         BAS   RE,FILL             ADD BANK & CHECK NUMBER                      
         BCT   R3,*-4                                                           
*                                                                               
REG05    LA    R6,RTRNL            NOW READ FOR FIRST NON-VOID                  
         BAS   RE,WRKREAD                                                       
         TM    RNSW,ERRS           ANY ERRORS                                   
         BO    XIT                                                              
         BAS   RE,GETL             R2=POSTING ELEMENT, R3=TRANSACTION           
         USING PSHEADD,R2                                                       
         CLI   0(R2),X'52'         TRAILER RECORD                               
         BE    REG10               MUST BE A ZERO RUN                           
         USING TRNELD,R3                                                        
         CLC   TRNNARR(L'VNARR),VNARR IS IT A VOID                              
         BE    REG05                                                            
*                                                                               
REG07    LA    R2,RTRN                                                          
         CLI   PSHDEL,X'50'        TEST POSTING HEADER                          
         BNE   REG10                                                            
         CLC   PSHDACC+1(4),=C'BANK'                                            
         BNE   REG09                                                            
         BAS   RE,FILL             CHECK DETAILS                                
         B     REG03                                                            
*                                                                               
REG09    CLC   PSHDACC+1(2),QUNIT  TEST VENDOR POSTING                          
         BNE   *+8                                                              
         BAS   RE,SUM              ADD SUMMARY RECORDS                          
         PUT   (R4),(R6)                                                        
         AP    WRKREC,ONE          COUNT WORKER FILE RECORDS                    
         B     REG03                                                            
*                                                                               
         USING PSSUBFD,R2          AND WRITE OUT TO DISK                        
REG10    CLI   PSSBEL,X'52'        TEST TRAILER ELEMENT                         
         BE    *+6                                                              
         DC    H'0'                ONLY ALLOW 50 & 52 RECORDS                   
         ZAP   TOTPOST,PSSBCASH                                                 
         CP    TOTPOST,=P'0'                                                    
         BNE   *+8                                                              
         OI    RNSW,ZERO           SET ZERO RUN                                 
         ZAP   DUB,LASTR                                                        
         SP    DUB,LASTC           HANDLE EXTRA VOIDS AT END                    
         TM    LGSW,LGMIC+LGFTP    IF MICRO CHECKS WILL NOT BE A                
         BNZ   *+10                TRAILING VOID ON POSTING FILE                
         SP    DUB,ONE             ALEADY HANDLED THE TOTAL PAGE                
         CVB   R3,DUB                                                           
         LTR   R3,R3                                                            
         BZ    REG11                                                            
         LA    R6,VTRNL            WRITE ENDING VOIDS                           
         BAS   RE,FILL             ADD BANK & CHECK NUMBER                      
         BCT   R3,*-4                                                           
*                                                                               
REG11    LA    R6,RTRNL            RESTORE TRAILER                              
         ZAP   PSSBRECS,WRKREC     POSTINGS RECORDS                             
         PUT   (R4),(R6)                                                        
         CLOSE ((R5))                                                           
         CLOSE ((R4))                                                           
         SP    CURRENT,ONE         TEST FOR FORM NUMBERING ERRORS               
         TM    LGSW,LGUPD          IMMEDIATE UPDATE                             
         BO    REG12                                                            
         TM    LGSW,LGMIC+LGFTP    IF MICRO PRINTING CANNOT GET LAST            
         BZ    REG13               CHECK NUMBER FROM LEDGER REC, MUST           
         TM    RNSW,SOON                                                        
         BZ    REG13                                                            
*                                                                               
REG12    ZAP   LASTR,CURRENT       COUNT ON AND GET IT FROM CURRENT             
*                                                                               
REG13    TM    RNSW,DRAFT          TEST DRAFT RUN                               
         BO    REG15                                                            
         CP    CURRENT,LASTR                                                    
         BNE   ERR6                                                             
         CLI   TYPCHK,TYPDDS       TEST DDS CHECK RUN                           
         BE    REG15               DON'T MARKE WRKFILE KEEP                     
         L     RE,AWRKB            R3 = WORK FILE BUFFER                        
         LH    RF,=Y(L'WRKB)       R4 = LENGTH OF BUFFER                        
         XCEFL (RE),(RF)           CLEAR IT                                     
         LA    R7,WKID                                                          
         USING UKRECD,R7                                                        
         XC    UKINDEX+L'UKKEY(L'UKINDEX-L'UKKEY),UKINDEX+L'UKKEY               
         BAS   RE,WRKINDX          GET INDEX RECORD                             
         OI    UKFLAG,X'10'        SET RETENTION                                
         XC    RTRNL(96),RTRNL                                                  
         LA    R6,RTRNL                                                         
         LA    RF,RTRNL+28                                                      
         USING WKRECD,RF                                                        
         MVC   WKRETN,=H'7'        CHANGE RETENTION TO 7                        
         BAS   RE,WRKRTN                                                        
         BAS   RE,WRKKEEP          MAKE INPUT FILE KEEP                         
         EJECT                                                                  
***********************************************************************         
* PRINT TOTALS AND END RUN                                            *         
***********************************************************************         
                                                                                
REG15    BAS   RE,RQT              PRINT TOTAL FOR LAST REQUEST                 
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,1                                                       
         TM    LGSW,LGCKE          TEST COKE                                    
         BO    *+8                                                              
         BAS   RE,MES              PRINT MEDIA SUMMARY                          
*                                                                               
         MVC   P+2(L'TXT6),TXT6    TOTAL CASH                                   
         EDIT  (P6,TOTCASH),(15,P+36),2,COMMAS=YES,MINUS=YES,FLOAT=$            
         MVI   SPACING,2                                                        
         BAS   RE,PRT                                                           
         MVC   P+2(L'TXT1),TXT1     GOOD CHECKS                                 
         LA    R2,CHOKS                                                         
         MVI   SPACING,2                                                        
         BAS   RE,EDIT                                                          
         MVC   P+2(L'TXT2),TXT2    VOID CHECKS                                  
         LA    R2,VOIDS                                                         
         MVI   SPACING,2                                                        
         BAS   RE,EDIT                                                          
         MVC   P+2(L'TXT3),TXT3    TOTAL CHECKS                                 
         LA    R2,CHUSED                                                        
         MVI   SPACING,2                                                        
         BAS   RE,EDIT                                                          
         CLI   SKIPCRD,C' '        IS THERE A SKIP CARD                         
         BNH   REG17                                                            
         MVC   P+2(L'SKIPCRD),SKIPCRD                                           
         MVI   SPACING,2                                                        
         BAS   RE,PRT                                                           
*                                                                               
REG17    BAS   RE,OFS              OFFICE SUMMARY                               
         BAS   RE,MOS              MEDIA/OFFICE SUMMARY                         
         CLI   PROGPROF+1,C'Y'                                                  
         BNE   *+8                                                              
         BAS   RE,PSCR             PRINT THE SORTED REPORT                      
         BAS   RE,PCOMM            PRINT JCL COMMENTS                           
         GOTO1 ADSORTER,DMCB,=C'END'                                            
         BAS   RE,POST             FILL IN CHECK DETAIL AND POST                
         EJECT                                                                  
***********************************************************************         
* REMOVE PENDING STATUS AND UNLOCK LEDGER                             *         
***********************************************************************         
                                                                                
         CLI   TYPCHK,TYPDDS       IS IT REGULAR DDS CHECK RUN                  
         BE    REG23                                                            
         TM    RNSW,DRAFT          OR IF DRAFT                                  
         BO    REG23                                                            
         MVC   REGKEY,SPACES       BUILD LEDGER KEY                             
         MVC   REGKEY(1),RCCOMPFL  COMPANY                                      
         MVC   REGKEY+1(2),QUNIT   UNIT/LEDGER                                  
         BAS   RE,READ                                                          
         L     R3,AIO                                                           
         USING ACTRECD,R3                                                       
         LA    R4,ACCORFST(R3)                                                  
         SR    R0,R0                                                            
*                                                                               
REG19    IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0             FIND OFFICE CHECK ELEMENT                    
         BE    REG20                                                            
         CLI   0(R4),X'EA'         LOCK ELEMENT                                 
         BNE   REG19                                                            
         TM    RNSW,SOON           TEST SOON RUN                                
         BZ    REG20                                                            
         USING LGLELD,R4                                                        
         XC    LGLDATE,LGLDATE     CLEAR LOCKED STATUS                          
         NI    LGLSTAT,X'FF'-LGLSLOCK                                           
*                                                                               
REG20    DS    0H                                                               
*MN      L     R4,AIO                                                           
         BAS   RE,READCKAU         CHECK AUTHORIZATION RECORD                   
*MN      BNE   *+8                                                              
         L     R4,AIO2                                                          
         LA    R4,ACCORFST(R4)                                                  
         B     REG20C                                                           
*                                                                               
REG20A   IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
*                                                                               
         USING OCNELD,R4                                                        
REG20C   CLI   0(R4),0             EOF?                                         
         BE    REG22               YES                                          
         CLI   0(R4),OCNELQ        X'54'-CHECK ELEMENT                          
         BNE   REG20A                                                           
         CLC   OCNOFFID,ORIGINUM   ELEMENT FOR THIS ID                          
         BNE   REG20A                                                           
         LA    R1,OCNDPSR          SOON                                         
         CLI   TYPCHK,TYPSOON                                                   
         BE    *+8                                                              
         LA    R1,OCNDPLR          LOCAL REGISTER                               
         XC    0(L'OCNDPSR,R1),0(R1) CLEAR PENDING DATE                         
         UNPK  OCNBEF,FIRSTR       STARTING NUMBER                              
         OI    OCNBEF+5,X'F0'                                                   
         AP    LASTR,ONE                                                        
         UNPK  OCNAFT,LASTR        NEXT NUMBER                                  
         OI    OCNAFT+5,X'F0'                                                   
         B     REG20A                                                           
*                                                                               
REG22    TM    LGSW,LGMIC          IF MICRO HAVE WRITTEN ALL RECORDS            
         BO    REG23               BACK IN CHECK(AC55) RUN                      
         TM    LGSW,LGFTP          DITTO FTP                                    
         BO    REG23                                                            
         L     R2,AIO2                                                          
*MN      TM    LGSW,LGCKA                                                       
*MN      BZ    *+8                                                              
         BAS   RE,WRITE            WRITE CHECK AUTH REC                         
         L     R2,AIO                                                           
         BAS   RE,WRITE            WRITE BACK LEDGER RECORD                     
*                                                                               
*EG23    MVC   REGKEY,MNACKEY      READ LAST MONACC RECORD                      
*        BAS   RE,READ                                                          
REG23    TM    RNSW,DRAFT+ZERO     TEST ZERO DRAFT                              
         BNO   *+8                                                              
         BAS   RE,KEEPIN           PUT INPUT FILE ON KEEP                       
*                                                                               
         TM    RNSW,SOON           TEST SOON                                    
         BO    XIT                                                              
         TM    LGSW,LGUPD          TEST IMMEDIATE UPDATE                        
         BNO   XIT                                                              
*                                                                               
         CLI   RCPOSTNG,C'N'                                                    
         BE    XIT                                                              
*                                                                               
         L     R6,LOGOC                                                         
         USING LOGOD,R6                                                         
         MVI   LOGOTYPE,C'E'       END LAST LOGO                                
         GOTO1 LOGO,DMCB,(R6)                                                   
         GOTO1 PRINT,DMCB,=C'CLOSE'                                             
         GOTO1 =V(POSTWRK),DMCB,(RA),(C'R',WKID)    UPDATE THE FILE             
         B     XIT                 THE END                                      
         EJECT                                                                  
***********************************************************************         
* FILL IN CHECK NUMBER & BANK ACCOUNT                                 *         
***********************************************************************         
                                                                                
FILL     NTR1  ,                                                                
         LR    R7,R6               R7 = INPUT TRANSACTION                       
         LA    RF,VTRNL            R4 = VOID TRANSACTION                        
         CR    R7,RF               PROCESSING START/END VOID                    
         BE    FILL3                                                            
*                                                                               
FILL1    MVI   VOIDSW,C'N'         TEST CURRENT NUMBER IS VOID                  
         BAS   RE,VSEARCH                                                       
         CLI   VOIDSW,C'Y'         DID WE MATCH CURRENT NO WITH A VOID          
         BNE   FILL3               NO-ALL IS OK                                 
         LA    R6,VTRNL            WRITE OUT A VOID RECORD                      
         LA    R2,VTRN                                                          
*                                                                               
FILL3    BAS   RE,GETL             R2=POSTING ELEMENT, R3=TRANSACTION           
         USING PSHEADD,R2                                                       
         USING TRNELD,R3                                                        
         MVC   PSHDACC,BANKACC     SET BANK ACCOUNT                             
         EDIT  CURRENT,(6,TRNREF),FILL=0                                        
         CLI   CHRC,0                                                           
         BE    *+10                                                             
         MVC   TRNREF(1),CHRC                                                   
         AP    CURRENT,ONE                                                      
         AP    CHUSED,ONE                                                       
         AP    WRKREC,ONE                                                       
         PUT   (R4),(R6)                                                        
         BAS   RE,FRMT                                                          
         LA    RF,VTRNL                                                         
         CR    RF,R6               IS CURRENT TRANSACTION A VOID                
         BNE   FILL4               WRITE CHECK RECORD                           
         CR    R7,R6               IS IT A START/END VOID                       
         BE    FILL4               WRITE CHECK RECORD                           
         AP    VOIDS,ONE           COUNT VOIDS IN THE MIDDLE                    
         LR    R6,R7               RESTORE THE ORGINAL TRANSACTION              
         BAS   RE,CKSKP            CHECK FOR SKIPPED NUMBRS                     
         B     FILL1               AND TRY AGAIN                                
*                                                                               
FILL4    MVC   CHKACC,PSHDSBAC     VENDOR NO.                                   
         MVC   CHKNO,TRNREF        CHECK NO.                                    
         MVC   CHKDTE,SPACES                                                    
         GOTO1 DATCON,DMCB,(1,TRNDATE),(8,CHKDTE)                               
         GOTO1 (RF),(R1),(1,TRNDATE),(2,CHKBDT)                                 
         MVC   CHKBNK,BANKACC      BANK NO.                                     
         ZAP   DUB1,TRNAMNT                                                     
         SR    R0,R0                                                            
         LR    RF,R3                                                            
*                                                                               
FILL6    IC    R0,1(RF)                                                         
         AR    RF,R0                                                            
         CLI   0(RF),0                                                          
         BE    FILL9                                                            
         CLI   0(RF),X'50'         USE CHECK TOTAL AMOUNT                       
         BNE   FILL6               IF PRESENT (ADDED BY AC55)                   
         USING SCIELD,RF                                                        
         CLI   SCITYPE,C'T'                                                     
         BNE   FILL6                                                            
         ZAP   DUB1,SCIAMNT                                                     
*                                                                               
FILL9    ZAP   CHKAMT,DUB1                                                      
         EDIT  (P8,DUB1),(12,CHKTOT),2,COMMAS=YES,MINUS=YES                     
         LA    R6,CHKREC           WRITE CHECK DETAIL FILE                      
         PUT   (R5),(R6)                                                        
*                                                                               
         BAS   RE,CKSKP                                                         
*                                                                               
         LA    RF,VOIDS            ADD TO AC5502 VOIDS                          
         CLC   TRNNARR(L'VNARR),VNARR TEST VOID TRANSACTION                     
         BE    *+8                                                              
         LA    RF,CHOKS            OR ADD TO OK CHECKS                          
         AP    0(L'VOIDS,RF),ONE                                                
         B     XIT                                                              
*                                                                               
CKSKP    LR    R0,RE               CHECK FOR SKIPPED NUMBERS                    
CKSKP1   L     RE,ASTAB            RE = SKIP TABLE                              
         ICM   RF,15,NSKP          RF = NUMBER OF SKIPS                         
         BZ    CKSKPX                                                           
CKSKP3   CP    CURRENT,0(L'STAB,RE) HAVE WE REACHED A SKIP                      
         BNE   CKSKP5                                                           
         ZAP   CURRENT,L'STAB(L'STAB,RE) SET NEW NUMBER                         
         AP    CURRENT,ONE                                                      
         B     CKSKP1                                                           
*                                                                               
CKSKP5   LA    RE,L'STAB*2(RE)                                                  
         BCT   RF,CKSKP3                                                        
CKSKPX   LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
* PRINT A LINE OF REGISTER                                            *         
***********************************************************************         
                                                                                
FRMT     NTR1  ,                                                                
         BAS   RE,GETL             R2=POSTING ELEMENT, R3=TRANSACTION           
         USING PSHEADD,R2                                                       
         USING TRNELD,R3                                                        
*                                                                               
FRM3     CLC   TRNNARR(L'VNARR),VNARR TEST VOID                                 
         BE    FRM7                                                             
         CLC   REQNO,TRNNARR       SAME REQUEST                                 
         BE    FRM7                                                             
         BAS   RE,RQT              SETUP REQUEST TOTAL                          
*                                                                               
FRM7     ZAP   DUB1,TRNAMNT        SAVE POSTING AMOUNT                          
         SR    R0,R0                                                            
         LR    RF,R3                                                            
*                                                                               
FRM9     IC    R0,1(RF)            USE CHECK TOTAL - IF PRESENT                 
         AR    RF,R0                                                            
         CLI   0(RF),0                                                          
         BE    FRM13                                                            
         CLI   0(RF),X'50'                                                      
         BNE   FRM9                                                             
         USING SCIELD,RF                                                        
         CLI   SCITYPE,C'T'                                                     
         BNE   FRM9                                                             
         ZAP   DUB1,SCIAMNT                                                     
*                                                                               
FRM13    AP    REQTOT,DUB1                                                      
         AP    TOTCASH,DUB1                                                     
         MVC   P+2(6),TRNREF                                                    
         EDIT  (P8,DUB1),(15,P+11),2,COMMAS=YES,FLOAT=$,MINUS=YES               
         MVC   P+33(12),PSHDSBAC+3                                              
         MVC   P+54(36),PSHDSBNM                                                
         CP    TRNAMNT,=P'0'                                                    
         BE    *+8                                                              
         BAS   RE,ADR              GET PAYEES ADDRESS                           
         MVI   SPACING,2                                                        
         BAS   RE,PRT              AND PRINT THE CHECK INFO                     
*                                                                               
         MVC   SRREF,TRNREF        ALSO, WRITE A RECORD TO SORT                 
         MVC   SRACC,PSHDSBAC                                                   
         MVC   SRAMT,DUB1                                                       
         MVC   SRNME,PSHDSBNM                                                   
         GOTO1 ADSORTER,DMCB,=C'PUT',SREC                                       
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PRINT REQUEST TOTAL                                                 *         
***********************************************************************         
                                                                                
RQT      NTR1  ,                                                                
         CLC   TRNNARR(L'FNARR),FNARR  FIRST TIME                               
         BE    RQT3                                                             
         BAS   RE,PRT              SKIP A LINE                                  
         MVC   P+1(L'TXT7),TXT7    TOTAL FOR REQUEST                            
         LA    R4,P+2+L'TXT7                                                    
         LA    R1,L'REQNO-1        GET REQUEST NUMBER                           
         LA    RF,REQNO                                                         
         CLI   0(RF),C' '          FIND FIRST CHARACTER                         
         BNE   *+12                                                             
         LA    RF,1(RF)                                                         
         BCT   R1,*-12                                                          
         EX    R1,*+4                                                           
         MVC   0(0,R4),0(RF)        REQUEST NUMBER                              
         LA    R4,2(R1,R4)                                                      
         MVC   0(L'TXT8,R4),TXT8    DATED                                       
         LA    R4,1+L'TXT8(R4)                                                  
         GOTO1 DATCON,DMCB,(1,REQDAT),(8,0(R4))                                 
         LA    R4,9(R4)                                                         
         MVC   0(L'TXT9,R4),TXT9    IS                                          
         LA    R4,1+L'TXT9(R4)                                                  
         EDIT  (P6,REQTOT),(12,0(R4)),2,MINUS=YES                               
         GOTO1 ADSQUASH,DMCB,P+1,60                                             
         BAS   RE,PRT                                                           
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
RQT3     MVC   REQNO,TRNNARR       SAVE NEXT NUMBER                             
         MVC   REQDAT,TRNDATE      DATE OF CHECK                                
         ZAP   REQTOT,=P'0'        INITIALIZE REQUEST TOTAL                     
         B     XIT                 SETUP FOR NEXT REQUEST                       
         EJECT                                                                  
***********************************************************************         
* FILL IN CHECK DETAILS & WRITE FROM DISK TO POST FILE                *         
***********************************************************************         
                                                                                
POST     NTR1  ,                                                                
         XC    CHKREC(CHKLNQ),CHKREC                                            
         TM    RNSW,CHECK          TEST CALLED FROM CHECK RUN                   
         BO    POST01                                                           
         BAS   RE,OPNWK            OPEN WORKER FILE                             
         B     POST02                                                           
*                                                                               
POST01   MVC   WKID,WKIDP          REPLACE WORKER ID                            
         MVC   AWRKB,AWRKBP        AND A(BUFFER)                                
*                                                                               
POST02   L     R5,ACHKF            OPEN THE CHECK NUMBER FILE                   
         OPEN  ((R5),(INPUT))                                                   
         L     R4,ATRNF            OPEN POSTING DETAILS                         
         OPEN  ((R4),(INPUT))                                                   
         LA    R6,RTRNL            R6 = RECORD LENGTH                           
         LA    R2,RTRN             R2 = FIRST ELEMENT                           
*                                                                               
POST03   GET   (R4),(R6)           GET A POSTING                                
         USING PSHEADD,R2                                                       
         CLI   PSHDEL,X'50'        TEST POSTING HEADER                          
         BNE   POST21                                                           
         LR    R3,R2                                                            
         SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0               FIRST ELEMENT MUST BE TRANSACTION            
         USING TRNELD,R3                                                        
         CLI   0(R3),X'44'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   PSHDACC+1(2),QUNIT  IF ITS A VENDOR POSTING WE HAVE TO           
         BNE   POST13              FILL IN SOME DETAILS                         
         CLI   TRNLN,X'20'         SHORT NARRATIVE MEANS REVERSAL               
         BL    POST21              POSTING-SO DO NOT FILL IN DETAILS            
*                                                                               
POST05   CLC   PSHDACC,CHKACC      SAME VENDOR ON WORK FILE& TEMP. FILE         
         BE    POST07              YES - GO & FILL IN DETAILS                   
         LA    R7,CHKREC                                                        
         GET   (R5),(R7)           READ WORK FILE                               
         B     POST05                                                           
*                                                                               
POST07   MVC   TRNNARR(41),CHKNO  FILL IN NARRATIVE WITH NO. ETC.               
         LA    R1,TRNELD                                                        
         SR    R0,R0                                                            
*                                                                               
         USING MPYELD,R1                                                        
POST09   IC    R0,MPYLN            SEARCH FOR X'64' ELEM                        
         AR    R1,R0                                                            
         CLI   MPYEL,0             TEST EOR                                     
         BE    POST21                                                           
         CLI   MPYEL,MPYELQ        TEST MANUAL PAYMENT ELEMENT                  
         BNE   POST09                                                           
         MVC   MPYNO,CHKNO         UPDATE CHECK DETAILS                         
         MVC   MPYDTE,CHKBDT                                                    
         ZAP   MPYAMNT,CHKAMT                                                   
         MVC   MPYBNK,CHKBNK+1                                                  
         B     POST21                                                           
*                                                                               
POST13   CLC   PSHDACC,BANKACC                                                  
         BNE   POST21              IF WE HAVE JUST READ A BANK POSTING          
         CLC   PSHDSBAC,VNARR      AND NOT A VOID                               
         BNE   POST15                                                           
         OI    TRNSTAT,X'02'       MARK VOIDS AS RECONCILED                     
         B     POST21                                                           
*                                                                               
POST15   MVC   TRNNARR(6),SPACES   CLEAR REQUEST NUMBER                         
         LA    R7,CHKREC                                                        
         GET   (R5),(R7)           READ WORK FILE                               
*                                                                               
POST21   BAS   RE,WRKADD           ADD TO WORKER POSTING FILE                   
         CLI   PSHDEL,X'52'        TEST TRAILER ELEMENT                         
         BNE   POST03                                                           
POST22   CLOSE ((R4))                                                           
         CLOSE ((R5))                                                           
         BAS   RE,WRKCLSE                                                       
         TM    RNSW,SOON                                                        
         BZ    XIT                                                              
         BAS   RE,WRKKEEP          REGISTER ON KEEP                             
         B     XIT                                                              
*                                                                               
POST23   TM    LGSW,LGMIC+LGFTP    EOF ON CHECK FILE                            
         BNZ   POST21                                                           
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* SEARCH VOID TABLE FOR A MATCH WITH CURRENT CHECK NO                 *         
***********************************************************************         
                                                                                
VSEARCH  NTR1                                                                   
         L     R4,AVTAB                                                         
         ICM   R0,15,NVOD          NUMBER IN TABLE                              
         BZ    XIT                 NO  VOIDS                                    
*                                                                               
VSRCH3   CP    0(4,R4),CURRENT     IS THE CURRENT NUMBER A VOID                 
         BE    VSRCH5                                                           
         LA    R4,L'VTAB(R4)                                                    
         BCT   R0,VSRCH3                                                        
         B     XIT                 NO MATCH                                     
*                                                                               
VSRCH5   MVI   VOIDSW,C'Y'                                                      
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ADD TRANSACTIONS TO SUMMARY TABLES                                  *         
***********************************************************************         
                                                                                
SUM      NTR1  ,                                                                
         USING PSHEADD,R2                                                       
         LR    R3,R2                                                            
         USING TRNELD,R3                                                        
         SR    R0,R0                                                            
*                                                                               
SUM03    IC    R0,TRNLN           FIND TRANSACTION ELEMENT                      
         AR    R3,R0                                                            
         CLI   0(R3),0                                                          
         BNE   *+6                                                              
         DC    H'0'                NO TRANSACTION ELEMENT                       
         CLI   0(R3),X'44'                                                      
         BNE   SUM03                                                            
         MVC   OFFC,TRNANAL        OFFICE CODE                                  
         OC    OFFC,SPACES                                                      
         CLC   OFFC,SPACES                                                      
         BNE   *+8                                                              
         MVI   OFFC,X'FE'          GET A LINE FOR NO OFFICE                     
         CLI   LVALN,12            ONE-LEVEL LEDGER ?                           
         BE    SUM05               NO MEDIA SUMMARIES                           
         MVC   MEKEY(MEKLQ),SPACES                                              
         MVC   MOKEY(MOKLQ),SPACES                                              
         SR    R1,R1                                                            
         IC    R1,LVALN                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   MEME(0),PSHDACC+3   LEVEL A ACCOUNT                              
         MVC   MOME,MEME           ALSO TO MEDIA/OFFICE RECORD                  
         ZAP   MEAMT,TRNAMNT       TRANSACTION AMOUNT                           
         GOTO1 BINADD,DMCB,MEREC,AMETAB ADD MEDIA RECORD TO TABLE               
         MVC   MEKEY(MEKLQ),SPACES                                              
         MVI   MEKEY,X'FF'         TOTAL RECORD                                 
         BASR  RE,RF                                                            
*                                                                               
         MVC   MOOF,OFFC           OFFICE CODE                                  
         ZAP   MOAMT,TRNAMNT       TRANSACTION AMOUNT                           
         GOTO1 BINADD,DMCB,MOREC,AMOTAB                                         
         MVC   MOKEY(MOKLQ),SPACES                                              
         MVI   MOKEY,X'FF'         TOTAL RECORD                                 
         BASR  RE,RF                                                            
*                                                                               
SUM05    MVC   OFKEY(OFKLQ),SPACES OFFICE SUMMARY                               
         MVC   OFOF,OFFC           OFFICE CODE                                  
         ZAP   OFAMT,TRNAMNT       TRANSACTION AMOUNT                           
         GOTO1 BINADD,DMCB,OFREC,AOFTAB                                         
         MVC   OFKEY(OFKLQ),SPACES                                              
         MVI   OFKEY,X'FF'         TOTAL RECORD                                 
         BASR  RE,RF                                                            
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PRINT MEDIA SUMMARY                                                 *         
***********************************************************************         
                                                                                
MES      NTR1  ,                                                                
         L     R4,AMETAB           R4 = MEDIA TABLE                             
         USING BIND,R4                                                          
         ICM   R0,15,BININ         NUMBER IN TABLE                              
         BZ    XIT                                                              
         LA    R7,BINTABLE                                                      
*                                                                               
MES03    MVC   MEREC,0(R7)                                                      
         CLI   MEME,X'FF'          TOTAL RECORD                                 
         BE    XIT                                                              
         EDIT  (P8,MEAMT),(15,P+36),2,FLOAT=$,COMMAS=YES,MINUS=YES              
*                                                                               
         MVC   REGKEY,SPACES        GET LEVEL 'A' NAME                          
         MVC   REGKEY(1),RCCOMPFL                                               
         MVC   REGKEY+1(2),QUNIT    UNIT/LEDGER                                 
         MVC   REGKEY+3(L'MEME),MEME  MEDIA IS FIRST LEVEL                      
         BAS   RE,HIGH                                                          
         BE    MES04                                                            
         MVC   P+2(7),=C'UNKNOWN'                                               
         B     MES09                                                            
*                                                                               
MES04    L     R3,AIO                                                           
         USING ACTRECD,R3                                                       
         LA    R4,ACCORFST(R3)                                                  
         SR    R1,R1                                                            
*                                                                               
         USING NAMELD,R4                                                        
MES05    CLI   0(R4),X'20'         GET NAME OF LEVEL 'A' RECORD                 
         BE    MES07                                                            
         CLI   0(R4),0             END-OF-RECORD                                
         BE    MES09                                                            
         IC    R1,NAMLN                                                         
         AR    R4,R1                                                            
         B     MES05                                                            
*                                                                               
MES07    IC    R1,NAMLN            NAME TO PRINT LINE                           
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P+2(0),NAMEREC                                                   
*                                                                               
MES09    MVI   SPACING,2           PRINT A MEDIA LINE                           
         BAS   RE,PRT                                                           
         LA    R7,MELNQ(R7)        NEXT TABLE ENTRY                             
         B     MES03                                                            
         EJECT                                                                  
***********************************************************************         
* PRINT OFFICE SUMMARY                                                *         
***********************************************************************         
                                                                                
OFS      NTR1  ,                                                                
         L     R4,AOFTAB           R4 = OFFICE TABLE                            
         USING BIND,R4                                                          
         ICM   R0,15,BININ         NUMBER IN TABLE                              
         BZ    XIT                                                              
         LA    R7,BINTABLE                                                      
         MVI   RCSUBPRG,3                                                       
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
OFS03    MVC   OFREC,0(R7)                                                      
         EDIT  (P8,OFAMT),(15,P+13),2,COMMAS=YES,FLOAT=$,MINUS=YES              
         CLI   OFOF,X'FF'          TOTAL LINE                                   
         BNE   OFS05                                                            
         MVC   P+1(L'TXT5),TXT5    * TOTAL *                                    
         B     OFS07                                                            
*                                                                               
OFS05    CLI   OFOF,X'FE'          MISSING OFFICE CODE                          
         BE    *+10                                                             
         MVC   P+4(L'OFOF),OFOF    OFFICE CODE                                  
*                                                                               
OFS07    BAS   RE,PRT                                                           
         CLI   OFOF,X'FF'          JUST PRINTED TOTAL                           
         BE    XIT                                                              
         LA    R7,OFLNQ(R7)        NEXT TABLE ENTRY                             
         B     OFS03                                                            
         EJECT                                                                  
***********************************************************************         
* PRINT MEDIA/OFFICE SUMMARY                                          *         
***********************************************************************         
                                                                                
MOS      NTR1  ,                                                                
         L     R4,AMOTAB           R4 = MEDIA/OFFICE TABLE                      
         USING BIND,R4                                                          
         ICM   R0,15,BININ         NUMBER IN TABLE                              
         BZ    XIT                                                              
         LA    R7,BINTABLE                                                      
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,2                                                       
*                                                                               
MOS03    MVC   MOREC,0(R7)                                                      
         EDIT  (P8,MOAMT),(15,P+16),2,FLOAT=$,COMMAS=YES,MINUS=YES              
         CLI   MOME,X'FF'          TEST TOTAL                                   
         BNE   MOS05                                                            
         MVC   P+1(L'TXT5),TXT5    * TOTAL *                                    
         B     MOS07                                                            
*                                                                               
MOS05    MVC   P+4(L'MOME),MOME    MEDIA                                        
         CLI   MOOF,X'FE'          MISSING OFFICE                               
         BE    *+10                                                             
         MVC   P+13(L'MOOF),MOOF   OFFICE                                       
*                                                                               
MOS07    BAS   RE,PRT              PRINT THE LINE                               
         CLI   MOME,X'FF'          JUST PRINTED TOTAL                           
         BE    XIT                                                              
         LA    R7,MOLNQ(R7)        NEXT TABLE ENTRY                             
         B     MOS03                                                            
         EJECT                                                                  
***********************************************************************         
* PRINT SORTED CHECK REGISTER                                         *         
***********************************************************************         
                                                                                
PSCR     NTR1  ,                                                                
         MVI   RCSUBPRG,4                                                       
         MVI   FORCEHED,C'Y'                                                    
         XC    LASTACC,LASTACC                                                  
         ZAP   DOUBLE,=P'0'                                                     
*                                                                               
PSCR3    GOTO1 ADSORTER,DMCB,=C'GET'                                            
         L     R7,DMCB+4                                                        
         LTR   R7,R7                                                            
         BNZ   *+12                                                             
         MVI   LASTACC+3,X'FF'     SET LAST RECORD SWITCH                       
         B     PSCR13                                                           
         MVC   SREC,0(R7)          ELSE MOVE RECORD TO LOCAL BUFFER             
*                                                                               
PSCR5    CLI   PROGPROF+2,C'Y'     LOGO BREAK ON HIGH LEVEL                     
         BNE   PSCR9                                                            
         SR    R2,R2                                                            
         IC    R2,LVALN                                                         
         BCTR  R2,0                R2 = L'LEVEL A ACCOUNT - 1                   
         EX    R2,*+8                                                           
         B     *+10                                                             
         CLC   LASTACC+3(0),SRACC+3 DID HIGH LEVEL CHANGE                       
         BE    PSCR9               PRINT A DETAIL LINE                          
         OC    LASTACC,LASTACC     FIRST TIME                                   
         BNZ   PSCR13              CHANGE OF LEVEL, PRINT TOTAL                 
*                                                                               
PSCR7    L     R6,LOGOC                                                         
         USING LOGOD,R6                                                         
         MVI   LOGOTYPE,C'E'       END LAST LOGO                                
         GOTO1 LOGO,DMCB,(R6)                                                   
*                                                                               
         MVI   LOGOTYPE,C'S'       START NEW LOGO                               
         MVC   LOGO1,SPACES                                                     
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   LOGO1(0),SRACC+3    MOVE HIGH LEVEL ACCOUNT CODE TO LOGO         
         GOTO1 LOGO,DMCB,(R6)                                                   
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
PSCR9    MVC   P+22(6),SRREF       SET AND PRINT DETAIL                         
         EDIT  (P8,SRAMT),(15,P+33),2,COMMAS=YES,FLOAT=$,MINUS=YES              
         AP    DOUBLE,SRAMT        GET A TOTAL                                  
         CLC   LASTACC,SRACC       SAME ACCOUNT                                 
         BE    PSCR11                                                           
         MVC   P+1(12),SRACC+3     ACCOUNT CODE                                 
         MVC   P+51(36),SRNME      AND NAME                                     
         MVC   LASTACC,SRACC                                                    
*                                                                               
PSCR11   BAS   RE,PRT                                                           
         B     PSCR3                                                            
*                                                                               
PSCR13   BAS   RE,PRT              PRINT TOTALS                                 
         MVC   P+20(L'TXT5),TXT5    * TOTAL *'                                  
         EDIT  (P8,DOUBLE),(15,P+33),2,COMMAS=YES,FLOAT=$,MINUS=YES             
         BAS   RE,PRT                                                           
         ZAP   DOUBLE,=P'0'                                                     
         LTR   R7,R7               TEST END OF REPORT                           
         BNZ   PSCR7               SET END/START LOGOS                          
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PRINT COMMENTS FROM JCL                                             *         
***********************************************************************         
                                                                                
PCOMM    NTR1  ,                                                                
         OC    COMLIN1,COMLIN1                                                  
         BZ    XIT                                                              
         MVI   RCSUBPRG,5                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVC   P+2(L'COMLIN1),COMLIN1                                           
         MVC   PSECOND+2(L'COMLIN2),COMLIN2                                     
         MVC   PTHIRD+2(L'COMLIN3),COMLIN3                                      
         BAS   RE,PRT                                                           
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* FIND PAYEE ADDRESS & PRINT                                          *         
***********************************************************************         
                                                                                
ADR      NTR1  ,                                                                
         MVC   REGKEY,SPACES                                                    
         USING PSHEADD,R2                                                       
         MVC   REGKEY(15),PSHDSBAC                                              
         BAS   RE,HIGH                                                          
         BE    *+6                                                              
         DC    H'0'                CAN'T READ VENDOR RECORD                     
         L     R3,AIO                                                           
         USING ACTRECD,R3                                                       
         LA    R4,ACCORFST(R3)                                                  
         SR    R0,R0                                                            
*                                                                               
ADR3     CLI   0(R4),0             GET THE ADDRESS ELEMENT                      
         BE    XIT                                                              
         CLI   0(R4),X'22'                                                      
         BE    ADR5                                                             
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     ADR3                                                             
*                                                                               
         USING ADRELD,R4                                                        
ADR5     MVC   PSECOND+54(26),ADRADD1 MOVE ADDRESS TO PRINT                     
         CLI   ADRNUM,2                                                         
         BL    XIT                                                              
         MVC   PSECOND+81(26),ADRADD1+26                                        
         CLI   ADRNUM,3                                                         
         BL    XIT                                                              
         MVC   PTHIRD+54(26),ADRADD1+52                                         
         CLI   ADRNUM,4                                                         
         BNE   *+10                                                             
         MVC   PTHIRD+81(26),ADRADD1+78                                         
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* GET ELEMENT ADDRESS R2 = HEADER, R3 = TRANSACTION                   *         
***********************************************************************         
                                                                                
GETL     LA    R2,L'VTRNL(R6)      R2 = POSTING ELEMENT                         
         CLI   0(R2),X'52'         TRAILER RECORD (ZERO RUN)                    
         BER   RE                                                               
         LR    R3,R2               GET TRANSACTION                              
         SR    R0,R0                                                            
GETL1    IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLI   0(R3),X'44'         R3 = TRANSACTION                             
         BER   RE                                                               
         CLI   0(R3),0             END-OF-RECORD                                
         BNE   GETL1                                                            
         DC    H'0'                MISSING TRANSACTION ELEMENT                  
         SPACE 2                                                                
***********************************************************************         
* EDIT ROUTINES                                                       *         
***********************************************************************         
                                                                                
EDIT     NTR1  ,                                                                
         EDIT  (P6,(R2)),(6,P+44),COMMAS=YES                                    
         BAS   RE,PRT                                                           
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ADD ITEM TO BINARY TABLE                                            *         
***********************************************************************         
                                                                                
         USING BIND,R4                                                          
BINADD   NTR1  ,                                                                
         LM    R3,R4,0(R1)         A(RECORD), BINSRCH PARAMETERS                
         MVC   PARM+8(16),BININ                                                 
         LA    R2,BINTABLE                                                      
         GOTO1 BINSRCH,PARM,(1,(R3)),(R2)                                       
         OC    PARM(4),PARM                                                     
         BNZ   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
         MVC   BININ,PARM+8        UPDATE COUNT                                 
         CLI   PARM,1                                                           
         BE    XIT                 NOT FOUND - ADDED                            
         L     R7,PARM             A(RECORD FOUND)                              
         SR    R6,R6                                                            
         IC    R6,BINFRST          DISP. TO FIRST BUCKET                        
         AR    R7,R6               RECORD FOUND                                 
         AR    R3,R6               NEW RECORD                                   
         ZIC   R5,BINNUMB          NUMBER OF BUCKETS                            
         TM    BINSTAT,X'80'                                                    
         BO    BINBIN              DATA IS BINARY                               
         AP    0(8,R7),0(8,R3)     ADD NEW TO OLD                               
         LA    R7,8(R7)                                                         
         LA    R3,8(R3)                                                         
         BCT   R5,*-14                                                          
         B     XIT                                                              
*                                                                               
BINBIN   L     RE,0(R3)                                                         
         L     RF,0(R7)                                                         
         AR    RF,RE                                                            
         ST    RF,0(R7)                                                         
         LA    R3,4(R3)                                                         
         LA    R7,4(R7)                                                         
         BCT   R5,BINBIN                                                        
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* SET HEADLINES AND PRINT A LINE                                      *         
***********************************************************************         
                                                                                
PRT      NTR1  ,                                                                
         MVC   HEAD4+104(12),BANKCODE                                           
         MVC   HEAD4+47(L'SYSNME),SYSNME  DEFAULT SYSTEM NAME                   
         CLI   PROGPROF,C'Y'           PRINT LEDGER NAME IN HEADLINES           
         BNE   *+10                                                             
         MVC   HEAD4+47(36),LDGNME                                              
         MVC   HEAD4+84(L'TXT4),TXT4   CHECKS                                   
         GOTO1 ADSQUASH,DMCB,HEAD4+47,44                                        
         TM    RNSW,DRAFT          IS IT DRAFT                                  
         BZ    *+10                                                             
         MVC   HEAD3+46(L'TXT10),TXT10                                          
*                                                                               
PRT3     GOTO1 ACREPORT                                                         
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* GET A55 CKECK PROFILE                                               *         
***********************************************************************         
                                                                                
CKP      NTR1  ,                                                                
         XC    CKPROF,CKPROF                                                    
         L     RF,AMONACC                                                       
         USING ACMD,RF                                                          
         XC    ACMPFKEY,ACMPFKEY                                                
         MVI   ACMPFSYS,C'A'       ACCOUNT                                      
         MVI   ACMPFPGM,C'0'                                                    
         MVC   ACMPFPGM+1(2),=C'55'                                             
         MVC   ACMPFAGY,ALPHAID                                                 
         MVC   ACMPFUNL,QUNIT      UNIT/LEDGER                                  
         GOTO1 GETPROF,DMCB,ACMPFKEY,CKPROF,(0,DATAMGR)                         
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* GET ID NAME                                                         *         
***********************************************************************         
         SPACE 1                                                                
IDNM     NTR1  ,                                                                
         MVC   IDABBR,SPACES       GET LOGO FOR ORIGIN ID                       
         MVC   POWCODE,ALPHAID     DEFAULT FOR POWER CODE IS ALPHA              
         XC    CNTFK,CNTFK                                                      
         USING CTIREC,R3                                                        
         LA    R3,CNTFK                                                         
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),ORIGINUM                                             
         L     R4,AIO                                                           
         GOTO1 DATAMGR,DMCB,DMRDHI,CTFILE,(R3),(R4)                             
         CLC   CTIKEY,0(R4)                                                     
         BNE   XIT                                                              
         LA    R2,28(R4)                                                        
         SR    R0,R0                                                            
*                                                                               
IDNM3    CLI   0(R2),0                                                          
         BE    XIT                                                              
         CLI   0(R2),X'02'                                                      
         BE    IDNM5                                                            
         CLI   0(R2),X'30'                                                      
         BE    IDNM7                                                            
*                                                                               
IDNM4    IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     IDNM3                                                            
*                                                                               
         USING CTDSCD,R2                                                        
IDNM5    SR    R1,R1                                                            
         IC    R1,1(R2)                                                         
         SH    R1,=H'3'                                                         
         CH    R1,=H'6'                                                         
         BNH   *+8                                                              
         LA    R1,6                                                             
         EX    R1,*+4                                                           
         MVC   IDABBR(0),CTDSC                                                  
         B     IDNM4                                                            
*                                                                               
         USING CTDSTD,R2                                                        
IDNM7    MVC   POWCODE,CTDSTPOW      USE POWER CODE                             
         B     IDNM4                                                            
         EJECT                                                                  
***********************************************************************         
* CHECK AUTHORIZATION RECORD                                          *         
***********************************************************************         
                                                                                
READCKAU NTR1                                                                   
         L     R4,ADLEDGER         BUILD KEY FROM LEDGER RECD                   
*MN      NI    LGSW,X'FF'-LGCKA                                                 
*                                                                               
         USING CHARECD,R2                                                       
         L     R2,AIO2             PUT CHK RECD IN AIO2                         
         MVC   CHAKEY,SPACES                                                    
         MVI   CHAKTYP,CHAKTYPQ                                                 
         MVC   CHAKCULA,0(R4)                                                   
         DROP  R2                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',(R2),(R2)                    
*MN      CLI   DMCB+8,0                                                         
*MN      BNE   *+8                                                              
*MN      OI    LGSW,LGCKA                                                       
         CLI   DMCB+8,0                                                         
*MN                                                                             
         BE    *+6                                                              
         DC    H'0'                                                             
*MN                                                                             
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DATAMGR INTERFACE                                                   *         
***********************************************************************         
                                                                                
READ     NTR1                                                                   
         GOTO1 DATAMGR,DMCB,DMREAD,ACCFIL,REGKEY,AIO                            
         CLI   8(R1),0                                                          
         BE    XIT                                                              
         DC    H'0'                                                             
*                                                                               
HIGH     NTR1                                                                   
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCFIL,REGKEY,AIO                            
         L     R3,AIO                                                           
         CLC   REGKEY,0(R3)                                                     
         B     XIT                                                              
*                                                                               
WRITE    NTR1                                                                   
         GOTO1 DATAMGR,DMCB,DMWRT,ACCFIL,(R2),(R2)                              
         CLI   8(R1),0                                                          
         BE    XIT                                                              
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* OPEN WORKER FILE                                                    *         
***********************************************************************         
                                                                                
OPNWK    TM    RNSW,DRAFT          IS IT DRAFT                                  
         BOR   RE                  SKIP THE OUTPUT FILE                         
         NTR1  ,                                                                
         L     RE,AWRKB            R3 = WORK FILE BUFFER                        
         LH    RF,=Y(L'WRKB)       R4 = LENGTH OF BUFFER                        
         XCEFL (RE),(RF)           CLEAR IT                                     
         LA    R7,WKID                                                          
         USING UKRECD,R7                                                        
         XC    UKINDEX+L'UKKEY(L'UKINDEX-L'UKKEY),UKINDEX+L'UKKEY               
         MVI   UKCLASS,C'P'        POSTING FILE                                 
         CLI   TYPCHK,TYPDDS       FOR SOON OR LOCAL (NOT DDS)                  
         BE    *+8                                                              
         MVI   UKFLAG,X'01'        ALLOW DUPLICATES FOR SOON                    
         BAS   RE,WRKOPEN          OPEN THE WORKER FILE                         
*                                                                               
         TM    RNSW,SOON                                                        
         BNO   XIT                                                              
         L     R4,ADLEDGER         GET LEDGER RECORD                            
         MVI   ELCODE,X'EA'        LEDGER LOCK ELEMENT                          
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING LGLELD,R4                                                        
         TM    RNSW,RUNTST         RUN=TEST                                     
         BO    *+14                                                             
         TM    LGLSTAT,LGLSLOCK    TEST LEDGER IS LOCKED                        
         BO    *+6                                                              
         DC    H'0'                LEDGER MUST BE LOCKED FOR SOON               
         MVC   LUID,LGLLUID        SAVE LUID                                    
         LA    R3,WORK                                                          
         XC    WORK,WORK                                                        
         USING FWRECD,R3                                                        
         MVC   FWRLEN,=Y(FWRLNQ)   USER INFO FOR SRUPD00                        
         MVC   FWRUSER,=C'USER='                                                
         MVC   FWRUSID,IDABBR                                                   
         MVC   FWRLUID,LUID                                                     
         MVC   FWRPQID,PQID                                                     
         MVC   FWRWKID,WKID        POSTING                                      
         L     RF,VSSB                                                          
         USING SSOOFF,RF                                                        
         L     R2,SSOFWNDX         INDEX                                        
         L     R0,SSOFWBUF         BUFFER                                       
         GOTO1 DATAMGR,DMCB,(0,FACADD),(0,FACWRK),(R2),(R3),(R0)                
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* CHANGE STATUS OF INPUT FILE TO KEEP                                 *         
***********************************************************************         
                                                                                
KEEPIN   NTR1  ,                                                                
         L     RE,AWRKB            R3 = WORK FILE BUFFER                        
         LH    RF,=Y(L'WRKB)       R4 = LENGTH OF BUFFER                        
         XCEFL (RE),(RF)           CLEAR IT                                     
         LA    R7,WKID                                                          
         USING UKRECD,R7                                                        
         XC    UKINDEX+L'UKKEY(L'UKINDEX-L'UKKEY),UKINDEX+L'UKKEY               
         BAS   RE,WRKINDX          GET INDEX RECORD                             
         NI    RNSW,ALL-DRAFT                                                   
         BAS   RE,WRKKEEP          MAKE INPUT FILE KEEP                         
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* CHECK WORKER ILE                                                    *         
***********************************************************************         
CKDUPWK  NTR1  ,                                                                
         L     RE,AWRKB            R3 = WORK FILE BUFFER                        
         LH    RF,=Y(L'WRKB)       R4 = LENGTH OF BUFFER                        
         XCEFL (RE),(RF)           CLEAR IT                                     
         LA    R7,WKID                                                          
         USING UKRECD,R7                                                        
         MVC   UKUSRID,ORIGINUM                                                 
         MVC   UKSYSPRG,=C'A55'                                                 
         MVC   UKSUBPRG,QLEDGER    LEDGER TO SUB-USER KEY FIELD                 
         MVC   UKDAY,TODAY3+2      SET DATE IN ID'S                             
         MVC   UKCLASS,TYPCHK      CAN BE S(OON) OR L(OCAL)  OR C(DDS)          
         XC    UKINDEX+L'UKKEY(L'UKINDEX-L'UKKEY),UKINDEX+L'UKKEY               
         MVI   UKCLASS,C'P'        POSTING FILE                                 
         MVC   WKCMD,WKINDX        INDEX                                        
         GOTO1 WORKER,DMCB,WKCMD,AWRKB,WKID,(R6)                                
         CLI   8(R1),0                                                          
         BNE   XIT                 IF NO FILE FOUND - IS OKAY                   
*                                                                               
         USING UKRECD,R7                                                        
         TM    UKSTAT,X'08'        TEST KEEP STATUS                             
         BZ    XIT                 IF NO FILE KEPT - NO DUPLICATE               
*                                  IMMEDIATE UPDATE                             
         MVI   FORCEHED,C'Y'                                                    
         MVC   P,SPACES                                                         
         BAS   RE,PRT                                                           
         MVC   P+10(L'ERRM10),ERRM10                                            
         BAS   RE,PRT                                                           
         OI    RNSW,ERRS                                                        
         B     XIT                                                              
*                                                                               
***********************************************************************         
* WORKER INTERFACE                                                    *         
***********************************************************************         
                                                                                
WRKINDX  MVC   WKCMD,WKINDX        INDEX                                        
         B     WRKR                                                             
*                                                                               
WRKREAD  MVC   WKCMD,WKREAD        READ                                         
         B     WRKR                                                             
*                                                                               
WRKKEEP  MVC   WKCMD,WKKEEP        KEEP                                         
         B     WRKOUT                                                           
*                                                                               
WRKRTN   MVC   WKCMD,WKRTN         RETAIN                                       
         B     WRKR                                                             
*                                                                               
WRKOPEN  MVC   WKCMD,WKOPEN        OPEN                                         
         B     WRKOUT                                                           
*                                                                               
WRKADD   MVC   WKCMD,WKADD         ADD                                          
         B     WRKOUT                                                           
*                                                                               
WRKCLSE  MVC   WKCMD,WKCLSE        CLOSE                                        
WRKOUT   TM    RNSW,DRAFT          IS IT DRAFT                                  
         BOR   RE                  SKIP THE OUTPUT FILE                         
         CLI   RCPOSTNG,C'N'                                                    
         BER   RE                                                               
*                                                                               
WRKR     NTR1  ,                                                                
         GOTO1 WORKER,DMCB,WKCMD,AWRKB,WKID,(R6)                                
         CLI   8(R1),0                                                          
         BNE   ERR9                WORKER FILE ERROR                            
         CLC   WKCMD,WKINDX        INDEX  CALL                                  
         BNE   XIT                 OK TO EXIT                                   
         LA    R7,WKID                                                          
         USING UKRECD,R7                                                        
*                                                                               
WRK3     TM    UKSTAT,X'08'        TEST KEEP STATUS                             
         BO    WRK5                SKIP THE KEPT FILES                          
         CLC   UKUSRID,ORIGINUM                                                 
         BNE   WRK5                                                             
         CLC   UKSYSPRG,=C'A55'                                                 
         BNE   WRK5                                                             
         CLC   UKSUBPRG,QLEDGER    LEDGER TO SUB-USER KEY FIELD                 
         BNE   WRK5                                                             
         CLC   UKDAY,TODAY3+2      SET DATE IN ID'S                             
         BNE   WRK5                                                             
         CLC   UKCLASS,TYPCHK      CAN BE S(OON) OR L(OCAL)  OR C(DDS)          
         BNE   WRK5                                                             
         B     XIT                 IGNORE FILE NUMBER TEST                      
*                                                                               
WRK5     BASR  RE,RF               TRY AGAIN                                    
         CLI   8(R1),0                                                          
         BE    WRK3                                                             
         B     ERR9                WORKER FILE ERROR                            
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
***********************************************************************         
* ERROR ROUTINES                                                      *         
***********************************************************************         
                                                                                
ERR1     MVC   P+91(L'ERRM1),ERRM1  INVALID FORMAT                              
         B     ERR99                                                            
*                                                                               
ERR2     MVC   P+91(L'ERRM2),ERRM2  INVALID OPTION                              
         MVC   P+93+L'ERRM2(10),12(R3)                                          
         B     ERR99                                                            
*                                                                               
ERR3     MVC   P+91(L'ERRM3),ERRM3  INVALID ACCOUNT                             
         MVC   P+93+L'ERRM3(14),REGKEY+1                                        
         B     ERR99                                                            
*                                                                               
ERR4     MVC   P+91(L'ERRM4),ERRM4  INVALID CHECK NUMBER                        
         B     ERR99                                                            
*                                                                               
ERR5     MVC   P+91(L'ERRM5),ERRM5  TOO MANY VOIDS                              
         B     ERR99                                                            
*                                                                               
ERR6     MVC   P+1(L'ERRM6),ERRM6   INPUT FILE AND CARDS DISAGREE               
         BAS   RE,PRT                                                           
         MVC   P+1(L'ERRM7),ERRM7   CURRENT=                                    
         EDIT  CURRENT,(6,P+11)                                                 
         BAS   RE,PRT                                                           
         MVC   P+1(L'ERRM8),ERRM8   EXPECTED=                                   
         EDIT  LASTR,(6,P+11)                                                   
         BAS   RE,PRT                                                           
         TM    RNSW,ZERO           TEST ZERO RUN                                
         BNO   *+8                                                              
         BAS   RE,KEEPIN                                                        
         B     ERRX                                                             
*                                                                               
ERR9     MVC   P+1(L'ERRM9),ERRM9   NO CHECKS TO PRINT                          
         BAS   RE,PRT                                                           
         B     ERRX                                                             
*                                                                               
*                                                                               
ERR99    MVC   P+1(80),CARDIO      PRINT THE CARD IMAGE                         
         GOTO1 ADSQUASH,DMCB,P+1,120                                            
         BAS   RE,PRT                                                           
         BAS   RE,PRT                                                           
*                                                                               
ERRX     TM    RNSW,SORT           HAS SORT BEEN OPENED                         
         BNO   ERRXX                                                            
         GOTO1 ADSORTER,DMCB,=C'END'                                            
*                                                                               
ERRXX    OI    RNSW,ERRS                                                        
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DATA CONSTANTS                                                      *         
***********************************************************************         
                                                                                
RELOTAB  DS    0A                                                               
         DC    A(TRNF)                                                          
         DC    A(CHKF)                                                          
         DC    A(VTAB)                                                          
         DC    A(STAB)                                                          
         DC    A(WRKB)                                                          
         DC    A(IO)                                                            
         DC    A(IO2)                                                           
         DC    A(METAB)                                                         
         DC    A(OFTAB)                                                         
         DC    A(MOTAB)                                                         
         DC    X'FF'                                                            
         SPACE 2                                                                
***********************************************************************         
* INPUT CARD OPTIONS                                                  *         
***********************************************************************         
                                                                                
OPTTAB   DS    0CL13                                                            
         DC    AL1(3),CL10'BANK      ',AL2(VALBANK-AC5602)                      
         DC    AL1(4),CL10'FIRST     ',AL2(VALFRST-AC5602)                      
         DC    AL1(3),CL10'LAST      ',AL2(VALLAST-AC5602)                      
         DC    AL1(3),CL10'SKIP      ',AL2(VALSKIP-AC5602)                      
         DC    AL1(3),CL10'VOID      ',AL2(VALVOID-AC5602)                      
         DC    AL1(3),CL10'COMMENT   ',AL2(VALCOMM-AC5602)                      
         DC    X'FF'                                                            
         SPACE 2                                                                
***********************************************************************         
* SYSTEM TABLE                                                        *         
***********************************************************************         
                                                                                
SYSTAB   DS    0CL21                                                            
         DC    C'P',CL16'PRINT           ',C'B002'                              
         DC    C'Q',CL16'CANADIAN PRINT  ',C'BC02'                              
         DC    C'S',CL16'SPOT            ',C'B001'                              
         DC    C'T',CL16'CANADIAN SPOT   ',C'BC01'                              
         DC    C'U',CL16'NETWORK         ',C'B009'                              
         DC    C'V',CL16'PRODUCTION      ',C'B003'                              
         DC    C'W',CL16'CANADIAN PRODN  ',C'BC03'                              
         DC    C'X',CL16'EXPENSE         ',C'B004'                              
         DC    C'Y',CL16'CANADIAN EXPENSE',C'BC04'                              
         DC    X'FF'                                                            
         EJECT                                                                  
SORTCARD DC    CL80'SORT FIELDS=(1,21,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=62'                                    
*                                                                               
VNARR    DC    CL15'   ***VOID***'                                              
FNARR    DC    CL6'     1'                                                      
ONE      DC    P'1'                                                             
*                                                                               
WKINDX   DC    CL8'INDEX'                                                       
WKREAD   DC    CL8'READ'                                                        
WKKEEP   DC    CL8'KEEP'                                                        
WKRTN    DC    CL8'RETAIN'                                                      
WKOPEN   DC    CL8'OPEN'                                                        
WKADD    DC    CL8'ADD'                                                         
WKCLSE   DC    CL8'CLOSE'                                                       
WKCMD    DC    CL8' '                                                           
*                                                                               
ACCFIL   DC    C'ACCOUNT '                                                      
ACCDIR   DC    C'ACCDIR  '                                                      
*MDTFA   DC    C'DTFAD   '                                                      
CTFILE   DC    C'CTFILE  '                                                      
*                                                                               
FACADD   DC    C'ADD     '                                                      
FACWRK   DC    C'FACWRK  '                                                      
*                                                                               
TXT1     DS    CL(L'ETXT1)                                                      
TXT2     DS    CL(L'ETXT2)                                                      
TXT3     DS    CL(L'ETXT3)                                                      
TXT4     DS    CL(L'ETXT4)                                                      
TXTLQ    EQU   *-TXT1                                                           
*                                                                               
TXT5     DC    C'* TOTAL *'                                                     
TXT6     DC    C'TOTAL CASH'                                                    
TXT7     DC    C'TOTAL FOR REQUEST'                                             
TXT8     DC    C'DATED'                                                         
TXT9     DC    C'IS'                                                            
TXT10    DC    C'** DRAFT **'                                                   
TXT11    DC    C'SKIPPED'                                                       
*                                                                               
*              US SPELLING                                                      
ETXT1    DC    C'GOOD CHECKS '                                                  
ETXT2    DC    C'VOID CHECKS '                                                  
ETXT3    DC    C'TOTAL CHECKS '                                                 
ETXT4    DC    C'CHECKS '                                                       
*                                                                               
*               CANADIAN SPELLING                                               
CTXT1    DC    C'GOOD CHEQUES'                                                  
CTXT2    DC    C'VOID CHEQUES'                                                  
CTXT3    DC    C'TOTAL CHEQUES'                                                 
CTXT4    DC    C'CHEQUES'                                                       
*                                                                               
*                                                                               
ERRM1    DC    C'INVALID FORMAT'                                                
ERRM2    DC    C'INVALID OPTION'                                                
ERRM3    DC    C'INVALID ACCOUNT'                                               
ERRM4    DC    C'INVALID CHECK NUMBER'                                          
ERRM5    DC    C'TOO MANY VOIDS'                                                
ERRM6    DC    C'INPUT FILE AND CARDS DISAGREE'                                 
ERRM7    DC    C'CURRENT='                                                      
ERRM8    DC    C'EXPECTED='                                                     
ERRM9    DC    C'NO CHECKS TO PRINT'                                            
ERRM10   DC    C'DUPLICATE WORKER FILE ALREADY UPDATED FOR THIS DAY'            
         EJECT                                                                  
***********************************************************************         
* LITERAL POOL                                                        *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* TABLES AND IOAREAS                                                  *         
***********************************************************************         
                                                                                
MXVOID   EQU   2000                                                             
         DS    0D                  TABLE OF VOID CHECKS                         
         DC    CL8'**VTAB**'                                                    
VTAB     DS    (MXVOID)PL4         2000 X 4 BYTES                               
*                                                                               
*                                                                               
MXSKIP   EQU   100                                                              
         DS    0D                  TABLE OF SKIP CHECKS                         
         DC    CL8'**STAB**'                                                    
STAB     DS    (MXSKIP*2)PL4       FROM - TO                                    
*                                                                               
*                                                                               
         DS    0D                  IOAREA                                       
         DC    CL8'***IO***'                                                    
IO       DS    2008C                                                            
*                                                                               
         DS    0D                  IOAREA- 2                                    
         DC    CL8'***IO2**'                                                    
IO2      DS    2008C                                                            
*                                                                               
         DS    0D                  WORKER FILE BUFFER                           
         DC    CL8'*WRKB**'                                                     
WRKB     DS    XL5000                                                           
*                                                                               
         EJECT                                                                  
***********************************************************************         
* MEDIA SUMMARY TABLE                                                 *         
***********************************************************************         
                                                                                
MXME     EQU   200                                                              
         DS    0D                  MEDIA SUMMARY TABLE                          
         DC    CL8'**METAB*'                                                    
METAB    DC    F'0'                NUMBER IN TABLE                              
         DC    AL4(MELNQ)          RECORD LENGTH                                
         DC    AL4(MEKLQ)          DISP. TO KEY/ KEY LENGTH                     
         DC    AL4(MXME)           MAX. IN TABLE                                
         DC    AL1(MENBK)          NUMBER OF BUCKETS                            
         DC    AL1(MEBK-MEREC)     DISP. TO BUCKETS                             
         DC    X'00'               STATUS                                       
         DC    AL1(0)                                                           
         DS    (MXME*MELNQ)C       TABLE                                        
         SPACE 2                                                                
***********************************************************************         
* OFFICE SUMMARY TABLE                                                *         
***********************************************************************         
                                                                                
MXOF     EQU   100                                                              
         DS    0D                  OFFICE SUMMARY TABLE                         
         DC    CL8'**OFTAB*'                                                    
OFTAB    DC    F'0'                NUMBER IN TABLE                              
         DC    AL4(OFLNQ)          RECORD LENGTH                                
         DC    AL4(OFKLQ)          DISP. TO KEY/ KEY LENGTH                     
         DC    AL4(MXOF)           MAX. IN TABLE                                
         DC    AL1(OFNBK)          NUMBER OF BUCKETS                            
         DC    AL1(OFBK-OFREC)     DISP. TO BUCKETS                             
         DC    X'00'               STATUS                                       
         DC    AL1(0)                                                           
         DS    (MXOF*OFLNQ)C       TABLE                                        
         SPACE 2                                                                
***********************************************************************         
* MEDIA/OFFICE SUMMARY TABLE                                          *         
***********************************************************************         
                                                                                
MXMO     EQU   500                                                              
         DS    0D                  MEDIA/OFFICE SUMMARY TABLE                   
         DC    CL8'**MOTAB*'                                                    
MOTAB    DC    F'0'                NUMBER IN TABLE                              
         DC    AL4(MOLNQ)          RECORD LENGTH                                
         DC    AL4(MOKLQ)          DISP. TO KEY/ KEY LENGTH                     
         DC    AL4(MXMO)           MAX. IN TABLE                                
         DC    AL1(MONBK)          NUMBER OF BUCKETS                            
         DC    AL1(MOBK-MOREC)     DISP. TO BUCKETS                             
         DC    X'00'               STATUS                                       
         DC    AL1(0)                                                           
         DS    (MXMO*MOLNQ)C       TABLE                                        
         EJECT                                                                  
***********************************************************************         
* DCB FOR THE CHECK NUMBER WORK FILE                                  *         
***********************************************************************         
                                                                                
CHKF     DCB   DDNAME=SOUT,                                            X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=70,                                               X        
               BLKSIZE=70,                                             X        
               MACRF=(PM,GM),                                          X        
               EODAD=POST23                                                     
*                                                                               
***********************************************************************         
* DCB FOR TEMP TRANSACTION POSTING FILE                               *         
***********************************************************************         
                                                                                
TRNF     DCB   DDNAME=OUT,                                             X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=01992,                                            X        
               BLKSIZE=01996,                                          X        
               MACRF=(PM,GM)                                                    
         EJECT                                                                  
***********************************************************************         
* DSECT FOR PROGRAM WORKING STORAGE                                   *         
***********************************************************************         
                                                                                
AC56D    DSECT                                                                  
ATYPES   DS    0A                                                               
ATRNF    DS    A                   A(OUT)                                       
ACHKF    DS    A                   A(CHKF)                                      
AVTAB    DS    A                   A(VTAB)                                      
ASTAB    DS    A                   A(STAB)                                      
AWRKB    DS    A                   A(WRKB)                                      
AIO      DS    A                   A(IO)                                        
AIO2     DS    A                   A(IO2)                                       
AMETAB   DS    A                   A(METAB)                                     
AOFTAB   DS    A                   A(OFTAB)                                     
AMOTAB   DS    A                   A(MOTAB)                                     
*                                                                               
PACUM    DS    0PL6                                                             
CHUSED   DS    PL6                 NUMBER OF CHECKS USED                        
TOTCASH  DS    PL6                 TOTAL CASH POSTINGS                          
CHOKS    DS    PL6                 NUMBER OF OK CHECKS                          
VOIDS    DS    PL6                 NUMBER OF VOID CHECKS                        
WRKREC   DS    PL6                 WORKER FILE RECORDS                          
FIRSTR   DS    PL6                 FIRST CHECK NUMBER FOR RUN                   
FIRSTC   DS    PL6                 FIRST GOOD CHECK                             
LASTC    DS    PL6                 LAST GOOD CHECK                              
LASTR    DS    PL6                 LAST CHECK FOR RUN                           
SKIP1    DS    PL6                 START OF SKIP                                
SKIP2    DS    PL6                 END OF SKIP                                  
VOID1    DS    PL6                 START OF VOIDS                               
VOID2    DS    PL6                 END OF VOIDS                                 
CURRENT  DS    PL6                 CURRENT CHECK NUMBER                         
REQTOT   DS    PL6                 TOTAL CASH FOR REQUEST                       
TOTPOST  DS    PL6                 CASH FROM PSSBEL                             
NPACUM   EQU   (*-PACUM)/6                                                      
*                                                                               
CARDIO   DS    CL80                CARD IO AREA                                 
SKIPCRD  DS    CL80                SKIP CARD                                    
*                                                                               
COMLINS  DS    0CL216                                                           
COMLIN1  DS    CL72                COMMENT CARD                                 
COMLIN2  DS    CL72                COMMENT CARD                                 
COMLIN3  DS    CL72                COMMENT CARD                                 
COMENTS  EQU   (*-COMLINS)/L'COMLIN1                                            
*                                                                               
WKID     DS    CL(L'UKINDEX)       INDEX ID                                     
TODAY6   DS    CL6                 YYMMDD - CHARACTER                           
TODAY3   DS    CL3                 YYMMDD - PACKED                              
*                                                                               
WKIDP    DS    CL(L'UKINDEX)       POSTING FILE - ID                            
AWRKBP   DS    A                   A(WORK FILE BUFFER)                          
*                                                                               
SYSENT   DS    0CL(L'SYSTAB)       CURRENT SYSTEM ENTRY                         
SYSLGD   DS    CL1                 LEDGER CODE                                  
SYSNME   DS    CL16                SYSTEM NAME                                  
SYSBANK  DS    CL4                 DEFAULT BANK CODE                            
*                                                                               
BANKACC  DS    0CL15               CASH ACCOUNT                                 
BANKCUL  DS    CL3                 UNIT LEDGER                                  
BANKCODE DS    CL12                CODE                                         
*                                                                               
*NACKEY  DS    CL42                KEY OF LAST RECORD READ BY MONACC            
REGKEY   DS    CL42                REGISTER KEY                                 
NVOD     DS    F                   NUMBER OF VOIDS                              
NSKP     DS    F                   NUMBER OF SKIPS                              
*                                                                               
LGSW     DS    XL1                 LEDGER OPTIONS                               
LGCAN    EQU   X'80'               CANADIAN LEDGER                              
LGCKE    EQU   X'40'               COKE EXPENDITURE                             
LGDIR    EQU   X'20'               PRINTING DIRECT                              
LGSHU    EQU   X'10'               SHUTTLE                                      
LGFTP    EQU   X'08'               FTP                                          
LGUPD    EQU   X'04'               RUN UPDATE                                   
LGMIC    EQU   X'02'               MICRO PRINTING                               
LGCKA    EQU   X'01'               LEDGER USING X'10' CHK AUTH REC              
*                                                                               
LSRSW    DS    XL1                 LASER TYPES                                  
LSRCOLR  EQU   X'80'               COLOR LASER CHECKS PRINT AT DDS              
*                                                                               
RNSW     DS    XL1                 RUN CONTROL SWITCH                           
ERRS     EQU   X'80'               ERRORS - ABORT RUN                           
SORT     EQU   X'40'               SORTER HAS BEEN OPENED                       
SOON     EQU   X'20'               REGISTER IS RUNNING SOON                     
DRAFT    EQU   X'10'               DRAFT REGISTER                               
FIRST    EQU   X'08'               FIRST TIME THRU LOOP                         
CHECK    EQU   X'04'               CALLED BY CHECK RUN                          
RUNTST   EQU   X'02'               RUN = TEST                                   
ZERO     EQU   X'01'               ZERO RUN                                     
ALL      EQU   X'FF'                                                            
*                                                                               
LDGNME   DS    CL36                LEDGER NAME                                  
LVALN    DS    XL1                 LEVEL A LENGTH                               
CHRC     DS    CL1                 LEADING CHECK NUMBER CHARACTER               
OFFC     DS    CL2                 TRANSACTION OFFICE                           
TYPCHK   DS    CL1                 TYPE OF CHECK RUN                            
TYPDDS   EQU   C'C'                OVERNIGHT - DDS                              
TYPSOON  EQU   C'S'                SOON CHECK RUN                               
TYPLOCL  EQU   C'L'                LOCAL CHECK RUN                              
*                                                                               
PARM     DS    6F                  BINSRCH PARAMETERS                           
DUB1     DS    D                                                                
VOIDSW   DS    CL1                                                              
REQNO    DS    CL6                                                              
REQDAT   DS    CL3                                                              
COMMAND  DS    CL6                                                              
ELCODE   DS    X                                                                
IDABBR   DS    CL7                                                              
POWCODE  DS    CL2                                                              
LASTACC  DS    CL15                                                             
FILNO    DS    XL2                 FILE NUMBER                                  
*IR      DS    CL8                                                              
CKPROF   DS    CL16                CHECK - A55 PROFILE                          
CNTFK    DS    CL25                CONTROL FILE KEY                             
VSSB     DS    A                                                                
LUID     DS    CL8                                                              
PQID     DS    XL7                                                              
*                                                                               
VTRNL    DS    F                   VOID TRANSACTION                             
VTRN     DS    CL500                                                            
RTRNL    DS    F                   REAL TRANSACTION                             
RTRN     DS    CL500                                                            
         EJECT                                                                  
***********************************************************************         
*        MEDIA SUMMARY RECORD                                         *         
***********************************************************************         
                                                                                
MEREC    DS    0CL(MELNQ)                                                       
MEKEY    DS    0C                                                               
MEME     DS    CL6                 MEDIA CODE (LEVEL A)                         
MEKLQ    EQU   *-MEREC                                                          
MEBK     DS    0PL8                                                             
MEAMT    DS    PL8                 AMOUNT                                       
MENBK    EQU   (*-MEBK)/L'MEBK                                                  
MELNQ    EQU   *-MEKEY                                                          
         SPACE 2                                                                
***********************************************************************         
* OFFICE SUMMARY RECORD                                               *         
***********************************************************************         
                                                                                
OFREC    DS    0CL(OFLNQ)                                                       
OFKEY    DS    0C                                                               
OFOF     DS    CL2                 OFFICE CODE                                  
OFKLQ    EQU   *-OFREC                                                          
OFBK     DS    0PL8                                                             
OFAMT    DS    PL8                 AMOUNT                                       
OFNBK    EQU   (*-OFBK)/L'OFBK                                                  
OFLNQ    EQU   *-OFKEY                                                          
         SPACE 2                                                                
***********************************************************************         
* MEDIA/OFFICE SUMMARY RECORD                                         *         
***********************************************************************         
                                                                                
MOREC    DS    0CL(MOLNQ)                                                       
MOKEY    DS    0C                                                               
MOME     DS    CL6                 MEDIA CODE                                   
MOOF     DS    CL2                 OFFICE CODE                                  
MOKLQ    EQU   *-MOREC                                                          
MOBK     DS    0PL8                                                             
MOAMT    DS    PL8                 AMOUNT                                       
MONBK    EQU   (*-MOBK)/L'MOBK                                                  
MOLNQ    EQU   *-MOKEY                                                          
         EJECT                                                                  
***********************************************************************         
* WORK FILE RECORD                                                    *         
***********************************************************************         
                                                                                
CHKREC   DS    0CL(CHKLNQ)                                                      
CHKACC   DS    CL15                                                             
CHKNO    DS    CL6                                                              
CHKDTE   DS    CL8                 MMMDD/YY                                     
CHKTOT   DS    CL12                                                             
CHKBNK   DS    CL15                BANK NO.                                     
CHKBDT   DS    XL2                 CHECK DATE (COMPRESSED)                      
CHKAMT   DS    PL6                 CHECK AMOUNT                                 
CHKLNQ   EQU   *-CHKACC                                                         
         SPACE 2                                                                
***********************************************************************         
* SORT RECORD                                                         *         
***********************************************************************         
                                                                                
SREC     DS    0CL(SRLNQ)                RECORD FOR SORTER                      
SRACC    DS    CL15                                                             
SRREF    DS    CL6                                                              
SRAMT    DS    PL8                                                              
SRNME    DS    CL36                                                             
SRLNQ    EQU   *-SRACC                                                          
         EJECT                                                                  
***********************************************************************         
* DSECT FOR THE BINSRCH LIST                                          *         
***********************************************************************         
                                                                                
BIND     DSECT                                                                  
BININ    DS    F                   NUMBER IN TABLE                              
BINLEN   DS    F                   RECORD LENGTH                                
BINDISP  DS    CL1                 DISPLACEMENT IN RECORD                       
BINKEY   DS    CL3                 KEY LENGTH                                   
BINMAX   DS    F                   MAXIMUM NUMBER IN TABLE                      
BINNUMB  DS    CL1                 NUMBER OF BUCKETS                            
BINFRST  DS    CL1                 DISP. TO FIRST BUCKET                        
BINSTAT  DS    CL1                 X'80' BINARY DATA                            
         DS    CL1                 SPARE                                        
BINTABLE DS    0CL1                                                             
         EJECT                                                                  
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* ACGENMODES                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
* ACGENPOST                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENPOST                                                      
         PRINT ON                                                               
* ACMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
* ACREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
* DDBIGBOX                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
* DDLOGOD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDLOGOD                                                        
         PRINT ON                                                               
* DDMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
* DDREMOTED                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDREMOTED                                                      
         PRINT ON                                                               
* DDREPXTRAD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDREPXTRAD                                                     
         PRINT ON                                                               
* DMDTFIS                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMDTFIS                                                        
         PRINT ON                                                               
* DMDTFPH                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMDTFPH                                                        
         PRINT ON                                                               
* DMWRKRD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMWRKRD                                                        
         PRINT ON                                                               
* DMWRKRK                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMWRKRK                                                        
         PRINT ON                                                               
* FASSBOFF                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
* SRUPDD                                                                        
         PRINT OFF                                                              
       ++INCLUDE SRUPDD                                                         
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'044ACREP5602S05/17/00'                                      
         END                                                                    
