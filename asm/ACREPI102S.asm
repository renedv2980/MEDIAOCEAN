*          DATA SET ACREPI102S AT LEVEL 009 AS OF 08/17/00                      
*PHASE ACI102A                                                                  
*INCLUDE SQUASHER                                                               
*INCLUDE UNDERLIN                                                               
*INCLUDE PERVERT                                                                
*INCLUDE CHOPPER                                                                
*INCLUDE HELLO                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE HELEN                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE CONVMOS                                                                
*INCLUDE ADDAY                                                                  
         TITLE 'ACI102  -  P R O D U C T I O N   I N T E R F A C E'             
***********************************************************************         
* INPUTS:                                                             *         
*   QOPT1:                                                            *         
*          N - NO TAPE OUTPUT                                         *         
*          Y -    TAPE OUTPUT                                         *         
*          D - DOWN-LOAD THE OUTPUT (NEVER WORKING SO DELETED)        *         
*                                                                     *         
*   QOPT2:                                                            *         
*          N - DEFAULT FORMAT (BASIC AGENCY)                          *         
*          Y - FORMAT  2      (SUBAGENCY REPORT)                      *         
*          G - FORMAT  3      (NO LONGER SUPPORTED- EDI STYLE BILLING)*         
*                                                                     *         
*   QOPT6:                                                            *         
*          Y - PARTIAL INPUT  DUMP REQUESTED                          *         
*                                                                     *         
*   QOPT7:                                                            *         
*          Y - PARTIAL OUTPUT DUMP REQUESTED                          *         
*          X - DUMP    INPUT  ELEMENTS AND NEW TABLE RECORDS IN ADDIT *         
*              BEFORE  BINSRCH     ADD                                *         
***********************************************************************         
         SPACE  1                                                               
ACI102   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACI1**,RA       BASE REGS RB   AND  RA                       
         L     RC,0(,R1)           ->   MONACC    INFO ...                      
         USING ACWORKD,RC                                                       
*                                                                               
         LA    R9,SPACEND          ->   START     OF   WORKING STORAGE          
         USING ACI1D,R9            MAP  WORKING   STORAGE                       
*                                                                               
         ST    RC,ADRC             SAVE RC   FOR  THE  NMODS                    
*                                                                               
         CLI   MODE,RUNFRST        1ST  TIME IN ?                               
         BE    RUNF                YES  DO   1ST  FOR  RUN                      
*                                  START     OF   REQUEST   OR                  
         CLI   MODE,REQFRST             NEW  REQUEST   CARD ?                   
         BE    REQF                YES, DO   REQUEST   INITIALIZATION           
         CLI   MODE,LEVAFRST       LEVEL-A   RECORD ?                           
         BE    LEVA                YES, PROCESS   LEVEL-A   RECORD              
         CLI   MODE,LEVBFRST       LEVEL-B   RECORD ?                           
         BE    LEVB                YES, PROCESS   LEVEL-B   RECORD              
         CLI   MODE,PROCACC        ACCOUNT   RECORD ?                           
         BE    PACC                YES, PROCESS   ACCOUNT   LEVEL RCD           
         CLI   MODE,PROCTRNS       TRANSACTION RECORD ?                         
         BE    PTRN                YES, PROCESS   TRANSACTION/ELEMENTS          
         CLI   MODE,ACCLAST        END  OF   ACCOUNT   RECORDS ?                
         BE    ACCL                YES, DO   END  OF   ACCOUNT    PROCS         
         CLI   MODE,LEVALAST       LAST FOR  CLIENT ?                           
         BE    LEVAL               YES, PROCESS   LAST FOR  CLIENT              
         CLI   MODE,REQLAST        END  OF   REQUEST   RUN ?                    
         BE    REQL                YES, DO   END  OF   REQUEST    PROCS         
         CLI   MODE,RUNLAST        END  OF   RUN  CALL ?                        
         BE    RUNL                YES, DO   END  OF   RUN  PROCEDURES          
*                                                                               
EXIT     XIT1                                                                   
         EJECT ,                                                                
***********************************************************************         
* RUN FIRST (RUNFRST)                                                 *         
***********************************************************************         
         SPACE 1                                                                
RUNF     DS    0H                                                               
         MVC   ACI1ID,=CL8'*ACI1D*'     INSERT    ID                            
         MVC   VTYPES(VLNQ),VCONS  SAVE V    AND  A    TYPE ADDRESSES           
*                                  INTO COMMON    WORK AREA                     
*                                                                               
         MVC   TAPESAVE,TAPEOUT    SAVE UN-OPENED OUTPUT    TAPE DCB            
*                                                                               
         ZAP   P0,=P'0'            PACKED    ZERO                               
*                                                                               
         ZAP   EVERY,=P'1'                                                      
         ZAP   MAXCNT,=P'0300'                                                  
         ZAP   DMPTOT,P0                                                        
         ZAP   MAXDMP,=P'1000'                                                  
         MVC   YR0101,=C'YY0101'                                                
         MVC   HEXFF(4),=4X'FF'                                                 
         MVC   NHOFF,=C'LLLVS'                                                  
         MVC   NHIHEAD,SPACES                                                   
         MVC   P2X,SPACES                                                       
*                                                                               
         MVC   BUFCOM,SPACES                                                    
         MVC   BUFC1(32),=5PL8'0'                                               
*                                                                               
         USING ACMD,R3             MAP  ACMASTD                                 
         L     R3,AMONACC                                                       
         GOTO1 ACMAJOBL,DMCB,FLDH,ACMACOLL,ADCOMFAC                             
         CLI   DMCB+4,X'00'                                                     
         BNE   EXIT                                                             
         DC    H'0'                                                             
         DROP  R3                                                               
         EJECT ,                                                                
***********************************************************************         
* REQUEST FIRST (REQFRST)                                             *         
***********************************************************************         
         SPACE 1                                                                
REQF     DS    0H                                                               
         SR    R1,R1                                                            
         ICM   R1,8,SPACES         PADDING   CHARACTER IS   A    BLANK          
         LA    RF,TAPEWKLQ         LENGTH    OF   TAPE AREA                     
         LA    RE,TAPEWK           CLEAR     THE  TAPE AREA                     
         MVCL  RE,R0                                                            
*                                                                               
         MVC   SVNUM,SPACES                                                     
         MVC   SVCLX,SPACES                                                     
         MVC   LCONTRA,SPACES                                                   
         XC    SVWCCODE,SVWCCODE   CLEAR     SAVE WORK CODE                     
*                                                                               
         ZAP   SVOE,P0             JOB  COL  ORIG ESTIMATE                      
         ZAP   SVCE,P0             JOB  COL  CURR ESTIMATE                      
         ZAP   SVPE,P0             JOB  COL  CURR GROSS     EST                 
*                                                                               
         ZAP   TRNREC,P0           TRAN      RECEIVABLE     AMOUNT              
         ZAP   TRNINV,P0           TRAN      INVENTORY      AMOUNT  NET         
         ZAP   TRNCOM,P0           TRAN      COMMISSION     AMOUNT              
         ZAP   TRNDIS,P0           TRAN      DISCOUNT       AMOUNT              
*                                                                               
         ZAP   CLTREC,P0           CLIENT    RECEIVABLE     AMOUNT              
         ZAP   CLTINV,P0           CLIENT    INVENTORY      AMOUNT  NET         
         ZAP   CLTCOM,P0           CLIENT    COMMISSION     AMOUNT              
         ZAP   CLTDIS,P0           CLIENT    DISCOUNT       AMOUNT              
*                                                                               
         ZAP   REQREC,P0           REQUEST   RECEIVABLE     AMOUNT              
         ZAP   REQINV,P0           REQUEST   INVENTORY      AMOUNT  NET         
         ZAP   REQCOM,P0           REQUEST   COMMISSION     AMOUNT              
         ZAP   REQDIS,P0           REQUEST   DISCOUNT       AMOUNT              
*                                                                               
         MVI   TAPEOPTN,TAPOPDEF   DEFAULT   IS   C'1'                          
         CLI   QOPT2,C'Y'          IS   QOPTION 2 C'Y' ?                        
         BNE   *+8                                                              
         MVI   TAPEOPTN,TAPOPYES   OPTIONAL  TAPE FORMAT    C'2'                
*                                                                               
         ZAP   REPCNT,P0           REPORT    COUNT                              
         PACK  TAPECNT,=C'0'       TAPE      COUNT                              
         ZAP   DUMPCNT,P0          COUNT     OF   I/O  RECORDS                  
         ZAP   PDUMP,P0            COUNT     OF   RECORDS   DUMPED              
         PACK  MOSWK,=C'0'         MONTH     OF   SERVICE   WORK AREA           
*                                                                               
         GOTO1 BUFFALO,DMCB,=C'SET',ADBUFC                                      
*                                                                               
         XC    QSTR2,QSTR2                                                      
         XC    QSTR3,QSTR3                                                      
         MVC   QEND2,HEXFF                                                      
         MVC   QEND3,HEXFF                                                      
         XC    BSUDAT,BSUDAT                                                    
*                                                                               
         CLC   QSTART,SPACES                                                    
         BE    REQF05                                                           
         GOTO1 DATCON,DMCB,(0,QSTART),(1,QSTR3)                                 
         GOTO1 (RF),(R1),,(2,QSTR2)                                             
*                                                                               
         GOTO1 ADDAY,DMCB,QSTART,DUB,F'-10'  GET  DATE OF   QSTART-10           
         GOTO1 DATCON,DMCB,(0,DUB),(2,BSUDAT)                                   
*                                                                               
REQF05   CLC   QEND,SPACES                                                      
         BE    REQF10                                                           
         GOTO1 DATCON,DMCB,(0,QEND),(1,QEND3)                                   
         GOTO1 (RF),(R1),,(2,QEND2)                                             
*                                                                               
REQF10   MVC   PAGE,=XL2'1'        IN   CASE 2ND  REQUEST                       
         MVI   FORCEHED,C'Y'       FORCE     HEADLINES                          
         MVI   RCSUBPRG,0          ASSUMES   AGENCY    IN   AGYTBL              
         CLI   QOPT1,C'Y'          TAPE REQUEST ?                               
         BE    *+8                                                              
         MVI   RCSUBPRG,1                                                       
         L     R1,AAGYTBL          ->   AGENCY/CLIENT  TABLE                    
*                                                                               
REQF20   MVC   SVTBL(SVTBLLNQ),0(R1)                                            
         CLC   SVTBL(2),SPACES     END  OF   TABLE ?                            
         BNE   REQF25              NO,  TEST ENTRY                              
         MVC   SVTBL(2),ALPHAID    YES, USE  DEFAULT   WITH COMPANY  ID         
         B     REQF40                                                           
*                                                                               
REQF25   CLC   ALPHAID,SVAGY       SAME AGENCY ?                                
         BNE   REQF30              NO,  TRY  NEXT ENTRY                         
         CLC   SVTBL+2(1),TAPEOPTN SAME TAPE OPTION ?                           
         BNE   REQF30                                                           
         CLC   QUNIT(5),SVULC      SAME UNIT/LEDGER/CLIENT ?                    
         BE    REQF40              YES, USE  IT                                 
         CLC   SVULC+2(3),SPACES   LAST ENTRY     FOR  AGENCY ?                 
         BE    REQF40              YES, USE  IT (DEFAULT   FOR AGENCY)          
*                                                                               
REQF30   LA    R1,SVTBLLNQ(,R1)    TRY  NEXT AGENCY/CLIENT                      
         B     REQF20                                                           
*                                                                               
REQF40   TM    SVSTAT,SVSUBR       SUBROUTINE     FOR  AGENCY ?                 
         BZ    REQF50                                                           
         BAS   RE,LOAD             FIND SUBROUTINE     FOR  AGENCY              
         BAS   RE,GO               REQ  1ST  FOR  AGENCY                        
         CLI   GOSTAT,NO           TAPE ALLOCATION     DESIRED ?                
         BE    REQFX               NO,  EXIT                                    
*                                                                               
*                                  RESTORE   DCB  TO   UN-OPENED                
REQF50   MVC   TAPEOUT(TAPELEN),TAPESAVE                                        
*                                                                               
         USING IHADCB,R1           DCB  DSECT                                   
         LA    R1,TAPEOUT          ->   OUTPUT    TAPE DCB                      
         MVC   DCBLRECL,SVRECL     SET  RCD  LEN  FOR  AGENCY                   
         MVC   DCBBLKSI,SVBLKL     SET  BLK  LEN                                
         MVC   DCBRECFM,SVRECF     SET  RCD  FORMAT   (U/FB/VB)                 
         MVC   DSPARM+13(L'SVAGY+L'SVAGYCPY),SVAGY                              
*                                                                               
         CLI   QOPT1,C'Y'          OUTPUT    TAPE REQUIRED ?                    
         BNE   REQF60              NO,  DO   NOT  ALLOCATE  ONE                 
*                                                                               
*                                  NUM  OF   TIMES    TAPE WAS                  
         LH    R2,OUTCNT                OPENED/CLOSED                           
         LA    R2,1(,R2)                                                        
         STH   R2,OUTCNT                                                        
         GOTO1 DYNALLOC,DMCB,(0,DDPARM),((R2),DSPARM)                           
         OPEN  (TAPEOUT,(OUTPUT))                                               
*                                                                               
REQF60   DS    0H                                                               
*                                                                               
REQFX    B     EXIT                EXIT,     RETURN    TO   MONACC              
*                                                                               
         DROP  R1                                                               
         EJECT ,                                                                
***********************************************************************         
* FIRST TIME ROUTINES  (LEVAFRST)                                     *         
***********************************************************************         
         SPACE 1                                                                
LEVA     DS    0H                                                               
         MVC   SVACLI(SVALEN),SPACES                                            
         L     R7,ADHEIRA          ->   LEVEL-A   RECORD                        
         LA    R8,SVACLI           ->   CLIENT    NAME SAVE AREA                
         BAS   RE,ACNUM            MOVE CLIENT    INFO                          
*                                                                               
         USING PPRELD,R7           MAP  PRODUCTION     PROFILE   EL             
         L     R7,ADLVASUP                                                      
         MVC   SVAOFF,PPRUFORA     UNIT FOR  ANALYSIS                           
         OI    SVAOFF,X'40'                                                     
*                                                                               
         MVC   SV2OFF,SPACES       TEST TWO BYTE OFFICE                         
         L     RF,ADCMPEL                                                       
         USING CPYELD,RF                                                        
         TM    CPYSTAT4,CPYSOFF2                                                
         BNO   *+10                                                             
         MVC   SV2OFF,PPRGAOFF                                                  
         DROP  RF                                                               
*                                                                               
         USING NAMELD,R7           MAP  NAME ELEMENT                            
         L     R7,ADLVANAM         ->   LEV  A    NAME ELEMENT                  
         SR    R2,R2                                                            
         IC    R2,NAMLN                                                         
         SH    R2,=Y(NAMLN1Q+1)                                                 
         EXMVC R2,SVANAME,NAMEREC  SAVE NAME                                    
*                                                                               
         CLC   ALPHAID,=C'NE'      NEEDHAM ?                                    
         BNE   LEVA20              NO,  SKIP                                    
         MVC   NHAGY,=C'DDW'       ASSUME    OFFICE    TYPE L,V OR S            
         LA    R2,L'NHOFF          GET  LEN  OF   TABLE                         
*                                                                               
LEVA10   LA    R1,NHOFF-1(R2)      ->   LOW  ORDER     BYTE OF   TABLE          
         CLC   SVAOFF,0(R1)        MATCH     WITH TABLE ?                       
         BE    LEVA20              YES, ASSUMPTION     CORRECT                  
         BCT   R2,LEVA10           NO,  TRY  AGAIN                              
         MVC   NHAGY,=C'DDB'       INCORRECT ASSUMPTION                         
*                                                                               
LEVA20   CLC   ALPHAID,=C'CW'      CARGILL ?                                    
         BNE   LEVA30              NO,  SKIP                                    
         MVC   NHAGY,=C'CWA'       YES, CARGILL,  WILSON,   ACREE CODE          
*                                                                               
LEVA30   MVI   FCRDTRNS,C'Y'       READ TRANSACTION -- STANDARD                 
         BAS   RE,GO               CALL USER SUBROUTINE                         
         B     EXIT                EXIT,     RETURN    TO   MONACC              
*                                                                               
         DROP  R7                                                               
         EJECT ,                                                                
***********************************************************************         
* FIRST TIME ROUTINES  (LEVBFRST)                                     *         
***********************************************************************         
         SPACE 1                                                                
LEVB     DS    0H                                                               
         MVC   SVBPRD,SPACES                                                    
         L     R7,ADHEIRB          ->   LEVEL-B   RECORD                        
         LA    R8,SVBPRD           ->   PRODUCT   NAME SAVE AREA                
         BAS   RE,ACNUM            MOVE PRODUCT   INFO                          
         BAS   RE,GO                                                            
         B     EXIT                EXIT,     RETURN    TO   MONACC              
         EJECT ,                                                                
***********************************************************************         
* PROCESS AN ACCOUNT  (PROCACC)                                       *         
***********************************************************************         
         SPACE 1                                                                
PACC     DS    0H                                                               
         MVI   SVEA,C' '           BLANK     OUT  159  CHARACTERS               
         MVC   SVEA+1(SV1LEN-1),SVEA                                            
         ZAP   SVOE,P0             ZERO JOB  COL  ORIG ESTIMATE                 
         ZAP   SVCE,P0             ZERO JOB  COL  CURR ESTIMATE                 
         ZAP   SVPE,P0             ZERO JOB  COL  CURR GROSS     EST            
*                                  MAKE SVJBOP3/JBCL3  HEX  ZEROES              
         XC    SVJBOP3(SVJBLEN),SVJBOP3                                         
         XC    SVWCCODE,SVWCCODE   CLEAR     SAVE WORK CODE                     
*                                                                               
         BAS   RE,LOOKUP                                                        
*                                                                               
         USING JBCOLD,R3           MAP  JOBBER    COL  OUTPUT TBL ENTRY         
         USING JBLOCKD,R5          MAP  JOBBER    INTERFACE   BLOCK             
         LH    R1,JBNROWS                                                       
         ZAP   SVOE,JBCOLVAL                                                    
         ZAP   SVCE,JBCOLVAL+6(6)                                               
*                                                                               
PACC10   CLI   JBCOLTYP,JBCOLTWC                                                
         BE    PACC20                                                           
         AH    R3,JBLCOL                                                        
         BCT   R1,PACC10                                                        
         B     PACC25                                                           
*                                                                               
PACC20   MVI   ESTSW,YES           INDICATE  FOUND     AN   ESTIMATE            
*                                                                               
         DROP  R3,R5                                                            
         USING ACGOD,R3            MAP  ACGOBLOCK DSECT                         
*                                                                               
PACC25   L     R3,ADGOBLOC                                                      
         MVC   SVPAYNET,GOPAYNET   NET  AMOUNT                                  
         DROP  R3                                                               
*                                                                               
         USING BIND,R5             MAP  BINARY    SEARCH    TBL  FORMAT         
         L     R5,ASKTAB                                                        
         XC    BININ,BININ         CLEAR     NUM  IN   TABLE                    
         DROP  R5                                                               
*                                                                               
         L     R8,ADACC            ->   ACCOUNT   RECORD                        
         AH    R8,DATADISP                                                      
*                                                                               
PACC30   CLI   0(R8),0             ANY  MORE ELEMENTS ?                         
         BE    PACC50              NO,  SKIP                                    
         CLI   0(R8),NAMELQ        X'20' -   NAME ELEMENT ?                     
         BE    GT20EL              YES, SAVE ACCOUNT   NAME                     
         CLI   0(R8),PPRELQ        X'24' -   PROD PROFILE   ELEMENT ?           
         BE    GT24EL              YES, SAVE PRINT     ON   BILL INFO           
         CLI   0(R8),FFNELQ        X'25' -   FREE FORM ELEMENT ?                
         BE    GT25EL              YES, SAVE MEDIA INFO                         
         CLI   0(R8),JOBELQ        X'26' -   PROD JOB  ELEMENT ?                
         BE    GT26EL              YES, SAVE CERTAIN   JOB  DATES               
         CLI   0(R8),ABIELQ        X'27'-    ACCT BILLING   INFO EL ?           
         BE    GT27EL              YES, SAVE BILLING   INFO                     
         CLI   0(R8),RSTELQ        X'30' -   RECORD    STATUS    EL ?           
         BE    GT30EL              YES, SAVE STATUS    INFO                     
*                                                                               
PACC40   SR    R2,R2               NO,  GET  NEXT ELEMENT                       
         IC    R2,1(,R8)                                                        
         AR    R8,R2               ->   NEXT EL   IF   ANY                      
         B     PACC30              PROCESS   NEXT ELEMENT   IF   ANY            
*                                                                               
PACC50   TM    SVSTAT,SVRDSI       X'80' -   NEED TO   READ SI ?                
         BZ    PACCX               NO,  EXIT                                    
         L     R2,ADACC                                                         
         CLC   LMEDIA,9(R2)        SAME MEDIA ?                                 
         BE    PACCX               YES, DO   NOT  READ SI                       
         MVC   LMEDIA,9(R2)        SAVE THE  MEDIA                              
         MVC   SVSPCL,SPACES       CLEAR     SPECIAL   CODE                     
*                                                                               
*                                  ************************************         
*                                  * GET AGENCY VERSION OF MEDIA CODE *         
*                                  ************************************         
*                                                                               
         USING PMDELD,R7           MEDIA     DSECT     ELEMENT                  
         L     R7,ADCOMP           ->   AGENCY    RECORD                        
         MVI   ELCODE,PMDELQ       X'11' -   MEDIA     ELEMENT                  
         GOTO1 AGETEL              ANY  MEDIA     ELEMENT ?                     
         BNE   PACCX               NO,  EXIT                                    
*                                                                               
PACC60   CLC   PMDCODE,LMEDIA      SAME MEDIA     CODE ?                        
         BE    PACC70              YES, CONTINUE                                
         GOTO1 ANEXTEL             GET  NEXT ELEMENT-11                         
         BE    PACC60                                                           
         DC    H'0'                NO   MEDIA     CODE MATCH                    
*                                                                               
PACC70   LA    R8,IOAREA           FOUND     MATCH                              
         MVC   15(27,R8),SPACES    BLANK     OUT  KEY  AREA (15+27=42)          
         MVC   0(L'PMDCOM1,R8),PMDCOM1  INSERT    ACT  NUM  IN   KEY            
         MVC   COMMAND,=C'DMREAD'  ASK  FOR  A    READ                          
         GOTO1 AREADDM,(R8)        CALL DATAMGR                                 
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                RECORD    NOT  FOUND                         
*                                                                               
         USING OTHELD,R7           OTHER     DSECT                              
         LR    R7,R8                                                            
         MVI   ELCODE,OTHELQ       X'23' -   OTHER     EL   REQUEST             
         GOTO1 AGETEL              ANY  ELEMENT ?                               
         BNE   *+10                NO,  SKIP IT                                 
         MVC   SVSPCL,OTHNUM       COPY AGENCY    VERS OF   MEDIA CODE          
*                                  RE-READ   ACCOUNT   TO   RESET SEQ           
         MVC   15(27,R8),SPACES    BLANK     OUT  KEY  AREA (15+27=42)          
         L     R2,ADACC                                                         
         MVC   0(15,R8),0(R2)      INSERT    ACT  NUM  IN   KEY                 
         MVC   COMMAND,=C'DMREAD'  ASK  FOR  A    READ                          
         GOTO1 AREADDM,(R8)        CALL DATAMGR                                 
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                RECORD    NOT  FOUND                         
*                                                                               
PACCX    BAS   RE,GO               CALL SUBROUTINE                              
         B     EXIT                EXIT,     RETURN    TO   MONACC              
*                                                                               
         DROP  R7                                                               
         EJECT ,                                                                
***********************************************************************         
* GET ELEMENT INFORMATION                                             *         
***********************************************************************         
         SPACE 1                                                                
         USING NAMELD,R8           MAP  NAME DSECT                              
         SPACE 1                                                                
GT20EL   SR    R2,R2                                                            
         IC    R2,NAMLN                                                         
         SH    R2,=Y(NAMLN1Q+1)                                                 
         EXMVC R2,SAVNAME,NAMEREC  SAVE NAME INFO FOR  MCORTN/FCORTN            
         B     GTRETN              GET  NEXT ELEMENT                            
*                                                                               
         USING PPRELD,R8           MAP  PRODUCTION     PROFILE   EL             
*                                  ADDITIONAL     BILLING   PRINT               
GT24EL   MVC   SAVBLPR,PPRBILLP    NEEDED    BY   SSC  AND  NEEDHAM             
         B     GTRETN              GET  NEXT ELEMENT                            
*                                                                               
         USING FFNELD,R8           MAP  FREE FORM ELEMENT                       
GT25EL   MVC   SVMEDIA,FFNUMBER    FREE FORM NUMBER                             
         B     GTRETN              GET  NEXT ELEMENT                            
*                                                                               
         USING JOBELD,R8           MAP  PRODUCTION     JOB  ELEMENT             
GT26EL   MVC   SVJBCL3,JOBCDATE                                                 
         CLI   JOBLN,JOBLN2Q       ANY  OPENING   DATE ?                        
         BL    GTRETN              NO,  GET  NEXT ELEMENT                       
         GOTO1 DATCON,DMCB,(1,JOBODATE),(X'20',SVJBST)                          
         MVC   SVJBOP3,JOBODATE                                                 
         B     GTRETN              GET  NEXT ELEMENT                            
*                                                                               
         USING ABIELD,R8           MAP  ACCOUNT   BILLING   INFO EL             
GT27EL   MVC   SVEA(L'ABIEANO),ABIEANO  EA        NUMBER                        
         MVC   SVAC(L'ABIACNO),ABIACNO  ACCOUNT   NUMBER                        
         CLI   ABILN,ABILN2Q       ANY  ESTIMATE  NUMBER ?                      
         BL    GTRETN              LOW, GET  NEXT ELEMENT                       
         MVC   SVES,ABIESNO        ESTIMATE  NUM  FOR  INTERFACE TPS            
         CLI   ABILN,ABILN3Q       ANY  BILLING   NUMBER ?                      
         BL    GTRETN              LOW, GET  NEXT ELEMENT                       
         MVC   SVBN,ABIBINO        BILLING        NUMBER   (FREEFORM)           
         B     GTRETN              GET  NEXT ELEMENT                            
*                                                                               
         USING RSTELD,R8           MAP  RECORD    STATUS    ELEMENT             
GT30EL   GOTO1 DATCON,DMCB,(1,RSTBDATE),(X'20',SVBBFDT)                         
         CLI   RSTSUBC,X'40'       SUB  DEPT LESS THAN A    SPACE ?             
         BNH   *+10                YES, LEAVE     IT   AS   A    SPACE          
         MVC   SVF4,RSTSUBC                                                     
*                                                                               
GTRETN   B     PACC40              RETURN    FOR  NEXT ELEMENT                  
*                                                                               
         DROP  R8                                                               
         EJECT ,                                                                
***********************************************************************         
* PROCESS A TRANSACTION  (PROCTRNS)                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNELD,R7           MAP  TRANSACTION    ELEMENT                  
         SPACE 1                                                                
PTRN     DS    0H                                                               
         L     R8,ADACC            ->   ADDR OF   ACCOUNT   RECORD              
         L     R7,ADTRANS          ->   ADDR OF   TRAN ELEMENT                  
         LR    R1,R7                                                            
         SH    R1,DATADISP         ->   TRAN RECORD    KEY                      
         ST    R1,ADTRKEY          USED BY   WRTAPE    RTN                      
         GOTO1 DATCON,DMCB,(1,TRNDATE),(X'20',TRIDAT)                           
         MVC   TRINUM,TRNREF                                                    
*                                  MOVE CLI,PRD,JOB.   R8   HAS  ADACC          
         MVC   TRNCLI(L'TRNCLI+L'TRNPRD+L'TRNJOB),3(R8)                         
         MVC   TRNMED,TRNJOB                                                    
         MVI   TRIMED,C' '                                                      
         MVI   PRCSS,YES           ASSUME    YES, NO   BYPASS    FLAG           
         MVI   GOSTAT,C'I'         SAY: THIS IS   THE  XXIRT     CALL           
         BAS   RE,GO               CALL USER ROUTINE                            
         CLI   PRCSS,C'R'          WANT IT   ON   REPORT ?                      
         BE    PTRN80                                                           
         CLI   PRCSS,YES           PROCESS   TRANSACTION ?                      
         BNE   PTRNX               NO,  GOTO SUBR THEN RET  TO   MONACC         
         CLI   TRNEL,TRNELQ        X'44' - TRANSACTION ELEMENT                  
         BNE   PTRNX               NO,  EXIT,     BAD  TRANSACTION              
         CLC   TRNANAL,=C'99'      WORK-CODE 99 ?                               
         BNE   PTRNX               NO,  NON  99'S ARE  HANDLED   ABOVE          
         ZAP   TRNDIS,TRNBLCD      SAVE DISCOUNT       AMOUNT                   
         ZAP   TRNCOM,TRNBLCOM     SAVE COMMISSION     AMOUNT                   
         ZAP   TRNINV,TRNAMNT      SAVE INVENTORY      AMOUNT                   
         ZAP   TRNREC,TRNBLPAY     SAVE RECEIVABLE     AMOUNT                   
         MVC   SVDAT3,TRNDATE      SAVE TRANSACTION    DATE                     
         TM    SVTEST,SVTSBBDO     BBDO ?                                       
         BO    PTRN30              YES, SKIP                                    
         MVI   ELCODE,TRSELQ       X'60' -   STATUS    ELEMENT                  
         GOTO1 ANEXTEL             ANY  STATUS    ELEMENT ?                     
         BNE   PTRN30              NO,  SKIP                                    
*                                                                               
         USING TRSELD,R7           TRANSACTION    STATUS    ELEMENT             
         GOTO1 DATCON,DMCB,(2,TRSDATE),(1,SVDAT3)                               
*                                                                               
         CLI   PROGPROF,C'Y'       SUPPRESS  INCOME    SUSPENSE (SK) ?          
         BNE   PTRN30              NO,  SKIP                                    
*                                                                               
         USING TRNELD,R7           TRANSACTION    ELEMENT                       
         L     R7,ADTRANS                                                       
         OC    TRNAM2SK,TRNAM2SK   ANY  INCOME    POSTED    TO   SK ?           
         BZ    PTRN30              NO,  SKIP                                    
         CP    TRNAM2SK,P0         AMOUNT    POSTED    TO   SK ?                
         BE    PTRN30              ZERO,     SKIP                               
         MVI   BYTE,0                                                           
         ZAP   TRNDIS,P0           CLEAR     DISCOUNT       AMOUNT              
         ZAP   TRNCOM,P0                     COMMISSION     AMOUNT              
         ZAP   TRNINV,P0                     INVENTORY      AMOUNT              
         ZAP   TRNREC,P0                     RECEIVABLE     AMOUNT              
         CLC   TRNSK2SI,QSTR2      POSTED    DATE VS   START     DATE           
         BL    PTRN10              LOW, SKIP                                    
         CLC   TRNSK2SI,QEND2      POSTED    DATE VS   END       DATE           
         BH    PTRN10              HIGH,     SKIP                               
         MVI   BYTE,YES                                                         
         AP    TRNCOM,TRNAM2SK     ADD  SK   AMOUNT    TO   COMMISSION          
         SP    TRNINV,TRNAM2SK     AND  SUBTRACT  IT   FROM INVENTORY           
*                                                                               
PTRN10   CLC   SVDAT3,QSTR3        ACTIVITY  DATE VS   BEGIN     DATE           
         BL    PTRN20              LOW, DO   NOT  ADD  REC. ETC.                
         CLC   SVDAT3,QEND3        ACTIVITY  DATE VS   END  DATE                
         BH    PTRN20              HIGH,     DO   NOT  ADD  REC.                
         MVI   BYTE,YES                                                         
         AP    TRNDIS,TRNBLCD      SAVE DISCOUNT       AMOUNT                   
         AP    TRNCOM,TRNBLCOM     SAVE COMMISSION     AMOUT                    
         SP    TRNCOM,TRNAM2SK     LESS SK   AMOUNT                             
         AP    TRNINV,TRNAM2SK     ADD  IT   TO   INVENTORY                     
         AP    TRNINV,TRNAMNT      SAVE INVENTORY      AMOUNT                   
         AP    TRNREC,TRNBLPAY     SAVE RECEIVABLE     AMOUNT                   
*                                                                               
PTRN20   CLI   BYTE,YES            WANT IT   ON   REPORT ?                      
         BE    PTRN40              YES, CONTINUE                                
         B     PTRNX               NO,  EXIT                                    
*                                                                               
PTRN30   CLC   SVDAT3,QSTR3        ACTIVITY  DATE VS   BEGIN     DATE           
         BL    PTRNX               LOW, BYPASS    TRANSACTION                   
         CLC   SVDAT3,QEND3        ACTIVITY  DATE VS   END  DATE                
         BH    PTRNX               HIGH,     BYPASS    TRANSACTION              
*                                                                               
PTRN40   L     R7,ADTRANS          ->   1ST  44   ELEMENT                       
         MVC   SVDUE,SPACES                                                     
         MVI   ELCODE,DUEELQ       X'61' -   DUE  DATE ELEMENT                  
         GOTO1 ANEXTEL             ANY  DUE  DATE ELEMENT ?                     
         BNE   PTRN50              NO,  SKIP                                    
*                                                                               
         USING DUEELD,R7           DUE  DATE ELEMENT   DSECT                    
         GOTO1 DATCON,DMCB,(2,DUEDATE),(X'20',SVDUE)                            
*                                                                               
PTRN50   DS    0H                                                               
         MVC   TRNDUE,SVDUE        TRNDUE    NOW  SVDUE     OR   BLANKS         
         CP    TRNINV,P0           NIL  BILL ?                                  
         BNE   PTRN70              NO,  BUILD     TAPE RECORD                   
*                                                                               
PTRN60   CLI   SVPAYNET,YES        PAY=NET,  ZERO COM  ON   NIL  BILLS          
         BNE   PTRN70                                                           
         ZAP   TRNCOM,P0           NO   COMM IF   JOB  IS CURRENTLY NET         
*                                                                               
PTRN70   TM    SVTEST,SVTSSTDT     BUILD     STD  TAPE RECORD ?                 
         BZ    PTRN80              NO,  SKIP                                    
         BAS   RE,XXORTN           BUILD     STD  TAPE RECORD                   
*                                                                               
PTRN80   MVI   GOSTAT,C'O'         SAY  THIS IS   AN   ORT  CALL                
         BAS   RE,GO               CALL SUBROUTINE                              
         GOTO1 AREPORT             BUILD     AND  PRINT     REPORT              
*                                                                               
PTRNX    B     EXIT                EXIT,     RETURN    TO   MONACC              
*                                                                               
         DROP  R7                                                               
         EJECT ,                                                                
***********************************************************************         
* LAST TIME RTN (ACCLAST)                                             *         
***********************************************************************         
         SPACE 1                                                                
ACCL     DS    0H                                                               
         TM    SVTEST,SVTSCLR2     CLEAR     TYPE-2    BUFF BACKER/PM ?         
         BZ    ACCL10              NO,  DO   NOT  CLEAR     TYPE-2              
         GOTO1 BUFFALO,DMCB,=C'CLEAR',(2,ADBUFC),1,(X'80',1)                    
*                                                                               
ACCL10   TM    SVTEST,SVTSEST      ESTIMATE  FOR  AGENCY/CLIENT ?               
         BZ    ACCLX               NO,  EXIT                                    
         CLI   ESTSW,YES           ESTIMATE  EL   FOUND ?                       
         BNE   ACCLX               NO,  EXIT                                    
         MVI   ESTSW,C'E'          SET  FLAG FOR  BUILD     TAPE RTN            
         MVC   TRINUM(TR2LEN),SPACES                                            
         ZAP   TRNREC,P0           ZERO TRAN RECEIVABLE     AMOUNT              
         ZAP   TRNINV,P0           ZERO TRAN INVENTORY      AMOUNT  NET         
         ZAP   TRNCOM,P0           ZERO TRAN COMMISSION     AMOUNT              
         ZAP   TRNDIS,P0           ZERO TRAN DISCOUNT       AMOUNT              
         TM    SVTEST,SVTSSTDT     BUILD     STD  TAPE RECORD ?                 
         BZ    ACCLX               NO,  SKIP                                    
         BAS   RE,XXORTN           BUILD     STD  TAPE RECORD                   
*                                                                               
ACCLX    BAS   RE,GO               CALL USER SUBROUTINE                         
         B     EXIT                EXIT,     RETURN    TO   MONACC              
         EJECT ,                                                                
***********************************************************************         
* LAST OF CLIENT (LEVALAST)                                           *         
***********************************************************************         
         SPACE 1                                                                
LEVAL    BAS   RE,GO               LAST LEVA CALL                               
         B     EXIT                                                             
         EJECT ,                                                                
***********************************************************************         
* FIND AGENCY SUBROUTINES                                             *         
***********************************************************************         
         SPACE 1                                                                
LOAD     NTR1                                                                   
         CLI   SVSUBR#,0           DO   WE   HAVE A    SUBROUTINE NUM ?         
         BNE   *+6                 YES, SKIP                                    
         DC    H'0'                NO,  SUBROUTINE     NOT  YET  CODED          
*                                                                               
         L     R1,AUSRSUBS         ->   ADDR OF   USER SUBROUTINE TBL           
         ZIC   R2,SVSUBR#          GET  USER SUBROUTINE     NUMBER              
         BCTR  R2,0                MINUS     ONE                                
         SLL   R2,2                TIMES     FOUR                               
         L     R3,0(R2,R1)         GET  ADDR OF   USER SUBROUTINE               
         ST    R3,AUSERSUB         SAVE ADDR                                    
*                                                                               
         B     EXIT                RETURN    TO   CALLER                        
         EJECT ,                                                                
***********************************************************************         
* GO TO  USER SUBROUTINE IF NEEDED                                    *         
***********************************************************************         
         SPACE 1                                                                
GO       NTR1                                                                   
         CLI   SVSUBR#,0           ANY  USER SUBROUTINES ?                      
         BE    GOX                 NO,  EXIT                                    
         GOTO1 AUSERSUB                                                         
*                                                                               
GOX      B     EXIT                                                             
         EJECT ,                                                                
***********************************************************************         
* FIND AND SAVE FREE FORM NUMBER                                      *         
*      R7 - POINTS TO RECORD                                          *         
*      R8 - NAME SAVED AREA                                           *         
***********************************************************************         
         SPACE 1                                                                
         USING FFNELD,R7           MAP  FREE FORM ELEMENT                       
         SPACE 1                                                                
ACNUM    NTR1                                                                   
         MVI   ELCODE,FFNELQ       X'25' -   FREE FORM NUM  ELEMENT             
         GOTO1 AGETEL              ANY  FREE FORM NUM  ELEMENT ?                
         BNE   EXIT                NO,  EXIT                                    
         SR    R2,R2                                                            
         IC    R2,FFNLN            GET  LENGTH    OF   ELEMENT                  
         SH    R2,=Y(FFNLN1Q+1)                                                 
         EXMVC R2,0(R8),FFNUMBER   MOVE LEVEL-INFO                              
         B     EXIT                EXIT,     RETURN    TO   CALLER              
*                                                                               
         DROP  R7                                                               
         EJECT ,                                                                
***********************************************************************         
* LAST TIME RTN - REQLAST                                             *         
***********************************************************************         
         SPACE 1                                                                
REQL     DS    0H                                                               
         BAS   RE,GO               CALL USER SUBROUTINE                         
*                                  NEW  MEDIA     TOTALS                        
         NI    SWITCHES,TURNOFF-MEDTOTSW                                        
         GOTO1 ACLITOT             PRINT     CLIENT    TOTALS                   
         LA    R2,4                                                             
         LA    R3,P+45                                                          
         LA    R4,REQREC                                                        
*                                                                               
TOT200   GOTO1 AGENEDIT                                                         
         LA    R3,16(,R3)                                                       
         LA    R4,6(,R4)                                                        
         BCT   R2,TOT200                                                        
         CLI   QOPT1,C'Y'          OUTPUT    TAPE REQUIRED ?                    
         BNE   TOT240              NO,  SKIP CLOSE                              
*                                                                               
         CLOSE TAPEOUT             CLOSE     OUTPUT    TAPE FILE                
*                                                                               
TOT240   MVC   P+1(22),=C'* TAPE RECORDS=      *'                               
         EDIT  (P3,TAPECNT),(5,P+16),ZERO=NOBLANK                               
*                                                                               
         MVC   P+27(14),=C'REQUEST-TOTALS'                                      
         GOTO1 APRINTIT                                                         
         MVI   FORCEHED,C'Y'       FORCE     HEADLINES                          
*                                  MAKE RCSUBPRG  A    2    OR   3              
         OI    RCSUBPRG,2          ..   CREATE    NEW  HEADING                  
         XC    BUFKEY,BUFKEY       CLEAR     BUFKEY                             
         MVI   BUFREC,1            FIND TYPE-1    TOTALS                        
         GOTO1 BUFFALO,DMCB,=C'HIGH',(1,ADBUFC),BUFREC,1                        
*                                                                               
TOT280   CLI   DMCB+8,0            DATA STILL     AVAILABLE ?                   
         BNE   REQLX               NO,  EXIT                                    
         LA    R5,P                ->   1ST  PRINT     LINE (STANDARD)          
         CLI   BUFKEY,X'FF'        ANY  OVERALL   TOTALS    BUFFER ?            
         BE    TOT560              YES, PRINT     OVERALL   TOTALS              
         CLI   BUFKEY+1,X'FF'      ANY  MINOR     TOTALS    BUFFER ?            
         BE    TOT520              YES, PRINT     MINOR     TOTALS              
*                                                                               
TOT320   TM    SWITCHES,MEDTOTSW   MEDIA     TOTALS    DONE ?                   
         BO    TOT480              YES, SKIP                                    
         OI    SWITCHES,MEDTOTSW   NEXT PASS MEDIA     TOTALS    DONE           
         GOTO1 APRINTIT            SPACING   LINE                               
         TM    SVTEST,SVTSCHRG     INSIDE    CHARGES   RUN  (NEEDHAM) ?         
         BO    TOT480              YES, NO   MEDIA     TOTALS                   
         L     R7,ADCOMP           ->   CO-AGENCY      RECORD                   
         MVI   ELCODE,PMDELQ       X'11' -   PRODUCTION        MEDIA EL         
         GOTO1 AGETEL              ANY  PRODUCTION     MEDIA     EL ?           
*                                                                               
         USING PMDELD,R7           MAP  PRODUCTION     MEDIA     EL             
TOT360   BNE   TOT440              NO,  SKIP                                    
         CLC   PMDCODE,BUFKEY      MEDIA     MATCH ?                            
         BE    TOT400              YES, CONTINUE                                
         GOTO1 ANEXTEL             NO,  TRY  NEXT ELEMENT                       
         B     TOT360                                                           
*                                                                               
TOT400   MVC   P+4(15),PMDSPACE    MOVE SPACES                                  
*                                                                               
TOT440   MVC   P+2(1),BUFKEY                                                    
         GOTO1 SQUASHER,DMCB,P+2,20                                             
         MVC   WORK40(17),P+2      SAVE FOR  A    FEW  INSTR     DOWN           
         GOTO1 UNDERLIN,DMCB,(20,P+2),PSECOND+2                                 
         LA    R5,PTHIRD           ->    PRINT    3RD  LINE GROUP               
*                                                                               
TOT480   MVC   6(36,R5),BUFCOM     R5   POINTS    TO   P    OR   PTHIRD         
         MVC   2(3,R5),BUFKEY+1                                                 
         B     TOT600                                                           
*                                                                               
TOT520   MVC   32(12,R5),=C'MEDIA-TOTALS'                                       
         MVC   2(17,R5),WORK40                                                  
*                                  NEXT TIME NEW  MEDIA     TOTAL               
         NI    SWITCHES,TURNOFF-MEDTOTSW                                        
         B     TOT600                                                           
*                                                                               
TOT560   GOTO1 APRINTIT            SPACING   LINE                               
         MVC   30(14,R5),=C'REQUEST TOTALS'                                     
*                                                                               
TOT600   LA    R2,4                NUM  OF   COUNTERS  TO   BE   EDITED         
         LA    R3,45(,R5)          ->   PRINTER   BEGINNING ADDR                
         LA    R4,BUFC1+2          ->   1ST  COUNTER   FOR  EDIT                
*                                                                               
TOT640   GOTO1 AGENEDIT            EDIT AMOUNT    TO   PRINT                    
         LA    R3,16(,R3)                                                       
         LA    R4,8(,R4)                                                        
         BCT   R2,TOT640           TRY  AGAIN                                   
         GOTO1 APRINTIT            PRINT     A    LINE(S)                       
         GOTO1 BUFFALO,DMCB,=C'SEQ',(1,ADBUFC),BUFREC,1                         
         B     TOT280                                                           
*                                                                               
REQLX    B     EXIT                EXIT,     RETURN    TO   MONACC              
         DROP  R7                                                               
         EJECT ,                                                                
***********************************************************************         
* RUN LAST CALL (RUNLAST)                                             *         
***********************************************************************         
         SPACE 1                                                                
RUNL     DS    0H                                                               
         BAS   RE,GO               CALL USER SUBROUTINE                         
         CLI   GOSTAT,C'C'         CLOSE     TAPE ?                             
         BNE   RUNLX               NO,  SKIP                                    
         CLOSE TAPEOUT             YES, CLOSE     TAPE                          
*                                                                               
RUNLX    B     EXIT                EXIT,     RETURN    TO   MONACC              
         EJECT ,                                                                
***********************************************************************         
* LOOKUP ROUTINE JOBBER                                               *         
***********************************************************************         
         SPACE 1                                                                
         USING ACMD,R3             MAP  ACMASTD                                 
         USING JBLOCKD,R5          MAP  JOBBER    INTERFACE   BLOCK             
         SPACE 1                                                                
LOOKUP   NTR1                                                                   
         L     R3,AMONACC                                                       
         L     R5,ACMAJOBB                                                      
*                                                                               
         MVC   JBAJOB,ADACC                                                     
         MVC   JBACOLS,ACMACOLL                                                 
         MVC   JBACOM,ADCOMFAC                                                  
         MVC   JBAGOBLK,ADGOBLOC                                                
         MVC   JBAIO,ACMAJOBI                                                   
         MVC   JBAKEY,LASTIO                                                    
         MVC   JBGETOPT,GETOPT                                                  
*                                                                               
         MVC   JBACOLTB,ACMACOL                                                 
         MVC   JBLCOLTB,ACMLCOL                                                 
         MVC   JBAOPVTB,ACMAOPV                                                 
         MVC   JBLOPVTB,ACMLOPV                                                 
*                                                                               
         GOTO1 ACMAJOBR,DMCB,ACMAJOBB                                           
         CLI   JBERROR,X'00'                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,ACMACOL                                                       
         XIT1  REGS=(R3,R5)                                                     
*                                                                               
         DROP  R3,R5                                                            
         EJECT ,                                                                
***********************************************************************         
* BUILDS/WRITES STANDARD(DEFAULT) TAPE RECORD                         *         
***********************************************************************         
         SPACE 1                                                                
         USING XXRECD,R8           MAP  DEFAULT   TAPE RECORD                   
         SPACE 1                                                                
XXORTN   NTR1                                                                   
         LA    R8,TAPEWK           ->   OUTPUT    TAPE AREA                     
         MVC   XXRAGY,SVAGY        AGENCY   (CREATED   IN REQ1ST RTN)           
         MVC   XXRMED(TR1LEN),TRNMED    TR   PREFIX    LABELS   (PTRN)          
         MVC   XXROFF,SVAOFF       UNIT FOR  ANALYSIS (FROM LEVA RTN)           
         MVC   XXRNUM(12),TRINUM   INVOICE   NUMBER/DATE                        
         UNPK  XXRREC,TRNREC       RECEIVABLE                                   
         UNPK  XXRINV,TRNINV       INVENTORY-NET  AMOUNT                        
         UNPK  XXRCOM,TRNCOM       COMMISSION     AMOUNT                        
         UNPK  XXRDIS,TRNDIS       DISCOUNT       AMOUNT                        
         MVC   XXRSTR(12),QSTART   START/END DATES                              
         MVC   XXRDUE(L'XXRDUE+L'XXRIMD),TRNDUE                                 
*                                                                               
         CLC   ALPHAID,=C'SG'                                                   
         BNE   XXO100                                                           
         MVC   XXRAGY,=C'MA'                                                    
         MVI   XXRAGY+2,C'C'                                                    
         B     XXO210                                                           
*                                                                               
XXO100   TM    SVTEST,SVTSSSC      SSC                                          
         BZ    XXO200              NO,  TRY  NEXT                               
         MVC   XXRIMD(4),SVBPRD    YES, MOVE LEVEL-B   PRODUCT                  
         MVC   TRIMED,SVBPRD       TRIMED    ALSO USED IN   REPORT  RTN         
*                                                                               
XXO200   TM    SVTEST,SVTSSCPY     MOVE SUBCOMPANY     NAME (FOR SCALI)         
         BZ    XXO210              NO,  EXIT                                    
         MVC   XXRIMD(1),SVF4      YES, MOVE SUBCOMPANY     NAME                
*                                                                               
XXO210   DS    0H                  WRITE     TO   TAPE                          
         GOTO1 AWRTAPE                                                          
         B     EXIT                EXIT,     RETURN    TO   CALLER              
         DROP  R8                                                               
         EJECT ,                                                                
***********************************************************************         
* MISC ROUTINES                                                       *         
***********************************************************************         
         SPACE 2                                                                
         GETEL R7,DATADISP,ELCODE                                               
         SPACE 2                                                                
         DROP  RA                                                               
         EJECT ,                                                                
***********************************************************************         
* WORKING STORAGE AREA                                                *         
***********************************************************************         
         SPACE 1                                                                
VCONS    DS    0F                                                               
         DC    V(SQUASHER)                                                      
         DC    V(UNDERLIN)                                                      
         DC    V(PERVERT)                                                       
         DC    V(PRNTBL)                                                        
         DC    V(CONVMOS)                                                       
         DC    A(BUFFALOC)                                                      
         DC    A(GETEL)                                                         
         DC    A(FIRSTEL)                                                       
         DC    A(NEXTEL)                                                        
         DC    A(WRTAPE)                                                        
         DC    A(TAPEOUT)                                                       
         DC    A(REPORT)                                                        
         DC    A(READDM)                                                        
         DC    A(CLITOT)                                                        
         DC    A(GETCMOS)                                                       
         DC    A(ADDIT)                                                         
         DC    A(GENEDIT)                                                       
         DC    A(PRINTIT)                                                       
         DC    A(GETFEE)                                                        
         DC    A(DEBUG)                                                         
         DC    A(PRDUMP)                                                        
         DC    A(PUTBUF)                                                        
         DC    A(SKTAB)                                                         
         DC    A(AGYTBL)                                                        
         DC    A(USERSUBS)                                                      
         EJECT ,                                                                
***********************************************************************         
* CONSTANTS                                                           *         
***********************************************************************         
         SPACE 1                                                                
FLDH     DC    AL1(8+L'FLD),4X'00',AL1(L'FLD),AL2(0)                            
FLD      DC    C'OE,CE,CEG'                                                     
*                                                                               
TAPEOUT  DCB   DDNAME=TAPEOUT,DSORG=PS,RECFM=U,LRECL=100,BLKSIZE=100,  X        
               MACRF=PM                                                         
TAPELEN  EQU   *-TAPEOUT           NEEDED TO SAVE TAPEOUT IN TAPESAVE           
*                                                                               
DDPARM   DC    CL8'TAPEOUT'                                                     
DSPARM   DC    CL20'ACCTAPE.AC0I1XXX'                                           
         EJECT ,                                                                
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
* UTILITY ROUTINES                                                    *         
***********************************************************************         
         SPACE 1                                                                
*---------------------------------------------------------------------*         
* BUILD/PRINT REPORT LINE                                             *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
REPORT   NMOD1 0,**REPT**                                                       
         L     RC,ADRC             RESTORE   RC                                 
         CLC   SVCLX,SPACES        ANY  CLIENT    CONTROL   KEY ?               
         BE    REP100              NO,  CONTINUE                                
         CLC   SVCLX,TRNCLI        SAME CLIENT    CONTROL   KEY ?               
         BE    REP100              NO,  CONTINUE                                
         GOTO1 ACLITOT             YES, PRINT     CLIENT    TOTALS              
*                                                                               
REP100   CLC   ALPHAID,=C'BD'      BDNY ?                                       
         BNE   REP200              NO,  SKIP                                    
*                                  **** AS   PER  MASC/HWEI 2/97                
*                                  **** SUPPRESS  JOBS FROM PRINTING            
*                                  **** WHEN RECEIVEABLES   AND                 
*                                  **** INVENTORY ARE  ZERO                     
         CP    TRNREC,P0           REFERENCE NUMBER    ZERO ?                   
         BNE   REP200              NO,  CONTINUE                                
         CP    TRNINV,P0           INVENTORY      AMOUNT    ZERO ?              
         BE    REP800              YES, SKIP                                    
*                                                                               
REP200   MVC   SVCLX,TRNCLI        SAVE CLIENT    CONTROL   KEY                 
         AP    REPCNT,=P'1'        USED IN   CLITOT    ROUTINE                  
         MVC   P+1(3),TRNCLI       CLIENT                                       
         MVC   P+5(3),TRNPRD       PRODUCT                                      
         MVC   P+9(6),TRNJOB       JOB                                          
*                                  FOR  SCALI:                                  
         TM    SVTEST,SVTSSCPY     MOVE COMPANY   NAME TO   REPORT ?            
         BZ    *+10                NO,                                          
         MVC   P+16(1),SVF4        YES, MOVE SUBCOMPANY     TO   REPORT         
*                                                                               
         GOTO1 SQUASHER,DMCB,P+1,(0,16)                                         
         MVC   P+35(6),TRINUM      ID                                           
         MVC   P+34(1),TRIMED      MEDIA                                        
         TM    SVTEST,SVTSSSC      SSC  AGENCY ?                                
         BZ    REP300              NO,  SKIP                                    
         MVI   P+34,C' '           YES, BLANK-OUT INV  MEDIA  INDICATOR         
*                                                                               
REP300   MVC   P+20(1),SVAOFF      UNIT FOR  ANALYSIS                           
         CLC   SV2OFF,SPACES                                                    
         BE    *+10                                                             
         MVC   P+20(2),SV2OFF                                                   
         CLI   TRIDAT,C' '         ANY  INVOICE   DATE ?                        
         BE    REP400              NO,  SKIP --   DO   NOT  USE                 
         GOTO1 DATCON,DMCB,(0,TRIDAT),(8,P+25)                                  
*                                                                               
REP400   LA    R2,4                NUM  OF   AMOUNT    COUNTERS                 
         LA    R3,P+45             STARTING  AMOUNT    PRINT   POSITION         
         LA    R4,TRNREC           STARTING  AMOUNT    FIELD                    
         LA    R5,CLTREC           STARTING  CLIENT-MINOR   TOTAL   FLD         
*                                                                               
REP500   GOTO1 AGENEDIT            EDIT AMOUNT    TO   PRINT                    
         AP    0(6,R5),0(6,R4)     ADD  AMOUNT    FOR  MINOR-TOTAL              
         LA    R3,16(,R3)                                                       
         LA    R4,6(,R4)                                                        
         LA    R5,6(,R5)                                                        
         BCT   R2,REP500                                                        
*                                                                               
REP600   TM    SVTEST,SVTSSSC      SSC ?                                        
         BZ    REP700              NO,  SKIP                                    
         CLI   SVSUBR#,SVSBNE2#    NEEDHAM   SUBROUTINE     2 ?                 
         BE    REP700              YES, SKIP                                    
         BAS   RE,SSCPRT           YES, SSC  CUSTOM    PRINT     RTN            
*                                                                               
REP700   MVC   PSECOND,P2X         IN   CASE A    2ND  LINE TO   PRINT          
         GOTO1 APRINTIT                                                         
         CLC   P2X,SPACES          P2X  LINE BLANK ?                            
         BE    REP800              YES, SKIP                                    
         MVC   P2X,SPACES          CLEAR     P2X  LINE                          
         GOTO1 APRINTIT                                                         
*                                                                               
REP800   ZAP   BUFC4,TRNDIS                                                     
         ZAP   BUFC3,TRNCOM                                                     
         ZAP   BUFC2,TRNINV                                                     
         ZAP   BUFC1,TRNREC        REFERENCE NUMBER                             
         MVC   BUFKEY+4(4),HEXFF                                                
         MVI   BUFKEY+3,C' '                                                    
         MVC   BUFKEY+1(2),TRINUM  SEE  PTRN ROUTINE                            
         MVI   BUFKEY,C' '                                                      
         MVI   BUFREC,1            TYPE-1    TOTALS                             
         TM    SVTEST,SVTSCHRG     INSIDE    CHARGES   RUN  (NEEDHAM) ?         
*                                  YES, USE  2-BYTE                             
         BO    REP900                   INSIDE    CHARGES   CODE                
         MVC   BUFCOM,SVANAME      CLIENT    NAME     (SEE  LEVA RTN)           
         MVC   BUFKEY(L'TRNMED+L'TRNCLI),TRNMED                                 
         GOTO1 APUTBUF             1ST  OF   3    PUT  BUFFALO                  
         MVC   BUFKEY+1(3),HEXFF   TOTAL     BY   JOB, 1ST  BYTE=MEDIA          
*                                                                               
*                                  2ND  GOTO BUFFALO -                          
REP900   GOTO1 APUTBUF             1ST  FOR  INSIDE    CHARGES                  
         MVC   BUFCOM,SPACES                                                    
         MVC   BUFKEY(4),HEXFF     FOR  OVERALL   TOTAL                         
         GOTO1 APUTBUF             3RD  GOTO BUFFALO                            
*                                                                               
         XIT1  ,                   RETURN    TO   CALLER                        
         EJECT ,                                                                
*---------------------------------------------------------------------*         
* SSC CUSTOM PRINT LINE                                               *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
SSCPRT   DS    0H                                                               
         CLC   SAVBLPR,SPACES      SAVBLPR   CREATED   BY   PACC                
         BE    SSC200                                                           
         MVC   P2X+25(L'SAVBLPR),SAVBLPR                                        
         MVC   P2X+15(9),=C'ORGI EST='                                          
*                                                                               
SSC200   MVC   P2X+78(12),=C'CLI/PRD NO.='                                      
         MVC   P2X+90(4),SVBPRD                                                 
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
         EJECT ,                                                                
*---------------------------------------------------------------------*         
* DATA MANGER INTERFACE                                               *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
READDM   NMOD1 0,**RDDM**                                                       
         L     RC,ADRC             RESTORE   RC                                 
         LR    R8,R1                                                            
         GOTO1 DATAMGR,DMCB,COMMAND,=C'ACCOUNT',(R8),(R8)                       
*                                                                               
         XIT1  ,                   RETURN    TO   CALLER                        
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
*---------------------------------------------------------------------*         
* PRINTING INTERFACE                                                  *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
PRINTIT  NMOD1 0,**PRIT**                                                       
         L     RC,ADRC             RESTORE   RC                                 
         MVC   HEAD3+78(L'NHIHEAD),NHIHEAD                                      
         GOTO1 ACREPORT                                                         
*                                                                               
PRINTX   XIT1  ,                   EXIT,     RETURN    TO   CALLER              
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
*---------------------------------------------------------------------*         
* EDITING INTERFACE                                                   *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
GENEDIT  NMOD1 0,**GEDT**                                                       
         L     RC,ADRC             RESTORE   RC                                 
         EDIT  (P6,(R4)),(15,(R3)),2,COMMAS=YES,MINUS=YES                       
*                                                                               
         XIT1  ,                   EXIT,     RETURN    TO   CALLER              
*                                                                               
         LTORG                                                                  
         EJECT ,                                                                
*---------------------------------------------------------------------*         
* PUT RECORD TO BUFFALO                                               *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
PUTBUF   NMOD1 0,**PBUF**                                                       
         L     RC,ADRC             RESTORE   RC                                 
         ZAP   BUFC5,=P'1'         DUMMY,    TO   KEEP ZERO ACCUMS              
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUFC,BUFREC                               
*                                                                               
         XIT1  ,                   EXIT,     RETURN    TO   CALLER              
*                                                                               
         LTORG                                                                  
         EJECT ,                                                                
*---------------------------------------------------------------------*         
* PRINTS CLIENT TOTALS ROUTINE                                        *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
CLITOT   NMOD1 0,**PBUF**                                                       
         L     RC,ADRC             RESTORE   RC                                 
         CP    REPCNT,P0           NO   REPORT    LINES     PRINTED ?           
         BE    CLITOTX             YES, EXIT                                    
         MVC   P+28(13),=C'CLIENT-TOTALS'                                       
         LA    R2,4                                                             
         LA    R3,P+45                                                          
         LA    R4,CLTREC                                                        
         LA    R5,REQREC                                                        
*                                                                               
CLI200   GOTO1 AGENEDIT                                                         
*                                  ADD  CLIENT    AMOUNT    TO                  
         AP    0(6,R5),0(6,R4)          REQUEST   TOTALS                        
         ZAP   0(6,R4),P0          ZERO-OUT  CLIENT    AMOUNT    FIELD          
         LA    R3,16(,R3)                                                       
         LA    R4,6(,R4)                                                        
         LA    R5,6(,R5)                                                        
         BCT   R2,CLI200                                                        
*                                                                               
         CP    REPCNT,=P'1'                                                     
         BE    CLI300                                                           
         GOTO1 APRINTIT                                                         
*                                                                               
CLI300   MVC   P,SPACES            BLANK-OUT PRINT     LINE                     
         GOTO1 APRINTIT                                                         
         ZAP   REPCNT,P0                                                        
*                                                                               
CLITOTX  XIT1  ,                   EXIT,     RETURN    TO   CALLER              
*                                                                               
         LTORG                                                                  
         EJECT ,                                                                
*---------------------------------------------------------------------*         
* GET THE TRANSACTION MOS (YEAR MONTH) IN DISPLAY MODE YYMM           *         
*                                                                     *         
*   INPUT:                                                            *         
*     R1 - A(OUTPUT AREA) - 3 BYTES                                   *         
*                                                                     *         
*   USES:                                                             *         
*     ADTRANS  - ADDRESS OF THE TRANSACTION ELEMENT                   *         
*     MOSWK    - WORK AREA FOR MOS DATE                               *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
GETCMOS  NMOD1 0,**GMOS**                                                       
         L     RC,ADRC             RESTORE   RC                                 
         LR    R7,R1               GET  OUTPUT    ADDRESS                       
*                                  CONVERT   MOS  DATE                          
         GOTO1 CONVMOS,DMCB,ADTRANS,MOSWK                                       
         MVI   MOSWK+2,X'01'       AFTER     YYMM INSERT    X'01'               
         GOTO1 DATCON,DMCB,(1,MOSWK),(X'20',WORK)                               
         MVC   0(4,R7),WORK        ONLY RETURN    FOUR CHARACTERS               
*                                                                               
         XIT1  ,                   EXIT,     RETURN    TO   CALLER              
*                                                                               
         LTORG                                                                  
         EJECT ,                                                                
*---------------------------------------------------------------------*         
* ADD THE RECORD TO THE TABLE                                         *         
*                                                                     *         
*   INPUT:                                                            *         
*     R1 - A(ELEMENT)                                                 *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
ADDIT    NMOD1 0,**ADIT**                                                       
         L     RC,ADRC             RESTORE   RC                                 
         CLI   QOPT7,C'X'                                                       
         BNE   ADD10                                                            
         MVC   MSG,=CL10'ELEMENT'  DUMP THE  ELEMENT                            
         LR    R7,R1                                                            
         ZIC   R3,1(,R7)           GET  LENGTH    OF   THE  ELEMENT             
         GOTO1 ADEBUG,DMCB,(R7),(R3)                                            
         MVC   MSG,=CL10'ADD RECORD'    RECORD    BEING     ADDED               
         GOTO1 ADEBUG,DMCB,SKREC,SKLEN                                          
*                                                                               
ADD10    L     R3,ASKTAB                                                        
         LA    R5,SKREC                                                         
         GOTO1 BINADD,DMCB,(R5),(R3)                                            
*                                                                               
ADDITEX  XIT1  ,                   EXIT,     RETURN    TO   CALLER              
         EJECT ,                                                                
*---------------------------------------------------------------------*         
* ROUTINE TO ADD TO A BINSRCH TABLE                                   *         
*                                                                     *         
*   INPUT:                                                            *         
*     PARM1 - A(DATA TO BE ADDED)                                     *         
*     PARM2 - A(BINSRCH PARMS)                                        *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
         USING BIND,R5             MAP  BINARY    SEARCH    TBL  FORMAT         
         SPACE 1                                                                
BINADD   NTR1                                                                   
         L     R3,0(,R1)           ->   A(RECORD)                               
         L     R5,4(,R1)           BINSRCH   PARAMETERS                         
         MVC   DMCB+8(16),BININ                                                 
         LA    R2,BINTABLE                                                      
         GOTO1 BINSRCH,DMCB,(1,(R3)),(R2)                                       
         OC    DMCB(4),DMCB                                                     
         BNZ   *+6                                                              
         DC    H'0'                TABLE     IS   FULL                          
*                                                                               
         MVC   BININ,DMCB+8        UPDATE    COUNT                              
         CLI   DMCB,1                                                           
         BE    BINXIT              NOT  FOUND,    ADDED                         
         L     R4,DMCB             A(RECORD  FOUND)                             
         ZIC   R6,BINFRST          DISP TO   1ST  BUCKET                        
         AR    R4,R6               RECORD    FOUND                              
         AR    R3,R6               NEW  RECORD                                  
         ZIC   R0,BINNUMB          NUMBER    OF   BUCKETS                       
*                                                                               
BIN20    AP    0(8,R4),0(8,R3)     ADD  NEW  TO   OLD                           
         LA    R4,8(,R4)                                                        
         LA    R3,8(,R3)                                                        
         BCT   R0,BIN20                                                         
*                                                                               
BINXIT   B     ADDITEX             RETURN    TO   CALLER                        
         DROP  R5                                                               
*                                                                               
         LTORG                                                                  
         EJECT ,                                                                
*---------------------------------------------------------------------*         
* WRITE TAPE RECORD                                                   *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
WRTAPE   NMOD1 0,**WTAP**                                                       
         L     RC,ADRC             RESTORE   RC                                 
*                                                                               
         CLC   QOPT6(2),SPACES     PARTIAL   I/O  DUMP REQUIRED ?               
         BE    WRT300              NO,  WRITE     OUTPUT    TAPE                
         AP    DUMPCNT,=P'1'                                                    
         BO    WRT300              ON   OVERFLOW, SKIP                          
         ZAP   DUB,DUMPCNT                                                      
         DP    DUB,EVERY                                                        
         CP    DUB+L'DUB-L'EVERY(L'EVERY),P0                                    
         BNE   WRT300                                                           
         CP    PDUMP,MAXCNT        MAX  RECORDS   TO   BE   DUMPED ?            
         BNL   WRT300              YES, SKIP                                    
         AP    PDUMP,=P'1'                                                      
         CLI   QOPT6,C'Y'          PARTIAL   INPUT     DUMP REQUIRED ?          
         BNE   WRT200              NO,  SKIP                                    
         MVC   MSG,=CL10'TRNREC'                                                
         LA    R4,MSG                                                           
         LA    R0,6                                                             
*                                                                               
         USING ACTRECD,R3          MAP  ACCOUNT   RECORD                        
         L     R3,ADTRKEY                                                       
         LH    R2,ACTRLEN                                                       
         DROP  R3                                                               
*                                                                               
         GOTO1 APRDUMP             DUMP INPUT     RECORD                        
*                                                                               
WRT200   CLI   QOPT7,C'Y'          PARTIAL   OUTPUT    TAPE DUMP REQ ?          
         BNE   WRT300              NO,  SKIP TAPE DUMP                          
         MVC   MSG,=CL10'OUTPUT'                                                
         LA    R4,MSG                                                           
         LA    R0,6                                                             
         LA    R3,TAPEWK           ->   OUTPUT    TAPE AREA                     
         LH    R2,SVRECL                                                        
         GOTO1 APRDUMP             DUMP OUTPUT    TAPE RECORD                   
*                                                                               
WRT300   AP    TAPECNT,=P'1'       RUNNING   COUNT     OF   TAPE RCDS           
         CLI   QOPT1,C'Y'          OUTPUT    TAPE REQUIRED ?                    
         BNE   WREXIT              NO,  EXIT                                    
*                                      (DOWN-LOAD DOES NOT  WORK)               
*        BNE   WRT400              NO,  TRY  DOWN-LOAD-DOES NOT WORK            
*                                                                               
         L     R5,ATAPEOUT                                                      
         PUT   (R5),TAPEWK         YES, WRITE     OUTPUT    TAPE FILE           
         B     WREXIT                                                           
*                                                                               
WREXIT   XIT1                      EXIT,     RETURN   TO   CALLER               
*                                                                               
         LTORG                                                                  
         EJECT ,                                                                
*---------------------------------------------------------------------*         
* SAVE BILLED CHARGES BY BILL NUMBER/WORKCODE                         *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
         USING TRNELD,R6           TRANSACTION    ELEMENT                       
         SPACE 1                                                                
GETFEE   NMOD1 0,**GFEE**                                                       
         L     RC,ADRC             RESTORE   RC                                 
         L     R6,ADTRANS                                                       
         CLC   TRNANAL,=C'99'      PROCESS   99'S IN   ORTN SECTION             
         BE    GETFEEX                                                          
         CLC   TRNANAL,SVWCCODE    RATE FOR  THIS W/C  AVAILABLE ?              
         BE    GETFEE10            YES, SKIP                                    
         MVC   SVWCCODE,TRNANAL    SAVE NEW  WORKCODE  DATA                     
*                                                                               
         USING ACGOD,R2            MAP  ACGOBLOCK DSECT                         
         L     R2,ADGOBLOC                                                      
         MVC   GOSELWC,TRNANAL     WORK CODE                                    
         GOTO1 GETOPT,DMCB,ACGOD                                                
         XC    GOSELWC,GOSELWC                                                  
         ZAP   SVRATE,GOAGYCOM     GET  AGENCY    COMMISSION     RATE           
         DROP  R2                                                               
*                                                                               
*                                  SINCE     NOT  99   TYPE WORK-CODE,          
GETFEE10 MVI   PRCSS,NO                 BYPASS    TRANSACTION                   
         MVI   BUFREC,2            TYPE OF   ACCUMULATORS                       
*                                                                               
         USING ACMD,R7             MAP  ACMASTD                                 
         L     R7,AMONACC          ->   1ST  PTAEL                              
         L     R7,ACMAPRO2                                                      
*                                                                               
         USING PTAEL,R7            PROD TRANSACTION    ACTIVITY  EL             
         CLI   0(R7),PTAELQ        ANY  PROD TRANSACTION    ACTIVITY ?          
         B     GETFEE25                                                         
*                                                                               
GETFEE20 MVI   ELCODE,PTAELQ       X'77' -   PROD TRANSACTION    ACT  E         
         GOTO1 ANEXTEL             ANY  PROD TRANSACTION    ACTIVITY ?          
*                                                                               
GETFEE25 BNE   GETFEEX             NO,  CHECK     REVERSALS                     
         CLI   PTATYPE,PTATRAL     BILLING   ELEMENT ?                          
         BNE   GETFEE20            NO,  GET  NEXT ELEMENT                       
         TM    PTASTAT1,PTASPEND   IS   IT   PENDING ?                          
         BO    GETFEE20            YES, GET  NEXT ELEMENT                       
         TM    PTASTAT1,PTASREVU   REVERSAL ?                                   
         BO    GETFEE30            YES, PROCESS   REVERSAL                      
         TM    PTASTAT1,PTASREVS   REVERSAL  OF   ALLOCATION ?                  
         BO    GETFEE40            PROCESS   ALLOCATION     REVERSAL            
*                                                                               
         MVC   BUFINV,PTARBLNO     MOVE BILLING   NUMBER                        
         ZAP   DUB,PTANET          BILLING   AMOUNT                             
         ZAP   BUFC1,DUB           RECEIVABLE     FOR  WORK CODE                
*                                  N/C  AMOUNT -                                
         ZAP   BUFC2,P0                (ASSUME    COMMISSIONABLE)               
         ZAP   BUFC3,DUB           RECEIVABLE    (WILL ADD  COMMISION)          
         ZAP   BUFC4,DUB           SAVE PURE NET  AMOUNTS                       
         BAS   RE,SETBUFF                                                       
         BAS   RE,PRTBUFF                                                       
         GOTO1 APUTBUF             PUT  TO   BUFFALO                            
         B     GETFEE20                                                         
*                                                                               
*                                  REVERSAL  OF   THIS BILL UPDATED             
*                                       X'02'                                   
GETFEE30 CLI   SVSUBR#,SVSBBS1#    IN   BSNY SUBROUTINE ?                       
         BNE   GETFEE35            NO,  SKIP                                    
         CLC   PTARBLDT,BSUDAT     BEFORE    START     DATE MINUS  10 ?         
         BL    GETFEE20            YES, GET  NEXT ELEMENT                       
*                                                                               
GETFEE35 XC    BUFINV,BUFINV                                                    
         MVC   BUFINV(4),=X'FEFEFEFE'   MOVE HIGH VALUES   TO   INV NUM         
         MVC   BUFINV+4(2),PTARBLDT     SAVE ORIGINAL  BILLING  DATE            
         ZAP   BUFC1,TRNAMNT       RECEIVABLE     FOR  WORK CODE                
*                                  N/C  AMOUNT -                                
         ZAP   BUFC2,P0                (ASSUME    COMMISSIONABLE)               
         ZAP   BUFC3,TRNAMNT       RECEIVABLE    (WILL ADD  COMMISION)          
         ZAP   BUFC4,TRNAMNT       SAVE PURE NET  AMOUNTS                       
         BAS   RE,SETBUFF                                                       
         BAS   RE,PRTBUFF                                                       
         GOTO1 APUTBUF             PUT  TO   BUFFALO                            
         B     GETFEE20                                                         
*                                                                               
*                                  REVERSAL       ALLOCATION    X'08'           
GETFEE40 CLI   SVSUBR#,SVSBBS1#    IN   BSNY SUBROUTINE ?                       
         BNE   GETFEE45            NO,  SKIP                                    
         CLC   PTARDATE,BSUDAT     BEFORE    START     DATE MINUS  10 ?         
         BL    GETFEE20            YES, GET  NEXT ELEMENT                       
*                                                                               
GETFEE45 XC    BUFINV,BUFINV                                                    
         MVC   BUFINV(4),=X'FFFFFFFF'   HIGH VALUES    TO   INV  NUMBER         
         MVC   BUFINV+4(2),PTARDATE     SAVE DATE REVERSED                      
         ZAP   BUFC1,TRNAMNT       RECEIVABLE     FOR  WORK CODE                
*                                  N/C  AMOUNT    -                             
         ZAP   BUFC2,P0                 ASSUME    COMMISSIONABLE)               
         ZAP   BUFC3,TRNAMNT       RECEIVABLE    (WILL ADD  COMMISION)          
         ZAP   BUFC4,TRNAMNT       SAVE PURE NET  AMOUNTS                       
         BAS   RE,SETBUFF                                                       
         BAS   RE,REVBUFF          REVERSE   AMOUNTS                            
         BAS   RE,PRTBUFF                                                       
         GOTO1 APUTBUF             PUT  TO   BUFFALO                            
         B     GETFEE20                                                         
*                                                                               
GETFEEX  XIT1  ,                   RETURN    TO   CALLER                        
         DROP  R6,R7                                                            
         EJECT ,                                                                
*---------------------------------------------------------------------*         
* SET DATA IN BUFFALO RECORD                                          *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
         USING TRNELD,R6           TRANSACTION    ELEMENT                       
         SPACE 1                                                                
SETBUFF  NTR1                                                                   
         L     R6,ADTRANS          ->   TRANSACTION    ELEMENT                  
         L     R1,ADTRKEY          ->   TRANSACTION    RCD  KEY                 
*                                                                               
         LR    R7,R6               ->   TRANSACTION    ELEMENT                  
         MVI   ELCODE,SCIELQ       X'50' -   SUBSIDIARY     CASH INFO           
         GOTO1 ANEXTEL             ANY  CASH INFO ELEMENT ?                     
         BNE   SETBUF20            NO,  SKIP                                    
*                                                                               
         USING SCIELD,R7           SUBSIDIARY     CASH INFO ELEMENT             
         CLI   SCITYPE,SCITCDSC    CASH DISCOUNT  AMOUNT    ELEMENT ?           
         BNE   SETBUF20            NO,  SKIP                                    
         SP    BUFC1,SCIAMNT       SUBTRACT  CASH DISCOUNT  AMOUNT              
*                                                                               
SETBUF20 MVC   BUFWKC,TRNANAL      MAKE WORK-CODE PART OF   BUFREC  KEY         
         ZAP   PL16,P0             INIT COMMISSION     TO   ZERO                
         TM    TRNSTAT,TRNSNOCM    X'01'     NON-COMMISSIONABLE  ITEM ?         
         BO    SETBUF30                                                         
*                                                                               
         ZAP   PL16,BUFC1          RECIEVABLE     FOR  W/C                      
         MP    PL16,SVRATE                                                      
         SRP   PL16,64-6,5                                                      
         AP    BUFC1,PL16                                                       
*                                                                               
*                                  FIND GROSS     AMOUNT    BEFORE              
         ZAP   PL16,BUFC3               CD   REMOVED                            
         MP    PL16,SVRATE                                                      
         SRP   PL16,64-6,5                                                      
         AP    BUFC3,PL16                                                       
*                                                                               
SETBUF30 CP    PL16,P0             ANY  COMMISSION     AMOUNT ?                 
         BNE   *+10                YES, SKIP                                    
         ZAP   BUFC2,BUFC3         SAVE NON-COM   BEFORE    CD                  
         B     GETFEEX                                                          
         DROP  R6,R7                                                            
         EJECT ,                                                                
*---------------------------------------------------------------------*         
* PRINT RECORD BEING PUT TO BUFFALO                                   *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
PRTBUFF  NTR1                                                                   
         CLC   QUESTOR,=CL12'PRTBUCKS'                                          
         BNE   PRTBUFX                                                          
         MVC   P(6),BUFINV                                                      
         CLI   BUFINV,X'FA'                                                     
         BL    PRTBUF20                                                         
         GOTO1 DATCON,DMCB,(2,BUFINV+4),(0,P)                                   
*                                                                               
PRTBUF20 MVC   P+7(2),BUFWKC                                                    
         EDIT  (P8,BUFC1),(10,P+10),2,MINUS=YES                                 
         EDIT  (P8,BUFC2),(10,P+20),2,MINUS=YES                                 
         EDIT  (P8,BUFC3),(10,P+30),2,MINUS=YES                                 
         GOTO1 ACREPORT                                                         
*                                                                               
PRTBUFX  B     GETFEEX                                                          
*                                                                               
         EJECT ,                                                                
*---------------------------------------------------------------------*         
* REVERSE SIGNS IN BUFFALO                                            *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
REVBUFF  NTR1                      REVERSE SIGNS                                
         MP    BUFC1,=P'-1'                                                     
         MP    BUFC2,=P'-1'                                                     
         MP    BUFC3,=P'-1'                                                     
         MP    BUFC4,=P'-1'                                                     
         B     GETFEEX                                                          
         LTORG                                                                  
         EJECT ,                                                                
*---------------------------------------------------------------------*         
* DEBUG ROUTINE                                                       *         
*                                                                     *         
*   INPUT:                                                            *         
*     PARM 1   ADDRESS OF DATA                                        *         
*     PARM 2   LENGTH  OF DATA                                        *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
DEBUG    NMOD1 0,**PDMP**                                                       
         L     RC,ADRC             RESTORE   RC                                 
         CP    DMPTOT,MAXDMP       WAS  MAXIMUM   REACHED ?                     
         BNL   DEBUGX              YES, EXIT                                    
         AP    DMPTOT,=P'1'                                                     
         LA    R0,L'MSG                                                         
         LA    R4,MSG                                                           
         L     R3,0(,R1)           ADDRESS   OF   DATA                          
         L     R2,4(,R1)           LENGTH    OF   DATA                          
         GOTO1 APRDUMP             DUMP THE  DATA                               
*                                                                               
DEBUGX   XIT1  ,                   RETURN    TO   CALLER                        
*                                                                               
         LTORG                                                                  
         EJECT ,                                                                
*---------------------------------------------------------------------*         
* PRINT DUMP                                                          *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
PRDUMP   NMOD1 0,**PDMP**                                                       
         L     RC,ADRC             RESTORE   RC                                 
         GOTO1 PRNTBL,DMCB,((R0),(R4)),(R3),C'DUMP',(R2),=C'2D'                 
*                                                                               
         XIT1  ,                   RETURN    TO   CALLER                        
*                                                                               
         LTORG                                                                  
         EJECT ,                                                                
         TITLE 'ACI102BI - EDI STYLE BILLING INTERFACE ROUTINE'                 
         TITLE 'ACI102BD - BDNY - PRODUCTION INTERFACE ROUTINE'                 
SUBRBD   NMOD1 0,**I1BD**                                                       
         L     RC,ADRC             RESTORE   RC                                 
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    BDREQF                                                           
         CLI   MODE,LEVAFRST                                                    
         BE    BDLEVA                                                           
         CLI   MODE,PROCACC                                                     
         BE    BDPACC                                                           
         CLI   MODE,PROCTRNS       TWO  ENTRIES   WITH PTRNS                    
         BE    BDPTRN              GOSTAT    SET  TO   EITHER    I OR O         
         CLI   MODE,REQLAST                                                     
         BE    BDREQL                                                           
*                                                                               
BDEXIT   XIT1  ,                                                                
         EJECT ,                                                                
***********************************************************************         
* PROCESS REQUEST FIRST                                               *         
***********************************************************************         
         SPACE 1                                                                
BDREQF   DS    0H                  1ST  FOR  REQUEST                            
         PACK  BDOSEQ,=C'1'        INITIALIZATION OF   BDNY FIELDS              
         ZAP   BDOTOT,P0                                                        
         MVI   BDTEST,0                                                         
         MVI   BDSTAT,0                                                         
         MVC   BDFDEA,SPACES                                                    
         MVC   BDFDACTD,SPACES                                                  
         MVC   BDFUDGEA,SPACES                                                  
         MVC   BDFUDGAC,SPACES                                                  
         XC    BDJBEA,BDJBEA                                                    
         XC    BDJBAC,BDJBAC                                                    
*                                                                               
         MVC   WORK(5),=5C'0'                                                   
         MVZ   WORK(5),QSELECT                                                  
         CLC   WORK(5),=5C'0'                                                   
         BNE   BDREQFX                                                          
         PACK  BDOSEQ,QSELECT(5)                                                
*                                                                               
BDREQFX  B     BDEXIT                                                           
         EJECT ,                                                                
***********************************************************************         
* PROCESS LEVEL A                                                     *         
***********************************************************************         
         SPACE 1                                                                
BDLEVA   DS    0H                  PROCESS   CLIENT    LEVEL                    
         MVI   FCRDTRNS,C'Y'       INIT READ TRANSACTION                        
         L     R2,ADHEIRA          A(CLIENT  KEY)                               
         CLC   3(3,R2),=C'CC '     CHRYSLER  CLIENT    CODE CC ?                
         BE    BDLEVAX             YES, OKAY READ TRANSACTION                   
         CLC   3(3,R2),=C'CD '                                                  
         BE    BDLEVAX                                                          
         CLC   3(3,R2),=C'CE '                                                  
         BE    BDLEVAX                                                          
         CLC   3(3,R2),=C'XCC'                                                  
         BE    BDLEVAX                                                          
         CLC   3(3,R2),=C'DS '                                                  
         BE    BDLEVAX                                                          
*                                  IF   NOT  ONE  OF   ABOVE    CLIENTS         
         MVI   FCRDTRNS,C'N'            DO   NOT  READ TRANSACTIONS             
*                                                                               
BDLEVAX  B     BDEXIT              RETURN    TO   CALLER                        
         EJECT ,                                                                
***********************************************************************         
* PROCESS ACCOUNT                                                     *         
***********************************************************************         
         SPACE 1                                                                
BDPACC   DS    0H                  PROCESS   ACCOUNT                            
         L     R2,ADACC            R2=  A(ACCOUNT RECORD)                       
         CLC   3(3,R2),=C'PDO'     CLIENT    CODE 'PDO'-PENTACOM DODGE?         
         BNE   BDPACC10                                                         
         CLC   6(3,R2),=C'GEN'     AND  PROD CODE 'GEN'-DODGE PENTACOM          
         BNE   BDPACC10                                                         
         CLI   9(R2),C'A'          AND  MEDIA     CODE 'A'                      
         BNE   BDPACC10                                                         
         MVI   FCRDTRNS,C'Y'       READ TRANSACTIONS                            
*                                                                               
BDPACC10 BAS   RE,BDLDUSER         EXTRACT   USER FIELD     DATA                
         B     BDEXIT              EXIT,     RETURN    TO   CALLER              
         EJECT ,                                                                
***********************************************************************         
* PROCESS TRANSACTION (GOSTAT BYTE SET TO 'I')                        *         
***********************************************************************         
         SPACE 1                                                                
*                                  BBDO INPUT     ROUTINE                       
BDPTRN   DS    0H                      (PROCTRNS  AND  I)                       
         CLI   GOSTAT,C'I'         SET  IN   MAIN PROCTRNS  ROUTINE             
         BNE   BDPTRNO                                                          
*                                                                               
*                                  R7=  ADTRANS,  R8=  ADACC,                   
*                                       SEE  BDPTRNO                            
         USING TRNELD,R7           MAP  TRANSACTION    ELEMENT                  
         L     R7,ADTRANS                                                       
         B     BDEXIT              EXIT,     RETURN    TO   CALLER              
         DROP  R7                                                               
         EJECT ,                                                                
***********************************************************************         
* REQUEST LAST PROCESSING                                             *         
***********************************************************************         
         SPACE 1                                                                
BDREQL   DS    0H                  END  OF   REQUEST   RUN                      
         MVC   SVANAME,SPACES                                                   
         B     BDPTO20                                                          
         EJECT ,                                                                
***********************************************************************         
* PROCESS TRANSACTION (GOSTAT BYTE SET TO 'O')                        *         
***********************************************************************         
         SPACE 1                                                                
BDPTRNO  DS    0H                                                               
         CLI   GOSTAT,C'O'                                                      
         BNE   BDPTOX                                                           
*                                                                               
         MVI   BDSTAT,0                                                         
         CLC   BDJBAC,SPACES       AC   #    FROM USER FIELD ?                  
         BE    BDPTO10             NO                                           
         MVC   BDFDACTD,BDJBAC                                                  
         OI    BDSTAT,BDSTUFAC                                                  
*                                                                               
BDPTO10  CLC   BDJBEA,SPACES       ANY  EA   #    USER FIELD ?                  
         BE    BDPTO180            NO,  PROCESS   TRANSACTION                   
         MVC   BDFDEA,BDJBEA                                                    
         OI    BDSTAT,BDSTUFEA                                                  
         B     BDPTO180                                                         
*                                                                               
BDPTO20  MVI   SVAOFF,C' '         BLANK-OUT UNIT FOR  ANALYSIS (LEVA)          
         LA    R8,TAPEWK+80        ->   1K   DM   IO,  1ST  80=TAPE OUT         
         ST    R8,ADTRKEY          IN   CASE NEEDED    BY   WRTAPE  RTN         
         L     R6,=A(BDOTBL)                                                    
*                                                                               
BDPTO30  CLI   0(R6),X'FF'         END  OF   ACC  NUM  TBL  REACHED ?           
         BE    BDPTO240            YES, DO   END  OF   BDPTRNO   INSTR          
*                                                                               
         BAS   RE,BDGETAC#         USE  SR   ACCOUNT   TO   GET  ACNO           
*                                                                               
         MVC   15(27,R8),SPACES    BLANK-OUT KEY  AREA 15+27=42                 
         MVC   0(1,R8),RCCOMPFL                                                 
         MVC   1(14,R8),0(R6)                                                   
         MVC   COMMAND,=C'DMRDHI'                                               
         GOTO1 AREADDM,(R8)                                                     
         CLC   1(14,R8),0(R6)                                                   
         BE    BDPTO50                                                          
*                                                                               
BDPTO40  LA    R6,L'BDOTBL(,R6)    ->   NEXT ACC-NUM                            
         B     BDPTO30                                                          
*                                                                               
BDPTO50  MVC   COMMAND,=C'DMRSEQ'  READ TRANSACTIONS   ON   THE ACCOUNT         
         GOTO1 AREADDM,(R8)                                                     
         CLC   1(14,R8),0(R6)      ANY  (MORE)    TRANSACTIONS                  
         BNE   BDPTO40             NO,  GET  NEXT ACCOUNT                       
*                                                                               
         MVI   BDTEST,0            INITIALLY RESET     TO   HEX  ZERO           
         MVI   BDSTAT,0                                                         
         MVC   SVEA(SV1LEN),SPACES BLANK     OUT  SVEA/SVAC/ETC                 
         ZAP   TRNREC,P0           ZERO TRAN RECEIVABLE     AMOUNT              
         ZAP   TRNINV,P0           ZERO TRAN INVENTORY      AMOUNT  NET         
         ZAP   TRNCOM,P0           ZERO TRAN COMMISSION     AMOUNT              
         ZAP   TRNDIS,P0           ZERO TRAN DISCOUNT       AMOUNT              
*                                                                               
         MVC   TRNCLI(12),3(R8)    CLI,PRD,JOB                                  
         MVC   TRNMED,TRNJOB                                                    
         LR    R7,R8               ->   TRAN RECORD----KEY                      
         AH    R7,DATADISP         ->   1ST  TRAN ELEMENT                       
*                                                                               
BDPTO60  CLI   0(R7),0             ANY  MORE ELEMENTS ?                         
         BE    BDPTO160            NO,  SEE  IF   WE   HAD  ANY  27/44          
         CLI   0(R7),ABIELQ        27   BILLING   ELEMENT ?                     
         BE    BDPTO80             YES, GET  BILLING   INFO                     
         CLI   0(R7),TRNELQ        44   TRANSACTION    ELEMENT ?                
         BE    BDPTO90             YES, GET  TRANSACTION    INFO                
         CLI   0(R7),SCIELQ        50   CASH ELEMENT ?                          
         BE    BDPTO100            YES, GET  CASH INFO                          
         CLI   0(R7),TRSELQ        60   STATUS    ELEMENT ?                     
         BE    BDPTO110            YES, GET  STATUS    INFO                     
         CLI   0(R7),MDTELQ        1A   MEDIA     TRANSFER  ELEMENT ?           
         BE    BDPTO120            YES, FUDGE     AN   EA   NUMBER              
         CLI   0(R7),UFSELQ        A2   MEDIA     TRANSFER  USER FLD ?          
         BE    BDPTO150            YES, FUDGE     AN   EA   NUMBER              
*                                                                               
BDPTO70  SR    R2,R2                                                            
         IC    R2,1(,R7)           GET  ELEMENT   LENGTH                        
         AR    R7,R2               ->   NEXT ELEMENT                            
         B     BDPTO60             CHECK     NEXT ELEMENT                       
*                                                                               
         USING ABIELD,R7                                                        
BDPTO80  MVC   SVEA(14),ABIEANO                                                 
         MVC   SVAC(14),ABIACNO                                                 
         OI    BDTEST,BDTFND27     X'27'     ELEMENT   PROCESSED                
         B     BDPTO70                                                          
*                                                                               
         USING TRNELD,R7                                                        
BDPTO90  CLI   TRNTYPE,9           BYPASS    TYPE 9'S,       MASC 10/94         
         BE    BDPTO50                                                          
         CLI   TRNTYPE,6           AND  6'S                                     
         BE    BDPTO50                                                          
*                                                                               
         ZAP   TRNREC,TRNAMNT                                                   
         ZAP   TRNINV,TRNAMNT                                                   
         GOTO1 DATCON,DMCB,(1,TRNDATE),(X'20',TRIDAT)                           
         MVC   SVDAT3,TRNDATE                                                   
         MVC   WORK40(3),TRNDATE   SAVE IN   CASE NEEDED    IN BDPTO160         
         MVC   TRINUM,TRNREF                                                    
         OI    BDTEST,BDTFND44     X'44'     ELEMENT   PROCESSED                
         B     BDPTO70                                                          
*                                                                               
         USING SCIELD,R7                                                        
BDPTO100 ZAP   TRNCOM,SCIAMNT                                                   
         SP    TRNINV,SCIAMNT                                                   
         B     BDPTO70                                                          
*                                                                               
         USING TRSELD,R7                                                        
BDPTO110 GOTO1 DATCON,DMCB,(2,TRSDATE),(1,SVDAT3)                               
         OI    BDTEST,BDTFND60     X'60'     ELEMENT   PROCESSED                
         B     BDPTO70                                                          
*                                                                               
BDPTO120 TM    BDTEST,BDTFND27     27   ELEMENT   AVAILABLE ?                   
         BO    BDPTO70             YES, GET  NEXT ELEMENT                       
*                                                                               
         USING MDTELD,R7                                                        
         LA    RF,MDTDSCP                                                       
         LA    R0,L'MDTDSCP                                                     
*                                                                               
BDPTO130 CLC   0(3,RF),=C'EA#'     SEARCH    FOR  THE  EA#                      
         BE    BDPTO140                                                         
         LA    RF,1(,RF)                                                        
         BCT   R0,BDPTO130                                                      
         B     BDPTO70                                                          
*                                                                               
BDPTO140 TM    BDTEST,BDTEAFA2     EA   FROM A2   ELEMENT ?                     
         BO    BDPTO70             YES, DO   NOT  USE  1A                       
         BAS   RE,BDSETEA          SET  BDFUDGEA                                
         BNE   *+8                 INVALID   EA ?                               
         OI    BDTEST,BDTEAF1A     GOT  EA   FROM 1A   EST  DESCRIPTION         
         B     BDPTO70             NEXT ELEMENT                                 
*                                                                               
         USING UFSELD,R7                                                        
BDPTO150 CLC   UFSCODE,=C'SN'      SPECIAL   NUMBER    USER FIELD ?             
         BNE   BDPTO70             NO,  DO   NOT  WANT IT                       
*                                                                               
         CLC   =C'EA#',UFSDATA     EA#  FROM USER FLD  EL   DATA ?              
         BNE   BDPTO70             NO,  NEXT ELEMENT                            
*                                                                               
         LA    RF,UFSDATA                                                       
         BAS   RE,BDSETEA          SET  BDFUDGEA                                
         BNE   *+8                 INVALID   EA ?                               
         OI    BDTEST,BDTEAFA2     EA#  FROM A2   ELEMENT                       
         B     BDPTO70             NEXT ELEMENT                                 
         DROP  R7                                                               
*                                                                               
*                                  WERE 27,  44   AND  60   ALL  FND ?          
BDPTO160 TM    BDTEST,BDTFND27+BDTFND44+BDTFND60                                
         BO    BDPTO170            YES, SKIP                                    
*                                                                               
         TM    BDTEST,BDTEAFA2+BDTEAF1A GOT  EA   FROM 1A   OR   A2 ?           
         BZ    BDPTO50             NO,  GET  NEXT TRANSACTION                   
         MVC   SVEA,BDFUDGEA                                                    
         MVC   SVAC,BDFUDGAC                                                    
         TM    BDTEST,BDTFND44+BDTFND60 GOT  THE  44   AND  60 ?                
         BNO   BDPTO50                  NO,  GET  NEXT TRANSACTION              
*                                                                               
BDPTO170 CLC   SVDAT3,QSTR3                                                     
         BL    BDPTO50                                                          
         CLC   SVDAT3,QEND3                                                     
         BH    BDPTO50                                                          
         CLC   WORK40(2),=X'8701'  TRAN DATE (EL44) =  JAN/87 ?                 
         BNE   BDPTO180            NO,  PROCESS   TRANSACTION                   
*                                                                               
         LR    R7,R8               YES, RECHECK - MUST HAVE AN   44  EL         
         MVI   ELCODE,TRNELQ       X'44'     TRANSACTION    ELEMENT             
         GOTO1 AGETEL              ANY  TRANSACTION    ELEMENT ?                
         BNE   BDPTO50             NO,  BYPASS -- SOMETHING WRONG               
         SR    R1,R1                                                            
         IC    R1,1(,R7)                                                        
         AR    R1,R7               ->   NEXT ELEMENT                            
         CLI   0(R1),TRSELQ        X'60'     ELEMENT ?                          
         BNE   BDPTO50             NO,  BYPASS -- RCD  PROCESSED JAN/87         
*                                                                               
* TRANSACTION PROCESSING  (FROM MONACC OR ABOVE)                                
*                                                                               
BDPTO180 TM    SVSTAT,SVFRST       ONE  TIME PER  REQUEST   CODE DONE ?         
         BO    BDPTO190            YES, SKIP                                    
         OI    SVSTAT,SVFRST       NO,  SET  ONE  TIME PER  REQ  CODE           
*                                                                               
         USING BDHSRECD,RE         MAP  OUTPUT    TAPE HS   RECORD              
         LA    RE,TAPEWK           ->   OUTPUT    TAPE AREA                     
         MVC   TAPEWK(80),BDHSCODE INSERT    BBDO HDR  RECORD                   
         UNPK  BDHSNUM,BDOSEQ                                                   
         CLC   TRNCLI,=C'DCG'      SPECIAL LOCATION CODE FOR CLI DCG            
         BNE   *+10                                                             
         MVC   BDHSSHTO,=C'000008050'                                           
         GOTO1 AWRTAPE             WRITE     HDR  RECORD                        
         DROP  RE                                                               
*                                                                               
*                                                 PER  MASC/HWEI 2/97           
BDPTO190 CP    TRNREC,P0           DO   NOT  WRITE     OUT    ANY  RCDS         
         BNE   BDPTO195                 WHEN RECEIVABLES      AND               
         CP    TRNINV,P0                INVENTORY ARE  ZERO                     
         BE    BDPTO230                                                         
*                                                                               
BDPTO195 BAS   RE,BDOADD           ADD  ONE  TO   SEQ  NUMBER                   
         TM    BDSTAT,BDSTUFEA     HAS  EA   BEEN SET  FROM USER FLD ?          
         BO    BDPTO210            YES, CHECK     AC                            
         MVC   BDFDEA,SPACES                                                    
*                                                                               
         CLI   SVEA+5,C'-'         IS   THIS THE  5-2-1     FORMAT ?            
         BNE   BDPTO200            NO,  USE  DIFFERNT  MOVES                    
         MVI   BDFDEA,C'0'         LEADING   ZERO                               
         MVC   BDFDEA+1(5),SVEA    5                                            
         MVC   BDFDEA+6(2),SVEA+6  2                                            
         MVC   BDFDEA+8(1),SVEA+9  1                                            
         B     BDPTO210                                                         
*                                                                               
BDPTO200 MVC   BDFDEA(6),SVEA      6                                            
         MVC   BDFDEA+6(2),SVEA+7  2                                            
         MVC   BDFDEA+8(1),SVEA+10 1                                            
*                                                                               
BDPTO210 TM    BDSTAT,BDSTUFAC     HAS  AC   BEEN SET  FROM USER FLD ?          
         BO    BDPTO220            YES, SKIP                                    
*                                                                               
         MVC   BDFDACTD(4),SVAC    ASSUME    ACNO IS   4-3-4                    
         MVC   BDFDACTD+4(3),SVAC+5                                             
         MVC   BDFDACTD+7(4),SVAC+9                                             
*                                                                               
         CLI   SVAC+9,C'-'         4-4-3     FORMAT ?                           
         BNE   BDPTO220                                                         
         MVC   BDFDACTD(4),SVAC    ACNO IS   4-4-3                              
         MVC   BDFDACTD+4(4),SVAC+5                                             
         MVC   BDFDACTD+8(3),SVAC+10                                            
*                                                                               
BDPTO220 MVC   P2X+20(10),=C'EA NUMBER='                                        
         MVC   P2X+30(12),BDFDEA                                                
         MVC   P2X+42(15),=C'ACCOUNT NUMBER='                                   
         MVC   P2X+57(11),BDFDACTD                                              
         AP    BDOTOT,TRNREC                                                    
*                                                                               
         USING BDFDRECD,RE         MAP  OUTPUT    TAPE FD   RECORD              
         LA    RE,TAPEWK           ->   OUTPUT    TAPE AREA                     
         MVC   TAPEWK(80),BDFDCODE MOVE BBDO TO   FD   RECORD                   
         UNPK  BDFDNUM,BDOSEQ                                                   
         MVC   BDFDTEA,BDFDEA                                                   
         MVC   BDFDTAC,BDFDACTD                                                 
         UNPK  BDFDAMT,TRNREC                                                   
         GOTO1 AWRTAPE             WRITE     FD   RECORD                        
         DROP  RE                                                               
*                                                                               
         BAS   RE,BDOADD           ADD  ONE  TO   SEQ  NUMBER                   
         MVC   YR0101(2),TRIDAT    MOVE IN   YY   WITH REST 0101                
         GOTO1 PERVERT,DMCB,YR0101,TRIDAT                                       
         LH    R1,DMCB+8           ACTUALLY  JULIAN    DATE                     
         CVD   R1,DOUBLE                                                        
         OI    DOUBLE+L'DOUBLE-1,X'0F'                                          
*                                                                               
         USING BDFTRECD,RE         MAP  OUTPUT    TAPE FT   RECORD              
         LA    RE,TAPEWK           ->   OUTPUT    TAPE AREA                     
         MVC   TAPEWK(80),BDFTCODE MOVE BBDO FT   RECORD                        
         UNPK  BDFTNUM,BDOSEQ                                                   
         MVC   BDFTSID(1),TRNJOB                                                
         MVC   BDFTSID+1(6),TRINUM                                              
         MVC   BDFTJYR,TRIDAT                                                   
         UNPK  BDFTJDAY,DOUBLE                                                  
         UNPK  BDFTAMT,TRNREC                                                   
         GOTO1 AWRTAPE             WRITE     FT   RECORD                        
         DROP  RE                                                               
*                                                                               
BDPTO230 CLI   MODE,REQLAST        END  OF   REQUEST   RUN ?                    
         BNE   BDPTOX              NO,  EXIT ---- RETURN    TO   CALLER         
         GOTO1 AREPORT             WRITE     REPORT    FROM HERE                
         B     BDPTO50             READ NEXT RECORD                             
*                                                                               
BDPTO240 CP    TAPECNT,P0          ANY  TAPE RECORDS   WRITTEN ?                
         BE    BDPTO250            NO,  SKIP                                    
*                                                                               
         AP    TAPECNT,=P'1'       ADD  ONE  TO   SEQ  NUMBER                   
*                                                                               
         USING BDTRRECD,RE         MAP  OUTPUT    TAPE TR   RECORD              
         LA    RE,TAPEWK           ->   OUTPUT    TAPE AREA                     
         MVC   TAPEWK(80),BDTRCODE WRITE     TRAILER   RECORD                   
         UNPK  BDTRAMT,BDOTOT                                                   
         UNPK  BDTRRECS,TAPECNT                                                 
         GOTO1 AWRTAPE                                                          
         DROP  RE                                                               
*                                                                               
*                                  CORRECT   TAPE COUNT -                       
         SP    TAPECNT,=P'1'           (SEE  REPORT    RTN)                     
         GOTO1 ACLITOT                                                          
         BAS   RE,BDOADD           ADD  ONE  TO   SEQ  NUMBER                   
*                                                                               
BDPTO250 CLI   QOPT1,C'Y'          OUTPUT    TAPE REQUIRED                      
         BNE   BDPTOX              NO,  EXIT NO   TAPE OUTPUT   CREATED         
         MVC   P+1(33),=C'* NEXT SEQUENCE NUMBER IS       *'                    
         EDIT  (P3,BDOSEQ),(5,P+28),0,COMMAS=NO,FILL=0                          
         GOTO1 APRINTIT                                                         
         GOTO1 APRINTIT            SPACING   LINE                               
*                                                                               
BDPTOX   B     BDEXIT                                                           
         EJECT ,                                                                
***********************************************************************         
* ADD TO COUNTER ROUTINE                                              *         
***********************************************************************         
         SPACE 1                                                                
BDOADD   NTR1                                                                   
*                                                                               
BDOADD00 AP    BDOSEQ,=P'1'        KEEP RUNNING   COUNT     FOR  SEQ  #         
         CP    BDOSEQ,P0           SEQ  NUM  ROLL OVER FROM 99999 ?             
         BE    BDOADD00            YES, ADD  ONE  MORE TIME                     
         OI    BDOSEQ+L'BDOSEQ-1,X'0F'  FOR  UNPK INSTRUCTION                   
         B     BDEXIT                                                           
         EJECT ,                                                                
***********************************************************************         
* USING THE THIRD LEVEL OF THE SR ACCOUNT IN 0(R6) AS A CLIENT        *         
*       AND DA2 AS THE PRODUCT, SAVE CHARACTERS 3-16 OF THE PRODUCT   *         
*       NAME AS AN EA NUMBER IN THE EVENT THE POSTINGS ON THIS SR     *         
*       ACCOUNT DON'T HAVE 27 ELEMENTS                                *         
***********************************************************************         
         SPACE 1                                                                
BDGETAC# NTR1                                                                   
         MVC   BDFUDGAC,SPACES                                                  
         LA    R8,TAPEWK+80        ->   1K   DM   IO,  1ST  80=TAPE OUT         
         MVC   0(42,R8),SPACES                                                  
         MVC   0(1,R8),RCCOMPFL                                                 
         MVC   1(2,R8),=C'SJ'                                                   
         MVC   3(3,R8),6(R6)       CLIENT                                       
         MVC   6(3,R8),=C'DA2'     PRODUCT                                      
         MVC   WORK(15),0(R8)                                                   
         MVC   COMMAND,=C'DMRDHI'                                               
         GOTO1 AREADDM,(R8)                                                     
         CLC   WORK(15),0(R8)                                                   
         BNE   BDGETAX             PROD NOT  FOUND                              
*                                                                               
         LR    R7,R8                                                            
         AH    R7,DATADISP                                                      
*                                                                               
BDGETA10 CLI   0(R7),0                                                          
         BE    BDGETAX                                                          
         CLI   0(R7),NAMELQ        X'20' -   NAME ELEMENT                       
         BE    BDGETA20                                                         
         SR    RF,RF                                                            
         IC    RF,1(,R7)                                                        
         AR    R7,RF                                                            
         B     BDGETA10                                                         
*                                                                               
BDGETA20 CLI   8(R7),C'-'          CHECK     FORMAT                             
         BNE   BDGETAX                                                          
         CLI   12(R7),C'-'                                                      
         BE    BDGETA30                                                         
         CLI   13(R7),C'-'                                                      
         BNE   BDGETAX                                                          
*                                                                               
BDGETA30 SR    R1,R1                                                            
         IC    R1,1(,R7)           AC   NO   IS   NAME+2                        
*                                  2    FOR  20NN,     1    FOR  MVC,           
         SH    R1,=H'5'            2    FOR  +2                                 
         EXMVC R1,BDFUDGAC,4(R7)                                                
*                                                                               
BDGETAX  B     BDEXIT                                                           
         EJECT ,                                                                
***********************************************************************         
* ASSUME RF POINTS TO AN EA#                                          *         
*        SAVES IT IN BDFUDGEA                                         *         
*        USES R1, RETURNS NEQ IF EA# IS NOT IN RIGHT FORMAT           *         
***********************************************************************         
         SPACE 1                                                                
BDSETEA  NTR1                                                                   
         CLI   9(RF),C'-'          VERIFY    FORMAT                             
         BNE   BDSETE10            TRY  6-2-1                                   
         CLI   12(RF),C'-'                                                      
         BNE   BDSETEAX            TRY  NEXT TRANSACTION                        
         LA    R1,10               SET  LENGTH                                  
         B     BDSETE20            AND  SAVE IT                                 
*                                                                               
BDSETE10 CLI   8(RF),C'-'          TRY  5-2-1                                   
         BNE   BDSETEAX            TRY  NEXT TRANSACTION                        
         CLI   11(RF),C'-'         5-2-1                                        
         BNE   BDSETEAX            TRY  NEXT TRANSACTION                        
         LA    R1,9                                                             
*                                                                               
BDSETE20 MVC   BDFUDGEA,SPACES                                                  
         EXMVC R1,BDFUDGEA,3(RF)                                                
         CR    RB,RB               SET  EQ   CC                                 
*                                                                               
BDSETEAX B     BDEXIT                                                           
         EJECT ,                                                                
***********************************************************************         
* UTILITY SUBROUTINES                                                 *         
***********************************************************************         
         SPACE 1                                                                
*---------------------------------------------------------------------*         
* LOAD BDJBDATA WITH DATA FROM THE USER FIELDS ON THIS JOB            *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
         USING BDLUD,R3            CLEAR     BDJBDATA                           
         SPACE 1                                                                
BDLDUSER NTR1                                                                   
         L     R3,=A(BDUSRTAB)     ->   BDNY USER FLD  TABLE                    
         LA    R0,BDLUNUM                                                       
*                                                                               
BDLU10   LA    R1,BDLUDLN          L'FIELD                                      
         SR    R2,R2                                                            
*                                  OFFSET    INTO BDJBDATA                      
         ICM   R2,3,BDLUJBF                  OF   THIS FIELD                    
         LA    R2,BDJBDATA(R2)                                                  
         BCTR  R1,0                                                             
         EXMVC R1,0(R2),SPACES                                                  
         BCT   R0,BDLU10                                                        
*                                                                               
         USING UFSELD,R7                                                        
         L     R7,ADACC            EXTRACT   USER FLD  DATA FROM ACNT           
         MVI   ELCODE,UFSELQ       X'A2' -   USER FLD  ELEMENT                  
         GOTO1 AGETEL                                                           
*                                                                               
BDLU20   BNE   BDLUX                                                            
*                                                                               
         USING BDLUD,R3                                                         
         L     R3,=A(BDUSRTAB)     ->   BDNY USER FLD  TABLE                    
         LA    R0,BDLUNUM                                                       
*                                                                               
BDLU30   CLC   UFSCODE,BDLUCODE    CODE IN   THE  TABLE ?                       
         BE    BDLU40              YES, SAVE IT   IN   BDJBDATA                 
         LA    R3,BDLUDLN(,R3)                                                  
         BCT   R0,BDLU30                                                        
         B     BDLU70              THIS USER FLD  NOT  NEEDED ON TAPE           
*                                                                               
BDLU40   SR    R2,R2                                                            
         IC    R2,UFSLN                                                         
         SH    R2,=Y(UFSLN1Q)                                                   
         BNP   BDLU70              NO   DATA ON   THIS USER FIELD               
*                                                                               
         SR    R1,R1               OFFSET    INTO BDJBDATA  FOR                 
         ICM   R1,3,BDLUJBF                  THIS FIELD                         
         LA    R1,BDJBDATA(R1)     R1=  A(OUTPUT  FIELD)                        
*                                                                               
         SR    R0,R0                                                            
         IC    R0,BDLUJBLN         SAVE LENGTH    OF   O/P  FIELD               
         SR    R6,R6                    COUNTER                                 
*                                                                               
         LA    R5,UFSDATA          R5=  A(I/P     FIELD)                        
         CLC   UFSCODE,=C'NU'      THIS IS   THE  ACCOUNT   NUMBER ?            
         BNE   BDLU50              NO,  SKIP                                    
         LA    R5,1(,R5)           SKIP 1ST  BYTE                               
         SH    R2,=H'1'                                                         
         BM    BDLU70                                                           
*                                                                               
BDLU50   CLI   0(R5),C'-'          ELIMINATE DASHES                             
         BNE   BDLU60                                                           
         LA    R5,1(,R5)                                                        
         SH    R2,=H'1'                                                         
         BNP   BDLU70                                                           
         B     BDLU50                                                           
*                                                                               
BDLU60   MVC   0(1,R1),0(R5)                                                    
         LA    R5,1(,R5)           BUMP I/P  POINTER                            
         LA    R1,1(,R1)           BUMP O/P  POINTER                            
         LA    R6,1(,R6)           BUMP COUNTER                                 
*                                                                               
         CR    R6,R0               MAX  BYTES     O/P ?                         
         BH    BDLU70              YES, GET  NEXT UF                            
         BCT   R2,BDLU50                                                        
*                                                                               
BDLU70   GOTO1 ANEXTEL                                                          
         B     BDLU20                                                           
*                                                                               
BDLUX    B     BDEXIT                                                           
         DROP  R3,R7                                                            
         EJECT ,                                                                
***********************************************************************         
* BDNY CONSTANTS                                                      *         
***********************************************************************         
         SPACE 1                                                                
BDHSCODE DC    C'HS'                                                            
         DC    CL5' '              WAS: HSNUM                                   
         DC    CL11' '                                                          
         DC    CL9'000002100'      WAS: HSSHTO                                  
         DC    CL11' '                                                          
         DC    CL9'000019609'      WAS: HSSUPC                                  
         DC    CL21' '                                                          
         DC    C'A'                WAS: HSSTATUS                                
         DC    CL11' '                                                          
*                                                                               
BDFDCODE DC    C'FD'                                                            
         DC    CL5' '              WAS: FDNUM                                   
         DC    CL14' '             WAS: FDEA                                    
         DC    CL11' '             WAS: FDACCTD                                 
         DC    CL1' '                                                           
         DC    CL14'O895702415-A'  WAS: FDPO                                    
         DC    C'00000001'         WAS: FDQTY                                   
         DC    C'EA'               WAS: FDUNIT                                  
         DC    CL11' '                                                          
         DC    CL9' '              WAS: FDAMT                                   
         DC    CL3' '                                                           
*                                                                               
BDFTCODE DC    C'FT'                                                            
         DC    CL5' '              WAS: FTNUMBR                                 
         DC    CL9' '              WAS: FTSHIPID                                
         DC    CL35' '                                                          
         DC    CL2' '              WAS: FTJYR                                   
         DC    CL3' '              WAS: FTJDAY                                  
         DC    CL15' '                                                          
         DC    CL9' '              WAS: FTAMT                                   
*                                                                               
BDTRCODE DC    C'TR '                                                           
         DC    CL10'CHRYSLER'                                                   
         DC    CL10' '                                                          
         DC    CL9'000019609'      WAS: TRSUPC                                  
         DC    CL1' '                                                           
         DC    CL25'BBDO, INC.'    WAS: TRSUPN                                  
         DC    CL7' '                                                           
         DC    CL9' '                                                           
         DC    CL1' '                                                           
         DC    CL5' '                                                           
         EJECT ,                                                                
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         TITLE 'ACI102BS 1 - BSNY 1 - BATES PROD INTERFACE ROUTINE'             
***********************************************************************         
* NOTE THE I1 CLEARS TYPE 2 BUFFALO RECORDS AT ACCLAST                *         
* WHEN SVTEST'S SVTSCLR2 (X'40') BIT IS ON                            *         
***********************************************************************         
         SPACE 1                                                                
SUBRBS1  NMOD1 0,**I1BS**                                                       
         L     RC,ADRC             RESTORE   RC                                 
*                                                                               
         CLI   MODE,PROCTRNS                                                    
         BE    BSPTRN                                                           
*                                                                               
BSEXIT   XIT1  ,                                                                
         EJECT ,                                                                
***********************************************************************         
* PROCESS TRANSACTION (PROCTRNS)                                      *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNELD,R6           TRANSACTION    ELEMENT                       
         SPACE 1                                                                
BSPTRN   DS    0H                                                               
         L     R6,ADTRANS                                                       
         CLI   TRNEL,TRNELQ        X'44' -   TRANSACTION    ELEMENT ?           
         BNE   BSEXIT              NO,  EXIT                                    
         CLI   GOSTAT,C'I'                                                      
         BNE   BSO100                                                           
         GOTO1 AGETFEE                                                          
         B     BSEXIT                                                           
*                                                                               
BSO100   CLC   TRNANAL,=C'99'                                                   
         BNE   BSEXIT                                                           
         MVC   P2X(6),=C'ESTNO='                                                
         MVC   P2X+6(10),SVBN                                                   
*                                                                               
*                                  ************************************         
*                                  * PREP  40 RECORD *                          
*                                  ************************************         
*                                                                               
         USING BSRECD,R8           MAP  OUTPUT    TAPE AREA                     
         LA    R8,TAPEWK           ->   OUTPUT    TAPE AREA                     
         MVC   BSREST,SVBN                                                      
         MVC   BSRJOB,TRNJOB                                                    
         MVC   BSRNUM(1),TRNJOB                                                 
         MVC   BSRNUM+1(6),TRINUM                                               
         MVC   TRIMED,TRNJOB                                                    
         MVC   BSRID,=C'40'                                                     
         MVC   BSRDAT(L'BSRDAT+L'BSRDUE),SPACES                                 
         MVI   BSRWKC-1,C'0'                                                    
         MVC   BSRAGY(13),=CL13'310774PDC'                                      
         MVC   BSRLEN,=X'00640000' THAT IS   100  BYTE LONG RECORD              
         ZAP   BSRNC,P0                                                         
         ZAP   BSRGRS,P0                                                        
*                                                                               
* 1/91  AS PER MCOR                                                             
*       BSRNET IS RECEIVABLE AMOUNT                                             
*       BSRNC  IS NON COMMISSIONABLE AMOUNT                                     
*       BSRGRS IS RECEIVABLE BEFORE CD IS SUBTRACTED                            
*       FOR BOTH THE 40 AND 43 RECORDS                                          
* 7/92  AS PER MCOR/MASC                                                        
*       COMPARE NET DETAILS W/ NET ON BILL                                      
*                                                                               
         XC    BUFWKC,BUFWKC       MAKE WORK-CODE LOW, HEX  ZEROES              
         ZAP   BSSVNC,P0                                                        
         ZAP   BSSVREC,P0                                                       
         ZAP   BSSVNET,P0                                                       
         MVI   BUFREC,2            TO   PROCESS   TYPE 2   ACCUMULATORS         
         MVI   DMCB+8,0                                                         
*                                                                               
         XC    BUFKEY,BUFKEY                                                    
         MVC   BUFINV,TRINUM       FIND INVOICE   NUM  IN   BUFFALO             
         MVC   BSCBUF#,TRINUM      INVOICE   NUM  TO   COMPARE   WITH           
         BAS   RE,BSWRBUFF         WRITE     RCD  FROM BUFFALO                  
         CLI   BSBLD43,YES         BUILD     43   RECORD ?                      
         BE    BSO400              YES, BUILD     IT                            
*                                                                               
*                                  ************************************         
*                                  * MATCH REVERSED CHARGES WITH THE  *         
*                                  * ORIGINAL BILL                    *         
*                                  ************************************         
*                                                                               
         XC    BUFKEY,BUFKEY       LOOK FOR  REVERSED  CHARGE    DATA           
         MVC   BUFINV(4),=X'FEFEFEFE'   HIGH VALUES    TO   INV  NUMBER         
         OC    TRNUNBIL,TRNUNBIL   HAS  THIS BILL BEEN REVESED  ?               
         BZ    BSO350              NO,  SKIP                                    
*                                                                               
         MVC   BUFINV+4(2),TRN2DAY MOVE IN   TRANSACTION    DATE                
         MVC   BSSVINV(L'BUFINV),BUFINV                                         
         MVC   BSCBUF#,BSSVINV     INVOICE   NUM  TO   COMPARE   WITH           
         BAS   RE,BSWRBUFF         WRITE     RCD  FROM BUFFALO                  
         CLI   BSBLD43,YES         BUILD     43   RECORD ?                      
         BE    BSO400              YES, BUILD     IT                            
*                                                                               
*                                  ************************************         
*                                  * MATCH REVERSED CHARGES WITH      *         
*                                  * REVERSAL BILL                    *         
*                                  ************************************         
*                                                                               
BSO350   XC    BUFKEY,BUFKEY       LOOK FOR  REVERSAL  CHARGE    DATA           
*                                  HIGH VALUES    TO   INV  NUMBER              
         MVC   BUFINV(4),=X'FFFFFFFF'                                           
         MVC   BUFINV+4(2),TRN2DAY MOVE IN   TRANSACTION    DATE                
         MVC   BSSVINV(L'BUFINV),BUFINV                                         
         MVC   BSCBUF#,BSSVINV     INVOICE   NUM  TO   COMPARE   WITH           
         BAS   RE,BSWRBUFF         WRITE     RCD  FROM BUFFALO                  
*        CLI   BSBLD43,YES         BUILD     43   RECORD ?                      
*        BE    BSO400              YES, BUILD     IT                            
*                                                                               
*                                  ************************************         
*                                  * BUILD A 43 RECORD                *         
*                                  ************************************         
*                                                                               
BSO400   MVC   BSRDAT,TRIDAT                                                    
         MVC   BSRDUE,SVDUE                                                     
         MVC   BSRWKC-1(L'BSRWKC+1),SPACES                                      
         MVC   BSRID,=C'43'        OVERALL   TOTAL     BY   INV-NO              
         ZAP   BSRNET,TRNREC                                                    
         AP    BSRNET,TRNDIS       ADD  BACK CD                                 
         ZAP   BSRGRS,TRNREC                                                    
         ZAP   BSRNC,BSSVNC        FROM W/C  TOTALS                             
         BAS   RE,BSPRTTAP         PRINT     TAPE INFORMATION                   
         GOTO1 AWRTAPE                                                          
*                                                                               
         B     BSEXIT              EXIT,     RETURN    TO   CALLER              
         DROP  R6,R8                                                            
         EJECT ,                                                                
**********************************************************************          
* WRITE RECORD(S) FROM BUFFALO                                       *          
*                                                                    *          
*   INPUT:                                                           *          
*     BSCBUF#  - INVOICE NUMBER TO  COMPARE WITH                     *          
*     BUFKEY   - BUFFALO KEY                                         *          
*     BSSVNC   - SAVED   NON-COM    AMOUNT                           *          
*     BSSVREC  - SAVED   RECEIVABLE AMOUNT                           *          
*     BSSVNET  - SAVED   NET        AMOUNT                           *          
*     TRNINV   - TRANSACTION        INVOICE NUMBER                   *          
*                                                                    *          
*   OUTPUT:                                                          *          
*     BSBLD43  - BUILD A 43 RECORD (Y/N)                             *          
**********************************************************************          
         SPACE 1                                                                
BSWRBUFF NTR1                                                                   
         MVI   BSBLD43,NO                                                       
         GOTO1 BUFFALO,DMCB,=C'HIGH',(2,ADBUFC),BUFREC,1                        
         CLI   DMCB+8,0            ANY  INVOICE   NUMBER    FOUND ?             
         BNE   BSWRBFEX            NO,  EXIT                                    
         CLC   BUFINV,BSCBUF#      RIGHT     INVOICE   NUMBER ?                 
         BNE   BSWRBFEX            NO,  EXIT                                    
*                                                                               
BSWRBF20 CLC   BUFINV,BSCBUF#      INVOICE   NUMBER    MATCH ?                  
         BNE   BSWRBF40            NO,  CHECK     SAVED     AMOUNT              
         BAS   RE,BSPUTTAP         PUT  BUFFALO   REC  TO   TAPE                
*                                  SAVE NON-COM   AMOUNTS   FOR                 
         AP    BSSVNC,BUFC2             99   TOTALS                             
         AP    BSSVREC,BUFC1       SAVE RECEIVABLE    (AFTER     CD)            
         AP    BSSVNET,BUFC4       SAVE NET  AMOUNT                             
         GOTO1 BUFFALO,DMCB,=C'SEQ',(2,ADBUFC),BUFREC,1                         
         CLI   DMCB+8,0            SOMETHING LEFT ?                             
         BE    BSWRBF20            NO, TRY  NEXT                                
*                                                                               
*                                  SAVED     INVOICE   AMOUNT   SAME AS         
BSWRBF40 CP    BSSVNET,TRNINV           TRANSACTION'S  INVOICE  AMOUNT?         
         BE    *+6                 YES, CONTINUE                                
         DC    H'0'                NO,  BLOW-UP                                 
*                                                                               
         MVI   BSBLD43,YES         WRITE     43   RECORD    SWITCH              
*                                                                               
BSWRBFEX B     BSEXIT                                                           
         EJECT ,                                                                
***********************************************************************         
* PUT A BUFFALO RECORD TO TAPE AS WORK CODE DETAIL                    *         
***********************************************************************         
         SPACE 1                                                                
         USING BSRECD,R8           MAP  OUTPUT    TAPE AREA                     
         SPACE 1                                                                
BSPUTTAP NTR1                                                                   
         MVC   BSRWKC,BUFWKC       MOVE NON-99    WORK-CODE TO   TAPE           
         ZAP   BSRNET,BUFC1                                                     
         ZAP   BSRNC,BUFC2                                                      
         ZAP   BSRGRS,BUFC3                                                     
*                                                                               
         CLC   BSRGRS(3*L'BSRGRS),=3PL6'0'   ALL  BUCKETS   ZERO ?              
         BE    BSPUTTX                       YES, SUPPRESS  RCD (MASC)          
         BAS   RE,BSPRTTAP                                                      
         GOTO1 AWRTAPE             WRITE NON-99   TRAN RECORDS                  
*                                                                               
BSPUTTX  B     BSEXIT              RETURN    TO   CALLER                        
         DROP  R8                                                               
         EJECT ,                                                                
***********************************************************************         
* PRINT TAPE ROUTINE                                                  *         
***********************************************************************         
         SPACE 1                                                                
         USING BSRECD,R8           MAP  OUTPUT    TAPE AREA                     
         SPACE 1                                                                
BSPRTTAP NTR1                                                                   
         CLC   QUESTOR,=CL12'PRTBUCKS'                                          
         BNE   BSPRTTX                                                          
         MVC   P(2),BSRID          40   OR   43                                 
         MVC   P+3(6),TRINUM                                                    
         EDIT  (P6,BSRGRS),(10,P+10),2,MINUS=YES                                
         EDIT  (P6,BSRNC),(10,P+20),2,MINUS=YES                                 
         EDIT  (P6,BSRNET),(10,P+30),2,MINUS=YES                                
         GOTO1 ACREPORT                                                         
*                                                                               
BSPRTTX  B     BSEXIT                                                           
         DROP  R8                                                               
         EJECT ,                                                                
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         TITLE 'ACI102BS 2 - BSNY 2 - BATES COMMISSION TAPE ROUTINE'            
***********************************************************************         
* NOTE THE I1 CLEARS TYPE 2 BUFFALO RECORDS AT ACCLAST                *         
* WHEN SVTEST'S SVTSCLR2 (X'40') BIT IS ON                            *         
***********************************************************************         
         SPACE 1                                                                
SUBRBS2  NMOD1 0,**I1B2**                                                       
         L     RC,ADRC             RESTORE   RC                                 
*                                                                               
         CLI   MODE,PROCTRNS                                                    
         BE    BS2TRNRT                                                         
*                                                                               
BS2EXIT  XIT1  ,                                                                
         EJECT ,                                                                
***********************************************************************         
* PROCESS TRANSACTIONS (PROCTRNS)                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNELD,R7           TRANSACTION    ELEMENT                       
         SPACE 1                                                                
BS2TRNRT L     R7,ADTRANS                                                       
         CLI   TRNEL,TRNELQ       X'44' -   TRANSACTION    ELEMENT ?            
         BNE   BS2TRNEX            NO,  EXIT                                    
         CLI   GOSTAT,C'I'                                                      
         BE    BS2TRNEX                                                         
*                                                                               
         CLC   TRNANAL,=C'99'                                                   
         BNE   BS2TRNEX                                                         
*                                                                               
         USING BS2RECD,R8          MAP  OUTPUT    TAPE RECORD                   
         LA    R8,TAPEWK           ->   OUTPUT    TAPE AREA                     
         MVC   0(104,R8),SPACES    SPACE     FILL                               
         MVC   BS2LEN,=X'00640000' SET  RECL FOR  VB   RECORD                   
*                                                                               
         MVC   BS2LIT,=C'310774PDC'                                             
         MVC   BS2NYY,TRIDAT       INVOICE   YEAR                               
         MVC   BS2NMED,TRNJOB      MEDIUM                                       
         MVC   BS2NNUM,TRINUM                                                   
         MVC   BS2JOB,TRNCLI       CLIENT    PROD JOB                           
         MVC   BS2REC,=C'50'                                                    
*                                                                               
         ZAP   BS2GRS,TRNREC                                                    
         AP    BS2GRS,TRNDIS                                                    
*                                                                               
         ZAP   BS2CD,TRNDIS                                                     
*                                                                               
         ZAP   BS2NET,TRNINV                                                    
         AP    BS2NET,TRNDIS                                                    
*                                                                               
         ZAP   BS2COM,P0                                                        
*                                                                               
         ZAP   BS2FEE,P0                                                        
*                                                                               
         BAS   RE,BS2PRTAP                                                      
         GOTO1 AWRTAPE                                                          
*                                                                               
BS2TRNEX B     BS2EXIT             EXIT,     RETURN    TO   CALLER              
         DROP  R7,R8                                                            
         EJECT ,                                                                
***********************************************************************         
* PRINT THE TAPE RECORDS                                              *         
***********************************************************************         
         SPACE  1                                                               
         USING  BS2RECD,R8         MAP  OUTPUT    TAPE AREA                     
         SPACE  1                                                               
BS2PRTAP NTR1                                                                   
         CLC   QUESTOR,=CL12'PRTBUCKS'                                          
         BNE   BS2PRTTX                                                         
         MVC   P(9),BS2NUM                                                      
         EDIT  (P6,BS2GRS),(10,P+10),2,MINUS=YES                                
         EDIT  (P6,BS2CD),(10,P+20),2,MINUS=YES                                 
         EDIT  (P6,BS2NET),(10,P+30),2,MINUS=YES                                
         GOTO1 ACREPORT                                                         
*                                                                               
BS2PRTTX B     BS2EXIT                                                          
         DROP  R8                                                               
         EJECT ,                                                                
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         TITLE 'ACI102JW - JWNYA - J. WALTER THOMPSON PROD INTERFACE'           
         TITLE 'ACI102NE - DNNYE 1 - NEEDHAM PROD INTERFACE ROUTINE'            
SUBRNE1  NMOD1 0,**I1NE**                                                       
         L     RC,ADRC             RESTORE   RC                                 
*                                                                               
         CLI   MODE,LEVAFRST                                                    
         BE    NELEVA                                                           
         CLI   MODE,PROCACC                                                     
         BE    NEACCRTN                                                         
         CLI   MODE,PROCTRNS                                                    
         BE    NETRNS                                                           
*                                                                               
NEEXIT   XIT1  ,                                                                
         EJECT ,                                                                
***********************************************************************         
* LEVEL A FIRST - CLIENT                                              *         
***********************************************************************         
         SPACE 1                                                                
NELEVA   DS    0H                  FIRST     FOR  REQUEST                       
         CLC   ALPHAID,=C'NR'      RETAIL    ???? (DNRFO)                       
         BNE   NELEVA30                                                         
         MVC   NHAGY,=C'RET'       USE  RET  AS   AGENCY    ON   TAPE           
         B     NELEVAEX                                                         
*                                                                               
NELEVA30 MVC   NHAGY,=C'DDB'                                                    
         LA    R2,NEDDWTAB                                                      
         LA    R0,L'NEDDWTAB                                                    
*                                                                               
NELEVA40 CLC   SVAOFF,0(R2)        IS   THIS A    DDW  OFFICE ?                 
         BNE   NELEVA50                                                         
*                                                                               
         MVC   NHAGY,=C'DDW'       YES                                          
         B     NELEVAEX                                                         
*                                                                               
NELEVA50 LA    R2,1(,R2)                                                        
         BCT   R0,NELEVA40                                                      
*                                                                               
NELEVAEX B     NEEXIT                                                           
         EJECT ,                                                                
***********************************************************************         
* PROCESS AN ACCOUNT  (PROCACC)                                       *         
***********************************************************************         
         SPACE 1                                                                
NEACCRTN MVC   NEUSERPR(L'NEOEST),SPACES                                        
         MVC   NEUSERPO(L'NEOEST),SPACES                                        
         L     R7,ADACC                                                         
         MVI   ELCODE,UFSELQ       X'A2' -   USER FLD  SELECT    EL             
         GOTO1 AGETEL              ANY  USER FLD  ELEMENT ?                     
         BNE   NEACCX              NO,  EXIT                                    
*                                                                               
         USING UFSELD,R7           USER FLD  SELECT    ELEMENT                  
NEACC10  ZIC   R1,UFSLN            GET  LEN  OF   DATA AREA                     
         SH    R1,=Y(UFSDATA-UFSELD)                                            
         BNP   NEACC40                                                          
*                                                                               
         BCTR  R1,0                                                             
         EXCLC R1,UFSDATA,SPACES   USER DATA DEFINED ?                          
         BNH   NEACC40             NO                                           
*                                                                               
         CLC   UFSCODE,=C'PR'                                                   
         BNE   NEACC30                                                          
         MVC   NEUSERPR(L'NEOEST),UFSDATA+2                                     
         B     NEACC40                                                          
*                                                                               
NEACC30  LA    R2,NEUSERPO                                                      
         LA    R3,UFSDATA                                                       
         CLC   UFSCODE,=C'PO'                                                   
         BNE   NEACC40                                                          
         CH    R1,=Y(L'NEOEST-1)                                                
         BNH   NEACC35                                                          
         LA    R1,L'NEOEST-1                                                    
*                                                                               
NEACC35  EXMVC R1,0(R2),0(R3)                                                   
*                                                                               
NEACC40  GOTO1 ANEXTEL             ANY  MORE ELEMENTS ?                         
         BE    NEACC10             YES, PROCESS   IT                            
*                                                                               
NEACCX   B     NEEXIT              RETURN    TO   CALLER                        
         DROP  R7                                                               
         EJECT ,                                                                
***********************************************************************         
* PROCESS A TRANSACTION  (PROCTRNS)                                   *         
***********************************************************************         
         SPACE 1                                                                
NETRNS   CLI   GOSTAT,C'I'                                                      
         BNE   NEORTN                                                           
*                                  EXCLUDE   MEDIUM    Z                        
         CLI   TRNMED,C'Z'         MEDIA     CODE =    Z ?                      
         BNE   NETRNSEX            NO,  PROCESS                                 
         MVI   PRCSS,NO            YES, BYPASS                                  
         B     NETRNSEX            RETURN                                       
*                                                                               
         USING NEORECD,R8          MAP  OUTPUT    TAPE RECORD                   
NEORTN   DS    0H                                                               
         LA    R8,TAPEWK           ->   OUTPUT    TAPE AREA                     
         MVC   NEOIDT+4(2),TRIDAT  YEAR                                         
         MVC   NEOIDT(4),TRIDAT+2  MONTH     AND  DAY                           
         UNPK  NEONET,TRNINV                                                    
         UNPK  NEOCOM,TRNCOM                                                    
         ZAP   DUB,TRNINV                                                       
         AP    DUB,TRNCOM                                                       
         UNPK  NEOGRS,DUB                                                       
         UNPK  NEODIS,TRNDIS                                                    
         MVC   NEOCOD,TRNJOB                                                    
         MVC   NEOJOB,TRNJOB                                                    
         MVC   NEOSDT+2(1),TRIDAT+1                                             
         MVC   NEOSDT(2),TRIDAT+2                                               
         MVC   NEOEST,SAVBLPR                                                   
         TM    SVTEST,SVTSSCPY     MOVE SUBSIDIARY     NAME TO   RPT ?          
         BO    NEO200              YES, SKIP THIS NONSENSE                      
*                                                                               
         CLC   NEUSERPO,SPACES                                                  
         BNH   *+10                                                             
         MVC   NEOEST,NEUSERPO     POB  OR   USER FLD  TO   NEOEST              
*                                                                               
*                                  3-6  CHARS     OF   JOBNAME   TYPE           
         CLC   TRNCLI(3),=C'GRY'        CLIENTS ?                               
         BE    NEO100                                                           
         CLC   TRNCLI(3),=C'GRD'                                                
         BE    NEO100                                                           
         CLC   TRNCLI(2),=C'GT'    CLIENT'S  1ST  TWO  CHARS     'GT' ?         
         BNE   NEO200              NO                                           
*                                                                               
NEO100   MVC   NEOEST(4),SAVNAME+2                                              
         CLC   NEUSERPR(L'NEOEST),SPACES                                        
         BNH   *+10                                                             
         MVC   NEOEST(4),NEUSERPR  NOTE:     NEUSERPR  IS   ALREADY +2          
*                                                                               
NEO200   MVC   NEONUM,TRINUM                                                    
         MVC   NEOPRD,SVBPRD                                                    
         MVC   NEOMED,TRNJOB                                                    
         MVC   NEOAGY,NHAGY                                                     
         MVI   NEOIDN,C'5'                                                      
         GOTO1 AWRTAPE                                                          
*                                                                               
NETRNSEX B     NEEXIT              RETURN    TO   CALLER                        
         DROP  R8                                                               
         EJECT ,                                                                
***********************************************************************         
* CONSTANTS AND LITERALS                                              *         
***********************************************************************         
         SPACE 1                                                                
NEDDWTAB DC    CL5'LVS78'                                                       
         SPACE 2                                                                
         LTORG                                                                  
         TITLE 'ACI102NE2 - DNNYE 2 -NEEDHAM CUSTOM INSIDE CHARGES RTN'         
SUBRNE2  NMOD1 0,**I1NE**                                                       
         L     RC,ADRC             RESTORE   RC                                 
*                                                                               
         CLI   MODE,PROCTRNS                                                    
         BE    NE2TRNS                                                          
*                                                                               
NE2EXIT  XIT1  ,                                                                
         EJECT ,                                                                
***********************************************************************         
* PROCESS A TRANSACTION                                               *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNELD,R7           TRANSACTION    ELEMENT                       
         SPACE 1                                                                
NE2TRNS  DS    0H                  PROCESS   A    TRANSACTION                   
         CLI   GOSTAT,C'I'                                                      
         BNE   NE2EXIT                                                          
*                                                                               
         MVC   NHIHEAD,=CL25'EXTRACT OF INSIDE CHARGES'                         
         L     R7,ADTRANS          ->   TRANSACTION    ELEMENT                  
         CLI   TRNEL,TRNELQ        X'44' -   TRANSACTION    ELEMENT ?           
         BNE   NE2T600             NO,  EXIT - SOMETHING    VERY WRONG          
*                                                                               
         MVI   ELCODE,TRSELQ       X'60' -   TRANSACTION    STATUS   EL         
         GOTO1 ANEXTEL             ANY  TRANSACTION    STATUS    EL ?           
         BNE   NE2T600             NO,  BYPASS    TRANSACTION                   
*                                                                               
         USING TRSELD,R7           TRANSACTION    STATUS    ELEMENT             
         GOTO1 DATCON,DMCB,(2,TRSDATE),(1,SVDAT3)                               
*                                                                               
         CLC   SVDAT3,QSTR3        BEFORE    BEGIN     DATE                     
         BL    NE2T600             YES, BYPASS    TRANSACTION                   
         CLC   SVDAT3,QEND3        AFTER     END       DATE ?                   
         BH    NE2T600             YES, BYPASS    TRANSACTION                   
*                                                                               
         USING TRNRECD,R6                                                       
         L     R6,ADTRKEY          ->   TRAN RECORD    KEY                      
         CLC   TRNKULC(2),=C'SC'   CONTRA    ACCOUNT   UNIT/LEDGER ?            
         BE    NE2T200             YES, PROCESS                                 
         CLC   TRNKULC(2),=C'SI'   CONTRA    ACCOUNT   UNIT/LEDGER ?            
         BNE   NE2T600             NO,  BYPASS    RECORD                        
*                                                                               
NE2T200  CLC   TRNKULC(7),=C'SCC02  '                  PER  ACUS 5/94           
         BNE   NE2T250             BYPASS    TYPE 3'S  ON   SCC02               
*                                                                               
         USING TRNELD,R7           TRANSACTION    ELEMENT                       
         L     R7,ADTRANS          ->   TRANSACTION    ELEMENT                  
         CLI   TRNTYPE,3           INPUT     TYPE 3 ?                           
         BE    NE2T600             YES, BYPASS    TRANSACTION                   
*                                                                               
NE2T250  L     R1,=A(NE2CODES)     ->   CONTRA    ID   CODES     TABLE          
*                                                                               
NE2T300  CLC   TRNKCACT+1(2),0(R1) ID   MATCH ?                                 
         BL    NE2T600             LOW, BYPASS    TRANSACTION                   
         BE    NE2T400             YES, PROCESS   TRANSACTION                   
*                                                                               
         LA    R1,2(,R1)           NEXT TBL  ENTRY                              
         CLI   0(R1),X'FF'         END  OF   TABLE ?                            
         BNE   NE2T300             NO,  TRY  AGAIN                              
         B     NE2T600             YES, BYPASS    TRANSACTION                   
         DROP  R6                                                               
*                                                                               
NE2T400  MVC   TRINUM,SPACES       CLEAR                                        
         MVC   TRINUM(2),0(R1)     SAVE THE  ID                                 
*                                                                               
         USING NE2IREC,R8          NEEDHAM   INSIDE    CHARGES   RCD            
         USING TRNELD,R7           TRANSACTION    ELEMENT                       
         LA    R8,TAPEWK           ->   OUTPUT    TAPE AREA                     
         L     R7,ADTRANS          ->   TRANSACTION    ELEMENT                  
         MVC   NE2IOFF,SVAOFF      UNIT FOR  ANALYSIS  (SEE LEVA RTN)           
*                                                                               
         CLC   ALPHAID,=C'NR'      REQUESTED BY   DNRFO ?                       
         BNE   NE2T450             NO,  CONTINUE                                
         CLI   SVAOFF,C'L'         DNRFO'S   LA   OFFICE ?                      
         BNE   NE2T450             NO,  CONTINUE                                
         MVI   NE2IOFF,C'A'        USE  CHAR A                                  
*                                                                               
NE2T450  MVC   NE2ICOD,0(R1)       SAVE ID                                      
         UNPK  NE2IAMT,TRNAMNT     CHARGE    AMOUNT                             
         ZAP   TRNINV,TRNAMNT      SAVE CHARGE    AMOUNT    FOR  REPORT         
         LA    R1,WORK40           ->   WORK AREA                               
         GOTO1 AGETCMOS            CONVERT   MOS  DATE                          
         MVC   NE2IBYY,WORK40      BILLING   YEAR                               
         MVC   NE2IBMM,WORK40+2    BILLING   MONTH                              
         MVC   NE2ISDT,SPACES      BILLING   DATE                               
*                                                                               
         USING ACMD,R4             MAP  ACMASTD                                 
         L     R4,AMONACC                                                       
         L     R4,ACMAPRO2         ->   PROD TRANSACTION     ACTIVITY E         
*                                                                               
         USING PTAELD,R4           MAP  PROD TRANSACTION     ACTIVITY E         
NE2T460  CLI   PTAEL,0             ANY  MORE ELEMENTS ?                         
         BE    NE2T500             NO,  SKIP BILL DATE                          
         CLI   PTAEL,PTAELQ        X'77' -   PROD TRANSACTION    ACT ?          
         BNE   NE2T470             NO,  GET  NEXT ELEMENT                       
         CLI   PTATYPE,PTATRAL     BILLING   ELEMENT ?                          
         BNE   NE2T470             NO,  GET  NEXT ELEMENT                       
         TM    PTASTAT1,PTASPEND   IS   IT   PENDING ?                          
         BO    NE2T470             YES, GET  NEXT ELEMENT                       
*                                  GET  BILL DATE                               
         GOTO1 DATCON,DMCB,(2,PTARBLDT),(X'20',TRIDAT)                          
         MVC   NE2ISDT+2(2),TRIDAT BILL DATE MM                                 
         MVC   NE2ISDT(2),TRIDAT+2 BILL DATE YY                                 
         B     NE2T500             CONTINUE                                     
*                                                                               
NE2T470  SR    R0,R0               GET  NEXT ELEMENT                            
         IC    R0,1(,R4)                                                        
         AR    R4,R0                                                            
         B     NE2T460                                                          
*                                                                               
*                                                                               
NE2T500  MVC   NE2IPRD,SVBPRD      PRODUCT   NAME                               
         MVC   NE2IIDN(6),=C'SP010P'    ID                                      
         GOTO1 AWRTAPE             WRITE     RECORD                             
         GOTO1 AREPORT             WRITE     REPORT                             
*                                                                               
NE2T600  MVI   PRCSS,NO                                                         
         B     NE2EXIT                                                          
         DROP  R4,R7                                                            
         EJECT ,                                                                
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         TITLE 'ACI102PG - P && G - SAATCHI - PROD INTERFACE ROUTINE'           
         TITLE 'ACI102TB - TBC - TED BATES PROD INTERFACE ROUTINE'              
         TITLE 'ACI102WI - WILA - WESTERN PRODUCTION INTERFACE ROUTINE'         
SUBRWI   NMOD1 0,**I1WI**                                                       
         L     RC,ADRC             RESTORE   RC                                 
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    WIREQF                                                           
         CLI   MODE,PROCTRNS                                                    
         BE    WIPTRN                                                           
*                                                                               
WIEXIT   XIT1  ,                                                                
         EJECT ,                                                                
***********************************************************************         
* PROCESS REQUEST FIRST                                               *         
***********************************************************************         
         SPACE 1                                                                
WIREQF   DS    0H                  FIRST     FOR  REQUEST                       
         MVI   FCGETOPT,FCGETOJB   NEED JOB  LEVEL     GETOPT    CALL           
         B     WIEXIT                                                           
         EJECT ,                                                                
***********************************************************************         
* PROCESS A TRANSACTION                                               *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNELD,R6                                                        
         SPACE 1                                                                
WIPTRN   DS    0H                  FIRST     FOR  REQUEST                       
         L     R6,ADTRANS                                                       
         CLI   GOSTAT,C'I'                                                      
         BNE   WIPTRN10                                                         
         CLC   TRNANAL,=C'99'      NO   99'S FOR  NOW                           
         BE    WIEXIT                                                           
         BAS   RE,WISAVTAX         SAVE TAX  ON   DEBITS                        
         B     WIEXIT                                                           
*                                                                               
WIPTRN10 CLC   TRNANAL,=C'99'      99'S ONLY PAST HERE                          
         BNE   WIEXIT                                                           
         DROP  R6                                                               
*                                                                               
         USING WIRECD,R8           WILA TAPE RECORD                             
         LA    R8,TAPEWK           ->   OUTPUT   TAPE AREA                      
         MVC   TAPEWK(132),SPACES  FILL WITH 132 SPACES                         
         MVC   TAPEWK+132(3),SPACES     THEN 3   MORE (LRECL=135)               
         MVC   WIRECORD,SPACES                                                  
*                                                                               
         L     R7,ADGOBLOC                                                      
         USING ACGOD,R7                                                         
         MVC   WIOFF,GOEFFOFC                                                   
         MVC   WIMED,=C'AJ'                                                     
*                                                                               
         L     R7,ADACC                                                         
         MVC   WICLI,3(R7)                                                      
         MVC   WIPRO,6(R7)                                                      
         MVC   WIJOB,9(R7)                                                      
*                                                                               
         LA    R0,L'WICLINM                                                     
         L     R2,ADLVANAM                                                      
         LA    R3,WICLINM                                                       
         BAS   RE,WINAMOUT                                                      
*                                                                               
         LA    R0,L'WIJOBNM                                                     
         L     R2,ADACCNAM                                                      
         LA    R3,WIJOBNM                                                       
         BAS   RE,WINAMOUT                                                      
*                                                                               
         USING TRNELD,R7                                                        
         L     R7,ADTRANS          ->   TRANSACTION                             
         MVC   WIINV,TRNREF        REFERENCE NUMBER                             
         GOTO1 DATCON,DMCB,(1,TRNDATE),(X'20',WIBDATE) TRANSACTION DATE         
*                                                                               
         BAS   RE,WIDUEOUT         GET  DUE  DATE                               
         BAS   RE,WIMOSOUT         GET  POSTING   MOS                           
*                                                                               
*              NUMERIC AMOUNTS                                                  
*                                                                               
         BAS   RE,WISETTAX         GET  ANY  TAXS FROM TABLE                    
         BAS   RE,WISETGST         GET  GST  TAX  FROM BILLING   RECORD         
         BAS   RE,WISETPST         GET  PST  TAX  FROM BILLING   RECORD         
*                                                                               
         LA    R2,TRNREC           GET  AMOUNT    DUE                           
         LA    R3,WIREC                                                         
         BAS   RE,WIAMTOUT                                                      
*                                                                               
         LA    R2,TRNDIS           GET  DISCOUNT  AMOUNT                        
         LA    R3,WICD                                                          
         BAS   RE,WIAMTOUT                                                      
*                                                                               
         LA    R2,WITAXAMT         GET  TAX  AMOUNT                             
         LA    R3,WITAX                                                         
         BAS   RE,WIAMTOUT                                                      
*                                                                               
         LA    R2,WIGSTAMT         GET  GST  TAX  AMOUNT                        
         LA    R3,WIGST                                                         
         BAS   RE,WIAMTOUT                                                      
*                                                                               
         LA    R2,WIPSTAMT         GET  PST  TAX  AMOUNT                        
         LA    R3,WIPST                                                         
         BAS   RE,WIAMTOUT                                                      
*                                                                               
         MVC   WIID,ALPHAID                                                     
*                                                                               
         ZAP   WIACRAMT,TRNREC     BASE AMOUNT                                  
         SP    WIACRAMT,TRNDIS     MINUS     DISCOUNT                           
         AP    WIACRAMT,WITAXAMT   PLUS TAXES                                   
         AP    WIACRAMT,WIGSTAMT   PLUS GST  TAXES                              
         AP    WIACRAMT,WIPSTAMT   PLUS PST  TAXES                              
         LA    R2,WIACRAMT                                                      
         LA    R3,WIACR                                                         
         BAS   RE,WIAMTOUT                                                      
*                                                                               
         GOTO1 AWRTAPE             WRITE     TAPE RECORD                        
*                                                                               
         B     WIEXIT              EXIT                                         
*                                                                               
         DROP  R7,R8                                                            
         EJECT ,                                                                
***********************************************************************         
* MOVE NAME FROM A GENERAL NAME ELEMENT TO AN OUTPUT FIELD            *         
*                                                                     *         
*   INPUT:                                                            *         
*     R0 - MAXIMUM LENGTH OF THE FIELD                                *         
*     R2 - ADDRESS OF THE GENERAL NAME ELEMENT (X'20' ELEMENT)        *         
*     R3 - ADDRESS OF THE OUTPUT FIELD                                          
*                                                                     *         
*   NOTE:                                                             *         
*     ASSUMES THAT THE FIELD CONTAINS SPACES ON ENTRY                 *         
***********************************************************************         
         SPACE 1                                                                
         USING NAMELD,R2                                                        
         SPACE 1                                                                
WINAMOUT NTR1                                                                   
         SR    R1,R1                                                            
         BCTR  R0,0                MAX  LEN  -1   FOR  EXECUTE                  
         IC    R1,NAMLN            GET  LEN  OF   ELEMENT                       
         SH    R1,=Y(NAMLN1Q+1)    MINUS     BASE EL   LEN  +1   FOR EX         
         CR    R1,R0               LEN  TOO  BIG ?                              
         BNH   *+6                 NO,  SKIP                                    
         LR    R1,R0               YES, USE  MAX  LENGTH                        
         EXMVC R1,0(R3),NAMEREC    MOVE THE  NAME                               
         B     WIEXIT              RETURN                                       
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
* FILL IN WIDUE WITH THE DUE DATE FROM THE TRANSACTION                *         
***********************************************************************         
         SPACE 1                                                                
         USING DUEELD,R7           DUE  DATE ELEMENT                            
         USING WIRECD,R8           WILA TAPA RECORD                             
         SPACE 1                                                                
WIDUEOUT NTR1                                                                   
         MVC   WIDUE,SPACES        DUE  DATE ON   TAPE                          
         L     R7,ADTRANS                                                       
         MVI   ELCODE,DUEELQ       X'61'                                        
         GOTO1 ANEXTEL             GET  DUE  DATE ELEMENT                       
         BNE   WIDUEX              NONE,     SKIP                               
         GOTO1 DATCON,DMCB,(2,DUEDATE),(X'20',WIDUE)                            
*                                                                               
WIDUEX   B     WIEXIT              RETURN                                       
         DROP  R7,R8                                                            
         EJECT ,                                                                
***********************************************************************         
* FILL IN WIMOS WITH THE POSTING MOS FROM THE TRANSACTION             *         
***********************************************************************         
         SPACE 1                                                                
         USING TRSELD,R7           TRANSACTION    STATUS    ELEMENT             
         USING WIRECD,R8           WILA TAPA RECORD                             
         SPACE 1                                                                
WIMOSOUT NTR1                                                                   
         MVC   WIMOS,SPACES        POSTING   MOS                                
         L     R7,ADTRANS                                                       
         MVI   ELCODE,TRSELQ       X'60'                                        
         GOTO1 ANEXTEL             GET  THE  ELEMENT                            
         BNE   WIMOSX              NONE,     SKIP                               
         MVC   FULL(2),TRSPMOS     POSTING   MOS  (PACKED YYMM)                 
         MVI   FULL+2,1                                                         
         GOTO1 DATCON,DMCB,(1,FULL),(X'20',WORK)                                
         MVC   WIMOS,WORK                                                       
*                                                                               
WIMOSX   B     WIEXIT              RETURN                                       
         DROP  R7,R8                                                            
         EJECT ,                                                                
***********************************************************************         
* EDIT A PL6 FIELD IN R2 INTO A CL9 FIELD IN R3, FILL=0, FLOAT=-      *         
***********************************************************************         
         SPACE 1                                                                
WIAMTOUT NTR1                                                                   
         EDIT  (P6,0(R2)),(9,0(R3)),FILL=0                                      
         CP    0(6,R2),P0                                                       
         BNL   *+8                                                              
         MVI   0(R3),C'-'                                                       
         B     WIEXIT                                                           
         EJECT ,                                                                
***********************************************************************         
* THIS ROUTINE ACCUMULATES BILLED TAX                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNELD,R6           MAP   TRANSACTION    ELEMENT                 
         SPACE 1                                                                
WISAVTAX NTR1                                                                   
         L     R6,ADTRANS          ->    TRANSACTION    ELEMENT                 
         CLI   TRNEL,TRNELQ        X'44' ELEMENT ?                              
         BNE   WISVTXEX            NO,   EXIT                                   
         CLC   TRNANAL,=C'99'      WORK  CODE 99 ?                              
         BE    WISVTXEX            YES,  EXIT                                   
         MVI   ELCODE,SUTELQ       X'5F' -    SALES/USE TAX  ELEMENT            
         LR    R7,R6               ->    TRANSACTION    ELEMENT                 
         GOTO1 ANEXTEL             ANY   SALES/USE TAX  EL ?                    
         BNE   WISVTXEX            NO,   EXIT                                   
*                                                                               
         USING SKD,R5                                                           
         LA    R5,SKREC            SET   SK   RCD  FOR  BINSRCH   TABLE         
         MVC   SKREC(SKLEN),SPACES                                              
*                                                                               
         ZAP   WISKAMT,P0                                                       
*                                                                               
         USING ACMD,R4             MAP  ACMASTD                                 
         L     R4,AMONACC                                                       
         L     R4,ACMAPRO2                                                      
*                                                                               
         USING PTAELD,R4           MAP  PROD TRANSACTION    ACTIVITY E          
WISVTX10 CLI   PTAEL,0             ANY  MORE ELEMENTS ?                         
         BE    WISVTXEX            NO,  EXIT                                    
         CLI   PTAEL,PTAELQ        X'77' -   PROD TRANSACTION    ACT E          
         BE    WISVTX25            YES, PROCESS   IT                            
*                                                                               
WISVTX20 SR    R0,R0               GET  NEXT ELEMENT                            
         IC    R0,1(,R4)                                                        
         AR    R4,R0                                                            
         B     WISVTX10                                                         
*                                                                               
WISVTX25 CLI   PTATYPE,PTATRAL     BILLING   ELEMENT ?                          
         BNE   WISVTX20            NO,  GET  NEXT ELEMENT                       
         TM    PTASTAT1,PTASPEND   PENDING ?                                    
         BO    WISVTX20            YES, GET  NEXT ELEMENT                       
         TM    PTASTAT1,PTASREVU   REVERSAL  OF   THIS BILL UPDATED ?           
         BO    WISVTX40            YES, PROCESS   IT                            
         TM    PTASTAT1,PTASREVS   REVERSAL  ALLOCATION ?                       
         BO    WISVTX50            YES, PROCESS   IT                            
*                                                                               
*                                  NOT  REVERSAL                                
         MVC   SKINVC,PTARBLNO     BILL NUMBER                                  
         MVC   WISKDATE,PTARBLDT   SAVE BILL DATE                               
         ZAP   SKAMT,PTANET        ADD  THIS PARTIAL   BILLING                  
         GOTO1 AADDIT,(R4)         ADD  TO   BINTABLE                           
         B     WISVTX20                                                         
*                                                                               
*                                  REVERSAL  OF   THIS BILL UPDATED             
*                                       X'02'                                   
WISVTX40 MVC   SKINVC,PTARUNBB     UNBILL    BILL NUMBER                        
         CLC   SKINVC,SPACES       IS   THERE     A    BILL NUMBER ?            
         BH    *+10                YES, SKIP                                    
*                                  NO,  USE  FFFFFFFFFFFF   KEY  FOR            
         MVC   SKINVC,=6X'FF'           REVERSAL  BILLING   NUMBER              
         ZAP   SKAMT,PTANET        BILL AMOUNT                                  
         MVC   WISKDATE,PTARUNBD   UNBILL    BILL DATE                          
         GOTO1 AADDIT,(R4)                                                      
         B     WISVTX20            LOOK FOR  ANOTHER   77   ELEMENT             
*                                                                               
*                                  REVERSAL  ALLOCATION     X'08'               
WISVTX50 MVC   SKINVC,PTARORGB     ORIGINAL  BILL NUMBER                        
         CLC   SKINVC,SPACES       IS   THERE     A    BILL NUMBER ?            
         BH    *+10                YES, SKIP                                    
*                                  NO,  USE  FEFEFEFEFEFE   KEY  FOR            
         MVC   SKINVC,=6X'FE'           REVERSAL  NUMBER                        
         ZAP   SKAMT,PTANET        BILL AMOUNT                                  
         MVC   WISKDATE,PTARORGD   ORIGINAL  BILL DATE                          
         GOTO1 AADDIT,(R4)                                                      
         B     WISVTX20            LOOK FOR  ANOTHER   77   ELEMENT             
*                                                                               
WISVTXEX B     WIEXIT              RETURN                                       
         DROP  R4,R5,R6                                                         
         EJECT ,                                                                
***********************************************************************         
* FILL IN WIGSTAMT WITH THE VAT AMOUNT BILLED                         *         
***********************************************************************         
         SPACE 1                                                                
         USING VBIELD,R7           VAT  BILLED    ELEMENT                       
         USING WIRECD,R8           WILA TAPA RECORD                             
         SPACE 1                                                                
WISETGST NTR1                                                                   
         ZAP   WIGSTAMT,P0         CLEAR                                        
         L     R7,ADTRANS                                                       
         MVI   ELCODE,VBIELQ       X'48'                                        
*                                                                               
WISETG10 GOTO1 ANEXTEL             ANY  VAT  BILLED    ELEMENT ?                
         BNE   WISETGX             NONE,     SKIP                               
         AP    WIGSTAMT,VBIVAT     ADD  VAT  BILLED    AMOUNT                   
         B     WISETG10            SEE  IF   ANY  MORE ELEMENTS                 
*                                                                               
WISETGX  B     WIEXIT              RETURN                                       
         DROP  R7,R8                                                            
         EJECT ,                                                                
***********************************************************************         
* FILL IN WIPSTAMT WITH THE PST TAX AMOUNT BILLED                     *         
***********************************************************************         
         SPACE 1                                                                
         USING PBIELD,R7           PST  BILLED    ELEMENT                       
         USING WIRECD,R8           WILA TAPA RECORD                             
         SPACE 1                                                                
WISETPST NTR1                                                                   
         ZAP   WIPSTAMT,P0         CLEAR                                        
         L     R7,ADTRANS                                                       
         MVI   ELCODE,PBIELQ       X'7D'                                        
*                                                                               
WISETP10 GOTO1 ANEXTEL             ANY  PST  BILLED    ELEMENT ?                
         BNE   WISETPX             NONE,     SKIP                               
         AP    WIPSTAMT,PBIPST     ADD  PST  TAX  AMOUNT                        
         B     WISETP10            SEE  IF   ANY  MORE ELEMENTS                 
*                                                                               
WISETPX  B     WIEXIT                                                           
         DROP  R7,R8                                                            
         EJECT ,                                                                
***********************************************************************         
* SET WITAXAMT TO THE AMOUNT OF TAX ON A BILL                         *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNELD,R6                                                        
         USING WIRECD,R8           WILA TAPA RECORD                             
         SPACE 1                                                                
WISETTAX NTR1                                                                   
         L     R6,ADTRANS                                                       
         CLI   TRNEL,TRNELQ        X'44' ?                                      
         BNE   WITAXOEX                                                         
         CLC   TRNANAL,=C'99'      WORK-CODE =    99 ?                          
         BNE   WITAXOEX            NO,  EXIT                                    
*                                                                               
         ZAP   WITAXAMT,P0         INIT TAX                                     
*                                                                               
         USING BIND,R5                                                          
         L     R5,ASKTAB                                                        
         L     R3,BININ                                                         
         LTR   R3,R3                                                            
         BZ    WITAXOEX            NOTHING   IN   TABLE                         
*                                                                               
         LA    R5,BINTABLE                                                      
*                                                                               
         USING SKD,R5                                                           
         USING TRNELD,R6                                                        
         L     R6,ADTRANS                                                       
*                                                                               
WITAXO20 CLC   WISKINVC,TRNREF     SAME REFERENCE NUMBER ?                      
         BNE   WITAXO25            NO,  SKIP                                    
*                                  YES, CHECK     DATE                          
*                                  CONVERT   TO   COMPRESSED                    
         GOTO1 DATCON,DMCB,(1,TRNDATE),(2,HALF)                                 
         CLC   HALF,WISKDATE                                                    
         BE    WITAXO40            SAME,     USE  IT                            
*                                                                               
WITAXO25 CLI   WISKINVC,X'FE'      TAX  FOR  A    REVERSAL ?                    
         BL    WITAXO50            NO,  GET  NEXT SK   ITEM                     
*                                                                               
WITAXO30 CLC   WISKDATE(2),TRNNARR+33   DATE MATCH ?                            
         BNE   WITAXO50                 NO,  GET  NEXT SK   ITEM                
*                                                                               
*                                                                               
WITAXO40 AP    WITAXAMT,WISKAMT                                                 
         MVC   WISKDATE(2),SPACES  ONE  BILL PER  TABLE     ITEM                
*                                                                               
WITAXO50 LA    R5,SKLEN(,R5)                                                    
         BCT   R3,WITAXO20                                                      
*                                                                               
WITAXOEX B     WIEXIT              RETURN                                       
         DROP  R5,R6,R8                                                         
         EJECT ,                                                                
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         TITLE 'ACI102YN - YNRO PRODUCTION INTERFACE ROUTINE'                   
***********************************************************************         
* NOTE THE I1 CLEARS TYPE 2 BUFFALO RECORDS AT ACCLAST                *         
* WHEN SVTEST'S SVTSCLR2 (X'40') BIT IS ON                            *         
*                                                                     *         
* REVISION HISTORY                                                    *         
* 4/95  JPS - MAJOR OVERHAUL - INCLUDED ACGENFILE                     *         
*             MADE PROGRAM CODE CHANGES SPECIFIC FOR CLIENT PMU       *         
***********************************************************************         
         SPACE 1                                                                
SUBRYN1  NMOD1 0,**I1YN**                                                       
         L     RC,ADRC             RESTORE   RC                                 
*                                                                               
         CLC   QACCOUNT(3),=C'PMU' WAS  PMU  REQUESTED?                         
         BE    YNPMUL              YES, USE  PMU  LOGIC                         
         CLI   MODE,PROCTRNS                                                    
         BE    YNPROC                                                           
         B     YNEXIT                                                           
*                                                                               
YNPMUL   CLI   MODE,REQFRST        PMU  LOGIC                                   
         BE    YNREQF                                                           
         CLI   MODE,PROCACC                                                     
         BE    YNPRACC                                                          
         CLI   MODE,PROCTRNS                                                    
         BE    YNPTRN                                                           
*                                                                               
YNEXIT   XIT1  ,                                                                
         EJECT ,                                                                
***********************************************************************         
* REQUEST FIRST                                                       *         
***********************************************************************         
         SPACE 1                                                                
YNREQF   DS    0H                                                               
         MVC   YNSVEST,SPACES      SPACE     OUT  FIELD                         
         B     YNEXIT                                                           
         EJECT ,                                                                
***********************************************************************         
* PROCESS ACCOUNT                                                     *         
***********************************************************************         
         SPACE 1                                                                
YNPRACC  DS    0H                                                               
         MVC   YNSVEST,SPACES      SPACE     OUT  FIELD                         
         B     YNEXIT                                                           
         EJECT ,                                                                
***********************************************************************         
* PROCESS TRANSACTIONS                                                *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNELD,R6           TRANSACTION    ELEMENT                       
         SPACE 1                                                                
YNPTRN   DS    0H                                                               
         L     R6,ADTRANS                                                       
         CLI   TRNEL,TRNELQ        X'44'     TRANSACTION    EL ?                
         BNE   YNPTRNX             NO,  EXIT                                    
         CLI   GOSTAT,C'I'                                                      
         BNE   YNPTRN03                                                         
         GOTO1  AGETFEE                                                         
         B     YNEXIT                                                           
*                                                                               
YNPTRN03 CLC   TRNANAL,=C'99'      MUST BE   ANALYSIS  CODE '99'                
         BNE   YNPTRNX                                                          
*                                                                               
         USING UFSELD,R7                                                        
         L     R7,ADACC                                                         
         AH    R7,DATADISP                                                      
         MVI   ELCODE,UFSELQ       X'A2' - USER FLD  SELECT      EL             
*                                                                               
YNPTRN05 GOTO1 ANEXTEL                                                          
         BNE   YNPTRN10                                                         
         CLC   UFSCODE,=CL2'PE'    ESTIMATE     NUMBER?                         
         BNE   YNPTRN05                                                         
         SR    R1,R1                                                            
         IC    R1,UFSLN                                                         
         SH    R1,=Y(UFSLN1Q+1)                                                 
         BM    YNPTRN10                                                         
         EXMVC R1,YNSVEST,UFSDATA  SAVE AWAY ESTIMATE     NUMBER                
*                                                                               
YNPTRN10 MVC   P2X(6),=C'ESTNO='                                                
         MVC   P2X+6(10),YNSVEST                                                
*                                                                               
*                                  ************************************         
*                                  * PREP  40 RECORD *                          
*                                  ************************************         
*                                                                               
         USING YNPMURD1,R8         MAP  OUTPUT    TAPE RECORD                   
         LA    R8,TAPEWK           ->   OUTPUT    TAPE AREA                     
         MVC   YNPMUEST,YNSVEST                                                 
         MVC   YNPMUJOB,TRNJOB                                                  
         MVC   YNPMUINV(1),TRNJOB                                               
         MVC   YNPMUINV+1(L'TRINUM),TRINUM                                      
         MVC   TRIMED,TRNJOB                                                    
         MVC   YNPMURTY,=C'40'                                                  
         MVC   YNPMUDT1(L'YNPMUDT1+L'YNPMUDUE),SPACES                           
         MVI   YNPMUWC1-1,C'0'                                                  
         MVC   YNPMUVN#(9),=CL9'331543PDC'                                      
         LH    R1,SVRECL           R1 = RCD  LENGTH                             
         SLL   R1,16               SHIFT     LEN  TO   HIGH ORDER HALF          
         STCM  R1,15,YNPMULEN      SAVE RCD  LENGTH                             
         ZAP   YNPMUNC1,P0                                                      
         ZAP   YNPMUGRS,P0                                                      
         XC    BUFWKC,BUFWKC       MAKE WORK-CODE LOW  HEX  ZEROES              
         ZAP   YNSVNC,P0                                                        
         ZAP   YNSVREC,P0                                                       
         ZAP   YNSVNET,P0                                                       
         MVI   BUFREC,2            TO   PROCESS   TYPE 2   ACCUMULATORS         
         MVI   DMCB+8,0                                                         
*                                                                               
         XC    BUFKEY,BUFKEY                                                    
         MVC   BUFINV,TRINUM       FIND INVOICE   NUM  IN  BUFFALO              
         MVC   YNCBUF#,TRINUM      INVOICE   NUM  TO   COMPARE  WITH            
         MVI   YNCHKAMT,YES        VALIDATE  INVOICE   AMOUNT                   
         BAS   RE,YNWRBUFF         WRITE     RCD  FROM BUFFALO                  
         CLI   YNBLD43,YES         BUILD     43   RECORD ?                      
         BE    YNPTR100            YES, BUILD     IT                            
*                                                                               
*                                  ************************************         
*                                  * MATCH REVERSED CHARGES WITH THE  *         
*                                  * ORIGINAL BILL                    *         
*                                  ************************************         
*                                                                               
         XC    BUFKEY,BUFKEY       LOOK FOR  REVERSED  CHARGE    DATA           
         MVC   BUFINV(4),=X'FEFEFEFE'   MOVE HIGH VALUES    TO  INV NUM         
         OC    TRNUNBIL,TRNUNBIL   HAS  THIS BILL BEEN REVERSED ?               
         BZ    YNPTRN70                                                         
         MVC   BUFINV+4(2),TRN2DAY MOVE IN   TRANSACTION    DATE                
         MVC   YNSVINV(L'BUFINV),BUFINV                                         
         MVC   YNCBUF#,YNSVINV     INVOICE   NUM  TO   COMPARE   WITH           
         MVI   YNCHKAMT,YES        VALIDATE  INVOICE   AMOUNT                   
         BAS   RE,YNWRBUFF         WRITE     RCD  FROM BUFFALO                  
         CLI   YNBLD43,YES         BUILD     43   RECORD ?                      
         BE    YNPTR100            YES, BUILD     IT                            
*                                                                               
*                                  ************************************         
*                                  * MATCH REVERSED CHARGES WITH      *         
*                                  * REVERSAL BILL                    *         
*                                  ************************************         
*                                                                               
YNPTRN70 XC    BUFKEY,BUFKEY       LOOK FOR REVERSAL   CHARGE    DATA           
         MVC   BUFINV(4),=X'FFFFFFFF'   HIGH VALUES    TO   INV  NUMBER         
         MVC   BUFINV+4(2),TRN2DAY MOVE IN   TRANSACTION    DATE                
         MVC   YNSVINV(L'BUFINV),BUFINV                                         
         MVC   YNCBUF#,YNSVINV     INVOICE   NUM  TO   COMPARE   WITH           
         MVI   YNCHKAMT,NO         DO   NOT  VALIDATE  INVOICE   AMOUNT         
         BAS   RE,YNWRBUFF         WRITE     RCD  FROM BUFFALO                  
         CLI   YNBLD43,YES         BUILD     43   RECORD ?                      
         BE    YNPTR100            YES, BUILD     IT                            
*                                                                               
*                                  ************************************         
*                                  * BUILD A 43 RECORD                *         
*                                  ************************************         
*                                                                               
YNPTR100 MVC   YNPMUDT1,TRIDAT                                                  
         MVC   YNPMUDUE,SVDUE                                                   
         MVC   YNPMUWC1-1(L'YNPMUWC1+1),SPACES                                  
*                                  NEEDED    FOR  OVERALL   TOTAL               
         MVC   YNPMURTY,=C'43'               BY   INV-NO                        
         ZAP   YNPMUNET,TRNREC                                                  
         AP    YNPMUNET,TRNDIS     ADD  BACK CD                                 
         ZAP   YNPMUGRS,TRNREC                                                  
         ZAP   YNPMUNC1,YNSVNC     FROM W/C  TOTALS                             
         BAS   RE,YNPRTTPE                                                      
         GOTO1 AWRTAPE                                                          
*                                                                               
YNPTRNX  B     YNEXIT              EXIT,     RETURN    TO   CALLER              
         DROP  R6,R7,R8                                                         
         EJECT ,                                                                
**********************************************************************          
* WRITE RECORD(S) FROM BUFFALO                                       *          
*                                                                    *          
*   INPUT:                                                           *          
*     YNCBUF#  - INVOICE  NUMBER TO  COMPARE WITH                    *          
*     YNCHKAMT - VALIDATE INVOICE    AMOUNT (Y/N)                    *          
*     BUFKEY   - BUFFALO  KEY                                        *          
*     YNSVNC   - SAVED    NON-COM    AMOUNT                          *          
*     YNSVREC  - SAVED    RECEIVABLE AMOUNT                          *          
*     YNSVNET  - SAVED    NET        AMOUNT                          *          
*     TRNINV   - TRANSACTION         INVOICE NUMBER                  *          
*                                                                    *          
*   OUTPUT:                                                          *          
*     YNBLD43  - BUILD A 43 RECORD (Y/N)                             *          
**********************************************************************          
         SPACE 1                                                                
YNWRBUFF NTR1                                                                   
         MVI   YNBLD43,NO                                                       
         GOTO1 BUFFALO,DMCB,=C'HIGH',(2,ADBUFC),BUFREC,1                        
         CLI   DMCB+8,0            ANY  INVOICE   NUMBER    FOUND ?             
         BNE   YNWRBFEX            NO,  EXIT                                    
         CLC   BUFINV,YNCBUF#      RIGHT     INVOICE   NUMBER ?                 
         BNE   YNWRBFEX            NO,  EXIT                                    
*                                                                               
YNWRBF20 CLC   BUFINV,YNCBUF#      INVOICE   NUMBER    MATCH ?                  
         BNE   YNWRBF40            NO,  CHECK     SAVED     AMOUNT              
         BAS   RE,YNPUTTPE         PUT  BUFFALO   REC  TO   TAPE                
*                                  SAVE NON-COM   AMOUNTS   FOR                 
         AP    YNSVNC,BUFC2             99   TOTALS                             
         AP    YNSVREC,BUFC1       SAVE RECEIVABLE    (AFTER     CD)            
         AP    YNSVNET,BUFC4       SAVE NET  AMOUNT                             
         GOTO1 BUFFALO,DMCB,=C'SEQ',(2,ADBUFC),BUFREC,1                         
         CLI   DMCB+8,0            SOMETHING LEFT ?                             
         BE    YNWRBF20            NO, TRY  NEXT                                
*                                                                               
YNWRBF40 CLI   YNCHKAMT,YES        VALIDATE  INVOICE   AMOUNT ?                 
         BNE   YNWRBF50            NO,  SKIP VALIDATION                         
*                                  SAVED     INVOICE   AMOUNT   SAME AS         
         CP    YNSVNET,TRNINV           TRANSACTION'S  INVOICE  AMOUNT?         
         BE    *+6                 YES, CONTINUE                                
         DC    H'0'                NO,  BLOW-UP                                 
*                                                                               
YNWRBF50 MVI   YNBLD43,YES         WRITE     43   RECORD    SWITCH              
*                                                                               
YNWRBFEX B     YNEXIT                                                           
         EJECT ,                                                                
***********************************************************************         
* YNRO SUBROUTINES                                                    *         
***********************************************************************         
         SPACE 1                                                                
*---------------------------------------------------------------------*         
* PUT A BUFFALO RECORD TO TAPE AS WORK CODE DETAIL                    *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
         USING YNPMURD1,R8         MAP  OUTPUT    TAPE RECORD                   
         SPACE 1                                                                
YNPUTTPE NTR1                                                                   
         MVC   YNPMUWC1,BUFWKC     MOVE NON-99    WORK-CODE TO   TAPE           
         ZAP   YNPMUNET,BUFC1                                                   
         ZAP   YNPMUNC1,BUFC2                                                   
         ZAP   YNPMUGRS,BUFC3                                                   
*                                  ALL  BUCKETS   ZERO ?                        
         CLC   YNPMUGRS(3*L'YNPMUGRS),=3PL6'0'                                  
         BE    YNPUTTPX            YES, SUPPRESS  RECORD   (MASC)               
         BAS   RE,YNPRTTPE                                                      
         GOTO1 AWRTAPE             WRITE     REG- NON-99    TRAN RCDS           
*                                                                               
YNPUTTPX B     YNEXIT                                                           
         DROP  R8                                                               
         EJECT ,                                                                
***********************************************************************         
* PRINT TAPE ROUTINE                                                  *         
***********************************************************************         
         SPACE 1                                                                
         USING YNPMURD1,R8         MAP  OUTPUT    TAPE RECORD                   
         SPACE 1                                                                
YNPRTTPE NTR1                                                                   
         CLC   QUESTOR,=CL12'PRTBUCKS'                                          
         BNE   YNPRTTPX                                                         
         MVC   P(2),YNPMURTY       40   OR   43                                 
         MVC   P+3(6),TRINUM                                                    
         EDIT  (P6,YNPMUGRS),(10,P+10),2,MINUS=YES                              
         EDIT  (P6,YNPMUNC1),(10,P+20),2,MINUS=YES                              
         EDIT  (P6,YNPMUNET),(10,P+30),2,MINUS=YES                              
         GOTO1 ACREPORT                                                         
*                                                                               
YNPRTTPX B     YNEXIT                                                           
         DROP  R8                                                               
         EJECT ,                                                                
***********************************************************************         
* OLD YNR ROUTINE USED FOR EVERYTHING EXCEPT PMU                      *         
***********************************************************************         
         SPACE 1                                                                
YNPROC   DS    0H                                                               
         CLI   GOSTAT,C'O'                                                      
         BNE   YNEXIT                                                           
*                                                                               
*                                  ************************************         
*                                  * BUILD A RECORD                   *         
*                                  ************************************         
*                                                                               
*        PROCESS DATES, CHANGE FORMAT FROM YYMMDD TO MMDDYY                     
*                                                                               
         USING YNRECD,R8           MAP  OUTPUT    TAPE RECORD                   
         LA    R8,TAPEWK           ->   OUTPUT    TAPE AREA                     
         LA    R1,WORK40           ->   WORK AREA                               
         GOTO1 AGETCMOS            CONVERT   MOS  DATE                          
         MVC   YNADVMM(2),WORK40+2 COPY ADV  MONTH     FIRST                    
         MVC   YNADVYY(2),WORK40   COPY ADV  YEAR                               
         MVC   YNINVMM(2),TRIDAT+2 COPY INVOICE   MONTH                         
         MVC   YNINVDD(2),TRIDAT+4 COPY INVOICE   DAY                           
         MVC   YNINVYY(2),TRIDAT   COPY INVOICE   YEAR                          
         MVC   YNDUEMM(2),TRNDUE+2 COPY DUE  MONTH                              
         MVC   YNDUEDD,TRNDUE+4    COPY DUE  DAY                                
         MVC   YNDUEYY,TRNDUE      COPY DUE  YEAR                               
*                                                                               
*        PROCESS EVERYTHING ELSE EXCEPT ACCOUNT NUMBER                          
*                                                                               
         MVI   YNINVNO,C'0'                                                     
         MVC   YNINVNO+1(6),TRINUM COPY INVOICE NUMBER                          
         MVI   YNTYPE,C'R'         COPY TYPE -    ALWAYS    AN   'R'            
         MVC   YNEST(6),TRNJOB     COPY ESTIMATE                                
         UNPK  YNGROSS,TRNREC      UNPACK    GROSS     AMOUNT                   
         UNPK  YNDISCNT,TRNDIS     UNPACK    DISCOUNT                           
         UNPK  YNCOST,TRNINV       UNPACK    COST                               
         MVC   YNOFFICE,SV2OFF     COPY OFFICE                                  
         MVC   YNMEDIA,SVSPCL      COPY SI   MEDIA     CODE (THE STATUS         
*                                       SWITCH    "SVSTAT"  MUST BE             
*                                       SET  TO   SVRDSI   (X80) BEFORE         
*                                       CALLING   THIS ROUTINE                  
*                                                                               
*        GET CLI/DIV/PRD ACT FIELD FROM PRODUCT RECORD                          
*                                                                               
         USING FFNELD,R6                                                        
         L     R6,ADHEIRB          LEVEL     B                                  
         AH    R6,DATADISP                                                      
*                                                                               
YNPROC10 CLI   0(R6),0                                                          
         BE    YNPROCX                                                          
         CLI   0(R6),FFNELQ        X'25' -   FREE FORM ELEMENT                  
         BE    YNPROC20                                                         
         SR    R0,R0                                                            
         IC    R0,1(,R6)                                                        
         AR    R6,R0                                                            
         B     YNPROC10                                                         
*                                                                               
YNPROC20 MVI   YNCLI,C'0'          1ST  DIGIT     SHOULD    BE   0              
         CLC   ALPHAID,=C'WW'                                                   
         BNE   *+8                                                              
         MVI   YNCLI,C'4'          FOR  WUNDERMAN IT'S A    4                   
         MVC   YNCLI+1(10),FFNUMBER     COPY ACCOUNT   NUMBER                   
*                                                                               
*                                  ************************************         
*                                  * WRITE THE RECORD TO TAPE         *         
*                                  ************************************         
*                                                                               
YNPROCX  GOTO1 AWRTAPE             WRITE     OUTPUT    TAPE RECORD              
         B     YNEXIT              RETURN    TO   MAIN ROUTINE                  
         DROP  R6,R8                                                            
         EJECT ,                                                                
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         TITLE 'ACI102YN2 - YNRO 2 - PM COMMISSION TAPE ROUTINE'                
***********************************************************************         
* NOTE THE I1 CLEARS TYPE 2 BUFFALO RECORDS AT ACCLAST                *         
* WHEN SVTEST'S SVTSCLR2 (X'40') BIT IS ON                            *         
***********************************************************************         
         SPACE 1                                                                
SUBRYN2  NMOD1 0,**I1Y2**                                                       
         L     RC,ADRC             RESTORE   RC                                 
*                                                                               
         CLC   QACCOUNT(3),=C'PMU' WAS  CLIENT    CODE PMU   REQUESTED?         
         BNE   YN2OLDL             NO,  PROCESS   OLD  YNR   ROUTINE            
         CLI   MODE,PROCTRNS                                                    
         BE    YN2PMUTR                                                         
         B     YN2EXIT                                                          
*                                                                               
YN2OLDL  CLI   MODE,PROCTRNS                                                    
         BE    YN2PTRN                                                          
*                                                                               
YN2EXIT  XIT1                                                                   
         EJECT ,                                                                
***********************************************************************         
* PROCESS TRANSACTION                                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNRECD,R6                                                       
         USING TRNELD,R7                                                        
         SPACE 1                                                                
YN2PTRN  DS    0H                                                               
         L     R7,ADTRANS                                                       
         LR    R6,R7                                                            
         SH    R6,DATADISP                                                      
         CLI   TRNEL,TRNELQ        X'44' -   TRANSACTION    ELEMENT ?           
         BNE   YN2PTRXN                                                         
         CLI   GOSTAT,C'I'         IRT  CALL ?                                  
         BNE   YN2PRORT            NO   ORT, BRANCH                             
         CLC   TRNANAL,=C'99'      WORK-CODE =    99 ?                          
         BE    YN2PRINV            YES, CHECK     IF   IN   TABLE               
         CLC   TRNKCUNT(2),=C'SK'  CONTRA-ACCOUNT =    SK ?                     
         BNE   YN2PTRXN            NO,  SIGNIFY   NOT  FOUND                    
*                                                                               
         USING SKD,R5                                                           
         LA    R5,SKREC            SET  SK   RCD  FOR  BINSRCH   TABLE          
         MVC   SKSOURCE,TRNKCULC                                                
         ZAP   SKAMT,P0                                                         
*                                                                               
         USING ACMD,R4             MAP  ACMASTD                                 
         L     R4,AMONACC                                                       
         L     R4,ACMAPRO2                                                      
*                                                                               
         USING PTAELD,R4           MAP  PROD TRANSACTION    ACTIVITY E          
YN2PTR10 CLI   PTAEL,0             ANY  MORE ELEMENTS ?                         
         BE    YN2PTRXN            NO,  EXIT                                    
         CLI   PTAEL,PTAELQ        X'77' -   PROD TRANSACTION    ACT E          
         BE    YN2PTR25            YES, PROCESS   IT                            
*                                                                               
YN2PTR20 SR    R0,R0               GET  NEXT ELEMENT                            
         IC    R0,1(,R4)                                                        
         AR    R4,R0                                                            
         B     YN2PTR10                                                         
*                                                                               
YN2PTR25 CLI   PTATYPE,PTATRAL     BILLING   ELEMENT ?                          
         BNE   YN2PTR20            NO,  GET  NEXT ELEMENT                       
         TM    PTASTAT1,PTASPEND   PENDING ?                                    
         BO    YN2PTR20            YES, GET  NEXT ELEMENT                       
         TM    PTASTAT1,PTASREVU   REVERSAL  OF   THIS BILL UPDATED ?           
         BO    YN2PTR40            YES, PROCESS   IT                            
         TM    PTASTAT1,PTASREVS   REVERSAL  ALLOCATION ?                       
         BO    YN2PTR50            YES, PROCESS   IT                            
*                                                                               
*                                  NOT  REVERSAL                                
         MVC   SKINVC,PTARBLNO     BILL NUMBER                                  
         XC    SKDATE,SKDATE       CLEAR     DATE                               
         ZAP   SKAMT,PTANET        ADD  THIS PARTIAL   BILLING                  
         MP    SKAMT,=P'-1'        HANDLE    AS   CREDIT    AMOUNT              
         GOTO1 AADDIT,(R4)         ADD  TO   BINTABLE                           
         B     YN2PTR20                                                         
*                                                                               
*                                  REVERSAL  OF   THIS BILL UPDATED             
*                                       X'02'                                   
YN2PTR40 MVC   SKINVC,=6X'FF'      NO   INVOICE   MATCH   FOR REVERSALS         
         ZAP   SKAMT,TRNAMNT       TRANSACTION    AMOUNT                        
         MVC   SKDATE,PTARBLDT     DATE ORIGINALY BILLED                        
         MP    SKAMT,=P'-1'        HANDLE    AS   CREDIT                        
         GOTO1 AADDIT,(R4)                                                      
         B     YN2PTR20                                                         
*                                                                               
*                                  REVERSAL  ALLOCATION     X'08'               
YN2PTR50 MVC   SKINVC,=6X'FF'      NO   INVOICE   MATCH   FOR REVERSALS         
         ZAP   SKAMT,TRNAMNT       TRANSACTION    AMOUNT                        
         MVC   SKDATE,PTARDATE     DATE REVERSED                                
         GOTO1 AADDIT,(R4)                                                      
         B     YN2PTR20            LOOK FOR  ANOTHER   77   ELEMENT             
*                                                                               
YN2PTRXN MVI   PRCSS,NO                                                         
         B     YN2EXIT             EXIT,     RETURN    TO   CALLER              
*                                                                               
         DROP  R4,R5,R6,R7                                                      
         EJECT ,                                                                
***********************************************************************         
* CHECK IF THIS INVOICE IN TABLE - PROCESS INVOICE                    *         
***********************************************************************         
         SPACE 1                                                                
         USING BIND,R5                                                          
         USING TRNELD,R7                                                        
         SPACE 1                                                                
YN2PRINV DS    0H                                                               
         L     R5,ASKTAB                                                        
         L     R3,BININ                                                         
         LTR   R3,R3                                                            
         BZ    YN2PRIXN            NOTHING   IN   TABLE                         
         LA    R5,BINTABLE                                                      
*                                                                               
         USING SKD,R5                                                           
YN2PRI10 CLC   SKINVC,TRNREF       THIS ITEM IN   TBL (PROS 99)                 
         BE    YN2PRIXY            YES, DO   NOT  PROCESS                       
         OC    SKDATE,SKDATE       DATE IN   THIS SK   ITEM ?                   
         BZ    YN2PRI20            NO,  DO   NOT  CHECK     FOR  DATES          
         CLC   SKDATE,TRN2DAY      BILL FOR  THIS JOB/DATE ?                    
         BE    YN2PRIXY                                                         
         CLC   SKDATE,TRNUNBIL     REVERSE   FOR  THIS JOB/DATE ?               
         BE    YN2PRIXY                                                         
*                                                                               
YN2PRI20 LA    R5,SKLEN(,R5)                                                    
         BCT   R3,YN2PRI10                                                      
         B     YN2PRIXN            NOT  FOUND                                   
*                                                                               
YN2PRIXN MVI   PRCSS,NO                                                         
*                                                                               
YN2PRIXY B     YN2EXIT                                                          
*                                                                               
         DROP  R5,R7                                                            
         EJECT ,                                                                
***********************************************************************         
* OUTPUT ROUTINE FOR Y&R INTERNAL CHARGES TAPE - ORT CALL             *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNELD,R7                                                        
         SPACE 1                                                                
YN2PRORT CLI   PRCSS,NO                                                         
         BE    YN2PROXN                                                         
         CLC   TRNANAL,=C'99'      WORK-CODE =    99 ?                          
         BNE   YN2PROXN            NO,  GET  OUT                                
*                                                                               
* GET ACT FIELD FROM PRODUCT RECORD                                             
*                                                                               
         USING FFNELD,R7                                                        
         L     R7,ADHEIRB          LEVEL     B                                  
         AH    R7,DATADISP                                                      
*                                                                               
YN2PRO10 CLI   0(R7),0                                                          
         BE    YN2PRO20                                                         
         CLI   0(R7),FFNELQ        X'25' -   FREE FORM ELEMENT                  
         BE    YN2PRO20                                                         
         SR    R0,R0                                                            
         IC    R0,1(,R7)                                                        
         AR    R7,R0                                                            
         B     YN2PRO10                                                         
*                                                                               
         USING YN2RECD,R8          MAP  OUTPUT    TAPE RECORD                   
YN2PRO20 LA    R8,TAPEWK           ->   OUTPUT    TAPE AREA                     
         MVI   YN2CLI,C'0'         1ST  DIGIT     SHOULD    BE   0              
         CLC   ALPHAID,=C'WW'                                                   
         BNE   *+8                                                              
         MVI   YN2CLI,C'4'         FOR  WUNDERMAN IT'S A    4                   
         MVC   YN2CLI+1(10),FFNUMBER    COPY ACCOUNT   NUMBER                   
*                                                                               
         LA    R1,WORK40                ->   WORK AREA                          
         GOTO1 AGETCMOS                 CONVERT   MOS  DATE                     
         MVC   YN2ADVMM(2),WORK40+2     COPY ADV  MONTH     FIRST               
         MVC   YN2ADVYY(2),WORK40       COPY ADV  YEAR                          
         MVC   YN2INVMM(2),TRIDAT+2     COPY INNVOICE  MONTH                    
         MVC   YN2INVDD(2),TRIDAT+4     COPY INVOICE   DAY                      
         MVC   YN2INVYY(2),TRIDAT       COPY INVOICE   YEAR                     
         MVC   YN2DUEMM(6),YN2INVMM     COPY TO   DUE  DATE                     
*                                                                               
         MVC   YN2INVNO,=C'0000000'     ZERO THE  INVOICE   NUMBER              
         MVI   YN2TYPE,C'R'             COPY TYPE (ALWAYS   AN   'R'            
         MVC   YN2EST(6),TRNJOB         COPY JOB  NUMBER                        
         MVC   YN2OFFIC,SV2OFF          COPY OFFICE                             
         ZAP   TRNINV,P0                                                        
*                                                                               
         USING BIND,R5                                                          
         L     R5,ASKTAB                                                        
         L     R3,BININ                                                         
         LTR   R3,R3                                                            
         BZ    YN2PROXN            NOTHING   IN   TABLE                         
         MVI   YN2SKSTA,YES        CHECK     FOR  REVERSALS                     
         LA    R5,BINTABLE                                                      
*                                                                               
         USING SKD,R5                                                           
         USING TRNELD,R7                                                        
         L     R7,ADTRANS                                                       
*                                                                               
YN2PRO30 CLC   SKINVC,TRNREF       FOUND     THIS ITEM IN   TABLE ?             
         BNE   YN2PRO40            NO,  SKIP                                    
         BAS   RE,YN2PUTTP         CREATE    OUTPUT    RECORD                   
*                                  DO   NOT  LOOK FOR  REVERSED,                
         MVI   YN2SKSTA,NO              GOT  INVOICE                            
         B     YN2PRO60                                                         
*                                                                               
YN2PRO40 CLI   YN2SKSTA,NO                                                      
         BE    YN2PRO60                                                         
         OC    SKDATE,SKDATE                                                    
         BZ    YN2PRO60                                                         
         CLC   SKDATE,TRN2DAY      BILL FOR  THIS JOB/DATE ?                    
         BNE   YN2PRO50                                                         
         BAS   RE,YN2PUTTP                                                      
         B     YN2PRO60                                                         
*                                                                               
YN2PRO50 CLC   SKDATE,TRNUNBIL     ANY  REVERSAL  FOR  THIS JOB/DATE ?          
         BNE   YN2PRO60                                                         
         BAS   RE,YN2PUTTP                                                      
*                                                                               
YN2PRO60 LA    R5,SKLEN(,R5)                                                    
         BCT   R3,YN2PRO30                                                      
*        B     YN2PROXY                                                         
*                                                                               
YN2PROXY MVI   PRCSS,NO                                                         
*                                                                               
YN2PROXN B     YN2EXIT                                                          
*                                                                               
         DROP  R5,R7,R8                                                         
         EJECT ,                                                                
***********************************************************************         
* WRITE RECORD TO TAPE                                                *         
***********************************************************************         
         SPACE 1                                                                
         USING SKD,R5                                                           
         USING ACTRECD,R7                                                       
         SPACE 1                                                                
YN2PUTTP NTR1                                                                   
         L     R7,ADTRANS                                                       
         SH    R7,DATADISP                                                      
         CLC   LCONTRA,SKSOURCE    SAME AS   LAST CONTRA    ACCOUNT ?           
         BE    YN2PUT30            YES, ALREADY   HAVE NUMBER                   
         MVC   SVNUM,SPACES                                                     
         MVC   LCONTRA,SPACES                                                   
         LA    R8,IOAREA                                                        
         MVC   0(42,R8),SPACES                                                  
         MVC   0(L'SKSOURCE,R8),SKSOURCE                                        
         MVI   2(R8),C'I'          SET  TO   READ SI   ACCOUNT                  
         MVC   COMMAND,=C'DMREAD'                                               
         GOTO1 AREADDM,(R8)                                                     
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                CAN  NOT  READ SI   ACCOUNT                  
*                                                                               
         USING OTHELD,R7                                                        
         LR    R7,R8                                                            
         AH    R7,DATADISP                                                      
*                                                                               
YN2PUT10 CLI   0(R7),0                                                          
         BE    YN2PUT30                                                         
         CLI   0(R7),OTHELQ        X'23' -   OTHERS    ELEMENT                  
         BE    YN2PUT20                                                         
         SR    R0,R0                                                            
         IC    R0,1(,R7)                                                        
         AR    R7,R0                                                            
         B     YN2PUT10                                                         
*                                                                               
*                                  SAVE MEDIA     NUM  FOR  SI  ACCOUNT         
YN2PUT20 MVC   SVNUM,OTHNUM        NOW  RE-READ   LAST TRANSACTION              
         MVC   0(42,R8),SPACES                                                  
         L     R7,ADTRANS                                                       
         SH    R7,DATADISP                                                      
*                                                                               
         USING ACTRECD,R7                                                       
         MVC   0(42,R8),ACTKCULA                                                
         MVC   LCONTRA,SKSOURCE    SAVE LAST CONTRA    ACCOUNT                  
         GOTO1 AREADDM,(R8)                                                     
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                CAN  NOT  READ TRANSACTION                   
*                                                                               
         USING TRNELD,R7                                                        
         USING YN2RECD,R8          MAP  OUTPUT    TAPE RECORD                   
YN2PUT30 LA    R8,TAPEWK           ->   OUTPUT    TAPE AREA                     
         MVC   YN2MEDIA,SVNUM      SPECIAL   MEDIA     CODE                     
         ZAP   TRNDIS,P0                                                        
         ZAP   TRNCOM,P0                                                        
         AP    TRNINV,SKAMT        SAVE INVENTORY AMOUNT                        
         UNPK  YN2COST,SKAMT       UNPACK    COST                               
         ZAP   TRNREC,P0                                                        
*                                                                               
* WRITE THE RECORD TO TAPE                                                      
*                                                                               
         MVI   PRCSS,C'R'          TELL MAIN ROUTINE   THAT WE'RE GOING         
         XC    SKDATE,SKDATE       DO   NOT  USE  THIS AMOUNT   ANYMORE         
         GOTO1 AWRTAPE             WRITE     THE  RCD  TO   THE  TAPE           
         B     YN2EXIT                                                          
         DROP  R5,R7,R8                                                         
         EJECT ,                                                                
***********************************************************************         
* PROCESS TRANSACTIONS FOR PMU ONLY                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNELD,R7           TRANSACTION    ELEMENT                       
         SPACE 1                                                                
YN2PMUTR L     R7,ADTRANS                                                       
         CLI   TRNEL,TRNELQ        X'44' -   TRANSACTION    ELEMENT ?           
         BNE   YN2PMUTX            NO,  EXIT                                    
         CLI   GOSTAT,C'I'                                                      
         BE    YN2PMUTX                                                         
*                                                                               
         CLC   TRNANAL,=C'99'                                                   
         BNE   YN2PMUTX                                                         
*                                                                               
         USING YN2PMUD,R8          MAP  OUTPUT    TAPE RECORD                   
         LA    R8,TAPEWK           ->   OUTPUT    TAPE AREA                     
         LH    R1,SVRECL           R1 = RECORD    LENGTH                        
         SLL   R1,16               SHIFT     LEN  TO   HIGH ORDER  HALF         
         STCM  R1,15,YN2PMLEN      SAVE IN   LENGTH    FIELD                    
*                                                                               
         MVC   YN2PMLIT,=C'331543PDC'                                           
         MVC   YN2PMNYY,TRIDAT     INVOICE   YEAR                               
         MVC   YN2PMNMD,TRNJOB     MEDIUM                                       
         MVC   YN2PMUN#,TRINUM                                                  
         MVC   YN2PMJOB,TRNCLI     CLIENT    PROD JOB                           
         MVC   YN2PMREC,=C'50'                                                  
*                                                                               
         ZAP   YN2PMGRS,TRNREC                                                  
         AP    YN2PMGRS,TRNDIS                                                  
         ZAP   YN2PMUCD,TRNDIS                                                  
         ZAP   YN2PMNET,TRNINV                                                  
         AP    YN2PMNET,TRNDIS                                                  
         ZAP   YN2PMCOM,P0                                                      
         ZAP   YN2PMFEE,P0                                                      
*                                                                               
         BAS   RE,YN2PRTTP                                                      
         GOTO1 AWRTAPE                                                          
*                                                                               
YN2PMUTX B     YN2EXIT             EXIT,     RETURN    TO   CALLER              
*                                                                               
         DROP  R7,R8                                                            
         EJECT ,                                                                
***********************************************************************         
* PRINT TAPE ROUTINE                                                  *         
***********************************************************************         
         SPACE 1                                                                
         USING YN2PMUD,R8                                                       
         SPACE 1                                                                
YN2PRTTP NTR1                                                                   
         CLC   QUESTOR,=CL12'PRTBUCKS'                                          
         BNE   YN2PRTTX                                                         
         MVC   P(9),YN2PMINV                                                    
         EDIT  (P6,YN2PMGRS),(10,P+10),2,MINUS=YES                              
         EDIT  (P6,YN2PMUCD),(10,P+20),2,MINUS=YES                              
         EDIT  (P6,YN2PMNET),(10,P+30),2,MINUS=YES                              
         GOTO1 ACREPORT                                                         
*                                                                               
YN2PRTTX B     YN2EXIT                                                          
*                                                                               
         DROP  R8                                                               
         EJECT ,                                                                
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG ,                                                                
         TITLE 'ACI102  -  TABLES'                                              
***********************************************************************         
* SK TABLE CONSTANTS                                                  *         
***********************************************************************         
         SPACE 1                                                                
SKTAB    DS    0D                                                               
         DC    F'0'                NUMBER OF TABLE ENTRIES                      
         DC    AL4(SKLEN)          RECORD LENGTH                                
         DC    AL4(SKKEYLN)        DISP OF KEYT/ KEY LENGTH                     
         DC    AL4(SKTABMAX)       MAX NUMBER OF ENTRIES                        
         DC    AL1(1)              NUMBER OF BUCKETS                            
         DC    AL1(SKBK-SKD)       DISPLACEMENT TO BUCKETS                      
         DC    AL1(0)              SPARE                                        
         DS    (SKTABMAX*SKLEN)C   TABLE                                        
         EJECT ,                                                                
***********************************************************************         
* TABLE OF AGENCY SUBROUTINES.  THESE SUBROUTINES ARE PART OF THIS    *         
* LOAD MODULE AND CALLED VIA A:  GOTO1 AUSERSUB                       *         
* THE SUBROUTINES PERFORM SPECIAL TAPE FORMATTING AND DO SOME         *         
* CUSTOMIZING OF REPORTS. SVSTAT (SVSUBR = X'01') MUST BE ON AND      *         
* SVSUBR# MUST NOT BE 0 IN ORDER TO CALL THE SUBROUTINES.             *         
*                                                                     *         
*        HEX FIELDS ARE: SVTEST, SVRECF, SVSTAT AND SVSUBR#           *         
***********************************************************************         
         SPACE 1                                                                
AGYTBL   DS    0H                                                               
         DC    CL8'BD1SJ   '                           * BDNWW - BBDO           
         DC    AL1(SVTSBBDO,SVSUBR,SVSBBD1#,SVRECFU)   * SUBROUTINE             
         DC    H'080',H'0800'                          *                        
*                                                                               
         DC    CL8'BS1SJPM '                           * BSNY - BATES           
         DC    AL1(SVTSCLR2,SVSUBR,SVSBBS1#,SVRECFVB)  * CLEAR TYPE 2           
         DC    H'100',H'4004'                          * SUBROUTINE             
*                                                                               
         DC    CL8'BS2SJPM '                           * BSNY - BATES           
         DC    AL1(SVTSCLR2,SVSUBR,SVSBBS2#,SVRECFVB)  * CLEAR TYPE 2           
         DC    H'100',H'4004'                          * SUBROUTINE             
*                                                                               
*        DC    CL8'BS1SJ   '                           * BSNY - BATES           
*        DC    AL1(SVTSSTDT,0,0,SVRECFU)               * STD  TAPE RTN          
*        DC    H'100',H'0100'                          * USES DEFAULT           
*                                                                               
         DC    CL8'NE1SJ   '                           * DNNYE - DDB            
         DC    AL1(0,SVSUBR,SVSBNE1#,SVRECFFB)         *   NEEDHAM #1           
         DC    H'093',H'0930'                          * SUBROUTINE             
*                                                                               
         DC    CL8'NE2SJ   '                           * DNNYE - DDB            
         DC    AL1(SVTSCHRG,SVSUBR,SVSBNE2#,SVRECFFB)  *   NEEDHAM #2           
         DC    H'082',H'0820'                          * INSIDE CHARGES         
*                                                      * SUBROUTINE             
*                                                                               
         DC    CL8'NR1SJ   '                           * DNRFO - DDBN           
         DC    AL1(SVTSSCPY,SVSUBR,SVSBNE1#,SVRECFFB)  *   RETAIL #1            
         DC    H'093',H'0930'                          * SUBCOMPANY NAM         
*                                                      * SUBROUTINE             
*                                                                               
         DC    CL8'NR2SJ   '                           * DBRFO - DDBN           
         DC    AL1(SVTSCHRG,SVSUBR,SVSBNE2#,SVRECFFB)  *   RETAIL #2            
         DC    H'082',H'0820'                          * INSIDE CHARGES         
*                                                      * SUBROUTINE             
*                                                                               
         DC    CL8'DW1SJ   '                           * DWSEC + DWNY           
         DC    AL1(0,0,0,SVRECFU)                      *   DUMMY P&G            
         DC    H'080',H'0080'                          *   TAPE                 
*                                                      * NO   TAPE RTN          
*                                                      * SUBROUTINE X           
*                                                                               
         DC    CL8'SC1SJ   '                           * SCA  - LOWE            
         DC    AL1(SVTSSCPY+SVTSSTDT,0,0,SVRECFU)      * SUBCOMPANY NAM         
         DC    H'100',H'0100'                          * STD  TAPE RTN          
*                                                                               
         DC    CL8'WI1SJ   '                           * WILA - WESTERN         
         DC    AL1(0,SVSUBR,SVSBWI1#,SVRECFU)          * SUBROUTINE             
         DC    H'162',H'0162'                          *                        
*                                                                               
         DC    CL8'XD1SJ   '                           * XYZ - CANADA           
         DC    AL1(0,SVSUBR,SVSBXD1#,SVRECFU)          * NON-EXISTANT           
         DC    H'153',H'0153'                          * SUBROUTINE             
*                                                                               
* NOTE: - USES THE STATUS X'80' (SVRDSI) BELOW TO GET THE                       
*         SI MEDIA CODE FOR THE YNR TAPES.                                      
*       - CLI CODE PMU HAS SPECIAL TAPE FORMAT AS PER TGER                      
*         AND ACUSI 7/95                                                        
*                                                      * YOUNG/RUBICAM          
         DC    CL8'YN1SJPMU'                           * YNRO #1 PMU            
         DC    AL1(SVTSCLR2,SVRDSI+SVSUBR,SVSBYN1#)    * CLEAR TYPE 2           
         DC    AL1(SVRECFVB)                           * READ SI                
         DC    H'408',H'4084'                          * SUBROUTINE             
*                                                                               
         DC    CL8'YN2SJPMU'                           * YNRO #2 PMU            
         DC    AL1(SVTSCLR2,SVRDSI+SVSUBR,SVSBYN2#)    * CLEAR TYPE 2           
         DC    AL1(SVRECFVB)                           * READ SI                
         DC    H'408',H'4084'                          * SUBROUTINE             
*                                                                               
         DC    CL8'YN1SJ   '                           * YNRO #1                
         DC    AL1(0,SVRDSI+SVSUBR,SVSBYN1#,SVRECFU)   * READ SI                
         DC    H'100',H'0100'                          * SUBROUTINE             
*                                                                               
         DC    CL8'YN2SJ   '                           * YNRO #2                
         DC    AL1(0,SVRDSI+SVSUBR,SVSBYN2#,SVRECFU)   * READ SI                
         DC    H'100',H'0100'                          * SUBROUTINE             
*                                                                               
         DC    CL8'WW1SJ   '                           * WWNY-WUNDERMAN         
         DC    AL1(0,SVRDSI+SVSUBR,SVSBYN1#,SVRECFU)   *   CATO ... #1          
         DC    H'100',H'0100'                          * READ SI                
*                                                      * SUBROUTINE             
*                                                                               
         DC    CL8'WW2SJ   '                           * WWNY-WUNDERMAN         
         DC    AL1(0,SVRDSI+SVSUBR,SVSBYN2#,SVRECFU)   *   CATO ... #2          
         DC    H'100',H'0100'                          * READ SI                
*                                                      * SUBROUTINE             
*                                                                               
*                                                                               
         DC    CL8'  1     '                           * DEFAULT                
         DC    AL1(SVTSSTDT,0,0,SVRECFU)               * STD  TAPE RTN          
         DC    H'100',H'0100'                          *                        
         EJECT ,                                                                
***********************************************************************         
* USER SUBROUTINES ADDRESS TABLE                                      *         
***********************************************************************         
         SPACE 1                                                                
USERSUBS DS    0F                                                               
         DC    A(SUBRBD)           ADDRESS OF BD   SUBROUTINE                   
         DC    A(SUBRBS1)          ADDRESS OF BS 1 SUBROUTINE                   
         DC    A(SUBRBS2)          ADDRESS OF BS 2 SUBROUTINE                   
         DC    A(SUBRNE1)          ADDRESS OF NE 1 SUBROUTINE                   
         DC    A(SUBRNE2)          ADDRESS OF NE 2 SUBROUTINE                   
         DC    A(SUBRWI)           ADDRESS OF WI   SUBROUTINE                   
         DC    A(SUBRYN1)          ADDRESS OF YN 1 SUBROUTINE                   
         DC    A(SUBRYN2)          ADDRESS OF YN 2 SUBROUTINE                   
         EJECT ,                                                                
***********************************************************************         
* BDNY ACCOUNTS TABLE                                                 *         
***********************************************************************         
         SPACE 1                                                                
BDOTBL   DC    CL14'SRCDBCC1000'   REQUIRED ACCT# FOR BBDO                      
         DC    CL14'SRCDBCC1001'      "                                         
         DC    CL14'SRCDBCC1002'      "                                         
         DC    CL14'SRCDBCC1004'      "                                         
         DC    CL14'SRCDBCD1000'      "                                         
         DC    CL14'SRCDBDS1000'      "                                         
         DC    X'FF'                                                            
         EJECT ,                                                                
***********************************************************************         
* BDNY USER FIELD TABLE                                               *         
*   TABLE DEFINES WHERE TO STORE USER FIELDS COVERED BY BDLUD         *         
***********************************************************************         
         SPACE 1                                                                
BDUSRTAB DS    0C                                                               
         DC    C'EA',AL2(BDJBEA-BDJBDATA),AL1(L'BDJBEA) EA NUMBER               
         DC    C'NU',AL2(BDJBAC-BDJBDATA),AL1(L'BDJBAC) AC NUMBER               
BDLUNUM  EQU   (*-BDUSRTAB)/BDLUDLN                                             
         EJECT ,                                                                
***********************************************************************         
* DNNYE 2 TABLE OF CONTRA ID CODES                                    *         
***********************************************************************         
         SPACE 1                                                                
NE2CODES DC    C'AAV9W1'           CONTRA    ID   CODES     TABLE               
         DC    C'02030405060809'                                                
         DC    C'10111213141516171819'                                          
         DC    C'20212223242526272829'                                          
         DC    C'30313233343536373839'                                          
         DC    C'404142434445464849'                                            
         DC    C'505152535456575859'                                            
         DC    C'60626567'                                                      
         DC    C'7075'                                                          
         DC    C'8081828586'                                                    
         DC    C'9094959697'                                                    
         DC    X'FF'                                                            
         EJECT ,                                                                
         TITLE 'ACI102  -  BUFFALO CONSTANTS'                                   
***********************************************************************         
* BUFFALO CONSTANTS                                                   *         
***********************************************************************         
         SPACE 1                                                                
         BUFF  LINES=200,ROWS=1,COLUMNS=5,COMMENT=36,FLAVOR=PACKED,    X        
               KEYLIST=(9,A)                                                    
         SPACE 2                                                                
*                                                                               
         DCBD  DSORG=PS,DEVD=TA                                                 
         EJECT ,                                                                
***********************************************************************         
* EQUATES                                                             *         
***********************************************************************         
         SPACE 1                                                                
EOF      EQU   X'FF'                                                            
TURNOFF  EQU   X'FF'                                                            
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
SKTABMAX EQU   200                                                              
*                                                                               
         TITLE 'ACI102 - S P A C E N D   W O R K - A R E A   D S E C T'         
***********************************************************************         
* SPACEND WORK AREA DSECT                                             *         
***********************************************************************         
         SPACE 1                                                                
ACI1D    DSECT                                                                  
ACI1ID   DS    CL8                 *ACI1D*    CONSTANT                          
VTYPES   DS    0F                  CONSTANTS                                    
SQUASHER DS    V                   SQUASHER   ROUTINE                           
UNDERLIN DS    V                   UNDERLIN   ROUTINE                           
PERVERT  DS    V                   PERVERT    ROUTINE                           
PRNTBL   DS    V                   PRNTBL     ROUTINE                           
CONVMOS  DS    V                   CONVMOS    ROUTINE                           
ADBUFC   DS    A                   BUFFALO    ROUTINE                           
*                                                                               
AGETEL   DS    A                   GETEL      EXPANSION                         
AFIRSTEL DS    A                   FIRSTEL    EXPANSION                         
ANEXTEL  DS    A                   NEXTEL     EXPANSION                         
*                                                                               
*                                  SUBROUTINES                                  
AWRTAPE  DS    A                   WRTAPE     NMOD                              
ATAPEOUT DS    A                   TAPEOUT    NMOD                              
AREPORT  DS    A                   REPORT     NMOD                              
AREADDM  DS    A                   READDM     NMOD                              
ACLITOT  DS    A                   CLITOT     NMOD                              
AGETCMOS DS    A                   GETCMOS    NMOD                              
AADDIT   DS    A                   ADDIT      NMOD                              
AGENEDIT DS    A                   GENEDIT    NMOD                              
APRINTIT DS    A                   PRINTIT    NMOD                              
AGETFEE  DS    A                   GETFEE     NMOD                              
ADEBUG   DS    A                   DEBUG      NMOD                              
APRDUMP  DS    A                   PRDUMP     NMOD                              
APUTBUF  DS    A                   PUTBUF     NMOD                              
ASKTAB   DS    A                   SK         TABLE                             
AAGYTBL  DS    A                   AGENCY     TABLE                             
AUSRSUBS DS    A                   USER       SUBROUTINES   TABLE               
VLNQ     EQU   *-VTYPES                                                         
         EJECT  ,                                                               
*                                  **** BEGIN KEEP     TOGETHER ****            
BUFREC   DS    0D                                                               
         DS    XL1                 X'00'                                        
BUFKEY   DS    0XL8                                                             
BUFINV   DS    XL6                                                              
BUFWKC   DS    XL2                                                              
BUFCOM   DS    CL36                                                             
BUFC1    DS    PL8                                                              
BUFC2    DS    PL8                                                              
BUFC3    DS    PL8                                                              
BUFC4    DS    PL8                                                              
BUFC5    DS    PL8                                                              
*                                  **** END       KEEP TOGETHER *******         
*                                                                               
ADTRKEY  DS    F                   A(TRANSACTION  RECORD)                       
ADRC     DS    F                   SAVE RC                                      
AUSERSUB DS    A                   A(USER    SUBROUTINE)                        
*                                                                               
OUTCNT   DS    H                   NUM  OF   TIMES   TAPE OPENED/CLOSED         
         EJECT ,                                                                
*                                  **** BEGIN     KEEP TOGETHER *******         
SVTBL    DS    0H                                                               
SVAGY    DS    CL2                 COMPANY   CODE                               
SVAGYCPY DS    CL1                 COMPANY   COPY                               
SVULC    DS    CL5                 UNIT,LEDGER,CLIENT-1,1,3                     
*                                                                               
SVTEST   DS    X                   COMPANY/CLIENT OPTION-1                      
SVTSBBDO EQU   X'80'               . BBDO                                       
SVTSCLR2 EQU   X'40'               . CLEAR   TYPE 2                             
*                                  . ESTIMATE     FOR                           
SVTSEST  EQU   X'20'                         AGENCY/CLIENT  WANTED              
SVTSSSC  EQU   X'10'               . SSC                                        
SVTSCHRG EQU   X'08'               . INSIDE  CHARGES   RUN                      
SVTSSCPY EQU   X'04'               . MOVE    SUBCOMPANY  NAME TO REPORT         
SVTSSTDT EQU   X'01'               . STANDARD     TAPE RCD  OUTPUT              
*                                                                               
SVSTAT   DS    X                   STATUS    CODE                               
*                                  . READ    SI  (GET  MEDIA CODE               
SVRDSI   EQU   X'80'                              FROM SI)                      
SVFRST   EQU   X'10'               . 1ST     TIME IN                            
*                                  . GOTO    SUBROUTINE     FOR                 
SVSUBR   EQU   X'01'                         FOR  THIS COMPANY                  
*                                                                               
SVSUBR#  DS    X                   SUBROUTINE     NUMBER    FOR COMPANY         
SVSBBD1# EQU   1                   . BD 1    SUBROUTINES                        
SVSBBD2# EQU   1                   . BD 2    SUBROUTINES                        
SVSBBS1# EQU   2                   . BS 1    SUBROUTINES                        
SVSBBS2# EQU   3                   . BS 2    SUBROUTINES                        
SVSBNE1# EQU   4                   . NE 1    SUBROUTINES                        
SVSBNE2# EQU   5                   . NE 2    SUBROUTINES                        
SVSBWI1# EQU   6                   . WI 1    SUBROUTINES                        
SVSBWI2# EQU   6                   . WI 2    SUBROUTINES                        
SVSBYN1# EQU   7                   . YN 1    SUBROUTINES                        
SVSBYN2# EQU   8                   . YN 2    SUBROUTINES                        
*                                                                               
SVSUBBI# EQU   0                   . BI      SUBROUTINES -  OBSOLETE            
SVSBJW1# EQU   0                   . JW 1    SUBROUTINES - DROP SUPPORT         
SVSBJW2# EQU   0                   . JW 2    SUBROUTINES - DROP SUPPORT         
SVSBPG1# EQU   0                   . PG 1    SUBROUTINES -  DOWN-LOAD           
SVSBPG2# EQU   0                   . PG 2    SUBROUTINES -  DOWN-LOAD           
SVSBTB1# EQU   0                   . TB 1    SUBROUTINES -  NO ACC FILE         
SVSBTB2# EQU   0                   . TB 2    SUBROUTINES -  NO ACC FILE         
SVSBWE1# EQU   0                   . WE 1    SUBROUTINES -  OBSOLETE            
SVSBWE2# EQU   0                   . WE 2    SUBROUTINES -  OBSOLETE            
SVSBXD1# EQU   0                   . XD 1    SUBROUTINES -                      
*                                                 NEVER     WRITTEN             
SVSBXD2# EQU   0                   . XD 2    SUBROUTINES -                      
*                                                 NEVER     WRITTEN             
*                                                                               
SVRECF   DS    X                   RCD  FORMAT    TO   MODIFY  THE DCB          
*                                  FOR  MORE INFO,     EXPAND:                  
*                                       DCBD DSORG=PS,DEVD=DA                   
SVRECFU  EQU   X'C0'               . UNDEFINED                                  
SVRECFFB EQU   X'90'               . FIXED        AND  BLOCKED                  
SVRECFVB EQU   X'50'               . VARIABLE     AND  BLOCKED                  
*                                                                               
SVRECL   DS    H                   RECORD    LENGTH                             
SVBLKL   DS    H                   BLOCK     LENGTH                             
SVTBLLNQ EQU   *-SVTBL                                                          
*                                  **** END       KEEP TOGETHER *******         
         EJECT ,                                                                
PL16     DS    PL16                WORK AREA                                    
WORK40   DS    CL40                WORK AREA                                    
*                                                                               
P0       DS    PL1                 PACKED    ZERO CONSTANT                      
YR0101   DS    CL6                 YY0101    OF   TRIDAT  (SEE BDPTRNO)         
HEXFF    DS    XL4                 ALL  X'FF'                                   
*                                                                               
NHOFF    DS    CL5                 LLLVS    (USED IN   LEVA)                    
NHAGY    DS    CL3                 FOR  NEEDHAM:  AGENCY   (SEE LEVA)           
*                                  . DDW     IF   ALPHAID = NE                  
*                                  . DDB     SEE  LEVA                          
*                                  . CWA     IF   ALPHAID = CWA                 
*                                                                               
*                                                                               
*                                  SWITCH    TO   TELL SUBROUTINES              
GOSTAT   DS    CL1                   WHICH   CALL WAS  MADE                     
*                                                                               
SVPAYNET DS    CL1                 PAY=NET   FROM GOBLOCK   SET IN PACC         
*                                                                               
TAPEOPTN DS    CL1                 OPTIONAL  TAPE FORMAT                        
TAPOPDEF EQU   C'1'                . QOPT2 = DEFAULT                            
TAPOPYES EQU   C'2'                . QOPT2 = YES                                
TAPOPG   EQU   C'3'                . QOPT2 = G                                  
*                                                                               
SWITCHES DS    XL1                 SWITCHES                                     
MEDTOTSW EQU   X'80'               . MEDIA   TOTALS    OUTPUTTED                
*                                                                               
SVBPRD   DS    CL20                PRODUCT   NAME SAVE AREA                     
*                                                                               
SVDAT3   DS    XL3                 TRANSACTION    DATE PACKED                   
QSTR3    DS    XL3                 QSTART         DATE PACKED                   
QEND3    DS    XL3                 QEND           DATE PACKED                   
QSTR2    DS    XL2                 QSTART         DATE COMPRESSED               
QEND2    DS    XL2                 QEND           DATE COMPRESSED               
BSUDAT   DS    XL2                 QSTART    -10  DATE COMPRESSED               
SVDUE    DS    CL6                 DUE            DATE DISPLAY                  
*                                                                               
PRCSS    DS    CL1                 PROCESS TRANSACTION FLAG                     
ELCODE   DS    XL1                 ELEMENT   CODE (GETEL, NEXTEL,1STEL)         
SVCLX    DS    CL3                 CLIENT         CONTROL KEY                   
COMMAND  DS    CL6                 DATAMGR        COMMAND                       
NHIHEAD  DS    CL25                LAST BYTES     OF   PRINT     LINE           
*                                                                               
LMEDIA   DS    CL1                 LAST MEDIA     CODE                          
SVSPCL   DS    CL3                 SAVE AREA FOR  SI   MEDIA     CODE           
LCONTRA  DS    CL15                LAST CONTRA    ACOUNT                        
SVNUM    DS    CL3                 SAVE MEDIA     CODE                          
SVWCCODE DS    CL2                 SAVE AREA FOR  WORK CODE                     
SVRATE   DS    PL4                 SAVE AREA FOR  RATE                          
*                                                                               
MSG      DS    CL10                HEADER    FOR  PRNTBL                        
*                                                                               
MAXCNT   DS    PL2                 MAX  NUM  OF   TAPE RCDS TO   PRINT          
EVERY    DS    PL2                 FREQUENCY OF   TAPE RCDS TO   PRINT          
MAXDMP   DS    PL3                 MAX  NUM  OF   ELEMENTS  TO   DUMP           
*                                                                               
DUMPCNT  DS    PL3                 COUNT     OF   OUTPUT    RCDS                
PDUMP    DS    PL(L'MAXCNT)        COUNT     OF   RECORDS   DUMPED              
DMPTOT   DS    PL(L'MAXDMP)        DUMP COUNT                                   
*                                                                               
SKREC    DS    (SKLEN)C            SK   RCD  TO   ADD                           
*                                                                               
REPCNT   DS    PL3                 REPORT    LINES     BY   CLIENT              
TAPECNT  DS    PL3                 COUNT     OF   TAPE RECORDS                  
MOSWK    DS    PL3                 MONTH     OF   SERVICE   WORK AREA           
         EJECT ,                                                                
*                                  **** BEGIN     KEEP TOGETHER *******         
*                                  THE  FOLLOWING CREATED BY PTRN RTN:          
TRINUM   DS    CL6                 TRANSACTION    REFERENCE NUMBER              
TRIDAT   DS    CL6                 TRANSACTION    DATE                          
TRNDUE   DS    CL6                 DUE  DATE FOR  RECEIVABLES                   
TRIMED   DS    CL1                 BLANK     OR   1ST  BYTE OF   JOB            
TR2LEN   EQU   *-TRINUM                                                         
*                                  **** END       KEEP TOGETHER *******         
         SPACE 2                                                                
*                                  **** BEGIN     KEEP TOGETHER *******         
*                                  THE  FOLLOWING CREATED BY PTRN  RTN:         
TRNMED   DS    CL1                 MEDIA    (1ST  BYTE OF   TRNJOB)             
TRNCLI   DS    CL3                 CLIENT                                       
TRNPRD   DS    CL3                 PRODUCT                                      
TRNJOB   DS    CL6                 JOB                                          
TR1LEN   EQU   *-TRNMED                                                         
*                                  **** END       KEEP TOGETHER *******         
         SPACE 2                                                                
*                                  **** BEGIN     KEEP TOGETHER *******         
SVOE     DS    PL6                 JOB  COL  ORIGINAL  ESTIMATE                 
SVCE     DS    PL6                 JOB  COL  CURRENT   ESTIMATE                 
SVPE     DS    PL6                 JOB  COL  CURRENT   GROSS      EST           
*                                                                               
TRNREC   DS    PL6                 TRAN RECEIVABLE     AMOUNT                   
TRNINV   DS    PL6                      INVENTORY      AMOUNT   NET             
TRNCOM   DS    PL6                      COMMISSION     AMOUNT                   
TRNDIS   DS    PL6                      DISCOUNT       AMOUNT                   
TRNLEN   EQU   *-TRNREC                                                         
*                                                                               
*                                  CLIENT    MINOR     TOTAL    AMOUNTS         
CLTREC   DS    PL6                      RECEIVABLE     AMOUNT                   
CLTINV   DS    PL6                      INVENTORY      AMOUNT                   
CLTCOM   DS    PL6                      COMMISSION     AMOUNT                   
CLTDIS   DS    PL6                      DISCOUNT       AMOUNT                   
*                                                                               
*                                  REQUEST   TOTAL     AMOUNTS                  
REQREC   DS    PL6                      RECEIVABLE     AMOUNT                   
REQINV   DS    PL6                      INVENTORY      AMOUNT                   
REQCOM   DS    PL6                      COMMISSION     AMOUNT                   
REQDIS   DS    PL6                      DISCOUNT       AMOUNT                   
*                                  **** END       KEEP TOGETHER *******         
         SPACE 2                                                                
*                                  **** BEGIN     KEEP TOGETHER *******         
*                                  THE  FOLLOWING CREATED BY PACC RTN:          
SVEA     DS    CL16                EA        NUMBER (LJ) XXXXX-XX-X             
SVAC     DS    CL16                ACCOUNT   NUMBER (LJ) XXXXXXXXXX             
SVES     DS    CL12                BUD. NUM  FOR  INTERFACE  TPS                
SVBN     DS    CL12                BILLING   NUMBER (A/N FREEFORM)              
SVF4     DS    CL1                 SUB  COMPANY   OR   SUB  DEPARTMENT          
SVMEDIA  DS    CL3                 MEDIA    (1ST  3    CHAR OF   ORD #)         
SVBBFDT  DS    CL6                 DATE BALANCE   BROUGHT   FORWARD             
SVJBST   DS    CL6                 JOB  OPEN DATE                               
ESTSW    DS    CL1                 ESTIMATE  FOUND     SWITCH                   
SAVNAME  DS    CL36                ACCOUNT   NAME                               
SAVBLPR  DS    CL50                ADDITIONAL     BILLING   PRINT               
SV1LEN   EQU   *-SVEA              USED BY   PACC TO   BLANK-OUT FIELDS         
*                                  **** END       KEEP TOGETHER *******         
         SPACE 2                                                                
*                                  **** BEGIN     KEEP TOGETHER *******         
*                                  THE  FOLLOWING RESET BY PACC RTN:            
SVJBOP3  DS    PL3                 JOB  OPEN DATE                               
SVJBCL3  DS    PL3                 ESTIMATED JOB  CLOSE     DATE                
SVJBLEN  EQU   *-SVJBOP3                                                        
*                                  **** END       KEEP TOGETHER *******         
         SPACE 2                                                                
*                                  **** BEGIN     KEEP TOGETHER *******         
*                                  THE  FOLLOWING CREATED  BY LEVA RTN:         
*                                  UNIT FOR  ANALYSIS (FROM PPRUFORA)           
*                                  NOTE:     THE  YN   ROUTINES PICK UP         
*                                            THE  1ST  BYTE OF   SVACLI         
*                                            AS   PART OF   SVAOFF              
SVAOFF   DS    CL1                 UNIT FOR  ANALYSIS                           
SV2OFF   DS    CL2                 2 BYTE OFFICE                                
*                                                                               
SVACLI   DS    CL3                 CLIENT    ID                                 
SVANAME  DS    CL36                CLIENT    NAME                               
SVALEN   EQU   *-SVACLI                                                         
*                                  **** END       KEEP TOGETHER *******         
*                                                                               
*                                  **** BEGIN     KEEP TOGETHER *******         
TAPEWK   DS    135D                MAX  SPACE     REQUIRED FOR TAPE I/O         
TAPEWKLQ EQU   *-TAPEWK                 I.E. 1080-BYTES                         
*                                  ***** END   KEEP TOGETHER **********         
         EJECT ,                                                                
TAPESAVE DS    CL120               SAVE AREA FOR  UN-OPENED TAPE DCB            
*                                                                               
P2X      DS    CL132               EXTRA     PRINT     LINE                     
*                                                                               
IOAREA   DS    CL1000              I/O  BUFFER    FOR SI    MEDIA  CODE         
*                                                                               
SPARE    DS    CL100               SPARE     FOR  ADDITIONAL    STORAGE         
*                                                                               
USERV    DS    0F                  SPACE     FOR  USER VARIABLES                
         EJECT ,                                                                
*---------------------------------------------------------------------*         
* BDNY VARIABLES                                                      *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
         ORG   USERV                                                            
BDOSEQ   DS    XL3                                                              
BDOTOT   DS    PL5                                                              
*                                                                               
BDTEST   DS    XL1                                                              
BDTFND27 EQU   X'80'               FND  X'27'     ACCOUNT   BILLING  EL         
BDTFND44 EQU   X'40'               FND  X'44'     TRANSACTION        EL         
BDTFND60 EQU   X'20'               FND  X'60'     STATUS             EL         
BDTEAFA2 EQU   X'02'               GOT  EA   FROM X'A2'     USER FLD EL         
BDTEAF1A EQU   X'01'               GOT  EA   FROM X'1A'     MEDIA TX EL         
*                                                                               
BDSTAT   DS    XL1                                                              
BDSTUFAC EQU   1                   GOT  A    USER FLD  AC   NUMBER              
BDSTUFEA EQU   2                   GOT  A    USER FLD  EA   NUMBER              
*                                                                               
BDFDEA   DS    CL(L'BDFDTEA)       WAS: FDEA                                    
BDFDACTD DS    CL(L'BDFDTAC)       WAS: FDACCTD                                 
*                                                                               
BDFUDGEA DS    CL16                ALTERNATE EA   AND  AC   NUMBERS             
BDFUDGAC DS    CL16                                                             
*                                                                               
BDJBDATA DS    0C                                                               
BDJBEA   DS    CL14                                                             
BDJBAC   DS    CL11                                                             
         EJECT ,                                                                
*---------------------------------------------------------------------*         
* BSNY VARIABLES                                                      *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
         ORG   USERV                                                            
BSSVNC   DS    PL6                 SAVE NON  COM  TOTALS FROM WORKCODES         
BSSVREC  DS    PL6                                                              
BSSVINV  DS    CL6                                                              
BSSVNET  DS    PL6                                                              
BSCBUF#  DS    XL6                 INVOICE   NUM  FOR  BUFFALO                  
BSBLD43  DS    CL1                 BUILD     43   RCD  SWITCH    (Y/N)          
         EJECT ,                                                                
*---------------------------------------------------------------------*         
* DNNYE (NEEDHAM) VARIABLES                                           *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
         ORG   USERV                                                            
NEUSERPO DS    CL(L'NEOEST)                                                     
NEUSERPR DS    CL(L'NEOEST)                                                     
         EJECT ,                                                                
*---------------------------------------------------------------------*         
* WILA (WESTERN) VARIABLES                                            *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
         ORG   USERV                                                            
WITAXAMT DS    PL6                 TAX       AMOUNT                             
WIGSTAMT DS    PL6                 GST       AMOUNT                             
WIPSTAMT DS    PL6                 PST  TAX  AMOUNT                             
WIACRAMT DS    PL6                 BASE      AMOUNT                             
         EJECT ,                                                                
*---------------------------------------------------------------------*         
* YNRO VARIABLES                                                      *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
         ORG   USERV                                                            
YNSVEST  DS    CL12                SAVE AREA FOR  ESTIMATE  NUMBERS             
YNSVNC   DS    PL6                 SAVE NON-COM   TOTALS FROM WORKCODES         
YNSVREC  DS    PL6                 SAVE AREA FOR RECEIVABLE  (AFTER CD)         
YNSVINV  DS    CL6                 SAVE AREA FOR  BUFFER    KEY                 
YNSVNET  DS    PL6                 SAVE AREA FOR  NET  AMOUNT                   
YNCBUF#  DS    XL6                 INVOICE   NUM  FOR  BUFFALO                  
YNCHKAMT DS    CL1                 CHECK     AMOUNTS                            
YNBLD43  DS    CL1                 BUILD     43   RCD  SWITCH    (Y/N)          
         EJECT ,                                                                
*---------------------------------------------------------------------*         
* YNRO 2 VARIABLES                                                    *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
         ORG   USERV                                                            
YN2SKSTA DS    CL1                 LOOK OR   DO NOT  LOOK FOR REVERSALS         
         SPACE 2                                                                
*                                                                               
         ORG   ,                                                                
ACI1DEND EQU   *                   END  OF   ACI102   SPACEND WORK AREA         
*                                                                               
         EJECT ,                                                                
***********************************************************************         
* SK RECORD ENTRY                                                     *         
***********************************************************************         
         SPACE 1                                                                
SKD      DSECT                                                                  
SKINVC   DS    CL6                 REFERENCE NUMBER                             
SKSOURCE DS    CL15                CONTRA    COMPANY   PLUS ACCOUNT             
SKDATE   DS    CL2                 BILL DATE                                    
SKKEYLN  EQU   *-SKD                                                            
SKBK     EQU   *                                                                
SKAMT    DS    CL8                 AMOUNT                                       
SKLEN    EQU   *-SKD                                                            
*                                                                               
*                                                                               
         ORG   SKD                 WILA      REDEFINITION   OF   SKD            
WISKINVC DS    CL6                 REFERENCE NUMBER                             
WISKDATE DS    CL2                 BILL DATE                                    
         DS    CL2                                                              
         ORG   SKD+SKKEYLN                                                      
WISKAMT  DS    PL8                 AMOUNT                                       
         ORG   ,                                                                
         EJECT 1                                                                
***********************************************************************         
* BINARY SEARCH TABLE FORMAT                                          *         
***********************************************************************         
         SPACE 1                                                                
BIND     DSECT                                                                  
BININ    DS    F                                                                
BINLEN   DS    F                                                                
BINDISP  DS    CL1                                                              
BINKEY   DS    CL3                                                              
BINMAX   DS    F                                                                
BINNUMB  DS    CL1                                                              
BINFRST  DS    CL1                                                              
         DS    CL1                                                              
BINLENQ  EQU   *-BIND                                                           
*                                                                               
BINTABLE DS    0CL1                                                             
         EJECT ,                                                                
***********************************************************************         
* BDNY USER FIELD TABLE DSECT                                         *         
***********************************************************************         
         SPACE 1                                                                
BDLUD    DSECT                                                                  
BDLUCODE DS    CL2                                                              
BDLUJBF  DS    AL2                                                              
BDLUJBLN DS    AL1                                                              
BDLUDLN  EQU   *-BDLUD                                                          
         EJECT ,                                                                
***********************************************************************         
* DEFAULT TAPE RECORD AREA                                            *         
***********************************************************************         
         SPACE 1                                                                
XXRECD   DSECT                     DEFAULT TAPE RECORD                          
XXRAGY   DS    CL2                 01 AGENCY                                    
         DS    CL1                 03 HI ORDER OF MEDIA                         
XXRMED   DS    CL1                 04 LO ORDER OF MEDIA                         
XXRCLI   DS    CL3                 05 CLIENT                                    
XXRPRD   DS    CL3                 08 PRODUCT                                   
XXRJOB   DS    CL6                 11 JOB                                       
XXROFF   DS    CL1                 17 OFFICE                                    
XXRNUM   DS    CL6                 18 INVOICE NUMBER                            
XXRDAT   DS    CL6                 24 INVOICE DATE                              
XXRREC   DS    CL10                30 RECEIVABLE AMOUNT                         
XXRINV   DS    CL10                40 INVENTORY                                 
XXRCOM   DS    CL10                50 COMMISSION                                
XXRDIS   DS    CL10                60 DISCOUNT                                  
XXRSTR   DS    CL6                 70 START DATE---YYMMDD                       
XXREND   DS    CL6                 76 END DATE                                  
XXRDUE   DS    CL6                 82 DUE DATE                                  
*                                  88 MEDIA TO PRINT BEFORE                     
XXRIMD   DS    CL1,CL3                INVOICE NUMBER                            
         EJECT ,                                                                
***********************************************************************         
* BDNY TAPE RECORD DSECTS                                             *         
***********************************************************************         
         SPACE 1                                                                
BDHSRECD DSECT                                                                  
         DS    CL2                 WAS: HSCODE                                  
BDHSNUM  DS    CL5                 WAS: HSNUM                                   
         DS    CL11                                                             
BDHSSHTO DS    CL9                 WAS: HSSHTO                                  
         DS    CL11                                                             
         DS    CL9                 WAS: HSSUPC                                  
         DS    CL21                                                             
         DS    CL1                 WAS: HSSTATUS                                
         DS    CL11                                                             
         SPACE 2                                                                
BDFDRECD DSECT                                                                  
         DS    CL2                 WAS: FDCODE                                  
BDFDNUM  DS    CL5                 WAS: FDNUM                                   
BDFDTEA  DS    CL14                WAS: FDEA                                    
BDFDTAC  DS    CL11                WAS: FDACCTD                                 
         DS    CL1                                                              
         DS    CL14                WAS: FDPO                                    
         DS    CL8                 WAS: FDQTY                                   
         DS    CL2                 WAS: FDUNIT                                  
         DS    CL11                                                             
BDFDAMT  DS    CL9                 WAS: FDAMT                                   
         DS    CL3                                                              
         SPACE 2                                                                
BDFTRECD DSECT                                                                  
         DS    CL2                 WAS: FTCODE                                  
BDFTNUM  DS    CL5                 WAS: FTNUM                                   
BDFTSID  DS    CL9                 WAS: FTSHIPID                                
         DS    CL35                                                             
BDFTJYR  DS    CL2                 WAS: FTJYR                                   
BDFTJDAY DS    CL3                 WAS: FTJDAY                                  
         DS    CL15                                                             
BDFTAMT  DS    CL9                 WAS: FTAMT                                   
         SPACE 2                                                                
BDTRRECD DSECT                                                                  
         DS    CL3                 WAS: TRCODE                                  
         DS    CL10                                                             
         DS    CL10                                                             
         DS    CL9                 WAS: TRSUPC                                  
         DS    CL1                                                              
         DS    CL25                WAS: TRSUPN                                  
         DS    CL7                                                              
BDTRAMT  DS    CL9                 WAS: TRAMT                                   
         DS    CL1                                                              
BDTRRECS DS    CL5                 WAS: TRRECS                                  
         EJECT ,                                                                
***********************************************************************         
* BSNY 1 - BATES (SJPM - BSC) OUTPUT TAPE DSECT                       *         
***********************************************************************         
         SPACE 1                                                                
BSRECD   DSECT                     BATES (SJPM-BSC) INTERFACE TAPE RCD          
BSRLEN   DS    XL4                                                              
BSRAGY   DS    CL6                                                              
BSRMED   DS    CL3                                                              
         DS    CL4                                                              
BSREST   DS    CL10                                                             
         DS    CL2                                                              
BSRJOB   DS    CL6                                                              
         DS    CL1                                                              
BSRWKC   DS    CL2                                                              
         DS    CL9                                                              
BSRID    DS    CL2                                                              
BSRGRS   DS    PL6                                                              
BSRNC    DS    PL6                                                              
BSRNET   DS    PL6                                                              
BSRNUM   DS    CL13                                                             
BSRDAT   DS    CL6                                                              
BSRDUE   DS    CL6                                                              
         EJECT ,                                                                
***********************************************************************         
* BSNY 2 - BATES (SJPM - BSC) COMMISSION TAPE DSECT                   *         
***********************************************************************         
         SPACE 1                                                                
BS2RECD  DSECT                     BATES (SJPM-BSC) INTERFACE TAPE RCD          
BS2LEN   DS    XL4                                                              
BS2LIT   DS    0CL9                310774PDC                                    
BS2VEN   DS    CL6                 310774                                       
BS2MED   DS    CL2                 PD                                           
BS2TYPE  DS    CL1                 C                                            
BS2NUM   DS    0CL13               INVOICE NUMBER                               
BS2NYY   DS    CL2                                                              
BS2NMED  DS    CL1                                                              
BS2NNUM  DS    CL6                                                              
BS2N     DS    CL4                 FILL OUT BS2NUM                              
BS2JOB   DS    CL12                C/P/JOB                                      
         DS    CL9                                                              
BS2REC   DS    CL2                 50                                           
BS2GRS   DS    PL6                                                              
BS2CD    DS    PL6                                                              
BS2NET   DS    PL6                                                              
BS2COM   DS    PL6                                                              
BS2FEE   DS    PL6                                                              
         EJECT ,                                                                
***********************************************************************         
* DNNYE 1 - NEEDHAM OUTPUT TAPE DSECT                                 *         
***********************************************************************         
         SPACE 1                                                                
NEORECD  DSECT                     NEEDHAM INTERFACE TAPE RECORD                
NEOIDN   DS    CL1                 01 RECORD IDENTIFIER                         
NEOAGY   DS    CL3                 02 AGENCY-COMPANY MNEMONIC-OFFICE            
NEOMED   DS    CL1                 05 MEDIA                                     
         DS    CL1                 06                                           
NEOPRD   DS    CL6                 07 LEVEL-B INFO (6 BYTES/IWEX/4/93)          
NEONUM   DS    CL6                 11 INVOICE NUMBER                            
NEOEST   DS    CL4                 17 ESTIMATE NUMBER---FROM PROF EL-24         
NEOSDT   DS    CL3                 21 SERVICE DATE---MMY                        
NEOJOB   DS    CL6                 24 JOB NUMBER---SAME AS DDS SYSTEM           
NEOCOD   DS    CL1                 30 ANOTHER MEDIA CODE: P-PRINT, ETC          
NEODIS   DS    CL8                 31 CASH DISCOUNT AMOUNT                      
NEOGRS   DS    CL9                 39 GROSS AMOUNT INCLUDING DISCOUNT           
         DS    CL8                 48                                           
NEOCOM   DS    CL8                 56 AGENCY COMMISSION AMOUNT                  
         DS    CL8                 64                                           
NEONET   DS    CL9                 72 A/P PENDING AMOUNT                        
         DS    CL5                 81                                           
NEOIDT   DS    CL6                 86 BILL-INVOICE DATE---MMDDYY                
         EJECT ,                                                                
***********************************************************************         
* DNNYE 2 - NEEDHAM OUTPUT TAPE DSECT                                 *         
***********************************************************************         
         SPACE 1                                                                
NE2IREC  DSECT                     NEEDHAM INSIDE CHARGES TAPE RECORD           
NE2IIDN  DS    CL6                 01 ID                                        
NE2IPRD  DS    CL6                 07 PRODUCT   6    BYTES    IWEX/4/93         
NE2IBMM  DS    CL2                 13 MONTH --- BILLING   DATE                  
NE2IBYY  DS    CL2                 15 YEAR                                      
NE2ISDT  DS    CL4                 17 BILL DATE                                 
NE2IAMT  DS    CL9                 21 TRANSACTION    AMOUNT                     
         DS    CL5                 30                                           
NE2ICOD  DS    CL2                 35 CODE                                      
         DS    CL43                37                                           
NE2IOFF  DS    CL1                 80 UNIT OF   ANALYSIS                        
         DS    CL2                 82                                           
         EJECT ,                                                                
***********************************************************************         
* WILA - WESTERN OUTPUT TAPE DSECT                                    *         
***********************************************************************         
         SPACE 1                                                                
WIRECD   DSECT                                                                  
WIRECORD DS    0CL162                                                           
WIOFF    DS    CL1                 OFFICE                                       
WIMED    DS    CL2                                                              
WICLI    DS    CL3                                                              
WICLINM  DS    CL30                                                             
WIPRO    DS    CL3                                                              
WIJOB    DS    CL6                                                              
WIJOBNM  DS    CL35                                                             
WIINV    DS    CL6                                                              
         DS    CL4                 SPACE PAD INVOICE NUMBER                     
WIBDATE  DS    CL6                 YYMMDD BILLING DATE                          
WIDUE    DS    CL6                 YYMMDD DUE DATE                              
WIMOS    DS    CL4                 YYMM MOS                                     
WIREC    DS    CL9                 AMOUNT DUE                                   
WICD     DS    CL9                                                              
WITAX    DS    CL9                                                              
WIGST    DS    CL9                                                              
WIPST    DS    CL9                                                              
WIID     DS    CL2                 ALPHA ID                                     
WIACR    DS    CL9                 ACCRUED - A/P                                
WIRECLEN EQU   *-WIRECD                                                         
*                                                                               
         EJECT ,                                                                
***********************************************************************         
* YNRO 1 OUTPUT TAPE DSECT (NOT PMU)                                  *         
***********************************************************************         
         SPACE 1                                                                
YNRECD   DSECT          BEG   END   LEN   DESCRIPTION                           
*                                                                               
YNINVNO  DS     CL7      01    07     7   INVOICE NUMBER                        
YNCLI    DS     CL5      08    12     5   CLIENT                                
YNDIVSN  DS     CL3      13    15     3   DIVISION                              
YNPROD   DS     CL3      16    18     3   PRODUCT                               
YNTYPE   DS     CL1      19    19     1   ALWAYS AN R                           
YNADVMM  DS     CL2      20    21     2   ADV MONTH                             
YNADVYY  DS     CL2      22    23     2   ADV YEAR                              
YNEST    DS     CL10     24    33    10   ESTIMATE                              
YNINVMM  DS     CL2      34    35     2   INVOICE MONTH                         
YNINVDD  DS     CL2      36    37     2   INVOICE DAY                           
YNINVYY  DS     CL2      38    39     2   INVOICE YEAR                          
YNDUEMM  DS     CL2      40    41     2   DUE MONTH                             
YNDUEDD  DS     CL2      42    43     2   DUE DAY                               
YNDUEYY  DS     CL2      44    45     2   DUE YEAR                              
YNGROSS  DS     PL9      46    54     9   GROSS AMMOUNT                         
YNDISCNT DS     PL9      55    63     9   DISCOUNT                              
YNCOST   DS     PL9      64    72     9   COST                                  
YNXTRA   DS     CL16     73    88    16   NOT USED                              
YNOFFICE DS     CL2      89    90     2   OFFICE                                
YNMEDIA  DS     CL3      91    93     3   MEDIA CODE                            
*                                    --                                         
*                      END = TOTAL = 93                                         
         EJECT ,                                                                
***********************************************************************         
* DSECT FOR THE YNRO 1 PMU OUTPUT TAPE                                *         
***********************************************************************         
         SPACE 1                                                                
YNPMURD1 DSECT                                          (WAS: PMURECD)          
YNPMULEN DS    XL4                 RECORD LENGTH        (WAS: PMULEN)           
YNPMUVN# DS    CL6                 VENDOR NUMBER - 331543    (PMUVENNO)         
         DS    CL3                 MEDIA CODE    - PDC  (WAS: PMUMED)           
         DS    CL4                 COMPANY CODE         (WAS: PMUCMP)           
YNPMUEST DS    CL12                ESTIMATE NUMBER      (WAS: PMUESTNO)         
YNPMUJOB DS    CL6                 JOB NUMBER           (WAS: PMUJOB)           
         DS    CL1                 SPACE                                        
YNPMUWC1 DS    CL2                 WORK CODE            (WAS: PMUWKC)           
         DS    CL9                 FILLER                                       
YNPMURTY DS    CL2                 RECORD TYPE          (WAS: PMURECTY)         
YNPMUGRS DS    PL6                 GROSS AMOUNT         (WAS: PMUGRS)           
YNPMUNC1 DS    PL6                 NON-COMMISSIONABLE AMOUNT (PMUNC)            
YNPMUNET DS    PL6                 NET AMOUNT           (WAS: PMUNET            
YNPMUINV DS    CL13                INVOICE NUMBER       (WAS: PMUINVNO)         
YNPMUDT1 DS    CL6                                      (WAS: PMUDAT)           
YNPMUDUE DS    CL6                                      (WAS: PMUDUE)           
YNPMULQ1 EQU   *-YNPMURD1                               (WAS: PMULNQ)           
         EJECT ,                                                                
***********************************************************************         
* DSECT FOR YNRO 2 COMMISSION TAPE - ALL OTHERS (NOT PMU)             *         
***********************************************************************         
         SPACE 1                                                                
YN2RECD  DSECT          BEG   END   LEN   DESCRIPTION                           
YN2INVNO DS     CL7      01    07     7   ALL ZERO'S                            
YN2CLI   DS     CL5      08    12     5   NUM2 =                                
YN2DIVSN DS     CL3      13    15     3      ON ACT RECORD                      
YN2PROD  DS     CL3      16    18     3      FOR PRODUCT                        
YN2TYPE  DS     CL1      19    19     1   ALWAYS AN R                           
YN2ADVMM DS     CL2      20    21     2   ADV MONTH                             
YN2ADVYY DS     CL2      22    23     2   ADV YEAR                              
YN2EST   DS     CL10     24    33    10   ESTIMATE (JOB NUMBER)                 
YN2INVMM DS     CL2      34    35     2   INVOICE MONTH                         
YN2INVDD DS     CL2      36    37     2   INVOICE DAY                           
YN2INVYY DS     CL2      38    39     2   INVOICE YEAR                          
YN2DUEMM DS     CL2      40    41     2   NOT USED                              
YN2DUEDD DS     CL2      42    43     2    "   "                                
YN2DUEYY DS     CL2      44    45     2    "   "                                
YN2GROSS DS     PL9      46    54     9    "   "                                
YN2DISCN DS     PL9      55    63     9    "   "                                
YN2COST  DS     PL9      64    72     9   SJ W/CA OF SK, REVERSE SIGN           
YN2EXTRA DS     CL16     73    88    16   NOT USED                              
YN2OFFIC DS     CL2      89    90     2   OFFICE                                
YN2MEDIA DS     CL3      91    93     3   MEDIA CODE                            
*                                    --                                         
*                      END = TOTAL = 93                                         
         EJECT ,                                                                
***********************************************************************         
* DSECT FOR YNRO 2 COMMISSION TAPE - CLIENT PMU                       *         
***********************************************************************         
         SPACE 1                                                                
YN2PMUD  DSECT                                                                  
YN2PMLEN DS    XL4                                      (WAS: PMU2D)            
YN2PMLIT DS    0CL9                331543PDC            (WAS: PMU2LIT)          
         DS    CL6                 331543               (WAS: PMU2VEN)          
         DS    CL2                 PD                   (WAS: PMU2MED)          
         DS    CL1                 C                    (WAS: PMU2TYPE)         
*                                                                               
YN2PMINV DS    0CL13               INVOICE NUMBER       (WAS: PMU2INV)          
YN2PMNYY DS    CL2                                      (WAS: PMU2NYY)          
YN2PMNMD DS    CL1                                      (WAS: PMU2NMED)         
YN2PMUN# DS    CL6                                      (WAS: PMU2NNUM)         
         DS    CL4                 FILL OUT PMU2INV     (WAS: PMU2N)            
*                                                                               
YN2PMJOB DS    CL12                C/P/JOB              (WAS: PMU2JOB)          
         DS    CL9                                                              
YN2PMREC DS    CL2                 50                   (WAS: PMU2REC)          
YN2PMGRS DS    PL6                                      (WAS: PMU2GRS)          
YN2PMUCD DS    PL6                                      (WAS: PMU2CD)           
YN2PMNET DS    PL6                                      (WAS: PMU2NET)          
YN2PMCOM DS    PL6                                      (WAS: PMU2COM)          
YN2PMFEE DS    PL6                                      (WAS: PMU2FEE)          
YN2PMLNQ EQU   *-YN2PMUD                                (WAS: PMU2LNQ)          
         EJECT ,                                                                
***********************************************************************         
* ++INCLUDES ACGOBLOCK DSECT                                          *         
***********************************************************************         
         SPACE 1                                                                
* ACGOBLOCK                                                                     
ACGOD    DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE ACGOBLOCK                                                      
         PRINT ON                                                               
         EJECT ,                                                                
***********************************************************************         
* ++INCLUDE ACJOBBLOCK DSECT                                          *         
***********************************************************************         
         SPACE 1                                                                
JBLOCKD  DSECT                     MAP  JOBBER    INTERFACE   BLOCK             
         PRINT OFF                                                              
       ++INCLUDE ACJOBBLOCK                                                     
         PRINT ON                                                               
         EJECT ,                                                                
***********************************************************************         
* ++INCLUDES                                                          *         
***********************************************************************         
         SPACE 1                                                                
* ACGENMODES                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* ACREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
* ACMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
* DDMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
* DDREMOTED                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDREMOTED                                                      
         PRINT ON                                                               
* ACJOBBERD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACJOBBERD                                                      
         PRINT ON                                                               
* ACVATICAND                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACVATICAND                                                     
         PRINT ON                                                               
* DDCTRYEQUS                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDCTRYEQUS                                                     
         PRINT ON                                                               
* DDREPXTRAD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDREPXTRAD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009ACREPI102S08/17/00'                                      
         END                                                                    
