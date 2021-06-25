*          DATA SET ACPOP01B   AT LEVEL 003 AS OF 05/18/06                      
*PHASE T60A01BA                                                                 
*INCLUDE POPTXT                                                                 
         TITLE 'ACPOP01 - PRODUCTION ORDERS PLUS- ADD/CHANGE/EDIT'              
ACPOP01  CSECT                                                                  
         PRINT NOGEN               ON ENTRY TO OVERLAY IF ACTION IS             
         NMOD1 0,**POP1**,RA       CHANGE OR EDIT, ORDER RECORD IS              
         USING POPWORKD,R9                                                      
         USING TWAD,R8                                                          
         L     RC,ASAVE                                                         
         USING LOCALD,RC                                                        
         EJECT                                                                  
***********************************************************************         
*        PREPARE SCREEN AND AIOAREA3                                  *         
***********************************************************************         
                                                                                
INITIAL  MVI   INVOICED,C'N'       PRESET TO NOT INVOICED                       
         LA    R1,APONUMH                                                       
         SR    R2,R2                                                            
         LA    R3,APOTABH                                                       
         CLI   ACTION,EDIT         DIS2, CHA2 OR EDIT, USE 2ND SCREEN           
         BL    INIT02                                                           
         LA    R3,AP2TABH                                                       
                                                                                
INIT02   IC    R2,0(R1)            'OR' IN SPACES                               
         SH    R2,=H'9'                                                         
         EX    R2,*+8                                                           
         B     *+10                                                             
         OC    8(0,R1),SPACES                                                   
         AH    R2,=H'9'                                                         
         BXLE  R1,R2,INIT02                                                     
                                                                                
         L     RE,AIOAREA3         CLEAR AIOAREA3 FOR ORDER RECORD              
         LHI   RF,LIOAREAS                                                      
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
                                                                                
         L     R3,AIOAREA3                                                      
         TM    ACTINDS,NEW         ADDING A NEW RECORD?                         
         BZ    CHGEDT              NO, MUST BE CHANGE OR EDIT                   
         EJECT                                                                  
***********************************************************************         
*        BUILD A NEW ORDER                                            *         
***********************************************************************         
                                                                                
         USING ORDRECD,R3                                                       
BLDNEW   MVI   ORDKTYP,ORDKTYPQ    R5(ORDER RECORD)                             
         MVC   ORDKCPY,COMPANY                                                  
         MVC   ORDKORD,APONUM                                                   
                                                                                
         LA    R5,ELEMENT          BUILD ORDELQ                                 
         USING ORDELD,R5                                                        
         MVI   ORDEL,ORDELQ                                                     
         MVI   ORDLN,ORDLN2Q                                                    
         MVC   ORDJOB,JOBKEY                                                    
         MVC   ORDSUPC,COMPANY                                                  
         MVC   ORDSUPU(14),APOSUP+1                                             
         CLI   APOSUP,C'*'                                                      
         BE    *+16                                                             
         MVC   ORDSUPU(2),SUPPUL                                                
         MVC   ORDSUPA,APOSUP                                                   
         MVC   ORDDATE,TODAYP                                                   
         GOTOR VALAUTH             VALIDATE DATA FROM AUTHORIZED LINE           
         BNE   ERXIT                                                            
                                                                                
         MVC   ORDAUTH,VAUTH       ADD DATA TO ORDELQ                           
         CLC   VDATE,SPACES                                                     
         BE    *+10                                                             
         MVC   ORDDATE,VDATE                                                    
         MVC   ORDDDTE,VDDATE                                                   
         CLC   VATTN,SPACES                                                     
         BE    *+10                                                             
         MVC   ORDATTN,VATTN                                                    
                                                                                
         GOTOR VALTAX              VALIDATE TAX FIELD                           
         BNE   ERXIT                                                            
         MVC   ORDTAX,VTAX         ADD DATA TO ORDELQ                           
         OI    ORDSTAT,ORDSPOP     SET STATUS TO ALWAYS USE POP                 
         GOTOR ADDELEM,AIOAREA3    ADD THE ELEMENT TO AIOAREA3                  
                                                                                
         CLI   TYPE,EXPENSE        IS THIS AN EXPENSE ORDER?                    
         BNE   BLDN02              NO                                           
         GOTOR VALAMT              VALIDATE AMOUNT (EXPENSE ONLY)               
         BNE   ERXIT                AND ADD OAMELQ                              
         B     BLDN04                                                           
                                                                                
BLDN02   GOTOR VALWC               VALIDATE WORKCODES AND AMOUNTS               
         BNE   ERXIT                AND ADD OAMELQ(S)                           
                                                                                
BLDN04   GOTOR ADDSTAT             ADD TRSELQ ELEMENT                           
         BNE   ERXIT                                                            
                                                                                
         GOTOR ADDREC              NOW ADD ORDER RECORD TO FILE                 
         BNE   ERXIT                                                            
         B     OKXIT                                                            
         DROP  R3,R5                                                            
         EJECT                                                                  
***********************************************************************         
*        CHANGE OR EDIT AN EXISTING ORDER                             *         
***********************************************************************         
                                                                                
CHGEDT   L     RE,AIOAREA1         ORDER IS IN AIOAREA1 AT ENTRY                
         LR    R0,R3               AIOAREA3 HAS BEEN CLEARED                    
         LH    RF,ACCORLEN(RE)                                                  
         LR    R1,RF                                                            
         MVCL  R0,RE               COPY EXISTING ORDER INTO AIO3                
                                                                                
         CLI   ACTION,CH2          CHANGING FIRST OR SECOND SCREEN?             
         BE    *+12                                                             
         CLI   ACTION,EDIT                                                      
         BNE   CHGE02              THE FIRST ONE                                
                                                                                
         USING ORDRECD,R3                                                       
         MVI   ELCODE,SCMELQ                                                    
         GOTOR REMELEM,AIOAREA3    REMOVE THE COMMENTS                          
         B     CHGE10                                                           
                                                                                
CHGE02   LA    RE,OAMTELS          CLEAR OAMTELS FOR OLD ELEMENTS               
         LHI   RF,L'OAMTELS                                                     
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
                                                                                
         LA    R1,OAMTELS                                                       
         MVI   ELCODE,OAMELQ       CHECK IF INVOICED & SAVE                     
         BAS   RE,GETELIO                                                       
         BE    CHGE06                                                           
         DC    H'0'                MUST HAVE AT LEAST ONE                       
                                                                                
         USING OAMELD,R6                                                        
CHGE04   BAS   RE,NEXTEL                                                        
         BNE   CHGE08                                                           
                                                                                
CHGE06   CP    OAMINUM,=P'0'       SET INVOICED MARKER IF ANY                   
         BE    *+8                                                              
         MVI   INVOICED,C'Y'                                                    
         MVC   0(OAMLN2Q,R1),OAMEL                                              
         AHI   R1,OAMLN2Q                                                       
         B     CHGE04                                                           
         DROP  R6                                                               
                                                                                
CHGE08   GOTOR REMELEM,AIOAREA3    THEN REMOVE THE OAMELQS                      
                                                                                
CHGE10   MVI   ELCODE,ORDELQ       GET ORDER ELEMENT                            
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                MUST HAVE                                    
                                                                                
         USING ORDELD,R5                                                        
         LR    R5,R6               BE CONSISTENT WITH ADD                       
                                                                                
         CLI   ACTION,EDIT         ARE WE EDITING?                              
         BNE   CHGE12              NO, SKIP OVER THIS                           
                                                                                
         LA    R0,10+XTRASCAN                                                   
         GOTOR =V(POPTXT),DMCB,APOACTH,(10,AP2FRSTH),TEMP,VSCANNER,    X        
               RR=RB                                                            
         CLI   DMCB,0                                                           
         BE    CHGE14                                                           
                                                                                
         MVC   FERN,DMCB                                                        
         MVC   FNDX,DMCB+4                                                      
         MVC   FADR,DMCB                                                        
         CLI   FERN,SPECIAL        IF FERN IS X'FE' OR X'FF' P3=A(MSG)          
         BL    ERXIT                                                            
         L     R1,DMCB+8                                                        
         MVC   MSG,0(R1)                                                        
         B     ERXIT                                                            
                                                                                
CHGE12   GOTOR VALCLI                                                           
         BNE   ERXIT                                                            
                                                                                
         GOTOR VALAUTH                                                          
         BNE   ERXIT                                                            
         MVC   ORDAUTH,VAUTH       UPDATE ORDELQ                                
         CLC   VDATE,SPACES                                                     
         BE    *+10                                                             
         MVC   ORDDATE,VDATE                                                    
         MVC   ORDDDTE,VDDATE                                                   
         CLC   VATTN,SPACES                                                     
         BE    *+10                                                             
         MVC   ORDATTN,VATTN                                                    
                                                                                
         GOTOR VALTAX                                                           
         BNE   ERXIT                                                            
         MVC   ORDTAX,VTAX                                                      
                                                                                
         CLI   ACTION,CH2                                                       
         BNE   CHGE16                                                           
                                                                                
CHGE14   GOTOR VALNAR                                                           
         BNE   ERXIT                                                            
         B     CHGE20                                                           
                                                                                
CHGE16   CLI   TYPE,EXPENSE        IS THIS AN EXPENSE ORDER?                    
         BNE   CHGE18              NO                                           
         GOTOR VALAMT              VALIDATE AMOUNT (EXPENSE ONLY)               
         BNE   ERXIT                AND ADD OAMELQ                              
         B     CHGE20                                                           
                                                                                
CHGE18   GOTOR VALWC                                                            
         BNE   ERXIT                                                            
                                                                                
CHGE20   GOTOR CHAREC              CHANGE THE ORDER RECORD HERE                 
         BNE   ERXIT                                                            
         B     OKXIT                                                            
         EJECT                                                                  
***********************************************************************         
*        CHECK FOR NO CHANGE TO CLI/PRO/JOB/SUP/EXP ON ACTION CHANGE  *         
***********************************************************************         
                                                                                
VALCLI   NTR1                                                                   
         CLI   TYPE,EXPENSE                                                     
         BE    VALC02                                                           
         LA    R1,APOCLIH                                                       
         CLC   LCLI,8(R1)                                                       
         BNE   VALCER                                                           
         LA    R1,APOPROH                                                       
         CLC   LPRO,8(R1)                                                       
         BNE   VALCER                                                           
         LA    R1,APOJOBH                                                       
         CLC   LJOB,8(R1)                                                       
         BNE   VALCER                                                           
         B     VALC04                                                           
                                                                                
VALC02   LA    R1,APOEXPH                                                       
         CLC   LEXP,8(R1)                                                       
         BNE   VALCER                                                           
                                                                                
VALC04   LA    R1,APOSUPH                                                       
         CLC   LSUPP,8(R1)                                                      
         BE    VALCX                                                            
                                                                                
VALCER   MVI   FERN,CANTAMND                                                    
         ST    R1,FADR                                                          
         B     ERXIT                                                            
                                                                                
VALCX    B     OKXIT                                                            
         EJECT                                                                  
***********************************************************************         
*        CHECK AUTHORIZED LINE PLUS ORDER DATE                        *         
*        R3=A(NEW ORDER RECORD FOR ADD)                               *         
*        R3=A(EXISTING ORDER RECORD FOR CHANGE)                       *         
*        R5=A(NEW ORDELQ FOR ADD)                                     *         
*        R5=A(EXISTING ORDELQ FOR CHANGE)                             *         
***********************************************************************         
                                                                                
         USING ORDRECD,R3                                                       
         USING ORDELD,R5                                                        
VALAUTH  NTR1                                                                   
         MVC   VAUTH(VAUTHL),SPACES                                             
         XC    VSTAT,VSTAT                                                      
         GOTOR AFVAL,APOAUTH       AUTHORIZED - REQUIRED                        
         BZ    VALAUER                                                          
         MVC   VAUTH,FLD                                                        
         GOTOR AFVAL,APODATH       ORDER DATE - OPTIONAL                        
         BZ    VALAU12                                                          
         GOTOR VDATVAL,DMCB,(0,FLD),FLD+8                                       
         OC    0(4,R1),0(R1)                                                    
         BNZ   VALAU04                                                          
                                                                                
VALAU02  MVI   FERN,INVALID        S.B. MMMDD/YY                                
         B     VALAUER                                                          
                                                                                
VALAU04  CLC   FLD+8(2),=C'90'     DON'T ALLOW BEFORE 1990                      
         BNH   VALAU02                                                          
         GOTOR VDATCON,DMCB,(0,FLD+8),(1,WORK)                                  
         CLI   ACTION,CHA                                                       
         BE    VALAU06                                                          
         CLI   ACTION,CH2                                                       
         BNE   VALAU08                                                          
                                                                                
VALAU06  CLC   ORDDATE,WORK                                                     
         BE    VALAU10                                                          
         MVI   FERN,CANTAMND       NO CHANGE TO ORDER DATE ALLOWED              
         B     VALAUER                                                          
                                                                                
VALAU08  CLC   WORK(L'TODAYP),TODAYP                                            
         BNL   VALAU10                                                          
         L     RF,AGOBLOCK                                                      
         USING GOBLOCKD,RF                                                      
         CLI   GOORDER,C'Y'        ALLOW BACKDATING?                            
         BNE   VALAU02             NO                                           
         DROP  RF                                                               
                                                                                
VALAU10  MVC   VDATE,WORK                                                       
                                                                                
VALAU12  GOTOR AFVAL,APODDTEH        DUE DATE - REQUIRED                        
         BZ    VALAUER                                                          
         GOTOR VDATVAL,DMCB,(0,FLD),FLD+8                                       
         OC    0(4,R1),0(R1)                                                    
         BNZ   *+12                                                             
         MVI   FERN,INVALID        S.B. MMMDD/YY                                
         B     VALAUER                                                          
                                                                                
         GOTOR VDATCON,DMCB,(0,FLD+8),(1,VDDATE)                                
         CLI   APOATTNH+5,0                                                     
         BE    VALAUX              ATTN NAME - OPTIONAL                         
         GOTOR AFVAL,APOATTNH                                                   
         MVC   VATTN,FLD                                                        
                                                                                
VALAUX   B     OKXIT                                                            
                                                                                
VALAUER  B     ERXIT                                                            
         DROP  R3,R5                                                            
         EJECT                                                                  
***********************************************************************         
*        VALIDATE TAX QUESTION                                        *         
***********************************************************************         
                                                                                
VALTAX   NTR1                                                                   
         LA    R1,APOTAXH                                                       
         ST    R1,FADR                                                          
         CLI   5(R1),0             DID THEY ANSWER TAX QUESTION ?               
         BE    VALT02              NO, SEE IF REQUIRED                          
                                                                                
         MVI   FERN,INVALID        YES, CHECK IF VALID                          
         CLI   8(R1),C'Y'                                                       
         BE    VALT04                                                           
         CLI   8(R1),C'N'                                                       
         BNE   VALTER                                                           
         B     VALT04                                                           
                                                                                
VALT02   MVI   FERN,NOINPUT        NO, SEE IF REQUIRED                          
         CLI   PROGPROF+9,C'Y'                                                  
         BE    VALTER                                                           
         CLI   PROGPROF+9,C'P'                                                  
         BE    VALTER                                                           
                                                                                
VALT04   MVC   VTAX,APOTAX                                                      
                                                                                
VALTX    B     OKXIT                                                            
                                                                                
VALTER   B     ERXIT                                                            
         EJECT                                                                  
***********************************************************************         
*        HANDLE AMOUNT AND BUILD NEW ELEMENT FOR EXPENSE ORDERS       *         
***********************************************************************         
                                                                                
         USING OAMELD,R5           BUILD A NEW ELEMENT WITH ZERO VALUES         
VALAMT   NTR1                                                                   
         LA    RE,NAMTELS                                                       
         LA    RF,L'OAMTELS                                                     
         SR    R1,R1                                                            
         MVCL  RE,R0               CLEAR NAMTELS                                
                                                                                
         LA    R5,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   OAMEL,OAMELQ                                                     
         MVI   OAMLN,OAMLN2Q                                                    
         MVC   OAMSTAT,STATUS                                                   
         ZAP   OAMAMNT,=P'0'                                                    
         ZAP   OAMINUM,=P'0'                                                    
         ZAP   OAMIVAL,=P'0'                                                    
         ZAP   OAMTVAL,=P'0'                                                    
         MVC   OAMWORK,SPACES                                                   
                                                                                
         USING SCREEND,R2                                                       
         LA    R2,APOWRKH                                                       
         GOTOR AFVAL,SCRAMTH                                                    
         BZ    VALAMER                                                          
                                                                                
         SR    R0,R0                                                            
         ICM   R0,1,SCRAMTH+5                                                   
         GOTOR VCASHVAL,DMCB,SCRAMT,(R0)                                        
         MVI   FERN,INVAMNT                                                     
         CLI   0(R1),X'FF'                                                      
         BE    VALAMER                                                          
                                                                                
         L     R0,4(R1)                                                         
         CVD   R0,DUB                                                           
                                                                                
         CP    DUB,HIAMT           IF 9 SIGNIFICANT #'S                         
         BL    *+8                 MUST USE TYPE 10                             
         OI    VSTAT,ORDSTY10                                                   
                                                                                
         CP    DUB,LOAMT           IF 9 SIGNIFICANT #'S                         
         BH    *+8                 MUST USE TYPE 10                             
         OI    VSTAT,ORDSTY10                                                   
                                                                                
         CP    DUB,=P'999999999'   AMOUNT IS TOO LARGE                          
         BH    VALAMER                                                          
                                                                                
         ZAP   OAMAMNT,DUB         AMOUNT INTO ELEMENT                          
                                                                                
         EDIT  (R0),SCRAMT,2,ALIGN=RIGHT,MINUS=YES                              
         OI    SCRAMTH+6,X'80'                                                  
                                                                                
         MVC   NAMTELS(L'ELEMENT),ELEMENT                                       
         GOTOR ADDELEM,AIOAREA3                                                 
                                                                                
VALAMX   B     OKXIT                                                            
                                                                                
VALAMER  LA    R2,SCRAMTH                                                       
         ST    R2,FADR                                                          
         B     ERXIT                                                            
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
*        VALIDATE NARRATIVE AND ADD COMMENT ELEMENTS                  *         
***********************************************************************         
                                                                                
VALNAR   NTR1                                                                   
         LA    R3,AP2FRSTH         R3/4/5 = BXLE REGISTERS                      
         SR    R4,R4                                                            
         LA    R5,AP2ENDH                                                       
         SR    R2,R2               R2 = COMMENT SEQUENCE NUMBER                 
         LA    R6,ELEMENT          R6 = NEW COMMENT ELEMENT                     
         XC    ELEMENT,ELEMENT                                                  
         USING SCMELD,R6                                                        
                                                                                
VALN02   GOTOR AFVAL,(R3)          LOOP FOR A LINE                              
         BZ    VALN14                                                           
         ZIC   RF,0(R3)            DROP TRAILING SPACES                         
         SH    RF,=H'8'                                                         
         LA    RE,7(R3,RF)                                                      
         CLI   0(RE),C' '                                                       
         BH    *+12                                                             
         BCT   RF,*-12                                                          
         B     VALN14              SPACES ONLY, GET NEXT LINE                   
                                                                                
         STC   RF,5(R3)                                                         
         CLC   FLD(2),=C'N='       WAS COMMENT NUMBER ENTERED?                  
         BNE   VALN10              NO                                           
                                                                                
         USING SCMRECD,KEY                                                      
         XC    SCMKEY,SCMKEY       PREAPRE KEY TO READ COMMENTS RECORD          
         MVI   SCMKTYP,SCMKTYPQ                                                 
         MVC   SCMKCPY,COMPANY                                                  
                                                                                
         GOTOR VSCANNER,DMCB,(R3),(8,TEMP)                                      
         ZIC   R0,DMCB+4                                                        
         CH    R0,=H'1'                                                         
         MVI   FERN,INVALID                                                     
         BL    VALNER                                                           
         BE    *+8                                                              
         MVI   FNDX,1                                                           
         LA    R7,TEMP                                                          
                                                                                
VALN04   CLI   0(R7),1             LOOP FOR AN ENTRY                            
         BNE   VALN06              CHECK FOR N=123456                           
         CLI   12(R7),C'N'                                                      
         BNE   VALN06                                                           
         CLI   1(R7),1                                                          
         BL    VALN06                                                           
         CLI   1(R7),6                                                          
         BNH   VALN08                                                           
                                                                                
VALN06   MVI   FERN,INVALID                                                     
         MVC   XTRAMESS(10),=C'(N=123456)'                                      
         B     EXIT                                                             
                                                                                
VALN08   MVC   VALUE,SPACES        RIGHT-ALIGN NUMBER IN VALUE                  
         LA    RF,VALUE+5                                                       
         ZIC   RE,1(R7)                                                         
         BCTR  RE,0                                                             
         SR    RF,RE                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),22(R7)                                                   
         MVC   SCMKCODE,VALUE                                                   
         GOTOR AREAD,AIOAREA2      CHECK COMMENT EXISTS                         
         BNE   VALNER                                                           
                                                                                
         MVI   SCMEL,SCMELQ        BUILD ELEMENT                                
         MVI   SCMLN,SCMLN2Q                                                    
         STC   R2,SCMSEQ                                                        
         MVI   SCMTYPE,SCMTPRAD    PRINT AFTER DATA                             
         LA    RF,AP2FOOTH                                                      
         CR    RF,R3                                                            
         BNE   *+8                                                              
         OI    SCMTYPE,SCMTPOFT    CHANGE TO FOOTLINE                           
         MVC   SCMCODE,VALUE                                                    
         GOTOR ADDELEM,AIOAREA3                                                 
         LA    R2,1(R2)            BUMP UP SEQUENCE                             
         CLI   FNDX,0              ANY MORE LINES?                              
         BE    VALN14              NO                                           
                                                                                
         LA    R7,32(R7)           GET NEXT TEMP ENTRY                          
         BCT   R0,VALN04                                                        
         B     VALN14                                                           
                                                                                
VALN10   CLC   FLD(2),=C'S='       ADDING SPACES(1-9 LINES)?                    
         BNE   VALN12              NO                                           
                                                                                
         MVI   FERN,TOOLONG        CHECK ITS NUMERIC 01-09 (1-9)                
         CLI   5(R3),4                                                          
         BH    VALNER                                                           
                                                                                
         ZIC   R1,5(R3)                                                         
         SH    R1,=H'3'                                                         
         MVI   FERN,NOINPUT                                                     
         BM    VALNADER                                                         
                                                                                
         MVC   WORK(2),=2C'0'                                                   
         EX    R1,VALNARMV                                                      
         CLC   WORK(2),=2C'0'                                                   
         MVI   FERN,INVNUM                                                      
         BNE   VALNADER                                                         
                                                                                
         EX    R1,VALNARPK                                                      
         CVB   R1,DUB                                                           
         MVI   FERN,NOINPUT                                                     
         LTR   R1,R1                                                            
         BZ    VALNADER                                                         
                                                                                
         MVI   FERN,TOOBIG                                                      
         CH    R1,=H'9'                                                         
         BH    VALNADER                                                         
                                                                                
VALN12   MVI   SCMEL,SCMELQ        STANDARD COMMENT - BUILD AN ELEMENT          
         MVI   SCMLN,SCMLN2Q                                                    
         STC   R2,SCMSEQ                                                        
         LA    RF,AP2FOOTH                                                      
         CR    RF,R3                                                            
         BNE   *+8                                                              
         OI    SCMTYPE,SCMTPOFT    FOOTLINE                                     
                                                                                
         ZIC   RE,5(R3)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   SCMNARR(0),FLD                                                   
                                                                                
         LA    RE,5(RE)                                                         
         STC   RE,SCMLN                                                         
         GOTOR ADDELEM,AIOAREA3                                                 
         LA    R2,1(R2)            BUMP SEQUENCE NO                             
                                                                                
VALN14   IC    R4,0(R3)            BUMP TO NEXT UNPROT (NEXT LINE)              
         BXLE  R3,R4,*+8                                                        
         B     VALN16                                                           
         TM    1(R3),X'20'                                                      
         BO    VALN14                                                           
         B     VALN02                                                           
                                                                                
VALN16   MVI   FNDX,0              AT END CHECK FOR ANY INPUT                   
         CLI   ACTION,EDIT                                                      
         BNE   VALNX                                                            
                                                                                
         MVC   MSG,VALMESS         IF EDIT JUST ASK USER TO SIGHT CHK           
         LA    R1,APOACTH                                                       
         ST    R1,FADR                                                          
         XC    APOACT,APOACT                                                    
         MVI   APOACTH+5,4                                                      
         OI    APOACTH+6,X'81'     MODIFY                                       
         MVC   APOACT(4),=C'CHA2'                                               
         B     OKEND                                                            
                                                                                
VALNX    B     OKXIT                                                            
                                                                                
VALNADER MVC   XTRAMESS(7),=C'(S=1-9)'                                          
VALNER   B     ERXIT                                                            
                                                                                
VALNARMV MVZ   WORK(0),FLD+2                                                    
VALNARPK PACK  DUB,FLD+2(0)                                                     
VALMESS  DC    CL60'EDITED NARRATIVE WILL BE AS SHOWN - HIT ENTER IF ASX        
                REQUIRED'                                                       
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        VALIDATE WORK CODES AND AMOUNTS FOR PRODUCTION ORDERS        *         
*        BUILD ORDER AMOUNT ELEMENTS IN NAMTELS SO THEY CAN BE        *         
*        ADDED TO THE ORDER RECORD AS WELL AS THE SJ RECORD           *         
***********************************************************************         
                                                                                
         USING SCREEND,R2                                                       
VALWC    NTR1                                                                   
         LA    R2,APOWRKH          START AT FIRST FIELD                         
         LHI   R6,20               MAX NUMBER OF WORKCODES                      
         ZAP   COMCNT,=P'0'        CLEAR WORKCODE COUNTERS                      
         ZAP   NONCNT,=P'0'                                                     
                                                                                
         LA    RE,NAMTELS          CLEAR NAMTELS FOR NEW ELEMENTS               
         LHI   RF,L'NAMTELS                                                     
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         GOTOR AFVAL,APOWRKH       MUST HAVE AT LEAST ONE WORKCODE              
         BZ    VALWER                                                           
                                                                                
         LA    R5,NAMTELS          BUILD THE NEW ELEMENTS HERE                  
                                                                                
VALW02   MVI   STATUS,0            CLEAR STATUS                                 
         MVI   FERN,CANTAMND                                                    
         CLI   INVOICED,C'Y'       HAS THIS BEEN INVOICED YET?                  
         BNE   VALW04              NO, CONTINUE                                 
         MVI   FERN,CANTAMND       YES, CAN'T CHANGE SOME THINGS                
         TM    SCRWRKH+4,X'80'     WORKCODE                                     
         BO    VALWER                                                           
         TM    SCRCOMH+4,X'80'     OR COMMISSION                                
         BO    VALWER                                                           
                                                                                
VALW04   CLI   SCRWRKH+5,0         ANYTHING ENTERED?                            
         BNE   VALW06              YES, VALIDATE                                
         CLI   SCRCOMH+5,0                                                      
         BNE   VALWER              STATUS - ERROR                               
         CLI   SCRAMTH+5,0                                                      
         BNE   VALWER              AMOUNT ERROR                                 
         B     VALW18              OK, GET NEXT LINE                            
                                                                                
VALW06   GOTO1 AFVAL,SCRAMTH                                                    
         BZ    VALAER                                                           
         MVI   FERN,INVALID        PREPARE FOR ERROR                            
         CLI   SCRWRKH+5,2         CHECK LENGTH                                 
         BNE   VALWER                                                           
                                                                                
         GOTOR AGETWC,SCRWRK                                                    
         BNE   VALWER                                                           
                                                                                
         MVC   SCRDES,WORK         PRINT THE DESCRIPTION                        
         OI    SCRDESH+6,X'80'                                                  
         CLI   SCRCOMH+5,0         IF COM BLANK, USE WC STATUS                  
         BNE   VALW08                                                           
         MVI   SCRCOM,C'C'         DEFAULT IS C                                 
         TM    WORK+15,WCOSNONC    IS IT NON/COM?                               
         BZ    *+8                                                              
         MVI   SCRCOM,C'N'         YES, CHANGE STATUS                           
         OI    SCRCOMH+6,X'80'                                                  
                                                                                
VALW08   CLI   SCRCOM,C'C'                                                      
         BE    VALW10                                                           
         CLI   SCRCOM,C'N'                                                      
         BNE   VALSER                                                           
         OI    STATUS,OAMSNOCM     MAKE NON-COMMISSIONABLE IF ENTERED           
                                                                                
VALW10   CLI   PROGPROF+3,2        IS AN ESTIMATE NEEDED?                       
         BNE   VALW12              NO                                           
         BAS   RE,CHKEST           YES, CHECK FOR IT                            
         BE    VALW14              FOUND IT                                     
         MVI   FERN,NOESTWC        NOT THERE, ERROR                             
         B     VALWER                                                           
                                                                                
VALW12   CLI   PROGPROF+3,3        IS AN APPROVED ESTIMATE REQUIRED?            
         BNE   VALW14              NO                                           
         BAS   RE,APPEST           YES, CHECK FOR IT                            
         BE    VALW14              FOUND IT                                     
         MVI   FERN,ESTAPP         NOT THERE, ERROR                             
         B     VALWER                                                           
                                                                                
VALW14   SR    R0,R0                                                            
         ICM   R0,1,SCRAMTH+5                                                   
         GOTOR VCASHVAL,DMCB,SCRAMT,(R0)                                        
         MVI   FERN,INVAMNT                                                     
         CLI   0(R1),X'FF'                                                      
         BE    VALAER                                                           
                                                                                
         L     R0,4(R1)                                                         
         CVD   R0,DUB                                                           
                                                                                
         CP    DUB,HIAMT           IF 9 SIGNIFICANT #'S                         
         BL    *+8                 MUST USE TYPE 10                             
         OI    VSTAT,ORDSTY10                                                   
                                                                                
         CP    DUB,LOAMT           IF 9 SIGNIFICANT #'S                         
         BH    *+8                 MUST USE TYPE 10                             
         OI    VSTAT,ORDSTY10                                                   
                                                                                
         CP    DUB,=P'999999999'   AMOUNT IS TOO LARGE                          
         BH    VALAER                                                           
                                                                                
         EDIT  (R0),SCRAMT,2,ALIGN=RIGHT,MINUS=YES                              
         OI    SCRAMTH+6,X'80'                                                  
                                                                                
         USING OAMELD,R5                                                        
         MVI   OAMEL,OAMELQ                                                     
         MVI   OAMLN,OAMLN2Q                                                    
         MVC   OAMSTAT,STATUS                                                   
         MVC   OAMWORK,SCRWRK                                                   
         ZAP   OAMAMNT,DUB                                                      
         ZAP   OAMINUM,=P'0'                                                    
         ZAP   OAMIVAL,=P'0'                                                    
         ZAP   OAMTVAL,=P'0'                                                    
                                                                                
         LA    RF,COMCNT                                                        
         CLI   SCRCOM,C'C'         KEEP COUNT OF WORKCODES                      
         BE    *+8                                                              
         LA    RF,NONCNT                                                        
         AP    0(1,RF),=P'1'                                                    
                                                                                
         CP    0(1,RF),=P'1'       IF MORE THAN ONE, MARK IT MINOR              
         BNH   *+8                                                              
         OI    OAMSTAT,OAMSMAMI                                                 
         CLI   INVOICED,C'Y'                                                    
         BNE   VALW16                                                           
                                                                                
         LHI   R1,20                                                            
         SR    R1,R6               GET SEQUENTIAL NUMBER OF WORKCODE            
         MH    R1,=Y(OAMLN2Q)                                                   
         LA    RE,OAMTELS(R1)      INDEX INTO OLD WORKCODE TABLE                
                                                                                
         CLC   OAMWORK,OAMWORK-OAMELD(RE)                                       
         BE    *+6                 WE BETTER MATCH                              
         DC    H'0'                                                             
                                                                                
         MVC   OAMINUM,OAMINUM-OAMELD(RE)                                       
         MVC   OAMIVAL,OAMIVAL-OAMELD(RE)                                       
         MVC   OAMTVAL,OAMTVAL-OAMELD(RE)                                       
         MVC   OAMLAST,OAMLAST-OAMELD(RE)                                       
                                                                                
VALW16   MVC   ELEMENT(OAMLN2Q),OAMEL                                           
         GOTOR ADDELEM,AIOAREA3                                                 
         AHI   R5,OAMLN2Q          BUMP UP TO NEXT SPOT                         
                                                                                
VALW18   BAS   RE,NEXTWORK         GET TO THE NEXT LINE                         
         BCT   R6,VALW02                                                        
                                                                                
VALWX    LA    R2,APOWRKH          START AT FIRST FIELD                         
         ST    R2,FADR                                                          
         B     OKXIT                                                            
                                                                                
VALAER   LA    R2,SCRAMTH                                                       
         B     VALWER                                                           
                                                                                
VALSER   LA    R2,SCRCOMH                                                       
         MVI   FERN,INVALID        PREPARE FOR ERROR                            
                                                                                
VALWER   ST    R2,FADR                                                          
         B     ERXIT                                                            
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
*        CHECK FOR ESTIMATE                                           *         
***********************************************************************         
                                                                                
         USING SCREEND,R2                                                       
CHKEST   NTR1                                                                   
         L     RE,AJOBLOCK                                                      
         USING JBLOCKD,RE                                                       
         L     R1,JBACOLTB                                                      
         USING JBCOLD,R1                                                        
         LH    R0,JBNROWS                                                       
                                                                                
CHKE02   CLI   JBCOLTYP,JBCOLTWC   TEST FOR WORKCODE ENTRY                      
         BNE   CHKE04                                                           
         CLC   JBCOLWC,SCRWRK      MATCH ON WORKCODE                            
         BE    CHKEX                                                            
                                                                                
CHKE04   AH    R1,JBLCOL                                                        
         BCT   R0,CHKE02                                                        
                                                                                
CHKEER   B     ERXIT                                                            
                                                                                
CHKEX    B     OKXIT                                                            
         DROP  R1,R2,RE                                                         
         EJECT                                                                  
***********************************************************************         
*        CHECK FOR APPROVED ESTIMATE                                  *         
***********************************************************************         
                                                                                
APPEST   NTR1                                                                   
         L     RE,AJOBLOCK                                                      
         USING JBLOCKD,RE                                                       
         L     R1,JBACOLTB                                                      
         USING JBCOLD,R1                                                        
         CLI   JBNEWEST,C'Y'       IS THIS A NEW ESTIMATE?                      
         BNE   APPE02              NO, CHECK DIFFERENT FIELD                    
         OC    JBHIAPP,JBHIAPP                                                  
         BNZ   APPEX                                                            
         B     APPEER                                                           
                                                                                
APPE02   CLI   JBOLDAPP,C'Y'       IS THE OLD ESTIMATE APPROVED?                
         BNE   APPEER                                                           
                                                                                
APPEER   B     ERXIT                                                            
                                                                                
APPEX    B     OKXIT                                                            
         DROP  R1,RE                                                            
         EJECT                                                                  
***********************************************************************         
*        ADD A STATUS ELEMENT TO ORDER IN AIOAREA3                    *         
***********************************************************************         
                                                                                
         USING TRSELD,R3                                                        
ADDSTAT  NTR1                                                                   
         LA    R3,ELEMENT                                                       
         XC    TRSEL(TRSLNQ),TRSEL                                              
         MVI   TRSEL,TRSELQ                                                     
         MVI   TRSLN,TRSLNQ                                                     
         GOTOR VDATCON,DMCB,(1,TODAYP),(2,TRSDATE)                              
         GOTOR ADDELEM,AIOAREA3                                                 
                                                                                
ADDSX    B     OKXIT                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*        ADD NEW ORDER TO ACCOUNT FILE                                *         
*        LOCK THE CONTROL RECORD FOR BOTH AUTO AND MANUAL             *         
*        NUMBER ASSIGNMENT TO FORCE ENQUEUING WITH CONCURRENT         *         
*        ORDER RESERVATIONS - UNLOCK CALL FREES THE LOCK              *         
***********************************************************************         
                                                                                
ADDREC   BAS   RE,GETCON           GET ORDER CONTROL RECORD INTO IO2            
         BNE   ADDRER                                                           
                                                                                
         ZIC   RF,APONUMH+5        CHECK FOR AUTO NUMBERING                     
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   APONUM(0),=C'AUTO'                                               
         BE    ADDR02                                                           
                                                                                
         BAS   RE,MANUAL           HANDLE MANUALLY INPUT NUMBER                 
         BNE   ADDRER                                                           
         B     ADDR04                                                           
                                                                                
ADDR02   BAS   RE,AUTO             ADD ORDER WITH AUTO NUMBERING                
                                                                                
         USING ORDRECD,R2                                                       
         L     R2,AIOAREA2         R2=A(ORDER CONTROL RECORD)                   
         LR    R3,R2                                                            
         AHI   R3,ACCORFST                                                      
         DROP  R2                                                               
                                                                                
         USING ONCELD,R3                                                        
         L     R5,AIOAREA3         R5=A(ORDER RECORD)                           
         USING ORDRECD,R5                                                       
         MVC   ONCNUM,ORDKORD      UPDATE CONTROL RECORD W/ORDER NUMBER         
                                                                                
         GOTOR AWRITE,AIOAREA2                                                  
         BNE   ADDRX                                                            
         MVC   APONUM,ORDKORD      AND ALSO TO SCREEN                           
         OI    APONUMH+6,X'80'                                                  
                                                                                
ADDR04   GOTOR AUNLOCK             UNLOCK CONTROL RECORD                        
                                                                                
         USING ORDELD,R6                                                        
         MVI   ELCODE,ORDELQ       GET ORDELQ FROM AIOAREA3                     
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                HAVE TO HAVE IT                              
                                                                                
         USING CHDRECD,R2                                                       
         LA    R2,KEY              CHECK FOR CONTRA HEADER                      
         MVC   CHDKEY,SPACES                                                    
         MVC   CHDKCULA,JOBKEY                                                  
         CLI   TYPE,PRODN                                                       
         BNE   *+10                                                             
         MVC   CHDKWRK,=2C'*'                                                   
         MVC   CHDKCULC,ORDSUP                                                  
         CLI   TYPE,PRODN                                                       
         BE    *+8                                                              
         MVI   CHDKCCPY,C' '       FOR NON-PROD BLANK OUT COMPANY CODE          
         MVC   CHDKSPAC,SPACES                                                  
         GOTOR AREAD,AIOAREA1                                                   
                                                                                
         L     R2,AIOAREA1                                                      
         LR    R3,R2                                                            
         AHI   R3,ACCORFST         GET TO FIRST ELEMENT                         
         TM    DMCB+8,X'10'        DID WE FIND THE RECORD?                      
         BZ    ADDR06              YES                                          
                                                                                
         L     RE,AIOAREA1         NO ADD A NEW ONE                             
         LHI   RF,LIOAREAS         CLEAR THE IOAREA FIRST                       
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
                                                                                
         USING CHDRECD,R2                                                       
         L     R1,AIOAREA1                                                      
         MVC   CHDKEY,KEY          BUILD KEY OF RECORD                          
         XC    CHDKNULL,CHDKNULL                                                
                                                                                
         USING CACELD,R3                                                        
         LA    R3,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   CACEL,CACELQ        BUILD ELEMENT NOW                            
         MVC   CACCNT,CHDKCULC                                                  
         MVC   CACNAME,LSUPNAME                                                 
         LA    R4,CACNAME+L'CACNAME-1                                           
         CLI   0(R4),C' '                                                       
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         LA    R4,1(R4)                                                         
         MVI   0(R4),0             TERMINATOR                                   
         SR    R4,R3                                                            
         STC   R4,CACLN                                                         
         GOTOR ADDELEM,AIOAREA1    ADD THE ELEMENT                              
                                                                                
         GOTOR AADD,AIOAREA1       ADD THE RECORD                               
         BNE   ADDRER                                                           
                                                                                
         USING TRNRECD,R2                                                       
         USING TRNELD,R3                                                        
ADDR06   L     RE,AIOAREA1         CLEAR IO AREA FOR TRANSACTION RECORD         
         LHI   RF,LIOAREAS                                                      
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
                                                                                
         L     R2,AIOAREA1         BUILD KEY FOR SJ RECORD                      
         MVC   TRNKEY(TRNKDATE-TRNKEY),KEY                                      
         MVC   TRNKDATE,ORDDATE                                                 
         MVC   TRNKREF,APONUM                                                   
                                                                                
         LA    R3,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   TRNEL,TRNELQ        BUILD THE ELEMENT NOW                        
         MVI   TRNLN,TRNLN1Q+1                                                  
         MVC   TRNDATE,ORDDATE                                                  
         MVC   TRNREF,APONUM                                                    
         MVI   TRNSTAT,TRNSDR                                                   
         MVI   TRNTYPE,TRNTORD                                                  
         MVC   TRNMOS,THISMON                                                   
         MVC   TRNBREF,SPACES                                                   
         ZAP   TRNAMNT,=P'0'                                                    
         MVC   TRNANAL,=2C'*'                                                   
         MVI   TRNNARR,C' '                                                     
         GOTOR ADDELEM,AIOAREA1    ADD TRNELQ                                   
         DROP  R3                                                               
                                                                                
         USING TRSELD,R3                                                        
         LA    R3,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   TRSEL,TRSELQ                                                     
         MVI   TRSLN,TRSLNQ                                                     
         GOTOR VDATCON,DMCB,(1,TODAYP),(2,TRSDATE)                              
         GOTOR ADDELEM,AIOAREA1    ADD TRSELQ                                   
         DROP  R3                                                               
                                                                                
         LA    R7,NAMTELS          ADD ORDER AMOUNT ELEMENTS                    
         LHI   R0,20               MAX IS 20                                    
                                                                                
         USING OAMELD,R3                                                        
ADDR08   CLI   0(R7),0                                                          
         BE    ADDR10                                                           
         XC    ELEMENT,ELEMENT                                                  
         MVC   ELEMENT(OAMLN2Q),0(R7)                                           
         GOTOR ADDELEM,AIOAREA1    ADD OAMELQS                                  
                                                                                
ADDR10   AHI   R7,OAMLN2Q                                                       
         BCT   R0,ADDR08                                                        
         DROP  R5                                                               
                                                                                
         MVC   KEY,TRNKEY          MOVE FROM AIO1 INTO KEY                      
         GOTOR AREADL,AIOAREA2     AND READ INTO AIO2                           
         CLI   FERN,NOTFOUND                                                    
         BE    ADDR12              IF NOT FOUND, ADD IT                         
         GOTOR AWRITE,AIOAREA1                                                  
         B     ADDR14                                                           
                                                                                
ADDR12   GOTOR AADD,AIOAREA1       ADD SJ RECORD FROM AIO1                      
                                                                                
ADDR14   BNE   ADDRER                                                           
         CLI   ACTION,APR          UNLESS ADDPRINT, SET COMPLETION MSG          
         BE    ADDRX                                                            
         MVC   MSG,=CL60'ORDER ADDED - ENTER NEXT ACTION'                       
         LA    R1,APOACTH                                                       
         ST    R1,FADR                                                          
                                                                                
ADDRX    B     OKEND                                                            
                                                                                
ADDRER   B     ERXIT                                                            
         DROP  R2,R3,R6                                                         
         EJECT                                                                  
***********************************************************************         
*        ADD AN ORDER WITH A MANUALLY INPUT ORDER NUMBER              *         
*        AT ENTRY, AIOAREA3=A(ORDER RECORD)                           *         
*        ON EXIT, CC=EQ IF OK, CC=NEQ IF ERROR, FERN/MSG SET          *         
***********************************************************************         
                                                                                
MANUAL   NTR1                                                                   
         LA    R2,BIGKEY                                                        
         USING OBRRECD,R2                                                       
         XC    OBRKEY,OBRKEY                                                    
         MVI   OBRKTYP,OBRKTYPQ    READ FOR AN OVERLAPPING                      
         MVC   OBRKCPY,COMPANY     RESERVATION                                  
         L     R5,AIOAREA3                                                      
         USING ORDRECD,R5                                                       
         MVC   OBRKLAST,ORDKORD                                                 
         GOTOR ARDHID,0                                                         
         CLC   OBRKEY(OBRKLAST-OBRKEY),KEYSAVE                                  
         BNE   MANU02              NOTHING OUT THERE                            
         CLC   OBRKFRST,ORDKORD    TEST IF RESERVATION BRACKETS NUMBER          
         BH    MANU02              NO                                           
         TM    OBRKSTA,X'80'       TEST FOR DELETED RESERVATION                 
         BO    MANU02                                                           
         MVI   FERN,ORDRES                                                      
         B     MANUER                                                           
                                                                                
         USING ORDELD,R6                                                        
MANU02   MVI   ELCODE,ORDELQ       GET ORDELQ FROM AIOAREA3                     
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                HAVE TO HAVE IT                              
                                                                                
         OC    ORDSTAT,VSTAT       SET STATUS IN CASE LARGE AMOUNT              
                                                                                
         CLI   TYPE,EXPENSE        WE DON'T NEED THIS FOR EXPENSE               
         BE    MANU04                                                           
                                                                                
         CP    COMCNT,=P'4'        MARK IF NOT AVAILABLE TO TY1 & 3             
         BH    *+14                                                             
         CP    NONCNT,=P'4'                                                     
         BNH   *+8                                                              
         OI    ORDSTAT,ORDSTY10                                                 
         DROP  R6                                                               
                                                                                
MANU04   MVC   KEY,ORDKEY          READ FOR RECORD BEFORE ADDING                
         GOTOR AREADL,AIOAREA4                                                  
         CLI   DMCB+8,0                                                         
         BNE   MANU10              NOT THERE, GO ADD IT                         
                                                                                
         L     R5,AIOAREA4                                                      
         CLI   ORDRSTA,X'00'       ONLY DELETED ORDERS CAN HAVE                 
         BE    MANU06              THEIR NUMBER REUSED                          
         CLI   ORDRSTA,X'40'                                                    
         BNE   MANU08                                                           
                                                                                
MANU06   MVI   FERN,ORDUSED                                                     
         B     MANUER                                                           
                                                                                
MANU08   GOTOR AWRITE,AIOAREA3                                                  
         BE    MANUX                                                            
         MVI   FERN,NOADD                                                       
         B     MANUER                                                           
                                                                                
MANU10   GOTOR AADD,AIOAREA3                                                    
         CLI   DMCB+8,0                                                         
         BE    MANUX                                                            
         DC    H'0'                                                             
                                                                                
MANUX    B     OKXIT                                                            
                                                                                
MANUER   B     ERXIT                                                            
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
*        GET THE ORDER CONTROL RECORD                                 *         
*        ADD ONE IF IT DOESN'T EXIST                                  *         
*        ON EXIT, AIOAREA2=A(CONTROL RECORD), NEXTNUM, NEXTBIN SET    *         
***********************************************************************         
                                                                                
GETCON   NTR1                                                                   
         LA    R2,KEY              READ ORDER NUMBER CONTROL RECORD             
         USING ORDRECD,R2                                                       
         XC    ORDKEY,ORDKEY                                                    
         MVI   ORDKTYP,ORDKTYPQ                                                 
         MVC   ORDKCPY,COMPANY                                                  
         MVC   ORDKORD,=6C'0'                                                   
         GOTOR AREADL,AIOAREA2                                                  
         TM    DMCB+8,X'EF'                                                     
         BNZ   GETCER                                                           
                                                                                
         L     R2,AIOAREA2         R2=A(CONTROL RECORD)                         
         LR    R3,R2                                                            
         AHI   R3,ACCORFST         GET TO FIRST ELEMENT                         
                                                                                
         CLI   DMCB+8,0            DID WE FIND A CONTROL RECORD?                
         BE    GETC02              YES                                          
                                                                                
         L     RE,AIOAREA2         NO, CLEAR AIO2 AND BUILD IT                  
         LHI   RF,LIOAREAS                                                      
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
                                                                                
         USING ONCELD,R3                                                        
         MVC   ORDKEY,KEY                                                       
         MVI   ONCEL,ONCELQ                                                     
         MVI   ONCLN,ONCLNQ                                                     
         MVC   ONCNUM,=6C'0'                                                    
                                                                                
         LR    RF,R2                                                            
         SR    RE,RE                                                            
         IC    RE,ONCLN                                                         
         AR    RF,RE                                                            
         MVI   0(RF),X'00'                                                      
         AHI   RF,1                                                             
         STH   RF,ORDRLEN                                                       
                                                                                
         GOTOR AADD,AIOAREA2       ADD CONTROL RECORD                           
         GOTOR AREADL,AIOAREA2     RE-READ FOR UPDATE                           
         TM    DMCB+8,X'EF'                                                     
         BNZ   GETCER                                                           
                                                                                
GETC02   MVC   NEXTNUM,ONCNUM                                                   
         PACK  DUB,NEXTNUM         BUMP LAST ORDER NUMBER                       
         CVB   R0,DUB                                                           
         AH    R0,=H'1'                                                         
         ST    R0,NEXTBIN          SAVE BINARY                                  
         EDIT  (R0),(6,NEXTNUM),FILL=0                                          
                                                                                
GETCX    B     OKXIT                                                            
                                                                                
GETCER   B     ERXIT                                                            
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
*        GET NEXT ORDER NUMBER AND ADD ORDER RECORD                   *         
*        ON ENTRY, AIOAREA3=A(ORDER RECORD), NEXTNUM=NEXT NUMBER      *         
***********************************************************************         
                                                                                
AUTO     NTR1                                                                   
         USING ORDELD,R6                                                        
         MVI   ELCODE,ORDELQ       GET ORDELQ FROM AIOAREA3                     
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                HAVE TO HAVE IT                              
                                                                                
         OC    ORDSTAT,VSTAT       SET STATUS IN CASE LARGE AMOUNT              
                                                                                
         CLI   TYPE,EXPENSE        DON'T NEED THIS FOR EXPENSE                  
         BE    AUTO02                                                           
                                                                                
         CP    COMCNT,=P'4'        MARK IF NOT AVAILABLE TO TY1 & 3             
         BH    *+14                                                             
         CP    NONCNT,=P'4'                                                     
         BNH   *+8                                                              
         OI    ORDSTAT,ORDSTY10                                                 
         DROP  R6                                                               
                                                                                
AUTO02   LA    R2,BIGKEY           INITIALIZE BIGKEY WITH ORDER RES KEY         
         USING OBRRECD,R2                                                       
                                                                                
AUTO04   XC    OBRKEY,OBRKEY       BUILD ORDER RESERVATION KEY                  
         MVI   OBRKTYP,OBRKTYPQ                                                 
         MVC   OBRKCPY,COMPANY                                                  
         MVC   OBRKLAST,NEXTNUM    SET NEXT NUMBER IN KEY                       
                                                                                
         GOTOR ARDHID,0                                                         
         CLC   OBRKEY(OBRKLAST-OBRKEY),KEYSAVE                                  
         BNE   AUTO06                                                           
         CLC   OBRKFRST,NEXTNUM                                                 
         BH    AUTO06                                                           
         TM    OBRKSTA,X'80'       TEST DELETED RESERVATION                     
         BO    AUTO06                                                           
                                                                                
         PACK  DUB,OBRKLAST        GET END OF RESERVATION                       
         CVB   R0,DUB                                                           
         AH    R0,=H'1'            ADD ONE TO NUMBER                            
         ST    R0,NEXTBIN                                                       
         EDIT  (R0),(6,NEXTNUM),FILL=0                                          
         B     AUTO04              TRY AGAIN                                    
                                                                                
AUTO06   L     R5,AIOAREA3         GET ORDER RECORD                             
         USING ORDRECD,R5                                                       
         MVC   ORDKORD,NEXTNUM     MOVE ORDER NUMBER INTO KEY                   
                                                                                
         GOTOR VDATAMGR,DMCB,(X'08',DMRDHI),ACCDIR,AIOAREA3,DIRIO               
         CLC   DIRIO(L'ACCKEY),0(R5)                                            
         BE    AUTO08              IF ORDER EXISTS, GET NEXT NUMBER             
                                                                                
         GOTOR AADD,AIOAREA3       ADD NEW ORDER                                
         CLI   DMCB+8,0                                                         
         BE    AUTOX               IF ERROR, GET NEXT NUMBER                    
                                                                                
AUTO08   L     R0,NEXTBIN          INCREMENT NUMBER                             
         AH    R0,=H'1'                                                         
         ST    R0,NEXTBIN                                                       
         EDIT  (R0),(6,NEXTNUM),FILL=0                                          
         B     AUTO04              AND TRY AGAIN                                
                                                                                
AUTOX    B     OKXIT                                                            
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
*        WRITE BACK CHANGED/EDITED ORDER TO ACCOUNT FILE              *         
***********************************************************************         
                                                                                
CHAREC   L     RE,AIOAREA3         NEW ORDER RECORD                             
         LH    RF,ACCORLEN(RE)                                                  
         L     R2,AIOAREA1         OLD OREDER RECORD                            
         LH    R3,ACCORLEN(R2)                                                  
         CR    RF,R3               IF LENGTHS OR DATA DIFFERENT,                
         BNE   CHAR02              UPDATE THE RECORD                            
         CLCL  RE,R2                                                            
         BNE   CHAR02                                                           
         MVI   FERN,NOCHANGE                                                    
         B     EXIT                                                             
                                                                                
CHAR02   CLI   ACTION,CH2                                                       
         BE    CHAR04                                                           
         CLI   ACTION,EDIT                                                      
         BE    CHAR04                                                           
                                                                                
         USING ORDELD,R6                                                        
         MVI   ELCODE,ORDELQ       GET ORDELQ FROM AIOAREA3                     
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                HAVE TO HAVE IT                              
                                                                                
         ZIC   R1,ORDAMNO          UPDATE AMEND NO/DATE AND DISPLAY             
         LA    R1,1(R1)                                                         
         STC   R1,ORDAMNO                                                       
                                                                                
         MVC   ORDAMDT,TODAYP                                                   
         NI    ORDSTAT,X'FF'-ORDSTY10                                           
                                                                                
         MVC   APODATS,SPACES                                                   
         OI    APODATSH+6,X'80'                                                 
         LA    R2,APODATS                                                       
         MVC   0(14,R2),=C'LAST AMENDMENT'                                      
                                                                                
         EDIT  ORDAMNO,(3,16(R2)),ALIGN=LEFT                                    
         AR    R2,R0                                                            
         MVI   16(R2),C','                                                      
                                                                                
         GOTOR VDATCON,DMCB,(1,ORDAMDT),(8,17(R2))                              
                                                                                
         OC    ORDSTAT,VSTAT       SET STATUS IN CASE LARGE AMOUNT              
                                                                                
         CLI   TYPE,EXPENSE        WE DON'T NEED THIS FOR EXPENSE               
         BE    CHAR04                                                           
         CP    COMCNT,=P'4'        MARK IF NOT AVAILABLE TO TY1 & 3             
         BH    *+14                                                             
         CP    NONCNT,=P'4'                                                     
         BNH   *+8                                                              
         OI    ORDSTAT,ORDSTY10                                                 
                                                                                
CHAR04   L     R3,AIOAREA3                                                      
         AH    R3,DATADISP                                                      
                                                                                
CHAR06   CLI   0(R3),0             END OF RECORD?                               
         BE    CHAR10              YES, ADD ELEMENT                             
         CLI   0(R3),GDAELQ        DATE ELEMENT?                                
         BE    CHAR12              YES                                          
                                                                                
CHAR08   SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     CHAR06                                                           
                                                                                
         USING GDAELD,R3                                                        
CHAR10   LA    R3,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   GDAEL,GDAELQ                                                     
         MVI   GDALN,GDALNQ                                                     
         MVI   GDATYPE,GDATODAT                                                 
         MVC   GDADATE,TODAYP                                                   
         GOTOR ADDELEM,AIOAREA3                                                 
         B     CHAR14                                                           
                                                                                
CHAR12   CLI   GDATYPE,GDATODAT    RIGHT TYPE?                                  
         BNE   CHAR08              NO, KEEP LOOKING                             
         MVC   GDADATE,TODAYP      YES, UPDATE THE DATE                         
         DROP  R3                                                               
                                                                                
CHAR14   GOTOR AWRITE,AIOAREA3     WRITE BACK                                   
         BNE   EXIT                                                             
                                                                                
         CLI   ACTION,CH2                                                       
         BE    CHAR20                                                           
         CLI   ACTION,EDIT                                                      
         BE    CHAR20                                                           
                                                                                
         LA    RE,OAMTELS          UPDATE JOB/EXP TRNS IF AFFECTED              
         LA    R0,NAMTELS                                                       
         LHI   R1,L'OAMTELS                                                     
         LR    RF,R1                                                            
         CLCL  RE,R0                                                            
         BE    CHAR20                                                           
                                                                                
         USING TRNRECD,R2                                                       
         LA    R2,KEY                                                           
         XC    TRNKEY,TRNKEY       BUILD KEY FOR JOB/EXP TRANSACTION            
         MVC   TRNKCULA,ORDJOB                                                  
         MVC   TRNKWORK,SPACES                                                  
         CLI   TYPE,PRODN                                                       
         BNE   *+10                                                             
         MVC   TRNKWORK,=2C'*'                                                  
         MVC   TRNKCULC,ORDSUP                                                  
         CLI   TYPE,PRODN                                                       
         BE    *+8                                                              
         MVI   TRNKCCPY,C' '                                                    
         MVC   TRNKDATE,ORDDATE                                                 
         MVC   TRNKREF,APONUM                                                   
         GOTOR AREADL,AIOAREA1     READ IT                                      
         BNE   EXIT                                                             
                                                                                
         MVI   ELCODE,OAMELQ       REMOVE OLD ELEMENTS                          
         GOTOR REMELEM,AIOAREA1                                                 
                                                                                
         LA    R7,NAMTELS          ADD NEW ORDER AMOUNT ELEMENTS                
         LHI   R0,20               MAX IS 20                                    
         LA    R3,ELEMENT                                                       
                                                                                
         USING OAMELD,R3                                                        
CHAR16   CLI   0(R7),0                                                          
         BE    CHAR18                                                           
         XC    ELEMENT,ELEMENT                                                  
         MVC   ELEMENT(OAMLN2Q),0(R7)                                           
         GOTOR ADDELEM,AIOAREA1                                                 
                                                                                
CHAR18   AHI   R7,OAMLN2Q                                                       
         BCT   R0,CHAR16                                                        
                                                                                
         GOTOR AWRITE,AIOAREA1     WRITE BACK                                   
         BNE   EXIT                                                             
                                                                                
CHAR20   MVC   MSG,=CL60'ACTION COMPLETED - ENTER NEXT'                         
         LA    R1,APOACTH                                                       
         ST    R1,FADR                                                          
         B     OKEND                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
*        GET T0 THE NEXT LINE                                         *         
***********************************************************************         
                                                                                
NEXTWORK LHI   R1,4                NUMBER OF TIMES TO BUMP                      
                                                                                
BUMPNXT  ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         BCT   R1,BUMPNXT                                                       
         BR    RE                                                               
                                                                                
BUMP     ZIC   RF,0(R2)            BUMP TO NEXT HEADER                          
         AR    R2,RF                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
*        ROUTINES TO GET AN ELEMENT, ADD OR REMOVE AN ELEMENT         *         
*        0(R1) HAS IO ADDRESS FOR ADDELEM AND REMELEM                 *         
***********************************************************************         
                                                                                
GETELIO  L     R6,AIOAREA3                                                      
         GETEL (R6),DATADISP,ELCODE                                             
                                                                                
ADDELEM  NTR1                                                                   
         CLI   ELEMENT,0                                                        
         BE    OKXIT                                                            
         L     R5,0(R1)                                                         
         GOTOR VHELLO,DMCB,(C'P',ACCFIL),(R5),ELEMENT,=C'ADD=CODE'              
         CLI   DMCB+12,0                                                        
         BE    OKXIT                                                            
         MVI   FERN,TOOLONG        DID RECORD GET TOO LONG                      
         CLI   DMCB+12,5                                                        
         BE    ERXIT                                                            
         DC    H'0'                OTHER ERRORS UNACCEPTABLE                    
                                                                                
REMELEM  NTR1                                                                   
         L     R5,0(R1)                                                         
         XC    ELEMENT,ELEMENT                                                  
         BAS   RE,GETELIO                                                       
         BNE   OKXIT                                                            
         ZIC   R1,1(R6)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELEMENT(0),0(R6)                                                 
         GOTOR VHELLO,DMCB,(C'D',ACCFIL),(ELCODE,(R5)),0                        
         B     OKXIT                                                            
         EJECT                                                                  
***********************************************************************         
*        EXITS TO ROOT                                                *         
***********************************************************************         
                                                                                
OKEND    MVI   FERN,OK             OK                                           
         XIT1                                                                   
                                                                                
OKXIT    CR    RB,RB               CC = EQU                                     
         B     EXIT                                                             
                                                                                
ERXIT    LTR   RB,RB               CC = NEQ                                     
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*        LITERAL DECLARATIONS                                         *         
***********************************************************************         
         LTORG                                                                  
                                                                                
DMRDHI   DC    CL8'DMRDHI'                                                      
ACCDIR   DC    CL8'ACCDIR'                                                      
ACCFIL   DC    CL8'ACCFIL'                                                      
                                                                                
HIAMT    DC    PL6'99999999'                                                    
LOAMT    DC    PL6'-99999999'                                                   
         EJECT                                                                  
* ACPOPDSECT                                                                    
       ++INCLUDE ACPOPDSECB                                                     
         EJECT                                                                  
***********************************************************************         
*        OVERLAY WORKING STORAGE AT (ASAVE)                           *         
***********************************************************************         
                                                                                
LOCALD   DSECT                                                                  
VALUE    DS    CL6                 PACKED VALUE                                 
COMMSW   DS    CL1                 COMMISSIONABLE SWITCH (SEE NONCOMM)          
STATUS   DS    CL1                 STATUS                                       
INVOICED DS    CL1                 Y OR N                                       
DIRIO    DS    XL60                DIRECTORY IO                                 
                                                                                
NEXTNUM  DS    CL6                 NEXT ORDER NUMBER (CHARACTER)                
NEXTBIN  DS    F                   NEXT ORDER NUMBER (BINARY)                   
                                                                                
ELEMENT  DS    CL256               NEW ELEMENT WORK AREA                        
OAMTELS  DS    CL(20*OAMLN2Q)      OLD ORDER AMOUNT ELEMENTS                    
NAMTELS  DS    CL(20*OAMLN2Q)      NEW ORDER AMOUNT ELEMENTS                    
                                                                                
NONCOMM  EQU   X'80'               NON-COMMISSIONABLE (ACOASTAT)                
MINOR    EQU   X'40'               MINOR (ACOASTAT)                             
                                                                                
COMCNT   DS    X                   # COMMISSIONABLE WORKCODES                   
NONCNT   DS    X                   # NON-COMMISSIONABLE WORKCODES               
                                                                                
VAUTH    DS    CL(L'ORDAUTH)       AUTHORIZED BY                                
VDATE    DS    CL(L'ORDDATE)       DATE                                         
VDDATE   DS    CL(L'ORDDDTE)       DUE DATE                                     
VATTN    DS    CL(L'ORDATTN)       ATTENTION                                    
VTAX     DS    CL(L'ORDTAX)        TAX                                          
VSTAT    DS    XL(L'ORDSTAT)       STATUS                                       
VAUTHL   EQU   *-VAUTH             LENGTH FOR CLEAR                             
         EJECT                                                                  
SCREEND  DSECT                                                                  
SCRWRKH  DS    CL8                                                              
SCRWRK   DS    CL2                                                              
SCRAMTH  DS    CL8                                                              
SCRAMT   DS    CL11                                                             
SCRCOMH  DS    CL8                                                              
SCRCOM   DS    C                                                                
SCRDESH  DS    CL8                                                              
SCRDES   DS    CL15                                                             
                                                                                
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003ACPOP01B  05/18/06'                                      
         END                                                                    
