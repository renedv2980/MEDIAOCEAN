*          DATA SET ACORD01    AT LEVEL 059 AS OF 10/17/18                      
*PHASE T60F01A                                                                  
*INCLUDE TEXTER                                                                 
         TITLE 'ACORD01 - PRODUCTION ORDERS - ADD/CHANGE/EDIT'                  
*SGAV 059 12SEP18 <SPEC-12211> BULK API EXTRACT CHANGES                         
ACORD01  CSECT                                                                  
         PRINT NOGEN               ON ENTRY TO OVERLAY IF ACTION IS             
         NMOD1 0,**ORD1**          CHANGE OR EDIT, ORDER RECORD IS              
         LA    RA,2048(RB)         LOCKED IN IOAREA1                            
         LA    RA,2048(RA)                                                      
         USING ACORD01+4096,RA                                                  
         USING ORDWORKD,R9                                                      
         USING TWAD,R8                                                          
         L     RC,ASAVE                                                         
         USING LOCALD,RC                                                        
         B     INITIAL                                                          
         EJECT                                                                  
*---------------------------------------------------------                      
*        BUILD BASIC ORDER RECORD IN IOAREA3                                    
*---------------------------------------------------------                      
*                                                                               
INITIAL  MVI   INVOICED,C'N'       PRESET TO NOT INVOICED (W/C CHA OK)          
         LA    R1,APONUMH          OR SPACES INTO ALL SCREEN FIELDS             
         SR    R2,R2                                                            
         LA    R3,APOTABH                                                       
                                                                                
INIT2    IC    R2,0(R1)                                                         
         SH    R2,=H'9'                                                         
         EX    R2,*+8                                                           
         B     *+10                                                             
         OC    8(0,R1),SPACES                                                   
         AH    R2,=H'9'                                                         
         BXLE  R1,R2,INIT2                                                      
                                                                                
         L     R5,AIOAREA3                                                      
         TM    ACTINDS,NEW         IF NEW SET UP BASIC REC WITH X'67'           
         BZ    INIT10              ELEMENT                                      
         USING ORDRECD,R5                                                       
         XC    ORDKEY(256),ORDKEY                                               
         MVI   ORDKTYP,ORDKTYPQ                                                 
         MVC   ORDKCPY,COMPANY                                                  
         MVC   ORDKORD,APONUM                                                   
         LA    R5,ACCORFST(R5)                                                  
         ST    R5,SAVE5                                                         
                                                                                
         USING ORDELD,R5                                                        
         MVI   ORDEL,ORDELQ                                                     
         MVI   ORDLN,ORDLN3Q                                                    
         MVC   ORDJOB,JOBKEY                                                    
         MVI   ORDSTAT2,ORDSAPPR                                                
         MVC   ORDSUPC,COMPANY                                                  
         MVC   ORDSUPU(L'ORDSUPU+L'ORDSUPL+L'ORDSUPA),APOSUP+1                  
         CLI   APOSUP,C'*'                                                      
         BE    *+16                                                             
         MVC   ORDSUPU(L'ORDSUPU+L'ORDSUPL),SUPPUL                              
         MVC   ORDSUPA,APOSUP                                                   
         MVC   ORDDATE,TODAYP                                                   
         XC    LWCC1(16),LWCC1                                                  
         B     VALAUTH                                                          
                                                                                
         USING ORDRECD,RE                                                       
INIT10   L     RE,AIOAREA1         IF CHANGE OR EDIT, MOVE REC TO               
         SR    RF,RF                                                            
         ICM   RF,3,ORDRLEN-ORDRECD(RE)                                         
         LR    R0,R5               IOAREA3 WITHOUT COMMENT ELS                  
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         GOTOR VHELLO,DMCB,(C'D',=C'ACCFIL'),(X'3E',(R5))                       
         AH    R5,DATADISP                                                      
         SR    R0,R0                                                            
         CLI   0(R5),ORDELQ                                                     
         BE    *+14                                                             
         IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     *-14                                                             
         LR    R6,R5                                                            
                                                                                
INIT12   IC    R0,1(R6)            SAVE OLD AMOUNT ELEMENTS IN OAMTELS          
         AR    R6,R0               AND NAMTELS                                  
         CLI   0(R6),0                                                          
         BE    INIT15                                                           
         CLI   0(R6),OAMELQ                                                     
         BNE   INIT12                                                           
                                                                                
         USING OAMELD,R6                                                        
         LA    R1,NAMTELSC         FIND POSITION 1 TO 8 IN NAMTELS              
         TM    OAMSTAT,NONCOMM     (MAJ/COM,MIN/COM,MAJ/N-C,MIN/N-C)            
         BZ    *+8                                                              
         LA    R1,NAMTELSN                                                      
         TM    OAMSTAT,MINOR                                                    
         BZ    INIT14                                                           
         LA    R1,OAMLN2Q(R1)                                                   
         OC    0(OAMLN2Q,R1),0(R1)                                              
         BZ    INIT14                                                           
         B     *-14                                                             
                                                                                
INIT14   MVC   0(OAMLN2Q,R1),OAMEL                                              
         CP    OAMINUM,=P'0'       SET INVOICED MARKER IF ANY                   
         BE    *+8                                                              
         MVI   INVOICED,C'Y'                                                    
         B     INIT12                                                           
                                                                                
INIT15   LA    R0,OAMTELS                                                       
         LA    RE,NAMTELS                                                       
         LA    R1,L'OAMTELS                                                     
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         ST    R5,SAVE5                                                         
         GOTOR VHELLO,DMCB,(C'D',=C'ACCFIL'),(X'68',AIOAREA3)                   
         CLI   ACTION,EDIT                                                      
         BNE   VALCLI                                                           
         LA    R0,10+XTRASCAN                                                   
         GOTOR =V(TEXTER),DMCB,APOACTH,(10,APOFRSTH),TEXTWS,VSCANNER,  X        
               RR=RB                                                            
         CLI   DMCB,0                                                           
         BE    VALNAR                                                           
         MVC   FERN,DMCB                                                        
         MVC   FNDX,DMCB+4                                                      
         MVC   FADR,DMCB                                                        
         CLI   FERN,SPECIAL        IF FERN IS X'FE' OR X'FF' P3=A(MSG)          
         BL    EXIT                                                             
         L     R1,DMCB+8                                                        
         MVC   MSG,0(R1)                                                        
         B     EXIT                                                             
         EJECT                                                                  
*---------------------------------------------------------                      
*        CHECK FOR NO CHANGE TO CLI/PRO/JOB/SUP/EXP ON ACTION CHANGE            
*---------------------------------------------------------                      
*                                                                               
VALCLI   CLI   TYPE,EXPENSE                                                     
         BE    VALCLI2                                                          
         LA    R1,APOCLIH                                                       
         CLC   LCLI,8(R1)                                                       
         BNE   VALCLERR                                                         
         LA    R1,APOPROH                                                       
         CLC   LPRO,8(R1)                                                       
         BNE   VALCLERR                                                         
         LA    R1,APOJOBH                                                       
         CLC   LJOB,8(R1)                                                       
         BNE   VALCLERR                                                         
         B     VALCLI4                                                          
                                                                                
VALCLI2  LA    R1,APOEXPH                                                       
         CLC   LEXP,8(R1)                                                       
         BNE   VALCLERR                                                         
                                                                                
VALCLI4  LA    R1,APOSUPH                                                       
         CLC   LSUPP,8(R1)                                                      
         BE    VALAUTH                                                          
                                                                                
VALCLERR ST    R1,FADR                                                          
         MVI   FERN,CANTAMND                                                    
         B     EXIT                                                             
         EJECT                                                                  
*---------------------------------------------------------                      
*        CHECK AUTHORISED BY                                                    
*---------------------------------------------------------                      
*                                                                               
VALAUTH  GOTOR AFVAL,APOAUTH                                                    
         BZ    EXIT                                                             
         MVC   ORDAUTH,FLD                                                      
         GOTOR AFVAL,APODATH       ORDER DATE - OPTIONAL                        
         BZ    VALA10                                                           
         GOTOR VDATVAL,DMCB,(0,FLD),FLD+8                                       
         OC    0(4,R1),0(R1)                                                    
         BNZ   VALA04                                                           
                                                                                
VALA02   MVI   FERN,INVALID        S.B. MMMDD/YY                                
         B     EXIT                                                             
                                                                                
VALA04   CLC   FLD+8(2),=C'90'     DON'T ALLOW BEFORE 1990                      
         BNH   VALA02                                                           
         GOTOR VDATCON,DMCB,(0,FLD+8),(1,WORK)                                  
         CLI   ACTION,CHA                                                       
         BNE   VALA06                                                           
         CLC   ORDDATE,WORK                                                     
         BE    VALA08                                                           
         MVI   FERN,CANTAMND       NO CHANGE TO ORD DATE ALLOWED                
         B     EXIT                                                             
                                                                                
VALA06   CLC   WORK(L'TODAYP),TODAYP                                            
         BNL   VALA08                                                           
         L     RF,AGOBLOCK                                                      
         USING GOBLOCKD,RF                                                      
         CLI   GOORDER,C'Y'        ALLOW BACKDATING?                            
         BNE   VALA02              NO                                           
         DROP  RF                                                               
                                                                                
VALA08   MVC   ORDDATE,WORK                                                     
                                                                                
VALA10   GOTOR AFVAL,APODDTEH        DUE DATE..REQUIRED                         
         BZ    EXIT                                                             
         GOTOR VDATVAL,DMCB,(0,FLD),FLD+8                                       
         OC    0(4,R1),0(R1)                                                    
         BNZ   *+12                                                             
         MVI   FERN,INVALID        S.B. MMMDD/YY                                
         B     EXIT                                                             
         GOTOR VDATCON,DMCB,(0,FLD+8),(1,ORDDDTE)                               
         CLI   APOATTNH+5,0                                                     
         BE    VALNAR              ATTN NAME..OPTIONAL                          
         GOTOR AFVAL,APOATTNH                                                   
         MVC   ORDATTN,FLD                                                      
         MVI   ORDLN,ORDLN3Q       IF THERE THEN LONG EL.                       
         B     VALNAR                                                           
         EJECT                                                                  
*---------------------------------------------------------                      
*        VALIDATE NARRATIVE AND BUILD NEW COMMENT ELEMENTS IN NCOMELS           
*---------------------------------------------------------                      
*                                                                               
VALNAR   DS    0H                                                               
         LA    R3,APOFRSTH         R3/4/5 = BXLE REGISTERS                      
         SR    R4,R4                                                            
         LA    R5,APOFOOTH                                                      
         SR    R2,R2               R2 = COMMENT SEQUENCE NUMBER ACOMSEQ         
         LA    R6,NCOMELS          R6 = NEW COMMENT ELEMENT POINTER             
         USING SCMELD,R6                                                        
                                                                                
VALNAR2  GOTOR AFVAL,(R3)          LOOP FOR A LINE                              
         BZ    VALNAR30                                                         
         ZIC   RF,0(R3)            DROP TRAILING SPACES FROM FLDILEN            
         SH    RF,=H'8'                                                         
         LA    RE,7(R3,RF)                                                      
         CLI   0(RE),C' '                                                       
         BH    *+12                                                             
         BCT   RF,*-12                                                          
         B     VALNAR30            SPACES ONLY                                  
         STC   RF,5(R3)                                                         
         CLC   FLD(2),=C'N='                                                    
         BNE   VALNAR10                                                         
                                                                                
VALNAR3  XC    KEY,KEY             HANDLE N= STRING                             
         MVI   KEY,X'0C'                                                        
         MVC   KEY+1(1),COMPANY                                                 
         GOTOR VSCANNER,DMCB,(R3),(8,TEMP)                                      
         ZIC   R0,DMCB+4                                                        
         CH    R0,=H'1'                                                         
         MVI   FERN,INVALID                                                     
         BL    EXIT                                                             
         BE    *+8                                                              
         MVI   FNDX,1                                                           
         LA    R7,TEMP                                                          
                                                                                
VALNAR5  CLI   0(R7),1             LOOP FOR AN ENTRY                            
         BNE   VALNAR6             CHECK FOR N=123456                           
         CLI   12(R7),C'N'                                                      
         BNE   VALNAR6                                                          
         CLI   1(R7),1                                                          
         BL    VALNAR6                                                          
         CLI   1(R7),6                                                          
         BNH   VALNAR7                                                          
                                                                                
VALNAR6  MVI   FERN,INVALID                                                     
         MVC   XTRAMESS(10),=C'(N=123456)'                                      
         B     EXIT                                                             
                                                                                
VALNAR7  MVC   VALUE,SPACES        RIGHT-ALIGN NUMBER IN VALUE                  
         LA    RF,VALUE+5                                                       
         ZIC   RE,1(R7)                                                         
         BCTR  RE,0                                                             
         SR    RF,RE                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),22(R7)                                                   
         MVC   KEY+2(6),VALUE                                                   
         GOTOR AREAD,AIOAREA2      CHECK COMMENT EXISTS                         
         BNE   EXIT                                                             
         MVI   SCMEL,SCMELQ        BUILD ELEMENT                                
         MVI   SCMLN,SCMLN2Q                                                    
         STC   R2,SCMSEQ                                                        
         MVI   SCMTYPE,SCMTPRAD    PRINT AFTER DATA                             
         LA    RF,APOFOOTH                                                      
         CR    RF,R3                                                            
         BNE   *+8                                                              
         OI    SCMTYPE,SCMTPOFT                                                 
         MVC   SCMCODE,VALUE                                                    
         LA    R6,10(R6)           BUMP ELEMENT POINTER                         
         LA    R2,1(R2)            BUMP SEQUENCE NO                             
         CLI   FNDX,0                                                           
         BE    VALNAR30                                                         
         ZIC   R1,FNDX             BUMP TO NEXT SCANNER ENTRY                   
         LA    R1,1(R1)                                                         
         STC   R1,FNDX                                                          
         LA    R7,32(R7)                                                        
         BCT   R0,VALNAR5                                                       
         B     VALNAR30                                                         
                                                                                
VALNAR10 CLC   FLD(2),=C'S='       HANDLE S=N (SPACE 1-9 LINES)                 
         BNE   VALNAR20                                                         
         MVI   FERN,TOOLONG        CHECK ITS NUMERIC 01-09 (1-9)                
         CLI   5(R3),4                                                          
         BH    EXIT                                                             
         ZIC   R1,5(R3)                                                         
         SH    R1,=H'3'                                                         
         MVI   FERN,NOINPUT                                                     
         BM    VALNARER                                                         
         MVC   WORK(2),=2C'0'                                                   
         EX    R1,VALNARMV                                                      
         CLC   WORK(2),=2C'0'                                                   
         MVI   FERN,INVNUM                                                      
         BNE   VALNARER                                                         
         EX    R1,VALNARPK                                                      
         CVB   R1,DUB                                                           
         MVI   FERN,NOINPUT                                                     
         LTR   R1,R1                                                            
         BZ    VALNARER                                                         
         MVI   FERN,TOOBIG                                                      
         CH    R1,=H'9'                                                         
         BNH   VALNAR20            THEN TREAT IT AS A STANDARD COMMENT          
                                                                                
VALNARER MVC   XTRAMESS(7),=C'(S=1-9)'                                          
         B     EXIT                ERROR                                        
                                                                                
VALNARMV MVZ   WORK(0),FLD+2                                                    
                                                                                
VALNARPK PACK  DUB,FLD+2(0)                                                     
                                                                                
VALNAR20 MVI   SCMEL,SCMELQ        STANDARD COMMENT - BUILD AN ELEMENT          
         STC   R2,SCMSEQ                                                        
         LA    RF,APOFOOTH                                                      
         CR    RF,R3                                                            
         BNE   *+8                                                              
         OI    SCMTYPE,SCMTPOFT                                                 
         ZIC   RE,5(R3)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   SCMNARR(0),FLD                                                   
         LA    RE,5(RE)                                                         
         STC   RE,SCMLN                                                         
         AR    R6,RE               BUMP ELEMENT POINTER                         
         LA    R2,1(R2)            BUMP SEQUENCE NO                             
                                                                                
VALNAR30 IC    R4,0(R3)            BUMP TO NEXT UNPROT (NEXT LINE)              
         BXLE  R3,R4,*+8                                                        
         B     VALNARX                                                          
         TM    1(R3),X'20'                                                      
         BO    VALNAR30                                                         
         B     VALNAR2                                                          
                                                                                
VALNARX  MVI   FNDX,0              AT END CHECK FOR ANY INPUT                   
         CLI   ACTION,EDIT                                                      
         BE    VALNARX1                                                         
         B     VALTAX                                                           
                                                                                
VALNARX1 MVC   MSG,EDMESS          IF EDIT JUST ASK USER TO SIGHT CHK           
         LA    R1,APOTABH                                                       
         ST    R1,FADR                                                          
         XC    APOACT,APOACT                                                    
         OI    APOACTH+6,X'81'     MODIFY                                       
         MVC   APOACT(6),=C'CHANGE'                                             
         B     OKEND                                                            
                                                                                
EDMESS   DC    CL60'EDITED NARRATIVE WILL BE AS SHOWN - HIT ENTER IF ASX        
                REQUIRED'                                                       
         DROP  R6                                                               
         EJECT                                                                  
*---------------------------------------------------------                      
*        VALIDATE TAX QUESTION                                                  
*---------------------------------------------------------                      
*                                                                               
VALTAX   LA    R1,APOTAXH                                                       
         ST    R1,FADR                                                          
         CLI   5(R1),0             DID THEY ANSWER TAX QUESTION ?               
         BE    VALTAX2             NO, SEE IF ANSWER NEEDED                     
         MVI   FERN,INVALID                                                     
         CLI   8(R1),C'Y'                                                       
         BE    VALTAX3                                                          
         CLI   8(R1),C'N'                                                       
         BNE   ERRXIT                                                           
         B     VALTAX3                                                          
                                                                                
VALTAX2  MVI   FERN,NOINPUT                                                     
         CLI   PROGPROF+9,C'Y'     CHECK IF REQUIRED                            
         BE    ERRXIT                                                           
         CLI   PROGPROF+9,C'P'                                                  
         BE    ERRXIT                                                           
                                                                                
VALTAX3  L     R5,SAVE5                                                         
         MVC   ORDTAX,APOTAX                                                    
         MVI   ORDLN,ORDLN3Q       IF THERE THEN LONG EL.                       
                                                                                
VALTAXX  B     VALWC                                                            
         EJECT                                                                  
*---------------------------------------------------------                      
*        VALIDATE WORK CODES AND AMOUNTS                                        
*---------------------------------------------------------                      
*                                                                               
VALWC    CLI   TYPE,EXPENSE                                                     
         BE    VALAMT                                                           
                                                                                
         MVI   COMMSW,0            SET SWITCH TO COMMISSIONABLE                 
         GOTOR AFVAL,APOWRKCH      ANY WORKCODES?                               
         BNZ   VALCOM              YES, VALIDATE                                
         CLI   APOVALCH+5,0        NO, ANY AMOUNT?                              
         BNE   EXIT                YES, ERROR                                   
                                                                                
         CLI   APOWRKNH+5,0        ANY NON-COMMISSIONABLE?                      
         BE    EXIT                NO, MUST HAVE SOMETHING                      
                                                                                
VALCOM   GOTOR CHECKWC,LWCC1                                                    
         BNE   EXIT                FOUND AN ERROR                               
                                                                                
VALTST   GOTOR AFVAL,APOWRKNH      ANY NON-COMMISSIONABLE?                      
         BNZ   VALNON              YES                                          
         CLI   APOVALNH+5,0        NO, ANY AMOUNT?                              
         BNE   EXIT                YES, ERROR                                   
                                                                                
VALNON   MVI   COMMSW,NONCOMM                                                   
         GOTOR CHECKWC,LWCN1                                                    
         BNE   EXIT                                                             
                                                                                
VALWCX   B     NEWREC                                                           
         EJECT                                                                  
*---------------------------------------------------------                      
*        HANDLE AMOUNT AND NAMTELS FOR EXPENSE ORDERS                           
*---------------------------------------------------------                      
*                                                                               
VALAMT   LA    RE,NAMTELS                                                       
         LA    RF,L'OAMTELS                                                     
         SR    R1,R1                                                            
         MVCL  RE,R0               CLEAR NAMTELS                                
                                                                                
         LA    R5,NAMTELS                                                       
         USING OAMELD,R5           BUILD A NEW ELEMENT WITH ZERO VALUES         
         MVI   OAMEL,OAMELQ        IN NAMTELS                                   
         MVI   OAMLN,OAMLN2Q                                                    
         MVC   OAMSTAT,STATUS                                                   
         ZAP   OAMAMNT,=P'0'                                                    
         ZAP   OAMINUM,=P'0'                                                    
         ZAP   OAMIVAL,=P'0'                                                    
         ZAP   OAMTVAL,=P'0'                                                    
         MVC   OAMWORK,SPACES                                                   
         LA    R1,APOVALCH                                                      
         GOTOR AFVAL                                                            
         BZ    ERRXIT                                                           
         ZIC   R0,FLDH+5                                                        
         GOTOR VCASHVAL,DMCB,FLD,(R0)                                           
         MVI   FERN,INVAMNT                                                     
         CLI   0(R1),X'FF'                                                      
         BE    ERRXIT                                                           
         L     R0,4(R1)                                                         
         CVD   R0,DUB                                                           
         ZAP   OAMAMNT,DUB         AMOUNT INTO ELEMENT                          
         B     NEWREC                                                           
         EJECT                                                                  
*---------------------------------------------------------                      
*        CHECK WORK CODE(S) AND AMOUNT(S)                                       
*---------------------------------------------------------                      
*                                                                               
* ADDRESS OF LAST WORK CODE VALUE(S) IS IN R1                                   
* ADDRESS OF WORK CODE FLD HDR IS IN FADR                                       
* CORRESPONDING AMOUNT FLD IS ASSUMED TO BE NEXT UNPROT                         
* COMMSW IS SET TO 0=COMM OR X'80'=NONCOMM                                      
*                                                                               
* RETURN WITH CC=EQU IF OK AND NEW AMOUNT ELS IN NAMTELS (MAJOR/COMM,           
* MINOR/COMM,MAJOR/NONCOMM,MINOR/NONCOMM - NULLS IF MISSING)                    
*                                                                               
* RETURN WITH CC=NEQ IF ERROR - FERN,FADR,FNDX SET                              
*                                                                               
CHECKWC  NTR1                                                                   
         LR    R3,R1               R1 = A(LWCC1 OR LWCCN1)                      
         L     R2,FADR             R2 = A(FLD HDR)                              
         GOTOR VSCANNER,DMCB,(R2),CRD                                           
         MVI   FERN,INVALID                                                     
         CLI   DMCB+4,4                                                         
         BH    ERRXIT                                                           
         MVI   FLAG1,0                                                          
         CLI   5(R2),0                                                          
         BE    *+10                                                             
         MVC   FLAG1,DMCB+4        SAVE NUMBER OF ENTRIES                       
                                                                                
         CLC   0(2,R3),8(R2)       DID ANY WORKCODES CHANGE ?                   
         BNE   CHECKWC0                                                         
         CLC   2(2,R3),11(R2)                                                   
         BNE   CHECKWC0                                                         
         CLC   4(2,R3),14(R2)      DID ANY WORKCODES CHANGE ?                   
         BNE   CHECKWC0                                                         
         CLC   6(2,R3),17(R2)                                                   
         BE    CHECKWC5                                                         
                                                                                
CHECKWC0 LR    RF,R2               CLEAR W/C DESCRIPTIONS                       
         ZIC   RE,0(RF)                                                         
         AR    RF,RE                                                            
         IC    RE,0(RF)                                                         
         SH    RE,=H'9'                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   8(0,RF),SPACES                                                   
         OI    6(RF),X'80'                                                      
         MVC   TEMP,SPACES         CLEAR TEMP FOR DESCRIPTIONS                  
                                                                                
         MVI   FERN,CANTAMND       CANT CHANGE CODES IF INVOICED                
         CLI   INVOICED,C'Y'                                                    
         BE    ERRXIT                                                           
         LA    R5,NAMTELSC         CLEAR NAMTELS FOR REBUILD OF ELS             
         TM    COMMSW,NONCOMM      R5 = NAMTELS POINTER                         
         BZ    *+8                                                              
         LA    R5,NAMTELSN                                                      
         XC    0(4*OAMLN2Q,R5),0(R5)                                            
         CLI   5(R2),0             NO INPUT                                     
         BE    CHECKWC5                                                         
                                                                                
CHECKWC1 LA    R4,CRD              PREPARE FOR LOOP FOR A CODE                  
         MVC   STATUS,COMMSW                                                    
         SR    R6,R6                                                            
         IC    R6,DMCB+4                                                        
                                                                                
CHECKWC2 MVI   FERN,INVALID        CHECK LENGTH OF CODE                         
         CLC   0(2,R4),=X'0200'                                                 
         BNE   ERRXIT                                                           
         GOTOR AGETWC,12(R4)                                                    
         BNE   ERRXIT                                                           
         CLI   PROGPROF+3,2                                                     
         BNE   CHECKW2A                                                         
         BAS   RE,CHKEST           CHECK IF EST. ON JOB                         
         BE    CHECKWCA                                                         
         MVI   FERN,NOESTWC                                                     
         B     ERRXIT                                                           
                                                                                
CHECKW2A CLI   PROGPROF+3,3                                                     
         BNE   CHECKWCA                                                         
         BAS   RE,APPEST           CHECK IF ESTIMATE APPROVED                   
         BE    CHECKWCA                                                         
         MVI   FERN,ESTAPP                                                      
         LA    R2,APOCLIH                                                       
         ST    R2,FADR                                                          
         B     ERRXIT                                                           
                                                                                
CHECKWCA LA    RF,TEMP             SAVE DESCRIPTION IN TEMP                     
         CLI   0(RF),C' '                                                       
         BE    CHECKWC3                                                         
         LA    RF,L'TEMP-1(RF)                                                  
         CLI   0(RF),C' '                                                       
         BNE   *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C','                                                       
         LA    RF,2(RF)                                                         
                                                                                
CHECKWC3 MVC   0(L'WCODESC,RF),WORK                                             
         USING OAMELD,R5           BUILD A NEW ELEMENT WITH ZERO VALUES         
         MVI   OAMEL,OAMELQ        IN NAMTELS                                   
         MVI   OAMLN,OAMLN2Q                                                    
         MVC   OAMSTAT,STATUS                                                   
         MVC   OAMWORK,12(R4)                                                   
         ZAP   OAMAMNT,=P'0'                                                    
         ZAP   OAMINUM,=P'0'                                                    
         ZAP   OAMIVAL,=P'0'                                                    
         ZAP   OAMTVAL,=P'0'                                                    
         LA    R4,32(R4)                                                        
         LA    R5,OAMLN2Q(R5)                                                   
         OI    STATUS,MINOR                                                     
         BCT   R6,CHECKWC2                                                      
                                                                                
CHECKWC4 ZIC   RE,0(R2)            SET UP DESCRIPTIONS FROM TEMP                
         AR    R2,RE                                                            
         IC    RE,0(R2)                                                         
         SH    RE,=H'9'                                                         
         EX    RE,CHECKMVC                                                      
         B     CHECKWC5                                                         
                                                                                
CHECKMVC MVC   8(0,R2),TEMP                                                     
                                                                                
CHECKWC5 SR    R0,R0               NOW HANDLE AMOUNT(S)                         
         IC    R0,0(R2)            BUMP TO NEXT UNPROT                          
         AR    R2,R0                                                            
         TM    1(R2),X'20'                                                      
         BO    *-10                                                             
         GOTOR AFVAL,(R2)          SET FADR ETC                                 
         BNZ   CHECKWC6            NO AMOUNTS                                   
         CLI   FLAG1,0             ANY WORKCODES ?                              
         BE    OKXIT               NO, OK WITH NO AMOUNTS THEN                  
         MVI   FERN,NOINPUT                                                     
         B     ERRXIT                                                           
                                                                                
CHECKWC6 GOTOR VSCANNER,DMCB,(R2),CRD                                           
         MVI   FERN,INCOMPAT                                                    
         CLC   FLAG1,DMCB+4                                                     
         BNE   ERRXIT              BUT IF INPUT MUST BE SAME NO AS W/C          
         SR    R6,R6                                                            
         IC    R6,DMCB+4                                                        
                                                                                
         LA    R4,CRD              PREPARE FOR LOOP FOR AN AMOUNT               
         LA    R5,NAMTELSC                                                      
         TM    COMMSW,NONCOMM                                                   
         BZ    CHECKWC8                                                         
         LA    R5,NAMTELSN                                                      
                                                                                
CHECKWC8 ZAP   DUB,=P'0'           CHECK AN AMOUNT - ZERO IF NO INPUT           
         CLI   5(R2),0                                                          
         BE    CHECKWC9            NO INPUT                                     
         ZIC   R0,0(R4)                                                         
         GOTOR VCASHVAL,DMCB,12(R4),(R0)                                        
         MVI   FERN,INVAMNT                                                     
         CLI   0(R1),X'FF'                                                      
         BE    ERRXIT                                                           
         L     R0,4(R1)                                                         
         CVD   R0,DUB                                                           
                                                                                
CHECKWC9 MVC   OAMAMNT,DUB+2       MOVE IT INTO ELEMENT                         
         LA    R4,32(R4)                                                        
         LA    R5,OAMLN2Q(R5)                                                   
         BCT   R6,CHECKWC8                                                      
         B     OKXIT                                                            
         DROP  R5                                                               
         EJECT                                                                  
*---------------------------------------------------------                      
*        CHECK FOR ESTIMATE                                                     
*---------------------------------------------------------                      
*                                                                               
CHKEST   NTR1                                                                   
         L     RE,AJOBLOCK                                                      
         USING JBLOCKD,RE                                                       
         L     R1,JBACOLTB                                                      
         CLI   JBNEWEST,JBMCSQ     IS THIS A BRANDO ESTIMATE?                   
         BE    CHKEST6             YES, HANDLE DIFFERENTLY                      
         USING JBCOLD,R1                                                        
         LH    R0,JBNROWS                                                       
                                                                                
CHKEST2  CLI   JBCOLTYP,JBCOLTWC   TEST FOR WORKCODE ENTRY                      
         BNE   CHKEST4                                                          
         CLC   JBCOLWC,12(R4)      MATCH ON WORKCODE                            
         BE    OKXIT                                                            
                                                                                
CHKEST4  AH    R1,JBLCOL                                                        
         BCT   R0,CHKEST2                                                       
         B     ERRXIT                                                           
         DROP  R1                                                               
                                                                                
         USING MJETABD,R1                                                       
CHKEST6  CLI   MJETTYP,MJETTEQ     AT END?                                      
         BE    ERRXIT              YES, ERROR                                   
         CLI   MJETTYP,MJETTWQ     AT WORKCODE?                                 
         BNE   CHKEST8                                                          
         OC    MJET1RA,MJET1RA     BUT NOT 1R-LEVEL DETAILS                     
         BNZ   CHKEST8                                                          
         CLC   MJETWCD,12(R4)      MATCH ON WORKCODE                            
         BNE   CHKEST8                                                          
         CP    MJETVAL+6(6),=P'0'                                               
         BNE   OKXIT                                                            
                                                                                
CHKEST8  XR    R0,R0                                                            
         IC    R0,MJETLEN                                                       
         AR    R1,R0                                                            
         B     CHKEST6                                                          
         DROP  R1,RE                                                            
         EJECT                                                                  
*---------------------------------------------------------                      
*        CHECK FOR APPROVED ESTIMATE                                            
*---------------------------------------------------------                      
*                                                                               
APPEST   NTR1                                                                   
         L     RE,AJOBLOCK                                                      
         USING JBLOCKD,RE                                                       
         L     R1,JBACOLTB                                                      
         USING JBCOLD,R1                                                        
         CLI   JBNEWEST,C'Y'       IS THIS A NEW ESTIMATE?                      
         BE    APPEST1             YES                                          
         CLI   JBNEWEST,JBMCSQ     NO, IS THIS BRANDO?                          
         BNE   APPEST2             NO, CHECK DIFFERENT FIELD                    
                                                                                
APPEST1  OC    JBHIAPP,JBHIAPP                                                  
         BNZ   APPESTX                                                          
         B     ERRXIT                                                           
                                                                                
APPEST2  CLI   JBOLDAPP,C'Y'       IS THE OLD ESTIMATE APPROVED?                
         BNE   ERRXIT                                                           
                                                                                
APPESTX  B     OKXIT                                                            
         DROP  R1,RE                                                            
         EJECT                                                                  
*---------------------------------------------------------                      
*        COMPLETE NEW ORDER RECORD IN IOAREA3                                   
*---------------------------------------------------------                      
*                                                                               
         USING ORDRECD,R5                                                       
NEWREC   L     R5,AIOAREA3                                                      
         LH    R6,DATADISP                                                      
         AR    R6,R5                                                            
                                                                                
NEWREC0  CLI   0(R6),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R6),X'67'                                                      
         BE    NEWREC1                                                          
         SR    R1,R1                                                            
         IC    R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     NEWREC0                                                          
                                                                                
         USING ORDELD,R6                                                        
NEWREC1  MVC   TEMP(100),ORDEL     SAVE IT                                      
         LA    R7,NCOMELS          ADD COMMENT ELS                              
         SR    R1,R1                                                            
                                                                                
NEWREC2  CLI   0(R7),0                                                          
         BE    NEWREC4                                                          
         IC    R1,1(R7)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R6),0(R7)                                                    
         LA    R1,1(R1)                                                         
         AR    R6,R1                                                            
         AR    R7,R1                                                            
         B     NEWREC2                                                          
                                                                                
         USING TRSELD,R6                                                        
NEWREC4  TM    ACTINDS,NEW         ADD STATUS ELEMENT FOR NEW                   
         BZ    NEWREC5                                                          
         XC    0(TRSLNQ,R6),0(R6)                                               
         MVI   TRSEL,TRSELQ                                                     
         MVI   TRSLN,TRSLNQ                                                     
         GOTOR VDATCON,DMCB,(1,TODAYP),(2,TRSDATE)                              
         SR    R1,R1                                                            
         IC    R1,TRSLN                                                         
         AR    R6,R1                                                            
                                                                                
         USING ORDELD,R6           SAVE ORDER EL                                
NEWREC5  MVC   0(100,R6),TEMP      RESTORE ORDER EL                             
         ZIC   RF,ORDLN                                                         
         AR    R6,RF                                                            
         LA    R7,NAMTELS          ADD ORDER AMOUNT EL(S)                       
         LA    R0,8                                                             
                                                                                
NEWREC6  CLI   0(R7),0                                                          
         BE    NEWREC8                                                          
         MVC   0(OAMLN2Q,R6),0(R7)                                              
         LA    R6,OAMLN2Q(R6)                                                   
                                                                                
NEWREC8  LA    R7,OAMLN2Q(R7)                                                   
         BCT   R0,NEWREC6                                                       
         MVI   0(R6),0             TERMINATE AND INSERT LENGTH                  
         LA    R6,1(R6)                                                         
         SR    R6,R5                                                            
         STCM  R6,3,ORDRLEN                                                     
                                                                                
         LA    R6,ORDRECD+ACCORFST                                              
         USING ORDELD,R6                                                        
         SR    R0,R0                                                            
         AR    R6,R0                                                            
         IC    R0,ORDLN                                                         
         CLI   0(R6),ORDELQ                                                     
         BNE   *-10                                                             
                                                                                
         LA    R1,APOACTH                                                       
         ST    R1,FADR                                                          
                                                                                
         TM    ACTINDS,NEW                                                      
         BO    ADDREC                                                           
         B     CHAREC                                                           
         DROP  R5                                                               
         EJECT                                                                  
*---------------------------------------------------------                      
*        ADD NEW ORDER TO ACCOUNT FILE                                          
*        LOCK THE CONTROL RECORD FOR BOTH AUTO AND MANUAL                       
*        NUMBER ASSIGNMENT TO FORCE ENQUEUING WITH CONCURRENT                   
*        ORDER RESERVATIONS - UNLOCK CALL FREES THE LOCK                        
*---------------------------------------------------------                      
*                                                                               
ADDREC   BAS   RE,GETCON           GET ORDER CONTROL RECORD INTO IO2            
         BNE   EXIT                                                             
                                                                                
         ZIC   RF,APONUMH+5        CHECK FOR AUTO NUMBERING                     
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   APONUM(0),=C'AUTO'                                               
         BE    ADDR02                                                           
                                                                                
         BAS   RE,MANUAL           HANDLE MANUALLY INPUT NUMBER                 
         BE    ADDR04                                                           
         B     EXIT                                                             
                                                                                
ADDR02   BAS   RE,AUTO             ADD ORDER WITH AUTO NUMBERING                
         L     R2,AIOAREA2         R2=A(ORDER CONTROL RECORD)                   
         LH    R3,DATADISP                                                      
         AR    R3,R2                                                            
         USING ONCELD,R3                                                        
         L     R5,AIOAREA3         R5=A(ORDER RECORD)                           
         USING ORDRECD,R5                                                       
         MVC   ONCNUM,ORDKORD      GRAB NUMBER FROM ORDER                       
                                                                                
         GOTOR AWRITE,AIOAREA2                                                  
         BNE   EXIT                                                             
         MVC   APONUM,ORDKORD      AND SCREEN                                   
         OI    APONUMH+6,X'80'                                                  
                                                                                
ADDR04   GOTOR AUNLOCK             UNLOCK CONTROL RECORD                        
         LA    R2,KEY              CHECK FOR CONTRA HEADER ON FILE              
         USING CHDRECD,R2          FOR JOB/EXP ORDER TRANSACTION                
         MVC   CHDKEY,SPACES                                                    
         MVC   CHDKCULA,JOBKEY                                                  
         CLI   TYPE,PRODN                                                       
         BNE   *+10                                                             
         MVC   CHDKWRK,=2C'*'                                                   
         MVC   CHDKCULC,ORDSUP                                                  
         CLI   TYPE,PRODN                                                       
         BE    *+8                                                              
         MVI   CHDKCCPY,C' '       FOR NON-PROD BLANK OUT COMPANY CODE          
         GOTOR AREAD,AIOAREA1                                                   
                                                                                
         L     R2,AIOAREA1                                                      
         LR    R3,R2                                                            
         AHI   R3,ACCORFST                                                      
         TM    DMCB+8,X'10'        DID WE FIND THE RECORD?                      
         BZ    ADDR06              YES                                          
                                                                                
         L     RE,AIOAREA1         NO, CREATE ONE                               
         LHI   RF,LIOAREAS         CLEAR THE IOAREA FIRST                       
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
                                                                                
         L     R2,AIOAREA1         BUILD KEY OF RECORD                          
         MVC   CHDKEY,KEY                                                       
         XC    CHDKNULL,CHDKNULL                                                
                                                                                
         USING CACELD,R3           BUILD ELEMENT NOW                            
         LA    R3,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   CACEL,CACELQ                                                     
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
         BNE   EXIT                                                             
                                                                                
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
         MVI   TRNEL,TRNELQ                                                     
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
*                                                                               
         TM    COMPSTA9,CPYSSRNM   DOES COMPANY USE SERIAL NO                   
         BZ    ADDR07              NO                                           
         LA    R5,BIGKEY           READ FOR SERIAL PASSIVE POINTER              
         USING TRSPASD,R5                                                       
         XC    TRSPKEY,TRSPKEY                                                  
         MVI   TRSPTYP,TRSPTYPQ                                                 
         MVI   TRSPSUB,TRSPSUBQ                                                 
         MVC   TRSPCPY,TRNKCPY                                                  
         GOTOR ARDHIDL,0                                                        
         XC    TRSPNULL,TRSPNULL   INIT FOR BINARY ZEROES                       
*                                                                               
         CLC   TRSPKEY(TRSPNULL-TRSPASD),KEYSAVE TRSPASD PRESENT?               
         BE    ADDR06A             YES:ADD 1 INTO EXISTING SERNO CNT            
         LHI   RE,1                NO:START FROM 1 FOR SERLED                   
         MVI   TRSPTYP,TRSPTYPQ                                                 
         MVI   TRSPSUB,TRSPSUBQ                                                 
         MVC   TRSPCPY,TRNKCPY                                                  
         B     ADDR06B                                                          
*                                                                               
ADDR06A  ICM   RE,15,TRSPSER       GET 2S COMPLIMENT SERIAL NUMBER              
         LPR   RE,RE               WORK OUT SERIAL NUMBER                       
         A     RE,=F'1'            ADD ONE TO GET NEXT SERIAL NUMBER            
*                                                                               
         USING SERELD,R3           SERIAL NUMBER ELEMENT                        
ADDR06B  LA    R3,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   SEREL,SERELQ                                                     
         MVI   SERLN,SERLNQ                                                     
         STCM  RE,15,SERNM         SAVE NEXT SERIAL NUMBER                      
         LNR   RE,RE               DO 2S COMPLIMENT FOR NEW PASSIVE             
         STCM  RE,15,TRSPSER                                                    
         L     RF,AIOAREA4         GET RECORD STORED IN AIO4                    
         MVC   0(L'TRSPKEY,RF),TRSPKEY                                          
         SR    R1,R1                                                            
         IC    R1,SERLN                                                         
         AR    R3,R1                                                            
         GOTOR ADDELEM,AIOAREA1    ADD SERELQ                                   
         DROP  R3                                                               
*                                                                               
ADDR07   LA    R7,NAMTELS          ADD UP TO 8 ORDER AMOUNT ELS                 
         LA    R0,8                FROM NAMTELS                                 
*                                                                               
         USING OAMELD,R3                                                        
ADDR08   CLI   0(R7),0                                                          
         BE    ADDR10                                                           
         LA    R3,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVC   0(OAMLN2Q,R3),0(R7)                                              
         GOTOR ADDELEM,AIOAREA1    ADD OAMELQS                                  
*                                                                               
ADDR10   AHI   R7,OAMLN2Q                                                       
         BCT   R0,ADDR08                                                        
*                                                                               
         MVC   KEY,TRNKEY          MOVE FROM AIO1 INTP KEY                      
         GOTOR AREADL,AIOAREA2     AND READ INTO AIOAREA2                       
         CLI   FERN,NOTFOUND                                                    
         BE    ADDR12              KEY WILL PROBABLY DIFFER                     
         MVC   KEY,TRNKEY          MOVE FROM AIO1 INTP KEY                      
         GOTOR AWRITE,AIOAREA1                                                  
         BNE   EXIT                                                             
         B     ADDR14                                                           
                                                                                
ADDR12   GOTOR AADD,AIOAREA1       ADD SJ FROM AIO1                             
         BNE   EXIT                                                             
*                                                                               
ADDR14   TM    COMPSTA9,CPYSSRNM                                                
         BZ    ADDR16                                                           
*                                                                               
         MVC   BIGKEY,TRNRECD                                                   
         GOTO1 AREADD,0                                                         
         BNE   EXIT                                                             
*                                                                               
         LA    R2,BIGKEY                                                        
         L     R5,AIOAREA4                                                      
         MVC   TRSPSTA,TRNKSTA                                                  
         MVC   TRSPDA,TRNKDA                                                    
         MVC   BIGKEY,0(R5)                                                     
         GOTO1 AADDDIR,0           MUST BE ADDED USING NEW FILE                 
         BNE   EXIT                                                             
         MVC   BIGKEY,KEYSAVE      LOAD SAVED LOCKED PASSIVE POINTER            
         GOTO1 AUNLOCKD                                                         
*                                                                               
ADDR16   CLI   ACTION,APR          UNLESS ADDPRINT, SET COMPLETION MSG          
         BE    OKEND                                                            
         MVC   MSG,=CL60'ORDER ADDED - ENTER NEXT ACTION'                       
         LA    R1,APOACTH                                                       
         ST    R1,FADR                                                          
         B     OKEND                                                            
         DROP  R2,R3,R5                                                         
         EJECT                                                                  
*---------------------------------------------------------                      
*        ADD AN ORDER WITH A MANUALLY INPUT ORDER NUMBER                        
*        AT ENTRY, AIOAREA3=A(ORDER RECORD)                                     
*        ON EXIT, CC=EQ IF OK, CC=NEQ IF ERROR, FERN/MSG SET                    
*---------------------------------------------------------                      
*                                                                               
MANUAL   NTR1  ,                                                                
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
         BNE   MAN02               NOTHING OUT THERE                            
         CLC   OBRKFRST,ORDKORD    TEST IF RESERVATION BRACKETS NUMBER          
         BH    MAN02               NO                                           
         TM    OBRKSTA,X'80'       TEST FOR DELETED RESERVATION                 
         BO    MAN02                                                            
         MVI   FERN,ORDRES                                                      
         B     ERRXIT                                                           
                                                                                
MAN02    LA    R2,KEY                                                           
         USING ONARECD,R2                                                       
         XC    ONAKEY,ONAKEY                                                    
         MVI   ONAKTYP,ONAKTYPQ    FIND ALLOCATION RECORDS                      
         MVI   ONAKSUB,ONAKSUBQ                                                 
         MVC   ONAKCPY,COMPANY                                                  
         GOTOR ARDHI,AIOAREA4                                                   
         L     R2,AIOAREA4                                                      
                                                                                
MAN04    CLC   ONAKEY(ONAKAPP-ONAKEY),KEYSAVE                                   
         BNE   MAN12               NOTHING OUT THERE                            
                                                                                
         USING ONCELD,R3                                                        
         L     R3,AIOAREA4                                                      
         AH    R3,DATADISP                                                      
                                                                                
MAN06    CLI   0(R3),0             END OF RECORD?                               
         BE    MAN08               YES, SEE IF ANY MORE RECORDS                 
         CLI   0(R3),ONCELQ        ORDER NUMBER CONTROL ELEMENT?                
         BE    MAN10               YES, CHECK RANGE                             
                                                                                
         SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     MAN06                                                            
                                                                                
MAN08    GOTOR ASEQ,AIOAREA4                                                    
         B     MAN04                                                            
                                                                                
MAN10    CLC   ORDKORD,ONCEND#                                                  
         BH    MAN08               NEW ORDER HIGHER THAN LAST ONE               
         CLC   ORDKORD,ONCSTA#                                                  
         BL    MAN08               OR LOWER THAN START                          
         MVI   FERN,ORDAFM                                                      
         B     ERRXIT                                                           
                                                                                
MAN12    MVC   KEY,ORDKEY          READ FOR RECORD BEFORE ADDING                
         GOTOR AREADL,AIOAREA4                                                  
         CLI   DMCB+8,0                                                         
         BNE   MAN16               NOT THERE, GO ADD IT                         
                                                                                
         L     R5,AIOAREA4                                                      
         TM    ORDRSTA,X'A0'       ONLY DELETED ORDERS CAN HAVE                 
         BNZ   MAN14               THEIR NUMBER REUSED                          
         MVI   FERN,ORDUSED                                                     
         B     ERRXIT                                                           
                                                                                
MAN14    GOTOR AWRITE,AIOAREA3                                                  
         BE    MAN18                                                            
         MVI   FERN,NOADD                                                       
         B     ERRXIT                                                           
                                                                                
MAN16    GOTOR AADD,AIOAREA3                                                    
         CLI   DMCB+8,0                                                         
         BE    MAN18                                                            
         DC    H'0'                                                             
                                                                                
MAN18    MVC   BIGKEY,ORDRECD      READ THE NEW RECORD                          
         GOTOR ARDHID,0                                                         
                                                                                
         LA    R5,BIGKEY                                                        
         CLC   ORDKEY(ORDKSEQ-ORDKEY),KEYSAVE                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVDA,DISKADD        SAVE THE ADDRESS                             
                                                                                
         L     RE,AIOAREA3                                                      
         LHI   RF,LIOAREAS                                                      
         L     R0,AIOAREA4                                                      
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         LA    R2,DMCB                                                          
         GOTOR VACCEMU,DMCB,=C'OLDN',,AIOAREA4,AIOAREA4                         
                                                                                
         USING CPTRBLK,XTCPTRBK                                                 
         XC    CPTRBLK(CPTRBLKL),CPTRBLK                                        
         GOTOR VPADDLE,DMCB,(C'A',AIOAREA4),CPTRBLK,SVDA,0,ACOMFACS             
                                                                                
MANX     B     OKXIT                                                            
         DROP  R2,R5                                                            
         EJECT                                                                  
*-----------------------------------------------------------------              
*        GET THE ORDER CONTROL RECORD - ADD ONE IF AGENCY                       
*        DOESN'T HAVE IT -                                                      
*        ON EXIT, AIOAREA2=A(CONTROL RECORD), NEXTNUM, NEXTBIN SET              
*-----------------------------------------------------------------              
*                                                                               
         USING ORDRECD,R2                                                       
GETCON   NTR1  ,                                                                
         LA    R2,KEY                                                           
         XC    ORDKEY,ORDKEY       READ ORDER NUMBER CONTROL RECORD             
         MVI   ORDKTYP,ORDKTYPQ                                                 
         MVC   ORDKCPY,COMPANY                                                  
         MVC   ORDKORD,=6C'0'                                                   
         GOTOR AREADL,AIOAREA2                                                  
         TM    DMCB+8,X'EF'                                                     
         BNZ   ERRXIT                                                           
                                                                                
         L     R2,AIOAREA2         R2=A(CONTROL RECORD)                         
         LR    R3,R2                                                            
         AHI   R3,ACCORFST         GET TO FIRST ELEMENT                         
                                                                                
         CLI   DMCB+8,0            TEST IF CONTROL RECORD FOUND                 
         BE    GETC02                                                           
                                                                                
         L     RE,AIOAREA2                                                      
         LHI   RF,LIOAREAS                                                      
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
                                                                                
         USING ONCELD,R3                                                        
         MVC   ORDKEY,KEY                                                       
         MVI   ONCEL,ONCELQ                                                     
         MVI   ONCLN,ONCOANQ                                                    
         MVC   ONCNUM,=6C'0'                                                    
                                                                                
         LHI   RF,ACCORFST                                                      
         SR    RE,RE                                                            
         IC    RE,ONCLN                                                         
         AR    RF,RE                                                            
         AHI   RF,1                                                             
         STH   RF,ORDRLEN                                                       
                                                                                
         GOTOR AADD,AIOAREA2       ADD CONTROL RECORD                           
         GOTOR AREADL,AIOAREA2     RE-READ FOR UPDATE                           
         TM    DMCB+8,X'EF'                                                     
         BNZ   ERRXIT                                                           
                                                                                
GETC02   MVC   NEXTNUM,ONCNUM                                                   
         PACK  DUB,NEXTNUM         BUMP LAST ORDER NUMBER                       
         CVB   R0,DUB                                                           
         AH    R0,=H'1'                                                         
         ST    R0,NEXTBIN          SAVE BINARY                                  
         EDIT  (R0),(6,NEXTNUM),FILL=0                                          
                                                                                
GETCX    B     OKXIT                                                            
         DROP  R2,R3                                                            
         EJECT                                                                  
*-----------------------------------------------------------------              
*        ADD THE ORDER USING AUTOMATIC NUMBER ASSIGNMENT                        
*        ON ENTRY, AIOAREA3=A(ORDER RECORD), NEXTNUM=NEXT NUMBER                
*-----------------------------------------------------------------              
*                                                                               
AUTO     NTR1  ,                                                                
         LA    R2,BIGKEY           INITIALIZE KEY W/ORDER RESERVATION           
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
                                                                                
AUTO06   LA    R3,KEY                                                           
         USING ONARECD,R3                                                       
         XC    ONAKEY,ONAKEY                                                    
         MVI   ONAKTYP,ONAKTYPQ    FIND ALLOCATION RECORDS                      
         MVI   ONAKSUB,ONAKSUBQ                                                 
         MVC   ONAKCPY,COMPANY                                                  
         GOTOR ARDHI,AIOAREA4                                                   
                                                                                
AUTO08   L     R3,AIOAREA4                                                      
         CLC   ONAKEY(ONAKAPP-ONAKEY),KEYSAVE                                   
         BNE   AUTO16              NOTHING OUT THERE                            
                                                                                
         USING ONCELD,R3                                                        
         L     R3,AIOAREA4                                                      
         AH    R3,DATADISP                                                      
                                                                                
AUTO10   CLI   0(R3),0             END OF RECORD?                               
         BE    AUTO12              YES, SEE IF ANY MORE RECORDS                 
         CLI   0(R3),ONCELQ        ORDER NUMBER CONTROL ELEMENT?                
         BE    AUTO14              YES, CHECK RANGE                             
                                                                                
         SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     AUTO10                                                           
                                                                                
AUTO12   GOTOR ASEQ,AIOAREA4                                                    
         B     AUTO08                                                           
                                                                                
AUTO14   CLC   NEXTNUM,ONCEND#                                                  
         BH    AUTO12              NEW ORDER HIGHER THAN LAST ONE               
         CLC   NEXTNUM,ONCSTA#                                                  
         BL    AUTO12              OR LOWER THAN START                          
         B     AUTO18                                                           
                                                                                
AUTO16   L     R5,AIOAREA3                                                      
         USING ORDRECD,R5                                                       
         MVC   ORDKORD,NEXTNUM     SET NEXT NUMBER IN KEY                       
                                                                                
         GOTOR VDATAMGR,DMCB,(X'08',DMRDHI),ACCDIR,AIOAREA3,DIRIO               
         CLC   DIRIO(L'ACCKEY),0(R5)                                            
         BE    AUTO18                                                           
                                                                                
         GOTOR AADD,AIOAREA3                                                    
         CLI   DMCB+8,0            TEST FOR ERROR                               
         BE    *+6                 NONE                                         
         DC    H'0'                                                             
                                                                                
         MVC   BIGKEY,ORDRECD      READ THE NEW RECORD                          
         GOTOR ARDHID,0                                                         
                                                                                
         LA    R5,BIGKEY                                                        
         CLC   ORDKEY(ORDKSEQ-ORDKEY),KEYSAVE                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVDA,DISKADD        SAVE THE ADDRESS                             
                                                                                
         L     RE,AIOAREA3                                                      
         LHI   RF,LIOAREAS                                                      
         L     R0,AIOAREA4                                                      
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         LA    R2,DMCB                                                          
         GOTOR VACCEMU,DMCB,=C'OLDN',,AIOAREA4,AIOAREA4                         
                                                                                
         USING CPTRBLK,XTCPTRBK                                                 
         XC    CPTRBLK(CPTRBLKL),CPTRBLK                                        
         GOTOR VPADDLE,DMCB,(C'A',AIOAREA4),CPTRBLK,SVDA,0,ACOMFACS             
         B     AUTOX                                                            
                                                                                
AUTO18   L     R0,NEXTBIN          INCREMENT NUMBER                             
         AH    R0,=H'1'                                                         
         ST    R0,NEXTBIN                                                       
         EDIT  (R0),(6,NEXTNUM),FILL=0                                          
         B     AUTO04              AND TRY AGAIN                                
                                                                                
AUTOX    B     EXIT                                                             
         DROP  R2,R3,R5                                                         
         EJECT                                                                  
*---------------------------------------------------------                      
*        WRITE BACK CHANGED/EDITED ORDER TO ACCOUNT FILE                        
*---------------------------------------------------------                      
*                                                                               
CHAREC   L     RE,AIOAREA3         CHECK FOR ANY CHANGE                         
         LH    RF,ACLENGTH-ACKEYD(RE)                                           
         L     R2,AIOAREA1                                                      
         LH    R3,ACLENGTH-ACKEYD(R2)                                           
         CR    RF,R3                                                            
         BNE   CHAR02                                                           
         CLCL  RE,R2                                                            
         BNE   CHAR02                                                           
         MVI   FERN,NOCHANGE                                                    
         B     EXIT                                                             
                                                                                
         USING ORDRECD,R2                                                       
CHAR02   L     R2,AIOAREA1                                                      
         MVC   BIGKEY,0(R2)        ORIGINAL ORDER                               
         GOTOR ARDHID,0                                                         
                                                                                
         LA    R2,BIGKEY                                                        
         CLC   ORDKEY(ORDKSEQ-ORDKEY),KEYSAVE                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVDA,DISKADD        SAVE THE ADDRESS                             
                                                                                
         L     RE,AIOAREA1                                                      
         LHI   RF,LIOAREAS                                                      
         L     R0,AIOAREA4                                                      
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         LA    R2,DMCB                                                          
         GOTO1 VACCEMU,DMCB,=C'OLDN',,AIOAREA4,AIOAREA4                         
                                                                                
         XC    CPTRBLK(CPTRBLKL),CPTRBLK                                        
         GOTO1 VPADDLE,DMCB,(C'D',AIOAREA4),(C'K',CPTRBLK),SVDA,0,     +        
               ACOMFACS                                                         
                                                                                
         L     RE,AIOAREA3                                                      
         ZIC   R1,ORDAMNO          UPDATE AMEND NO/DATE AND DISPLAY             
         LA    R1,1(R1)                                                         
         STC   R1,ORDAMNO                                                       
         MVC   ORDAMDT,TODAYP                                                   
         MVC   APODATS,SPACES                                                   
         OI    APODATSH+6,X'80'                                                 
         LA    R2,APODATS                                                       
         MVC   0(14,R2),=C'LAST AMENDMENT'                                      
         EDIT  ORDAMNO,(3,16(R2)),ALIGN=LEFT                                    
         AR    R2,R0                                                            
         MVI   16(R2),C','                                                      
         GOTOR VDATCON,DMCB,(1,ORDAMDT),(8,17(R2))                              
                                                                                
         L     R3,AIOAREA3                                                      
         AH    R3,DATADISP                                                      
                                                                                
CHAR04   CLI   0(R3),0             END OF RECORD?                               
         BE    CHAR08              YES, ADD ELEMENT                             
         CLI   0(R3),GDAELQ        DATE ELEMENT?                                
         BE    CHAR10              YES                                          
                                                                                
CHAR06   SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     CHAR04                                                           
                                                                                
         USING GDAELD,R3                                                        
CHAR08   LA    R3,NGDAEL                                                        
         XC    NGDAEL,NGDAEL                                                    
         MVI   GDAEL,GDAELQ                                                     
         MVI   GDALN,GDALNQ                                                     
         MVI   GDATYPE,GDATODAT                                                 
         MVC   GDADATE,TODAYP                                                   
         GOTOR VHELLO,DMCB,(C'P',=C'ACCFIL'),AIOAREA3,NGDAEL,0                  
         CLI   DMCB+12,0           EVERYTHING OK?                               
         BE    CHAR12              YES                                          
         MVI   FERN,BIGERR                                                      
         CLI   DMCB+12,5           TOO BIG?                                     
         BE    EXIT                YES                                          
         DC    H'0'                                                             
                                                                                
CHAR10   CLI   GDATYPE,GDATODAT    RIGHT TYPE?                                  
         BNE   CHAR06              NO, KEEP LOOKING                             
         MVC   GDADATE,TODAYP      YES, UPDATE THE DATE                         
         DROP  R3                                                               
                                                                                
CHAR12   GOTOR AWRITE,AIOAREA3     WRITE BACK                                   
         BNE   EXIT                                                             
                                                                                
         USING ORDRECD,R2                                                       
         L     R2,AIOAREA3                                                      
         MVC   BIGKEY,0(R2)        UPDATED ORDER                                
         GOTOR ARDHID,0                                                         
                                                                                
         LA    R2,BIGKEY                                                        
         CLC   ORDKEY(ORDKSEQ-ORDKEY),KEYSAVE                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVDA,DISKADD        SAVE THE ADDRESS                             
                                                                                
         L     RE,AIOAREA3                                                      
         LHI   RF,LIOAREAS                                                      
         L     R0,AIOAREA4                                                      
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         LA    R2,DMCB                                                          
         GOTO1 VACCEMU,DMCB,=C'OLDN',,AIOAREA4,AIOAREA4                         
                                                                                
         XC    CPTRBLK(CPTRBLKL),CPTRBLK                                        
         GOTO1 VPADDLE,DMCB,(C'A',AIOAREA4),CPTRBLK,SVDA,0,ACOMFACS             
*                                                                               
         LA    R2,KEY                                                           
         USING TRNRECD,R2                                                       
         XC    TRNKEY,TRNKEY       BUILD KEY FOR JOB/EXP TRANSACTION            
         MVC   TRNKCULA,ORDJOB                                                  
         MVC   TRNKWORK,SPACES                                                  
         CLI   TYPE,PRODN                                                       
         BNE   *+10                                                             
         MVC   TRNKWORK,=2C'*'                                                  
         MVC   TRNKCULC,ORDSUP                                                  
         CLI   TYPE,PRODN                                                       
         BE    *+8                                                              
         MVI   TRNKCULC,C' '                                                    
         MVC   TRNKDATE,ORDDATE                                                 
         MVC   TRNKREF,APONUM                                                   
         GOTOR AREADL,AIOAREA1     READ IT                                      
         BNE   EXIT                                                             
         L     R2,AIOAREA1         REPLACE X'68' ELEMENTS ONWARDS               
         LH    R3,DATADISP         WITH 1 TO 8 ELEMENTS IN NAMTELS              
         AR    R3,R2                                                            
         SR    R0,R0                                                            
                                                                                
CHAR14   IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLI   0(R3),0                                                          
         BE    *+12                                                             
         CLI   0(R3),X'68'                                                      
         BNE   CHAR14                                                           
         LA    R7,NAMTELS                                                       
         LA    R0,8                                                             
                                                                                
CHAR16   CLI   0(R7),0                                                          
         BE    CHAR18                                                           
         MVC   0(OAMLN2Q,R3),0(R7)                                              
         LA    R3,OAMLN2Q(R3)                                                   
                                                                                
CHAR18   LA    R7,OAMLN2Q(R7)                                                   
         BCT   R0,CHAR16                                                        
*                                                                               
         TM    COMPSTA9,CPYSSRNM   DOES COMPANY USE SERIAL NO                   
         BZ    CHAR19              NO                                           
         LA    R5,BIGKEY           READ FOR SERIAL PASSIVE POINTER              
         USING TRSPASD,R5                                                       
         XC    TRSPKEY,TRSPKEY                                                  
         MVI   TRSPTYP,TRSPTYPQ                                                 
         MVI   TRSPSUB,TRSPSUBQ                                                 
         MVC   TRSPCPY,TRNKCPY                                                  
         GOTOR ARDHIDL,0                                                        
         XC    TRSPNULL,TRSPNULL   INIT FOR BINARY ZEROES                       
*                                                                               
         CLC   TRSPKEY(TRSPNULL-TRSPASD),KEYSAVE TRSPASD PRESENT?               
         BE    CHAR18A             YES:ADD 1 INTO EXISTING SERNO CNT            
         LHI   RE,1                NO:START FROM 1 FOR SERLED                   
         MVI   TRSPTYP,TRSPTYPQ                                                 
         MVI   TRSPSUB,TRSPSUBQ                                                 
         MVC   TRSPCPY,TRNKCPY                                                  
         B     CHAR18B                                                          
*                                                                               
CHAR18A  ICM   RE,15,TRSPSER       GET 2S COMPLIMENT SERIAL NUMBER              
         LPR   RE,RE               WORK OUT SERIAL NUMBER                       
         A     RE,=F'1'            ADD ONE TO GET NEXT SERIAL NUMBER            
*                                                                               
         USING SERELD,R3           SERIAL NUMBER ELEMENT                        
CHAR18B  MVI   SEREL,SERELQ                                                     
         MVI   SERLN,SERLNQ                                                     
         STCM  RE,15,SERNM         SAVE NEXT SERIAL NUMBER                      
         LNR   RE,RE               DO 2S COMPLIMENT FOR NEW PASSIVE             
         STCM  RE,15,TRSPSER                                                    
         L     RF,AIOAREA4         GET RECORD STORED IN AIO4                    
         MVC   0(L'TRSPKEY,RF),TRSPKEY                                          
         SR    R1,R1                                                            
         IC    R1,SERLN                                                         
         AR    R3,R1                                                            
         DROP  R3                                                               
*                                                                               
CHAR19   MVI   0(R3),0             TERMINATE AND INSERT LENGTH                  
         LA    R3,1(R3)                                                         
         SR    R3,R2                                                            
         STH   R3,TRNRLEN                                                       
         GOTOR AWRITE,AIOAREA1     WRITE BACK                                   
         BNE   EXIT                                                             
*                                                                               
         TM    COMPSTA9,CPYSSRNM                                                
         BZ    CHAR20                                                           
         MVC   BIGKEY,TRNRECD                                                   
         GOTO1 AREADD,0                                                         
         BNE   EXIT                                                             
*                                                                               
         LA    R2,BIGKEY                                                        
         L     R5,AIOAREA4                                                      
         MVC   TRSPSTA,TRNKSTA                                                  
         MVC   TRSPDA,TRNKDA                                                    
         MVC   BIGKEY,0(R5)                                                     
         GOTO1 AADDDIR,0           MUST BE ADDED USING NEW FILE                 
         BNE   EXIT                                                             
         MVC   BIGKEY,KEYSAVE      LOAD SAVED LOCKED PASSIVE POINTER            
         GOTO1 AUNLOCKD                                                         
*                                                                               
CHAR20   MVC   MSG,=CL60'ACTION COMPLETED - ENTER NEXT'                         
         B     OKEND                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
*        ROUTINES TO GET AN ELEMENT, ADD OR REMOVE AN ELEMENT         *         
*        0(R1) HAS IO ADDRESS FOR ADDELEM AND REMELEM                 *         
***********************************************************************         
                                                                                
GETELIO  L     R6,AIOAREA3                                                      
         GETEL (R6),DATADISP,ELCODE                                             
                                                                                
ADDELEM  NTR1                                                                   
         CLI   ELEMENT,0                                                        
         BE    OKEND                                                            
         L     R5,0(R1)                                                         
         GOTOR VHELLO,DMCB,(C'P',=C'ACCFIL'),(R5),ELEMENT,=C'ADD=CODE'          
         CLI   DMCB+12,0                                                        
         BE    OKXIT                                                            
         MVI   FERN,TOOLONG        DID RECORD GET TOO LONG                      
         CLI   DMCB+12,5                                                        
         BE    EXIT                                                             
         DC    H'0'                OTHER ERRORS UNACCEPTABLE                    
                                                                                
REMELEM  NTR1                                                                   
         L     R5,0(R1)                                                         
         XC    ELEMENT,ELEMENT                                                  
         BAS   RE,GETELIO                                                       
         BNE   OKEND                                                            
         ZIC   R1,1(R6)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELEMENT(0),0(R6)                                                 
         GOTOR VHELLO,DMCB,(C'D',=C'ACCFIL'),(ELCODE,(R5)),0                    
         B     OKEND                                                            
         EJECT                                                                  
*---------------------------------------------------------                      
*        EXITS TO ROOT                                                          
*---------------------------------------------------------                      
*                                                                               
OKEND    MVI   FERN,OK             OK - SAVE CURRENT WORKCODES                  
         MVC   LWCC1,APOWRKC                                                    
         MVC   LWCC2,APOWRKC+3                                                  
         MVC   LWCC3,APOWRKC+6                                                  
         MVC   LWCC4,APOWRKC+9                                                  
         MVC   LWCN1,APOWRKN                                                    
         MVC   LWCN2,APOWRKN+3                                                  
         MVC   LWCN3,APOWRKN+6                                                  
         MVC   LWCN4,APOWRKN+9                                                  
         B     EXIT                                                             
                                                                                
OKXIT    SR    RB,RB               CC = EQU                                     
ERRXIT   LTR   RB,RB               CC = NEQ                                     
EXIT     XIT1                                                                   
         EJECT                                                                  
*---------------------------------------------------------                      
*        LITERAL DECLARATIONS                                                   
*---------------------------------------------------------                      
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
DMRDHI   DC    CL8'DMRDHI'                                                      
ACCDIR   DC    CL8'ACCDIR'                                                      
         EJECT                                                                  
*---------------------------------------------------------                      
*        ACORDDSECT                                                             
*---------------------------------------------------------                      
*                                                                               
       ++INCLUDE ACORDDSECT                                                     
         EJECT                                                                  
*---------------------------------------------------------                      
*        OVERLAY WORKING STORAGE AT (ASAVE)                                     
*---------------------------------------------------------                      
*                                                                               
LOCALD   DSECT                                                                  
VALUE    DS    CL6                 PACKED VALUE                                 
COMMSW   DS    CL1                 COMMISSIONABLE SWITCH (SEE NONCOMM)          
STATUS   DS    CL1                 STATUS                                       
INVOICED DS    CL1                 Y OR N                                       
SAVE5    DS    F                                                                
DIRIO    DS    XL60                DIRECTORY IO                                 
*                                                                               
NEXTNUM  DS    CL6                 NEXT ORDER NUMBER (CHARACTER)                
NEXTBIN  DS    F                   NEXT ORDER NUMBER (BINARY)                   
XTCPTRBK DS    XL128               USED FOR PADDLE CPTRBLK CALLS                
SVDA     DS    XL4                 SAVED DISK ADDRESS                           
*                                                                               
ELEMENT  DS    CL256               NEW ELEMENT WORK AREA                        
OAMTELS  DS    CL(8*OAMLN2Q)       OLD ORDER AMOUNT ELS - 4XCOMM,4XNONC         
NAMTELS  DS    0CL(OAMLN2Q)        NEW ORDER AMOUNT ELS - DITTO                 
NAMTELSC DS    CL(4*OAMLN2Q)       (NULL = NO SUCH ELEMENT)                     
NAMTELSN DS    CL(4*OAMLN2Q)                                                    
*                                                                               
NGDAEL   DS    CL(GDALNQ)          NEW GENERAL DATE ELEMENT                     
*                                                                               
NCOMELS  DS    1000C               NEW COMMENT ELEMENTS                         
*                                                                               
TEXTWS   DS    1200C               W/S FOR TEXTED MODULE                        
*                                                                               
NONCOMM  EQU   X'80'               NON-COMMISSIONABLE (OAMSTAT)                 
MINOR    EQU   X'40'               MINOR (ACOASTAT)                             
*                                                                               
* ACGENBOTH                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'059ACORD01   10/17/18'                                      
         END                                                                    
