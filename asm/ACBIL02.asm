*          DATA SET ACBIL02    AT LEVEL 008 AS OF 12/17/12                      
*PHASE T60E02A                                                                  
*INCLUDE TEXTER                                                                 
         TITLE 'ACBIL02 - CREATIVE BILLING - ADD/CHA/EDIT/DEL/INS'              
ACBIL02  CSECT                                                                  
         PRINT NOGEN               ON ENTRY IOAREA1 CONTAINS EXISTING           
*                                  PARAGRAPH REC IF ANY                         
         NMOD1 LWSX-LWSD,**BIL2**,RA,RR=R5,CLEAR=YES                            
         LA    RA,2048(,RB)                                                     
         LA    RA,2048(,RA)                                                     
*                                                                               
         USING ACBIL02+4096,RA                                                  
         USING GWS,R9                                                           
         USING TWAD,R8                                                          
         USING LWSD,RC                                                          
*                                                                               
         ST    R5,PRELOC                                                        
         TM    SAVPSTAT,FOOTLINE                                                
         BNO   CLEAR                                                            
         MVI   FOOT,C'Y'           SET FOOT TO Y FOR EXISTING FOOTLINE          
         B     CLEAR                                                            
         EJECT ,                                                                
***********************************************************************         
* IF ACTION DELETE OR CHANGE, SUBTRACT PARAGRAPH AMOUNTS BEFORE       *         
* ALLOWING CHANGES                                                    *         
***********************************************************************         
         SPACE 1                                                                
CLEAR    ZAP   PNET,=P'0'                                                       
         ZAP   PCOM,=P'0'                                                       
         ZAP   PNON,=P'0'                                                       
         ZAP   PCSD,=P'0'                                                       
         ZAP   PGST,=P'0'                                                       
         ZAP   PPST,=P'0'                                                       
*                                                                               
         CLI   ACTION,CHA                                                       
         BE    CLEAR1                                                           
         CLI   ACTION,DEL                                                       
         BNE   INITIAL                                                          
         CLC   BILPARA,=C'ALL'                                                  
         BE    DELETE                                                           
*                                                                               
         USING PARAD,R5                                                         
         USING CHRD,RF                                                          
CLEAR1   L     R5,AIOAREA1                                                      
         LA    RF,CHRTOT                                                        
         SP    CHRBNET,PARANET                                                  
         SP    CHRBCOM,PARACOM                                                  
         SP    CHRBNON,PARANON                                                  
         SP    CHRBCSD,PARACSD                                                  
         SP    CHRBGST,PARAGST                                                  
         SP    CHRBPST,PARAPST                                                  
         DROP  R5,RF               KEEP IT   CLEAN                              
         EJECT ,                                                                
***********************************************************************         
* HANDLE DELETE 'ALL' AND DELETE A PARAGRAPH                          *         
***********************************************************************         
         SPACE 1                                                                
DELETE   CLI   ACTION,DEL                                                       
         BNE   INITIAL                                                          
         CLC   BILPARA,=C'ALL'                                                  
         BNE   DELETE2                                                          
         GOTO1 AWRKDEL,AWRKIO                                                   
         BNE   EXIT                                                             
*                                  BILL XXXXXX DELETED -                        
         MVC   FVMSGNO,=AL2(97)      ENTER NEXT ACTION                          
         MVI   FVMTYPE,FVMINFO                                                  
         MVC   XTRAMESS(6),BILNUM                                               
         B     OKEND                                                            
*                                                                               
DELETE2  MVC   WRKPARA,PARAHEX     SINGLE PARA                                  
         GOTO1 AWRKLDEL,AWRKIO                                                  
         BNE   EXIT                                                             
         ZIC   R2,WRKLPARA         ADD 1 TO TOTAL PARAS FOR END MESSAGE         
         LA    R2,1(,R2)                                                        
         STC   R2,WRKLPARA                                                      
         GOTO1 APARNOFM,MSG                                                     
         BCTR  R2,0                                                             
         STC   R2,WRKLPARA                                                      
         MVI   PARAHEX,0                                                        
*                                                                               
         LR    R2,R1               ->   WHERE TO START THE MESSAGE              
         LA    R3,MSG+L'MSG        ->   JUST  AFTER    THE MESSAGE              
         SR    R3,R2               GET  AVAILABLE SPACE IN MSG                  
*                                  DELETED - ENTER NEXT ACTION                  
         GOTO1 ATXTGET,DMCB,('FVMINFO',98),((R3),(R2)),0,0                      
*                                                                               
         CLI   FOOT,C'Y'           IF IT WAS A FOOTLINE                         
         BNE   DELETE3             CLEAR FOOTLINE BIT IN HEADER STATUS          
         MVI   WRKPARA,0                                                        
         GOTO1 AWRKLGTL,AWRKIO                                                  
         BNE   EXIT                                                             
*                                                                               
         USING HEADERD,R5                                                       
         L     R5,AWRKIO                                                        
         NI    HSTAT,X'FF'-FOOTLINE                                             
         GOTO1 AWRKLPUT                                                         
         BNE   EXIT                                                             
         B     OKEND                                                            
         DROP  R5                                                               
*                                                                               
DELETE3  GOTO1 ADISTOT                                                          
         B     OKEND                                                            
         EJECT ,                                                                
***********************************************************************         
* BUILD BASIC BILL PARAGRAPH RECORD IN IOAREA3                        *         
***********************************************************************         
         SPACE 1                                                                
INITIAL  SR    R2,R2               OR SPACES INTO SPECIFIC SCREEN FLDS          
         LA    R1,BILNUMH                                                       
         BAS   RE,UPCASE                                                        
         LA    R1,BILPARAH                                                      
         BAS   RE,UPCASE                                                        
         LA    R1,BILCLIH                                                       
         BAS   RE,UPCASE                                                        
         LA    R1,BILPROH                                                       
         BAS   RE,UPCASE                                                        
         LA    R1,BILJOBH                                                       
         BAS   RE,UPCASE                                                        
         LA    R1,BLPWRKH                                                       
         BAS   RE,UPCASE                                                        
         CLI   PFUPPER,C'M'        MIXED    CASE  REPORT ?                      
         BE    INIT5               NO,      SKIP  OVER SUBROUTINE               
         LA    R1,BLPFRSTH         ->       FIRST FLD  HEADER                   
         LA    R5,BLPFINLH         ->       LAST  FLD  HEADER                   
*                                                                               
INIT1    BAS   RE,UPCASE           UPPER    CASE  IT                            
         CR    R1,R5               CURRENT  FIELD PAST LAST FIELD ?             
         BNH   INIT1               NO,      UPPER CASE IT                       
*                                                                               
         USING PARAD,R5                                                         
INIT5    L     R5,AIOAREA3         BUILD SKELETON PARA REC                      
         CLI   ACTION,EDIT                                                      
         BE    INIT15                                                           
         XC    PARAD(PARATXT-PARAD),PARAD                                       
         MVI   PARAEL,1                                                         
         MVI   PARALEN,PARALNQ                                                  
         MVI   PARATYPE,C'B'                                                    
         ZAP   PARANET,=P'0'                                                    
         ZAP   PARACOM,=P'0'                                                    
         ZAP   PARANON,=P'0'                                                    
         ZAP   PARACSD,=P'0'                                                    
         ZAP   PARAGST,=P'0'                                                    
         ZAP   PARAPST,=P'0'                                                    
         CLI   FOOT,C'Y'                                                        
         BNE   *+8                                                              
         OI    PARASTAT,FOOTLINE                                                
         MVI   PARATXT,0                                                        
         LA    R1,PARATXT+1                                                     
         SR    R1,R5                                                            
         STH   R1,PARARLEN                                                      
         B     VALCLI                                                           
         DROP  R5                                                               
*                                                                               
INIT15   DS    0H                  USE TEXTER TO MODIFY TWA                     
         BNE   VALCLI                                                           
         GOTO1 =V(TEXTER),DMCB,BILACTH,(10,BLPFRSTH),TEXTWS,VSCANNER,  X        
               RR=PRELOC                                                        
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
         EJECT ,                                                                
***********************************************************************         
* CHECK FOR NO CHANGE TO CLI/PRO/JOB/SUP ON ACTION CHANGE             *         
***********************************************************************         
         SPACE 1                                                                
VALCLI   LA    R1,BILCLIH                                                       
         CLC   LCLI,8(R1)                                                       
         BNE   VALCLERR                                                         
         LA    R1,BILPROH                                                       
         CLC   LPRO,8(R1)                                                       
         BNE   VALCLERR                                                         
         LA    R1,BILJOBH                                                       
         CLC   LJOB,8(R1)                                                       
         BE    VALWC                                                            
*                                                                               
VALCLERR ST    R1,FADR                                                          
         MVI   FERN,CANTAMND                                                    
         B     EXIT                                                             
         EJECT ,                                                                
***********************************************************************         
* CHECK WORK CODE LINE / GET TOTALS FOR PARAGRAPH                     *         
*   FORMAT IS: AA=50N,AB,AC=10C,AC=20N,BB=N,DD=C                      *         
***********************************************************************         
         SPACE 1                                                                
         USING PARAD,R5                                                         
         SPACE 1                                                                
VALWC    MVC   PARAWRK,SPACES                                                   
         GOTO1 AFVAL,BLPWRKH                                                    
         BE    VALNAR              NOT REQUIRED IF JUST NARRATIVE               
         CLC   FLD(2),=C', '       CONTINUATION ONLY ?                          
         BE    VALNAR              ONLY NARRATIVE                               
         CLC   FLD(2),=C'; '       CONTINUATION ONLY ?                          
         BE    VALNAR              ONLY NARRATIVE                               
         MVI   FERN,INVALID                                                     
         CLI   FOOT,C'Y'           NOT ALLOWED ON FOOTLINES                     
         BE    EXIT                                                             
         MVC   PARAWRK,FLD                                                      
         LA    RF,L'PARAWRK                                                     
         GOTO1 AWKDCODE,DMCB,((RF),PARAWRK)                                     
         TM    SAVHSTAT,MANBILL    IF MANUAL BILL                               
         BO    VALWC03             DON'T GET CHARGES                            
         GOTO1 APARCHR,DMCB        GET CHARGES FOR PARAGRAPH                    
*                                                                               
VALWC03  ZAP   PARANET,PNET        COMMISSIONABLE                               
         ZAP   PARACOM,PCOM        COMMISSION                                   
         AP    PARANON,PNON        NON-COMMISSIONABLE                           
         ZAP   PARACSD,PCSD        CASH DISCOUNT                                
         ZAP   PARAGST,PGST        GST AMOUNT                                   
         ZAP   PARAPST,PPST        PST AMOUNT                                   
         TM    SAVHSTAT,MANBILL                                                 
         BNO   VALWC06                                                          
*                                                                               
         USING CHRD,RF                                                          
         LA    RF,CHRTOT                                                        
         AP    CHRBNET,PNET        IF MANUAL MUST ADD TO BILL TOTALS            
         AP    CHRBCOM,PCOM                                                     
         AP    CHRBNON,PNON                                                     
         AP    CHRBCSD,PCSD                                                     
         AP    CHRBGST,PGST                                                     
         AP    CHRBPST,PPST                                                     
         DROP  RF                                                               
*                                                                               
VALWC06  GOTO1 ADISTOT             PARAGRAPH AND BILL TOTAL                     
         DROP  R5                                                               
         EJECT ,                                                                
***********************************************************************         
* VALIDATE NARRATIVE AND BUILD NEW COMMENT ELEMENTS IN NCOMELS        *         
***********************************************************************         
         SPACE 1                                                                
         USING PARAD,R5                                                         
         SPACE 1                                                                
VALNAR   DS    0H                                                               
         CLI   PFUPPER,C'M'        MIXED     CASE DATA ?                        
         BE    VALNAR0             YES, SKIP                                    
         LA    R1,BLPDSCH          ->   PARAGRAPH DESCRIPTION                   
         BAS   RE,UPCASE           TRANSLATE TO   UPPER     CASE                
*                                                                               
VALNAR0  DS    0H                                                               
         MVC   PARADSC,SPACES      CLEAR     PARAGRAPH      DESCRIPTION         
*                                  GET  SCREEN    FIELD     LENGTH              
         ZIC   R4,BLPDSCH+FLDILEN-FLDHDRD                                       
         SH    R4,=H'1'            MINUS     ONE  FOR  EXECUTE                  
         BM    VALNAR1             NONE,     SKIP                               
         EXMVC R4,PARADSC,BLPDSC   PARAGRAPH DESCRIPTION    CASE                
         DROP  R5                                                               
*                                                                               
VALNAR1  DS    0H                                                               
         LA    R3,BLPFRSTH         R3/4/5 =  BXLE REGISTERS                     
         SR    R4,R4                                                            
         LA    R5,BLPFINLH                                                      
         SR    R2,R2               R2 = COMMENT   SEQUENCE NO.  SCMSEQ          
*                                                                               
         USING SCMELD,R6                                                        
         LA    R6,NCOMELS          R6 = NEW  COMMENT   EL  POINTER              
         MVI   NAREQCHG,C'N'       NARRATIVE WITH EQUALS   NO   CHANGES         
         MVI   NAREQPAS,1          NARRATIVE WITH EQUALS   PASS NUMBER          
*                                                                               
VALNAR2  DS    0H                  LOOP FOR  A    LINE                          
         ST    R2,SAVER2           SAVE R2                                      
         ST    R6,SAVER6           SAVE R6                                      
         GOTO1 AFVAL,(R3)          ANY  DATA ?                                  
         BZ    VALNAR30            NO,  NEXT LINE                               
         ZIC   RF,0(,R3)           DROP TRAILING SPACES FROM FLDILEN            
         SH    RF,=H'8'                                                         
         TM    1(R3),X'02'         EXTENDED HEADER                              
         BZ    VALNAR2A                                                         
         SH    RF,=H'8'            RF   HAS  THE  DATA LENGTH                   
*                                                                               
VALNAR2A DS    0H                  RF   HAS  THE  LENGTH                        
         LA    RE,7(RF,R3)         FIND ACTUAL    DATA LENGTH                   
         CLI   0(RE),C' '                                                       
         BH    VALNAR2B                                                         
         BCT   RF,VALNAR2A                                                      
         B     VALNAR30            ONLY SPACES,   NEXT LINE                     
*                                                                               
VALNAR2B DS    0H                  TEST FOR  SPECIAL   CASES                    
         STC   RF,5(,R3)           SAVE ACTUAL    DATA LENGTH                   
*                                                                               
         CLC   FLD(2),=C'N='       UPPER     CASE N    EQUALS ?                 
         BE    VALNAR3             YES, OKAY                                    
         CLC   FLD(2),=X'957E'     LOWER     CASE N    EQUALS ?                 
         BNE   VALNAR10            NO,  TRY  NEXT SPECIAL   CASE                
         B     VALNARUP            YES, UPPER     CASE THE  LINE                
*                                                                               
VALNAR3  DS    0H                  N=   CASE                                    
         XC    KEY,KEY             HANDLE    N=   STRING                        
         MVI   KEY,X'0C'                                                        
         MVC   KEY+1(1),COMPANY                                                 
*                                  NOTES:                                       
*                                  1.   THE  SPEC REQUIRES  US   TO             
*                                       SUPPORT   ONLY EIGHT     N=             
*                                       STRINGS   ON   ONE  OR   MORE           
*                                       CONSECUTIVE    LINES     ON             
*                                       THE  SCREEN.   'TEMPOV'  ALLOWS         
*                                       US   TO   PROCESS   ALL  OF             
*                                       THE  REST THAT MAY  BE   ON             
*                                       THE  LINE.                              
*                                  2.   WE   STILL     CAN  NOT                 
*                                       SUPPORT   MORE THAN A    TOTAL          
*                                       OF   16   N=   STRINGS   ON             
*                                       CONSECUTIVE LINES;  BUT                 
*                                       THERE     IS   NO   REQUIREMENT         
*                                       FOR  US   TO   CHECK     FOR            
*                                       THIS CASE AT   THIS TIME.               
         GOTO1 VSCANNER,DMCB,(R3),(17,TEMP)                                     
         ZIC   R0,DMCB+4           GET  NUMBER    OF   FIELDS                   
         MVI   FERN,TOOMANY        TOO  MANY INPUT     FIELDS                   
         MVI   FNDX,17             NO.  OF   TOO  MANY                          
         CH    R0,=H'16'           >    16   INPUTS    ON   1    LINE ?         
         BH    EXIT                YES, INVALID                                 
         MVI   FNDX,0                                                           
         MVI   FERN,INVALID        INVALID   INPUT     FIELD                    
         CH    R0,=H'1'                                                         
         BL    EXIT                NO   DATA,     EXIT                          
         BE    *+8                                                              
         MVI   FNDX,1              MORE THAN ONE  INPUT                         
         LA    R7,TEMP                                                          
*                                                                               
VALNAR5  DS    0H                  LOOP FOR  AN   ENTRY                         
         CLI   0(R7),1             ONE  CHAR TO   LEFT OF   SIGN ?              
         BNE   VALNAR6             NO,  INVALID                                 
         CLI   12(R7),C'N'         FOUND     UPPER     CASE N= ?                
         BE    VALNAR5A            YES, CONTINUE                                
         CLI   12(R7),X'95'        FOUND     LOWER     CASE N= ?                
         BNE   VALNAR6             NO,  INVALID                                 
         B     VALNARUP            YES, UPPER     CASE THE  LINE                
*                                                                               
VALNAR5A DS    0H                  LOOP FOR  AN   ENTRY                         
         CLI   1(R7),1             LENGTH    OF   INPUT                         
         BL    VALNAR6                  MUST BE                                 
         CLI   1(R7),6                       1    THRU 6                        
         BNH   VALNAR7                            ELSE INVALID                  
*                                                                               
VALNAR6  DS    0H                  INVALID   N=   CASE                          
         MVI   FERN,INVALID        INVALID   INPUT     FIELD                    
         MVC   XTRAMESS(7),=C'(N=123456)'                                       
         B     EXIT                                                             
*                                                                               
VALNAR7  DS    0H                  RIGHT-ALIGN    NUMBER    IN   VALUE          
         MVC   VALUE,SPACES                                                     
         LA    RF,VALUE+5          ->   LAST BYTE IN   VALUE                    
         ZIC   RE,1(,R7)           GET  LENGTH                                  
         BCTR  RE,0                MINUS     ONE                                
         SR    RF,RE               FIND 1ST  BYTE OF   VALUE                    
         EXMVC RE,0(RF),22(R7)     INSERT    DATA INTO VALUE                    
         MVC   KEY+2(6),VALUE      MOVE THE  DATA INTO KEY                      
         GOTO1 AREAD,AIOAREA2      CHECK     IF   COMMENT   EXISTS              
         BE    VALNAR8             YES, CONTINUE                                
*                                                                               
         CLI   NAREQPAS,1          1ST  PASS ?                                  
*                                  YES, THE  N=   MAY  HAVE BEEN IN             
*                                       UPPER     CASE,     BUT  THE            
*                                       VALUE     MAY  HAVE BEEN IN             
*                                       LOWER     CASE,     SO   TRY            
         BE    VALNARUP                 TO   UPPER     CASE THE  LINE           
*                                                                               
         MVI   FERN,INVALID        NO,  INVALID   INPUT     FIELD               
         MVC   XTRAMESS(6),VALUE   INDICATE  THE  BAD  DATA                     
         B     EXIT                EXIT                                         
*                                                                               
VALNAR8  DS    0H                  BUILD     AN   ELEMENT                       
         MVI   SCMEL,SCMELQ        X'3E' - STANDARD    COMMENT   EL             
         MVI   SCMLN,10                                                         
         STC   R2,SCMSEQ                                                        
         MVI   SCMTYPE,4           INDICATE  COMMENT   CODE                     
         MVC   SCMNARR(6),VALUE                                                 
         LA    R6,10(,R6)          BUMP ELEMENT POINTER                         
         LA    R2,1(,R2)           BUMP SEQUENCE NO                             
         CLI   FNDX,0                                                           
         BE    VALNAR30                                                         
         ZIC   R1,FNDX             BUMP TO NEXT SCANNER ENTRY                   
         LA    R1,1(,R1)                                                        
         STC   R1,FNDX                                                          
         LA    R7,32(,R7)                                                       
         BCT   R0,VALNAR5                                                       
         B     VALNAR30                                                         
*                                                                               
VALNARUP DS    0H                  UPPER     CASE THE  N=   LINE                
         LR    R1,R3               ->   FIELD     HEADER                        
         BAS   RE,UPCASE           TRANSLATE LINE TO   UPPER     CASE           
         MVI   NAREQCHG,C'Y'       SAY  NARRATIVE WITH =    CHANGED             
         MVI   NAREQPAS,2          SAY  2ND  PASS OF   N=                       
         L     R2,SAVER2           RESTORE   R2                                 
         L     R6,SAVER6           RESTORE   R6                                 
         B     VALNAR2             REDO THIS LINE                               
*                                                                               
VALNAR10 DS    0H                  HANDLE    S=N (SPACE     1-9  LINES)         
         CLC   FLD(2),=C'S='       UPPER     CASE S    EQUALS ?                 
         BE    VALNAR13            YES, OKAY                                    
         CLC   FLD(2),=X'A27E'     LOWER     CASE S    EQUALS ?                 
         BNE   VALNAR20            NO,  CONTINUE  STANDARD  PROCESSING          
         MVI   FLD,C'S'            FIX  FIELD                                   
         MVI   8(R3),C'S'          FIX  DISPLAY   TO   UPPER CASE               
         OI    6(R3),FOUTTRN       TRANSMIT  THE  FIELD                         
         MVI   NAREQCHG,C'Y'       SAY  NARRATIVE WITH EQUALS   CHANGED         
*                                                                               
VALNAR13 DS    0H                                                               
         MVI   FERN,TOOLONG        CHECK ITS NUMERIC 01-09 (1-9)                
         CLI   5(R3),4                                                          
         BH    EXIT                                                             
         ZIC   R1,5(,R3)                                                        
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
*                                                                               
VALNARER DS    0H                  INVALID   S=   CASE                          
         MVC   XTRAMESS(7),=C'(S=1-9)'                                          
         B     EXIT                ERROR                                        
*                                                                               
VALNARMV MVZ   WORK(0),FLD+2                                                    
VALNARPK PACK  DUB,FLD+2(0)                                                     
*                                                                               
VALNAR20 DS    0H                  BUILD     AN   ELEMENT                       
         MVI   SCMEL,SCMELQ        X'3E' -   STANDARD  COMMENT   EL             
         STC   R2,SCMSEQ                                                        
         ZIC   RE,5(,R3)                                                        
         BCTR  RE,0                                                             
         EXMVC RE,SCMNARR,FLD                                                   
         LA    RE,5(,RE)                                                        
         STC   RE,SCMLN                                                         
         AR    R6,RE               BUMP ELEMENT POINTER                         
         LA    R2,1(,R2)           BUMP SEQUENCE NO                             
*                                                                               
VALNAR30 DS    0H                  BUMP TO   NEXT UNPROT   (NEXT LINE)          
         IC    R4,0(,R3)                                                        
         BXLE  R3,R4,*+8                                                        
         B     VALNARX                                                          
*                                                                               
         TM    1(R3),X'20'                                                      
         BO    VALNAR30                                                         
         B     VALNAR2                                                          
*                                                                               
VALNARX  DS    0H                  AT   END                                     
         MVI   FNDX,0              CHECK     FOR  ANY  INPUT                    
         CLI   ACTION,EDIT                                                      
         BE    VALNARX1                                                         
         B     NEWREC                                                           
*                                                                               
VALNARX1 DS    0H                  IF   EDIT, JUST ASK USER TO                  
         LA    R1,BLPJOBTH                    SIGHT    CHECK                    
         ST    R1,FADR                                                          
         XC    BILACT,BILACT                                                    
         OI    BILACTH+6,X'81'     MODIFY                                       
         MVC   BILACT(6),=C'CHANGE'                                             
*                                  EDITED NARRATIVE WILL BE AS SHOWN -          
         MVC   FVMSGNO,=AL2(77)      HIT ENTER IF AS REQUIRED                   
         MVI   FVMTYPE,FVMINFO                                                  
         B     OKEND                                                            
         DROP  R6                                                               
         EJECT ,                                                                
***********************************************************************         
* COMPLETE NEW BILL RECORD IN IOAREA3                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING PARAD,R5                                                         
         SPACE 1                                                                
NEWREC   L     R5,AIOAREA3                                                      
         MVI   FERN,OK                                                          
         LA    R6,PARATXT                                                       
         LA    R7,NCOMELS          ADD COMMENT ELS                              
         SR    R1,R1                                                            
*                                                                               
NEWREC2  CLI   0(R7),0                                                          
         BE    NEWREC4                                                          
         IC    R1,1(,R7)                                                        
         BCTR  R1,0                                                             
         EXMVC R1,0(R6),0(R7)                                                   
         LA    R1,1(,R1)                                                        
         AR    R6,R1                                                            
         AR    R7,R1                                                            
         B     NEWREC2                                                          
*                                                                               
NEWREC4  MVI   0(R6),0             TERMINATE AND INSERT LENGTH                  
         LA    R6,1(,R6)                                                        
         SR    R6,R5                                                            
         STH   R6,PARARLEN                                                      
         LA    R1,BILACTH                                                       
         ST    R1,FADR                                                          
         TM    ACTINDS,NEWPARA                                                  
         BO    ADDREC0                                                          
         B     CHAREC                                                           
         DROP  R5                                                               
         EJECT ,                                                                
***********************************************************************         
* ADD NEW PARA TO WORKER LIBRARY BOOK                                 *         
***********************************************************************         
         SPACE 1                                                                
ADDREC0  CLI   ACTION,NEWBILL      ADDING A NEW BOOK                            
         BNE   ADDREC20                                                         
*                                                                               
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         GOTO1 AREADL,AIOAREA2      GET COMPANY RECORD                          
         BNE   EXIT                                                             
*                                                                               
ADDREC1  L     RE,AIOAREA2                                                      
         AH    RE,DATADISP                                                      
         SR    RF,RF                                                            
*                                                                               
ADDREC2  CLI   0(RE),0             LOCATE COMPANY ELEMENT                       
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   0(RE),X'10'                                                      
         BE    ADDREC3                                                          
         IC    RF,1(,RE)                                                        
         AR    RE,RF                                                            
         B     ADDREC2                                                          
*                                                                               
         USING CPYELD,RE                                                        
ADDREC3  OC    CPYSBILL,CPYSBILL   GET BILL NUMBER                              
         BNZ   *+10                                                             
         ZAP   CPYSBILL,=P'0'       FIRST CLIENT BILL                           
         AP    CPYSBILL,=P'1'                                                   
         ZAP   BILNUMP,CPYSBILL    SAVE FOR DISPLAY                             
         EDIT  BILNUMP,(6,BILNUM),ALIGN=LEFT                                    
         OI    BILNUMH+6,FVOXMT    TRANSMIT                                     
         GOTO1 AWRITE,AIOAREA                                                   
         BNE   EXIT                                                             
         DROP  RE                                                               
*                                                                               
ADDREC8  GOTO1 AWRKID              GET WORKER ID INTO WRKEY                     
         GOTO1 AFINDBIL,AIOAREA1   IS IT ALREADY IN USE                         
         BE    ADDREC1             IF SO TRY AGAIN                              
         GOTO1 VDATAMGR,DMCB,DMUNLK,ACCOUNT,AIOAREA                             
         MVI   WRKLPARA,0                                                       
*                                                                               
         USING UKRECD,RF                                                        
ADDREC10 LA    RF,WRKEY            NOW ADD A NEW BOOK                           
         L     R7,AIOAREA1                                                      
*                                                                               
         USING WKRECD,R7                                                        
         LA    R7,28(,R7)                                                       
         MVI   UKFLAG,X'1A'        SET RETN/SET COMMENT/LIBRARY-TYPE            
         XC    WKCOMNT,WKCOMNT                                                  
         MVC   WKCOMNT(L'JOBKEY-3),JOBKEY+3                                     
         MVC   WKRETN,=H'7'                                                     
         GOTO1 AWRKOPE,AIOAREA1                                                 
         BE    ADDREC12                                                         
         TM    DMCB+8,X'20'        DUPLICATE                                    
         BZ    EXIT                                                             
         CLI   FLAG,C'A'                                                        
         BE    ADDREC1             TRY AGAIN IF AUTO                            
         B     EXIT                                                             
         DROP  R7,RF                                                            
*                                                                               
         USING HEADERD,R7                                                       
ADDREC12 L     R7,AIOAREA1         ADD HEADER REC                               
         XC    0(HEND,R7),0(R7)                                                 
         MVI   HTYPE,C'A'                                                       
         MVC   HCLI,BILCLI                                                      
         OC    HCLI,SPACES                                                      
         MVC   HCLIN,BILCLIN                                                    
         MVC   HPRO,BILPRO                                                      
         OC    HPRO,SPACES                                                      
         MVC   HPRON,BILPRON                                                    
         MVC   HJOB,BILJOB                                                      
         OC    HJOB,SPACES                                                      
         MVC   HJOBN,BILJOBN                                                    
         MVI   HBILL,C'N'          NOT YET BILLED                               
         MVC   HINVNUM,SPACES                                                   
         CLI   FOOT,C'Y'                                                        
         BNE   *+8                                                              
         OI    HSTAT,FOOTLINE                                                   
         CLI   MANB,C'Y'                                                        
         BNE   *+8                                                              
         OI    HSTAT,MANBILL                                                    
         MVC   HPLANG,PLANG        SAVE PRINT LANGUAGE                          
         CLI   PLANG,0             ANY  PRINT     LANGUAGE ?                    
         BNE   *+10                YES, SKIP                                    
         MVC   HPLANG,CULANG       USE  CURRENT   LANGUAGE                      
         LA    R0,HEND                                                          
         STH   R0,HLEN                                                          
         GOTO1 AWRKADD,AIOAREA1                                                 
         BNE   EXIT                                                             
         GOTO1 AWRKCLO                                                          
         BNE   EXIT                                                             
*                                                                               
ADDREC20 MVC   WRKPARA,PARAHEX     ADD PARA REC                                 
         CLI   ACTION,INS          IS THIS AN INSERT                            
         BNE   ADDREC22            NO                                           
*                                                                               
         USING PARAD,R5                                                         
         L     R5,AIOAREA3         MAKE SURE FOOT BYTE IS TURNED OFF            
         NI    PARASTAT,X'FF'-FOOTLINE                                          
*                                                                               
ADDREC22 GOTO1 AWRKLADD,AIOAREA3                                                
         BNE   EXIT                                                             
*                                                                               
ADDREC25 CLI   ACTION,NEWBILL      MARK HEADER AS CONTAINING FOOTLINE           
         BE    ADDREC30                                                         
         CLI   FOOT,C'Y'                                                        
         BNE   ADDREC30                                                         
         MVI   WRKPARA,0                                                        
         GOTO1 AWRKLGTL,AIOAREA1                                                
         BNE   EXIT                                                             
*                                                                               
         L     R7,AIOAREA1                                                      
         OI    HSTAT,FOOTLINE                                                   
         GOTO1 AWRKLPUT                                                         
         DROP  R7                                                               
*                                                                               
ADDREC30 CLI   ACTION,INS          END MESSAGE - INSERT                         
         BNE   ADDREC35                                                         
         GOTO1 APARNOFM,MSG                                                     
*                                                                               
         LR    R2,R1               ->   WHERE TO START THE MESSAGE              
         LA    R3,MSG+L'MSG        ->   JUST  AFTER    THE MESSAGE              
         SR    R3,R2               GET  AVAILABLE SPACE IN MSG                  
*                                  INSERTED - ENTER NEXT ACTION                 
         GOTO1 ATXTGET,DMCB,('FVMINFO',100),((R3),(R2)),0,0                     
*                                                                               
         B     OKEND                                                            
*                                                                               
ADDREC35 DS    0H                  END MESSAGE - ADD                            
         EDIT  PARAHEX,(2,MYTEMP),FILL=0                                        
*                                  PARAGRAPH NN ADDED - ENTER NEXT              
         GOTO1 ATXTGET,DMCB,('FVMINFO',2114),(L'MSG,MSG),(2,MYTEMP),0           
         CLI   PARAHEX,35                                                       
         BE    ADDREC36                                                         
         CLI   FOOT,C'Y'                                                        
         BE    ADDREC38                                                         
         LA    R0,BLPWRKH                                                       
         ST    R0,FADR                                                          
         CLC   BILACT(8),=C'ADD,NEXT'                                           
         BE    OKEND                                                            
         XC    BILACT,BILACT                                                    
         MVC   BILACT(8),=C'ADD,NEXT'                                           
         OI    BILACTH+6,FVOXMT    TRANSMIT                                     
         B     OKEND                                                            
         DROP  R5                                                               
*                                                                               
ADDREC36 LA    R1,L'MSG            ->   MSG  FIELD                              
         LA    R2,MSG                                                           
         BAS   RE,FINDFREE         FIND 1ST  AVAILABLE BYTE                     
         LA    R2,1(,R2)                                                        
         LA    R3,MSG+L'MSG        ->   JUST  AFTER    THE MESSAGE              
         SR    R3,R2               GET  AVAILABLE SPACE IN MSG                  
*                                  ACTION (BOOK FULL)                           
         GOTO1 ATXTGET,DMCB,('FVMERR',2223),((R3),(R2)),0,0                     
*                                                                               
         B     OKEND                                                            
*                                                                               
ADDREC38 LA    R1,L'MSG            ->   MSG  FIELD                              
         LA    R2,MSG                                                           
         BAS   RE,FINDFREE         FIND 1ST  AVAILABLE BYTE                     
         MVCDD 1(6,R2),AC#ACT      C'ACTION'                                    
         LA    R1,1(,R2)                                                        
         BAS   RE,CALLDICT                                                      
         B     OKEND                                                            
         EJECT ,                                                                
***********************************************************************         
* WRITE BACK CHANGED/EDITED PARA TO WORKER FILE                       *         
***********************************************************************         
         SPACE 1                                                                
CHAREC   DS    0H                  TEST FOR  ANY  CHANGE                        
         L     RE,AIOAREA3                                                      
         LH    RF,0(,RE)                                                        
         L     R2,AIOAREA1                                                      
         LH    R3,0(,R2)                                                        
         MVC   WRKPARA,PARAHEX                                                  
*                                                                               
         CR    RF,R3               ANY  LENGTH    CHANGE ?                      
         BNE   CHAREC4             YES, DEL  PLUS ADD                           
         TM    WRKSTAT2,WRKFIXED   WAS  WORKER    FILE CONVERTED ?              
         BO    CHAREC4             YES, DEL  PLUS ADD                           
         CLCL  RE,R2               ANY  DATA CHANGE ?                           
         BNE   CHAREC2             YES, UPDATE    RECORD                        
         CLI   NAREQCHG,C'Y'       ANY  NARRATIVE WITH =    CHANGED ?           
         BE    CHAREC2             YES, UPDATE    RECORD                        
         MVI   FERN,NOCHANGE                                                    
         B     EXIT                                                             
*                                                                               
CHAREC2  DS    0H                  UPDATE    THE  RECORD                        
         GOTO1 AWRKLPUT,AIOAREA3                                                
         BNE   EXIT                                                             
         B     CHAREC6                                                          
*                                                                               
CHAREC4  DS    0H                  REC  LEN  CHANGE    ENTAILS                  
         GOTO1 AWRKLDEL,AIOAREA3        DELETE    PLUS                          
         BNE   EXIT                                                             
         GOTO1 AWRKLADD                                                         
         BNE   EXIT                     ADD                                     
*                                                                               
CHAREC6  DS    0H                                                               
         GOTO1 APARNOFM,MSG                                                     
*                                                                               
         LR    R2,R1               ->   WHERE TO START THE MESSAGE              
         LA    R3,MSG+L'MSG        ->   JUST  AFTER    THE MESSAGE              
         SR    R3,R2               GET  AVAILABLE SPACE IN MSG                  
*                                  CHANGED - ENTER NEXT ACTION                  
         GOTO1 ATXTGET,DMCB,('FVMINFO',102),((R3),(R2)),0,0                     
*                                                                               
         B     OKEND                                                            
         EJECT ,                                                                
***********************************************************************         
* UPPER CASE A FIELD                                                  *         
*                                                                     *         
*        INPUT:                                                       *         
*          R1       - ADDRESS OF FIELD HEADER                         *         
*                                                                     *         
*        OUTPUT:                                                      *         
*          R1       - ADDRESS OF NEXT  FIELD HEADER                   *         
***********************************************************************         
         SPACE 1                                                                
         USING FLDHDRD,R3          MAP      FIELD HEADER                        
         USING FACTSD,R4           MAP      GETFACT    BLOCK                    
         SPACE 1                                                                
UPCASE   NTR1                                                                   
         LR    R3,R1               ->       FIELD HEADER                        
         L     R5,AUPPERCT         ->   UPPER     CASE TABLE                    
         ZIC   R2,FLDLEN           GET      FIELD LENGTH                        
         SH    R2,=H'9'            GET      DATA  LENGTH    -    1              
         TM    FLDATB,FATBXHDR     EXTENDED FIELD HEADER ?                      
         BZ    *+8                 NO,      SKIP                                
         SH    R2,=H'8'            LEAVE    EXTENDED   HEADER    FLD            
         EX    R2,UPCASETR         UPPER    CASE  IT                            
         LA    RE,1(,R2)           GET      NO.   OF   BYTES                    
         LA    RF,FLDDATA          ->       DATA  AREA                          
*                                                                               
UPCASE10 DS    0H                  BLANK    OUT   NULL BYTES                    
         CLI   0(RF),X'00'         NULL     BYTE ?                              
         BNE   *+8                 NO,      SKIP                                
         MVI   0(RF),X'40'         INSERT   SPACE                               
         LA    RF,1(,RF)           ->       NEXT  BYTE                          
         BCT   RE,UPCASE10         TEST     NEXT  BYTE                          
*                                                                               
         OI    FLDOIND,FOUTTRN     TRANSMIT THE   FIELD                         
         IC    R2,FLDLEN           GET      FIELD LENGTH                        
         LA    R1,0(R2,R3)         ->       NEXT  FLD  HEADER                   
         XIT1  REGS=(R1)           RETURN   TO    CALLER                        
*                                                                               
UPCASETR TR    FLDDATA(0),0(R5)    TRANSLATE      TO   UPPER     CASE           
*                                                                               
         DROP  R3,R4               KEEP     IT    CLEAN                         
         EJECT ,                                                                
***********************************************************************         
* CALL DICTATE                                                        *         
*                                                                     *         
* INPUT:                                                              *         
*   R1   = ADDRESS OF FIELD TO BE TRANSLATED                          *         
*          NOTE: THE FIELD MUST HAVE BEEN PREVIOUSLY INITIALIZED WITH *         
*                AN MVCDD INSTRUCTION.                                *         
*                                                                     *         
* USES:                                                               *         
*   DMCB = PARM AREA                                                  *         
***********************************************************************         
         SPACE 1                                                                
CALLDICT DS    0H                                                               
         ST    R6,SAVER6           SAVE     R6                                  
         ST    RE,SAVERE           SAVE     RE                                  
         LR    R6,R1                                                            
         GOTO1 VDICTAT,DMCB,C'SL  ',(R6),0                                      
         LR    R1,R6               RESTORE  R1                                  
         L     R6,SAVER6           RESTORE  R6                                  
         L     RE,SAVERE           RESTORE  RE                                  
         BSM   0,RE                RETURN                                       
         EJECT ,                                                                
***********************************************************************         
* FIND FIRST AVAILABLE (BYTE)                                         *         
*                                                                     *         
*   INPUT:                                                            *         
*     R1 = NUMBER  OF BYTES TO CONSIDER                               *         
*     R2 = ADDRESS OF FIELD                                           *         
***********************************************************************         
         SPACE 1                                                                
FINDFREE AR    R2,R1               FIND LAST BYTE IN   FIELD                    
         BCTR  R2,0                                                             
*                                                                               
FINDFR10 CLI   0(R2),C' '          FIND LAST CHARACTER                          
         BH    FINDFR20                                                         
         BCTR  R2,0                                                             
         BCT   R1,FINDFR10                                                      
*                                                                               
FINDFR20 LA    R2,1(,R2)           1ST  FREE BYTE                               
         BSM   0,RE                RETURN    TO   CALLER                        
         EJECT ,                                                                
***********************************************************************         
* EXITS TO ROOT                                                       *         
***********************************************************************         
         SPACE 1                                                                
OKEND    MVI   FERN,OK                                                          
         B     EXIT                                                             
*                                                                               
OKXIT    SR    RB,RB               CC = EQU                                     
*                                                                               
ERRXIT   LTR   RB,RB               CC = NEQ                                     
*                                                                               
EXIT     XIT1                                                                   
         EJECT ,                                                                
         SPACE 1                                                                
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
* OVERLAY WORKING STORAGE                                             *         
***********************************************************************         
         SPACE 1                                                                
LWSD     DSECT                                                                  
PRELOC   DS    F                                                                
SAVER2   DS    A                   SAVE REGISTER  R2                            
SAVER6   DS    A                   SAVE REGISTER  R6                            
SAVERE   DS    A                   SAVE REGISTER  RE                            
*                                                                               
VALUE    DS    CL6                 PACKED    VALUE                              
MYTEMP   DS    CL10                TEMPORARY AREA                               
NAREQPAS DS    XL1                 NARRATIVE WITH EQUALS   PASSES               
NAREQCHG DS    CL1                 NARRATIVE WITH EQUALS   CHANGED              
NCOMELS  DS    1000C               NEW  COMMENT   ELEMENTS                      
TEXTWS   DS    1200C               W/S  FOR  TEXTED    MODULE                   
*                                                                               
LWSX     DS    0C                                                               
         EJECT ,                                                                
         SPACE 1                                                                
* ACBILDSECT                                                                    
       ++INCLUDE ACBILDSECT                                                     
* ACBILWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACBILWORK                                                      
         PRINT ON                                                               
         EJECT ,                                                                
         SPACE 1                                                                
* ACBILFDD                                                                      
       ++INCLUDE ACBILFDD                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008ACBIL02   12/17/12'                                      
         END                                                                    
