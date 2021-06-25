*          DATA SET PRSFM51    AT LEVEL 039 AS OF 06/22/10                      
*PHASE T41C51A                                                                  
*                                                                               
*      CHANGE LOG                                                               
*                                                                               
* SMYE 6/18/10 DELETE REFERENCE TO POWER CODES NO LONGER ON DDS SYSTEM          
*                                                                               
* BPLA 4/9/92  CODE FOR "US" REPORT                                             
*              ALSO CHANGE ON-LINE LIST REC LOGIC TO CONTINUE                   
*              DISPLAYING EVEN IF CODE IS GIVEN - THIS NOW                      
*              MEANS TO START DISPLAYING WITH THE REQUESTED CODE                
* BPLA 4/9/91  DISALLOW SCANNER FIELD TO BEGIN WITH A SPACE                     
*              CAUSED A DUMP TRYING TO PACK                                     
* ROSA 1/26/89 CHANGED SCREEN NAME FORM PRSFMF2XD TO PRSFMF2D      L09          
*                                                                  L08          
* ROSA 11/17/88 ADD LINE SPACING OPTION                            L08          
*                                                                  L08          
* ROSA 9/27/88 ADD DIVISION NAME COLUMN OPTION  (28)               L07          
*                                                                  L07          
*ROSA  9/14/88 ADD OTHER AGENCY NAME (OAN)  36 BY CODE 37 BY NAME  L06          
*                                       38 BY NAME AND CODE        L06          
*  ROSA 7/22/88 BUG01 -- SELECTION LINE LENGTH EXCEEDED WHEN DISPLAYING         
**           RECORD.. MULTIPLICATION EXPANSION IS THE CULPRIT....BUG01          
** BLAT 3/31/88                                                     L05         
**                                                                  L05         
* ROSA 3/15/88 ALLOW FOR INVISIBLE FIELDS ASSOCIATED WITH 93                    
**    *L04*    TOGETHER WITH ARITHMETIC CALCULATORS                             
**                                                                              
**ROSA 3/3/88 UPDATE THE COMPUTATION DETERMINING WHEN TO MOVE                   
**            SELECTIONS TO SECOND LINE                                         
**                                                                              
** ROSA 2/29/88  GIVE USER THE OPTION TO LABEL THE 93 FIELD - TWO LINES         
**    *L04*      FWITH MAX LIMIT OF 18 CHARACTERS. USE OF A / SEPARATES         
**               TWO LINES  EG, D=THIS IS THE/NEW                               
**                                                                              
* ROSA 2/22/88 ALLOW FOR A PERCENTAGE MULTIPLIER FOR SELECTION 93               
**    *L03*    TOGETHER WITH ARITHMETIC CALCULATORS                             
**                                                                              
* ROSA 2/12/88 L02   ALLOW FOR WEEKLY SUMMARY TOTALS                            
**                                                                              
** ROSA 1/28/88 L01  CHANGE PGM TO EDIT NEW FIELDS                              
*     CALCULATION (93), COST (46) AND STANDARD COMMENTS                         
*                                                                               
         TITLE 'T41C51 - USERP RECS FOR USER CONTROLLED PRINT'                  
T41C51   CSECT                                                                  
*                                                                 L01           
         NMOD1 0,T41C51                                                         
         LA    R7,1(RB)                                                         
         LA    R7,4095(R7)                                                      
         USING T41C51,RB,R7                                                     
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM+THIS PROG             
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         SPACE 3                                                                
         CLI   MODE,VALKEY                                                      
         BE    VK                                                               
         CLI   MODE,VALREC                                                      
         BE    VR                                                               
         CLI   MODE,DISPREC                                                     
         BE    DR                                                               
         CLI   MODE,DISPKEY                                                     
         BE    DK                                                               
         CLI   MODE,LISTRECS                                                    
         BE    LR                                                               
         CLI   MODE,PRINTREP                                                    
         BE    PR                                                               
         B     EXIT                                                             
         SPACE 3                                                                
******************************************                                      
*                                        *                                      
*  THIS PROGRAM SETS UP A USERP RECORD   *                                      
*  WHICH CONTROLS THE USER REPORT        *                                      
*                                        *                                      
*  HEADINGS WILL APPEAR IN THE ORDER     *                                      
*  SET IN THE USERP REC.                 *                                      
*                                        *                                      
*  S = PRIMARY SORT FIELD                *                                      
*  T = GIVES SUB-TOTALS ON FIELD BREAK   *                                      
*  M = ACCUMS MONTHLY TOTAL              *                                      
*  W + ACCUMS WEEKLY TOTALS                                         L03         
*  A = THIS IS A COMBINATION OF T+M      *                                      
*      IT ACCUMS MONTHLY TOTAL BUT ALSO  *                                      
*      GIVES SUB-TOTAL ON BRK OF MONTH   *                                      
*                                        *                                      
*  HELP OR ? IN SELECTION FIELD ON A     *                                      
*  CHANGE OR ADD GIVES HELP INFO.        *                                      
******************************************                                      
         EJECT                                                                  
* VALIDATE KEY *                                                                
         SPACE                                                                  
VK       XC    KEY,KEY                                                          
         LA    R2,PRTMEDH                                                       
         GOTO1 VALIMED                                                          
         XC    WORK(4),WORK                                                     
         LA    R2,PRTCODEH         ON LIST, REC CODE IS OPTIONAL                
         CLI   5(R2),0                                                          
         BNE   VK5                                                              
         CLI   ACTNUM,ACTLIST                                                   
         BE    VK10                                                             
         CLI   ACTNUM,ACTREP        OR REPORT                                   
         BE    VK10                                                             
VK5      GOTO1 ANY                                                              
*BUILD KEY *                                                                    
VK10     MVC   KEY(2),AGENCY                                                    
         MVC   KEY+2(1),QMED                                                    
         MVI   KEY+3,X'30'                                                      
         MVC   KEY+4(4),WORK                                                    
         MVC   SVKEY,KEY                                                        
         SPACE                                                                  
         B     EXIT                                                             
         EJECT                                                                  
         SPACE                                                                  
* VALIDATE RECORD *                                                             
         SPACE                                                                  
VR       DS    0H                                                               
         MVI   OPERA,0                                              L03         
*                                                                   L04         
         XC    P4(22),P4                                            L04         
         XC    ELEM,ELEM                                            L04         
         LA    R6,ELEM                                              L04         
*                                                                   L04         
         MVI   ELCODE,93    COMMENT FOR 93                          L04         
         GOTO1 REMELEM                                              L04         
*                                                                   L04         
         XC    CONHEAD,CONHEAD CLEAR ERROR MESSAGE                L01           
*                                                                 L01           
         MVI   ELCODE,X'10'        TITLE                                        
         GOTO1 REMELEM                                                          
         LA    R6,ELEM                                                          
         USING PUSRTELM,R6                                                      
         XC    ELEM,ELEM                                                        
         LA    R2,PRTTITLH                                                      
         GOTO1 ANY                                                              
         MVI   ELEM,X'10'          ELEM CODE                                    
         MVI   PUSRTLN,PUSRTLNE         LENGHT                                  
         MVC   PUSRTNM,WORK             TITLE NAME                              
         GOTO1 ADDELEM                                                          
         SPACE 3                                                                
*                                                                               
         MVI   ELCODE,X'15'        PROFILE ELEMENT                              
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   VR3                                                              
         GOTO1 REMELEM             REMOVE ELEMENT                               
VR3      LA    R6,ELEM                                                          
         USING PUSRPELM,R6                                                      
         XC    ELEM,ELEM                                                        
         MVI   ELEM,X'15'                                                       
         MVI   PUSRPLN,PUSRPLNE                                                 
*                                                                  L08          
         MVI   PUSRSPAC,1         DEFAULT IS TWO/ ONE IS ADDED IN  L08          
         LA    R2,PRTLSPAH        FOR CURSOR POSITIONING                        
         CLI   PRTLSPAH+5,0       ANY INPUT                        L08          
         BE    NSPACE                                              L08          
         CLI   PRTLSPA,C'0'                                        L08          
         BL    ERRINV                                              L08          
         CLI   PRTLSPA,C'4'                                        L08          
         BL    MSPACIN                                             L08          
ERRINV   MVI   ERROR,INVALID                                       L08          
         MVC   CONHEAD(20),=C'SPACING M/B 0-3     '                             
         B     TSERRX2A                                            L08          
*                                                                  L08          
MSPACIN  MVC   PUSRSPAC,PRTLSPA                                    L08          
         NI    PUSRSPAC,15                                         L08          
*                                                                  L08          
*                                                                  L08          
*                                                                  L08          
*                                                                  L08          
NSPACE   LA    R2,PRTSUPRH                                                      
*                                                                 L01           
* SUPPPRESS REQUESTOR IS NOW AN OPTION// IF NOTHING ENTERED       L01           
*    THEN 'N'                                                     L01           
         CLI   PRTSUPRH+5,0    ANYTHING ENTERD                    L01           
         BNE   *+8                                                L01           
         MVI   PRTSUPR,C'N'                                       L01           
         MVC   PUSRPSUP,PRTSUPR                                   L01           
         CLI   PRTSUPR,C'Y'    VERIFY Y OR N                      L01           
         BE    VRCK4C          CHECK 4 STD COMMENTS               L01           
         CLI   PRTSUPR,C'N'    VERIFY Y OR N                      L01           
         BE    VRCK4C                                             L01           
         XC    CONHEAD,CONHEAD PREPARE FOR ERR MSG                L01           
         MVC   CONHEAD(18),=C'OPTIONS ARE Y OR N'                 L01           
         B     TSERRX2A                                           L01           
****                                                              L01           
VRCK4C   DS    0H                                                 L01           
         XC    PUSRCOM1(L'PUSRCOM1*2),PUSRCOM1                    L01           
         LA    R2,PRTSCM1H     THIS IS OPTIONAL INPUT             L01           
         CLI   5(R2),0         LENGTH                             L01           
         BE    NOCOMNT1                                           L01           
         LA    RF,241          X'F1'  COMMENT NUMBER              L01           
         BAS   RE,GETCOMNT     VERIFY EXISTANCE                   L01           
         MVC   PUSRCOM1,WORK   MOVE COMMENT 1 ID                  L01           
         BNE   TSERRX2A                                           L01           
NOCOMNT1 LA    R2,PRTSCM2H     OPTIONAL INPUT                     L01           
         CLI   5(R2),0                                            L01           
         BE    NOCOMNT2                                           L01           
         LA    RF,242          X'F2'                              L01           
         BAS   RE,GETCOMNT     VERIFY EXISTANCE                   L01           
         MVC   PUSRCOM2,WORK   MOVE COMMENT 2 ID                  L01           
         BNE   TSERRX2A                                           L01           
         CLC   PUSRCOM2,PUSRCOM1   CHECK TO SEE IF DUPLICATED     L01           
         BNE   NOCOMNT2                                           L01           
         MVC   CONHEAD(22),=C'DUPLICATE COMMENT I.D.'             L01           
         B     TSERRX2A                                           L01           
NOCOMNT2 DS    0H                                                 L01           
***                                                               L01           
***                                                               L01           
VR5      GOTO1 ADDELEM             ADD ELEMENT                                  
*                                                                               
         MVI   ELCODE,X'20'        SELECTION CODES                              
         GOTO1 REMELEM                                                          
         LA    R6,ELEM                                                          
         USING PUSRSELM,R6                                                      
         XC    LINRM,LINRM                                                      
         XC    ELEM,ELEM                                                        
         MVI   ELEM,X'20'                                                       
         MVI  PUSRSLN,PUSRSLNE-6  NON 93 SELECTION                  L03         
*                                                                   L03         
         XC    SELSW,SELSW                                                      
         LA    R2,PRTSEL1H                                                      
         GOTO1 ANY                                                              
         SPACE                                                                  
         CLC   8(4,R2),=C'HELP'    DO THEY NEED HELP                            
         BE    VR10                                                             
         CLC   8(1,R2),=C'?'                                                    
         BNE   VR15                                                             
VR10     XC    PRTSEL1,PRTSEL1                                                  
         MVC   PRTSEL1+5(29),=C'SELECT CODES FROM MENU BELOW,'                  
         XC    PRTSEL2,PRTSEL2                                                  
         MVC   PRTSEL2+5(55),=C'VALID ENTRY=NN(S/T/M) S=SORT,T=TOTALS,MX        
               =MONTHLY TOTALS'                                                 
         LA    R2,PRTSEL1H                                                      
         FOUT  (R2)                                                             
         LA    R2,PRTSEL2H                                                      
         FOUT  (R2)                                                             
         B     EXIT                                                             
         SPACE                                                                  
* MOVE BOTH SELECTION INPUT LINES TO AIO2+1000 FOR SCANNER                      
*   AND SET UP DUMMY HEADER WHICH SCANNER NEEDS                                 
         SPACE                                                                  
VR15     L     RE,AIO2                                                          
         LH    RF,=H'2000'                                                      
         XCEF                                                                   
         L     R5,AIO2             R4=DUMMY HEADER(NOT BUMPED)                  
         LA    R5,1000(R5)                                                      
         LR    R4,R5               R5=HEADER + LINE(BUMPED)                     
         ZIC   R1,5(R2)                                                         
         ST    R1,FULL             SAVE INSERT LENGTH IN FULL                   
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R5),8(R2)                                                    
         LA    R2,PRTSEL2H                                                      
         CLI   5(R2),0                                                          
         BE    VR17                                                             
         L     RF,FULL             INCREMENT R5 LINE                            
         AR    R5,RF                                                            
         SPACE                                                                  
         BCTR  R5,0                TEST FOR COMMA/ADD IF NOT                    
         CLI   8(R5),C','                                                       
         BNE   VR18                                                             
         LA    R5,1(R5)                                                         
         B     VR19                                                             
VR18     LA    R5,1(R5)            NO COMMA,                                    
         MVI   8(R5),C','            SO ADD IT                                  
         LA    R5,1(R5)                  AND INCREMENT LINE                     
         LA    RF,1(RF)                      AND INCREMENT LENGTH.              
         SPACE                                                                  
VR19     ZIC   R1,5(R2)                                                         
         AR    RF,R1               SAVE SEL1+SEL2 INPUT L'                      
         ST    RF,FULL                  IN FULL.                                
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R5),8(R2)                                                    
* SET UP LENGTHS IN DUMMY HEADER                                                
VR17     L     R1,FULL                                                          
         STC   R1,5(R4)                                                         
         LA    R1,8(R1)                                                         
         STC   R1,0(R4)                                                         
         SPACE                                                                  
         LR    R2,R4                 SET R2 TO SCAN LINE                        
         L     R4,AIO2               SET R4 TO SCAN OUTPUT AREA                 
         BAS   RE,CCHK             GOTO SUCCESSIVE COMMAS CHECK                 
         SPACE                                                                  
         SR    R3,R3               R3=COUNTER FOR POSITION IN                   
         LA    R3,1                   X'20' ELEMS/ALSO FOR SCAN BLOCKS          
         PRINT GEN                                                  L04         
         GOTO1 SCANNER,DMCB,(R2),(R4),C',=,<'  DO NOT SCAN FOR =    L04         
         PRINT NOGEN                                                L04         
*                                                                   L04         
         SPACE                                                                  
         BAS   RE,SCANCHK                                                       
VR20     CLC   12(2,R4),=C'D='     TITLE FOR 93                     L04         
         BE    VR93DE                                               L04         
*                            ELSE MUST START WITH 2 DIGITS                      
         CLI   12(R4),C'0'                                                      
         BL    SCANERR       WILL CAUSED DUMP WHEN TRYING TO PACK               
         CLI   13(R4),C'0'                                                      
         BL    SCANERR       WILL CAUSED DUMP WHEN TRYING TO PACK               
*                                                                               
         CLI   14(R4),X'40'  VALID FIELD = M, S, T, A               L04         
*                                                                   L04         
         BE    VR21                                                             
         CLI   14(R4),C'A'                                                      
         BE    VR21                                                             
         CLI   14(R4),C'M'                                                      
         BE    VR21                                                             
         CLI   14(R4),C'W'       WEEKLY OPTION                    L02           
         BE    VR21                                               L02           
***                                                               L02           
         CLI   14(R4),C'S'                                                      
         BE    VR21                                                             
         CLI   14(R4),C'T'                                                      
         BE    VR21                                                             
         CLI   14(R4),C'O'                                                      
         BE    CKOPDIF                                                          
         CLI   14(R4),C'D'                                                      
         BE    CKOPDIF                                                          
         CLC   14(2,R4),=C'NP'    NO PRINT OPTIONS                  L04         
         BE    CKOPDIF                                              L04         
         CLC   12(2,R4),=C'93'  CALCULATION WILL BE VERIFIED      L01           
         BE    VR21             LATER                             L01           
***                                                               L01           
*****                                                                           
         CLI   14(R4),C'('           ONLY SELECTION 19(COMMENT) CAN             
         BNE   SCANERR               SPECIFY LENGTH BY USING BRACKETS           
         CLC   12(2,R4),=C'26'      OR BLANK COLUMN                             
         BE    VR21                                                             
         CLC   12(2,R4),=C'19'      COMMENT                                     
         BNE   SCANERR                                                          
         B     VR21                                                             
*****                                                                           
*                                                                               
CKOPDIF  CLC   12(2,R4),=C'40'                                                  
         BL    SCANERR                                                          
         CLC   12(2,R4),=C'85'                                                  
         BH    SCANERR                                                          
*                                                                               
VR21     STCM  R3,1,PUSRSPOS       SET POSITION IN 20 ELEMS                     
         PACK  DUB,12(2,R4)                                                     
         CVB   R1,DUB                                                           
         STCM  R1,1,PUSRSSEL       SET SELECTION CODE                           
         SPACE                                                                  
         LA    R5,SELTBL                                                        
*                                                                               
VR22     DS    0H                                                               
*                                                                               
VR22A    CLC   12(2,R4),=C'21'                                                  
         BNE   VR22E                                                            
VR22B    LA    R2,CONHEADH                                                      
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(37),=C'** ERROR - UNAUTHORIZED SELECTION #21'            
VR22D    FOUT  (R2)                                                             
         GOTO1 ERREX2                                                           
*                                                                               
VR22E    CLC   0(2,R5),12(R4)                                                   
         BNE   VR26                                                             
         L     RF,LINRM                                                         
*****                                                                           
         BAS   RE,CHKCOM                                                        
         LTR   R0,R0                                                            
         BZ    VR22H                                                            
         LR    R1,R0                                                            
         B     VR22I                                                            
*****                                                                           
VR22H    ZIC   R1,3(R5)                                                         
VR22I    DS    0H  DO NOT INCLUDE FIELD LENGTH IF (NP) NOT PRONTING L04         
         CLC   14(2,R4),=C'NP'                                      L04         
         BE    VR22M                                                L04         
         AR    RF,R1                                                L04         
*                          TO BE LATER TAKEN OUT                                
*        CLC   AGENCY,=C'SJ'                                                    
*        BE    VR22M                                                            
*        CLC   AGENCY,=C'NW'                                                    
*        BE    VR22M                                                            
*        CH    RF,=H'132'                                                       
*        BNH   VR28                                                             
*        LA    R2,CONHEADH                                                      
*        XC    CONHEAD,CONHEAD                                                  
*        MVC   CONHEAD(47),=C'** ERROR-MAX COL HEADER=132,AT     YOU HA         
*              VE    '                                                          
*        B     VR22P                                                            
*                                                                               
VR22M    CH    RF,=H'164'                                                       
         BNH   VR28                                                             
         LA    R2,CONHEADH                                                      
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(47),=C'** ERROR-MAX COL HEADER=164,AT     YOU HAX        
               VE    '                                                          
VR22P    MVC   CONHEAD+31(2),12(R4)                                             
         LR    R1,RF                                                            
         LA    R5,CONHEAD+44                                                    
         EDIT  (R1),(4,(R5)),ALIGN=LEFT                                         
         FOUT  (R2)                                                             
         GOTO1 ERREX2                                                           
         SPACE                                                                  
VR26     ZIC   R1,4(R5)                                                         
         AR    R5,R1                                                            
         CLI   0(R5),0                                                          
         BE    INVSEL                                                           
         B     VR22                                                             
VR28     STCM  RF,15,LINRM                                                      
         SPACE 2                                                                
*****                                                                           
         LR    R2,R4                                                            
         CLC   12(2,R4),=C'26'                                                  
         BE    VR2800                                                           
         CLC   12(2,R4),=C'19'                                                  
         BNE   VR2801                                                           
VR2800   CLI   14(R2),C'('                                                      
         BNE   VR2801                                                           
         LA    R1,17(R2)                                                        
         CLI   16(R2),C')'                                                      
         BE    VR2800B                                                          
         CLI   17(R2),C')'                                                      
         BNE   SCANERR                                                          
         LA    R1,1(R1)                                                         
VR2800B  CLI   0(R1),X'40'               CHK FOR T/S/M/A                        
         BE    VR38                                                             
         CLC   12(2,R4),=C'26'           NOTHING FOR BLANK COLUMN               
         BE    SCANERR                                                          
         B     VR2801B                                                          
*                                                                               
VR2801   CLI   14(R2),X'40'              ANY T/S/M/A                            
         BE    VR38                                                             
*****                                                                           
*                                                                               
VR2801B  CLI   14(R2),C'O'               FOR OPEN RATES.                        
         BNE   VR2801D                                                          
         ZIC   R1,PUSRSSEL                                                      
         LA    R1,100(R1)                ADD 100 TO SELECTION CODE              
         STC   R1,PUSRSSEL                   BEFORE STORING.                    
         OI    PUSRSTS,X'10'             SET OPEN INDICATOR.                    
         B     VR38                                                             
VR2801D  CLI   14(R2),C'D'               FOR DIFFERENCE OF OPEN AND             
         BNE   VR2801F                       CONTRACT.                          
         ZIC   R1,PUSRSSEL                                                      
         LA    R1,160(R1)                ADD 160 TO SELECTION CODE              
         STC   R1,PUSRSSEL                   BEFORE STORING.                    
         OI    PUSRSTS,X'20'             SET DIFFERENCE INDICATOR.              
         B     VR38                                                             
*                                                                               
VR2801F  CLI   14(R2),C'W'      WEEKLY OPTION                     L02           
         BNE   VR28AX                                             L02           
         CLC   12(2,R2),=C'30'   ONLY FOR INSERT DATES            L02           
         BE    *+14                                               L02           
         MVC   CONHEAD(34),=C'WEEKLY OPTION FOR INSERT DATE ONLY' L02           
         B     TSERRX2                                            L02           
***                                                               L02           
         OI    PUSRSTS,X'08'    WEEKLY BIT ON                     L02           
         B     VR28B            CHECK FOR DATES                   L02           
VR28AX   CLI   14(R2),C'A'                                        L02           
***                                                               L02           
         BNE   VR28A                                                            
         OI    PUSRSTS,X'06'    X'06'=MONTH ACCUM+SUB-TOT ON MONTH BRK          
         B     VR28B                                                            
VR28A    CLI   14(R2),C'M'                                                      
         BNE   VR30                                                             
         OI    PUSRSTS,X'04'       X'04'=MONTH ACCUM ON DATE FIELDS             
VR28B    CLC   12(2,R2),=C'30'                                                  
         BL    TMERR                                                            
         CLC   12(2,R2),=C'35'       30-35 ARE DATES                            
         BH    TMERR                                                            
         B     VR38                                                             
VR30     CLI   14(R2),C'S'                                                      
         BNE   VR32                                                             
         OI    PUSRSTS,X'01'       X'01'=SORT                                   
         B     VR34                                                             
VR32     CLI   14(R2),C'T'                                                      
         BNE   VR38Z                                                L04         
         OI    PUSRSTS,X'02'       X'02'=TOTALS                                 
         OI    PUSRSTS,X'01'       IF TOTALS THEN SET SORT                      
VR34     DS    0H                                                               
         CLC   8(2,R2),=C'26'                                                   
         BE    VR38                                                             
         CLC   8(2,R2),=C'19'                                                   
         BE    VR38                                                             
         CLC   12(2,R2),=C'40'     TEST TOTALS/SORT ON COST FIELDS              
         BNL   TSERR               ALL ABOVE 40 ARE $ FIELDS                    
VR37     CLC   12(2,R2),=C'04'     TEST TOTALS/SORT ON REG/DST NAME             
         BE    TSERR2                   IF YES ERROR                            
         CLC   12(2,R2),=C'05'                                                  
         BE    TSERR2                                                           
         CLC   12(2,R2),=C'25'                                                  
         BE    TSERR2                                                           
         B     VR38                                                             
VR38Z    CLC   14(2,R2),=C'NP'    NO PRINT OPTION                   L04         
         BNE   VR38                                                 L04         
* ENSURE NP IS CONNECTED TO A $ SELECTION                           L04         
         CLC   12(2,R2),=C'40'                                      L04         
         BL    NPERROR                                              L04         
         CLC   12(2,R2),=C'93'                                      L04         
         BL    NPISOK                                               L04         
*                                                                   L04         
NPERROR  MVC   CONHEAD(35),=C'NP OPTION VALID FOR SELECTION 40-92' L04          
         B     LAR2PRT                                              L04         
*                                                                   L04         
*                                                                   L04         
NPISOK   MVI   PUSRSDSP,X'F0'     NO PRINT INDICATOR                L04         
         MVC   PUSRSSRT,2(R5)                                       L04         
         B     VR38XX                                               L04         
*                                                                   L04         
VR38     MVC   PUSRSSRT,2(R5)       SET SORT LENGTH                             
         MVC   PUSRSDSP,3(R5)       SET DISPLAY LENGTH                          
VR93DE   CLC   12(2,R4),=C'D='    DESCRIPTION ID FOR 93             L04         
         BNE   MB93                                                 L04         
*                                                                   L04         
         CLI   0(R4),17  SINGLE  COMMENT M/B LT 15 UNLESS THERE IS  L04         
         BL    COMMOK            A SLASH / SEPARATING COMMENT       L04         
         LR    RF,R4              INTO TWO LINES                    L04         
         ZIC   RE,0(R4)          LENGTH OF INPUT                    L04         
         SH    RE,=H'2'           DESCRIPTOR D=                     L04         
LOOKAGN  CLI   14(RF),C'/'        LINE SEPARATOR                    L04         
         BE    CALCLEN                                              L04         
         LA    RF,1(RF)                                             L04         
         BCT   RE,LOOKAGN                                           L04         
COMLT14  MVC   CONHEAD(25),=C'COMMENT ELEMENT M/B LT 15'            L04         
         B     LAR2PRT                                              L04         
*                                                                   L04         
CALCLEN  CH    RE,=H'15'                                            L04         
         BH    COMLT14                                              L04         
         ZIC   RF,0(R4)     ACTUAL LEN INCLUDES DESCRIPTOR D=       L04         
         SR    RF,RE                                                L04         
         CH    RF,=H'16'                                            L04         
         BH    COMLT14                                              L04         
*                                                                   L04         
COMMOK   LR    RF,R4        DESCRIPTION MUST FOLLOW 93             L04          
         SH    RF,=H'32'    BACK POINTER TO PREVIOUS ELEMENT        L04         
         CLC   12(2,RF),=C'93'                                      L04         
         BE    *+14                                                 L04         
         MVC   CONHEAD(28),=C'SELECTION PRIOR TO D= M/B 93'         L04         
         B     LAR2PRT                                              L04         
         ZIC   RF,0(R4)                                             L04         
         BCTR  RF,0          GET LEN-1                              L04         
         LA    R6,ELEM                                              L04         
         USING PUSRDELM,R6                                          L04         
         EX    RF,*+8                                               L04         
         B     *+10                                                 L04         
         MVC   PUSRDTTL(0),14(R4) MOVE W/O D= DESCRIPTOR            L04         
         LA    RF,1(RF)           LENGTH OF ELEMENT                 L04         
         MVI   PUSRDELM,93                                        L04           
         STC   RF,PUSRDLN                                           L04         
         GOTO1 ADDELEM                                              L04         
         USING PUSRSELM,R6        RESET FOR NORMAL 20 ELEMENTS      L04         
         B     VR60A                                                L04         
********************                                                L04         
MB93     DS    0H                                                   L04         
         CLC   12(2,R4),=C'93'  CALCULATION ENTRY                 L01           
         BNE   VR38XX                                             L01           
OKONE    MVI   PUSRSSRT,0       ASSUME 2 COLUMN ADD               L01           
*  CONTENTS OF PUSRESSRT  80 = THREEE COLUMNS 40= MINUS FIRST     L01           
*  TWO COLUMNS  20= MUNUS SECOND AND THIRD                        L01           
*                                                                 L01           
         CLI   0(R2),3           CHECK FOR 3 BYTES 93+            L01           
         BL    VR38X             MB 2 BYTES 93                    L01           
         CLI   14(R2),C'+'       ADD 1ST TWO COL                  L01           
         BE    ISPLUS                                             L01           
         CLI   14(R2),C'-'       MINUS                            L01           
*                                                                   L03         
* SELECTION 93 MAY BE FOLLOWED IMMEDIATLY BY A '('                  L03         
*                                                                   L03         
         BE    OIPUSSRT                                             L03         
         CLI   14(R2),C'('    LEFT PAREN                            L03         
         BNE   OPERERR                                              L03         
         MVI   OPERA,4                                              L03         
         LA    RF,32       POINT TO PRIOR COLUMN                    L03         
         LR    RE,R2       CURRENT COLUMN                           L03         
         SR    RE,RF       PRIOR  COLUMN                            L03         
         B     CKAGN       VERIFY LEFT COLUMN FOR TOTAL             L03         
OIPUSSRT DS    0H                                                   L03         
         OI    PUSRSSRT,X'40'    MINUS INDICATOR                  L01           
ISPLUS   CLI   0(R2),4           93+-   ?                         L01           
         BL    VR38X             JUST VERIFIED 3                  L01           
         BE    OIPUSR                                               L03         
         CLI   15(R2),C'('   LEFT PAREN                             L03         
         BNE   OIPUS3        POSSIBLE 3 COL SUM AND MULT            L03         
         MVI   OPERA,8                                              L03         
         B     VR38X                                                L03         
OIPUS3   DS    0H                                                   L03         
         CLI   16(R2),C'('   LEFT PAREN M/B HERE                    L03         
         BNE   MULTINX                                              L03         
         MVI   OPERA,12                                             L03         
OIPUSR   DS    0H                                                   L03         
         OI    PUSRSSRT,X'80'     THREE COLUMNS                                 
         CLI   15(R2),C'+'                                        L01           
         BE    VR38X                                              L01           
         CLI   15(R2),C'-'       MINUS                            L01           
         BNE   OPERERR                                            L01           
         OI    PUSRSSRT,X'20'                                     L01           
VR38X    DS    0H                                                 L01           
*                                                                 L01           
* MUST DETERMINE IF PRIOR TWO OR THREE COLUMNS ARE TOTALS         L01           
*                                                                 L01           
         MVI   PUSROPT,C'A'  SUMS ARE INVOLVED WITH MULTIPLICATION  L03         
*                                                                   L03         
         LA    RF,96    ASSUME 3 OPERATORS                        L01           
         LR    RE,R2                                              L01           
         SR    RE,RF         CALC ADDRESS 3 ELEMENTS BACK         L01           
         TM    PUSRSSRT,X'80'  THREE COL                          L01           
         BO    *+8                                                L01           
         LA    RE,32(RE)        POINT 2 ELEMENTS BACK             L01           
CKAGN    CLC   12(2,RE),=C'40'  TOTALS                            L01           
         BL    ELERR                                              L01           
         CLC   12(2,RE),=C'92'                                    L01           
         BH    ELERR                                              L01           
         LA    RE,32(RE)        TO NEXT ELEMENT                   L01           
         CR    RE,R2            IF EQUAL - OUT                    L01           
         BL    CKAGN                                              L01           
*                                                                   L03         
*   MULTIPLIER VALIDATION                                           L03         
*                                                                   L03         
         CLI   OPERA,0      WAS A LEFT PAREN ENCOUNTERD             L03         
         BE    VR38XX       IF SO MUST DETERMINE LENGTH OF INPUT    L03         
         LA    RE,14(R2)                                            L03         
         LA    RF,3                                                 L03         
LEFT     CLI   0(RE),C'('                                           L03         
         BE    LEFTFND                                              L03         
         LA    RE,1(RE)                                             L03         
         BCT   RF,LEFT                                              L03         
*                                                                   L03         
         B     OPERERR   NOT FOUND                                  L03         
*                                                                   L03         
LEFTFND  ZIC   RF,0(R2)     TOTAL LENGTH OF ENTRY                   L03         
         LA    RF,11(R2,RF) POINT TO END OF ENTRY                   L03         
         CLI   0(RF),C')'    MUST BE A RIGHT PAREN                  L03         
         BNE   MULTINX                                             L03          
         SR    RF,RE         LEN OF MULTIPLIER                      L03         
         BCTR  RF,0                                                 L03         
         LA    RE,1(RE)      BUMP PAST LEFT PAREN                   L03         
         PRINT GEN                                                  L03         
         GOTO1 CASHVAL,DMCB,(5,(RE)),(RF)                           L03         
         PRINT NOGEN                                                L03         
         CLI   0(R1),255      ERROR                                 L03         
         BNE   CASHOK                                               L03         
MULTINX  MVC   CONHEAD(35),=C'MULTIPLIER INCORRECT FOR 93 ELEMENT'  L03         
         B     LAR2PRT                                              L03         
CASHOK   L     RF,4(R1)          LOAD BINARY NUMBER                 L03         
         CVD   RF,DUB                                               L03         
         ZAP   PUSRMULT,DUB      PACKED SIGNED MULTIPLIER           L03         
         MVI  PUSRSLN,PUSRSLNE        93 SELECTION LENGTH IS 13     L03         
*                                                                   L04         
VR38XX   DS    0H                                                 L01           
*****                                                                           
         BAS   RE,CHKCOM            DO CHECK IF COMMENT LENGTH HAS              
         LTR   R0,R0                BEEN SPECIFIED                              
         BZ    VR60                                                             
         STC   R0,PUSRSDSP          STORE LENGTH TO RECORD                      
*****                                                                           
VR60     GOTO1 ADDELEM                                                          
         LA    R3,1(R3)  INCREMENT POSITION COUNTER                 L04         
VR60A    DS    0H                                                   L04         
*                                                                   L04         
         XC    ELEM,ELEM                                                        
         MVI   ELEM,X'20'                                                       
         MVI  PUSRSLN,PUSRSLNE-6  NON 93 SELECTION                  L03         
         LA    R4,32(R4)            EACH SCAN BLOCK=32                          
         CLI   0(R4),0                                                          
         BNE   VR20                                                             
         B     VRX                                                L01           
**                                                                L01           
ELERR    CLI  PUSROPT,C'A'  ARE THERE ARITHMETIC OPTIONS            L03         
         BE   YARITHOP                                              L03         
         MVC  CONHEAD(37),=C'SELECTION PRIOR TO 93 MUST BE A TOTAL' L03         
         B    LAR2PRT                                               L03         
YARITHOP MVC  CONHEAD(40),=C'PRIOR 2 SELECTIONS FOR 93 MUST BE TOTALS'          
*                                                                   L03         
         TM    PUSRSSRT,X'80'   ARE THERE 3 FIELDS                L01           
         BNO   *+8                                                L01           
         MVI   CONHEAD+6,C'3'                                     L01           
LAR2PRT  DS    0H                                                   L03         
         LA    R2,PRTSEL1H            FOR CURSOR ERROR POSITONING L01           
         B     TSERRX2A                                           L01           
**                                                                L01           
         SPACE                                                                  
VRX      DS    0H                                                               
         L     RF,LINRM                                                         
         LA    RF,1(RF)      FINISHED WITH THE SELECTIONS //        L04         
         ST    RF,LINRM      MUST ALLOW FOR RIGHTMOST VERT BOX LINE L04         
*                                                                   L04         
*                                                                   L04         
         CH    RF,=H'132'                                                       
         BNH   EXIT                                                             
         MVC   CONHEAD(45),=C'** NOTE **  -  REPORT REQUIRES WIDE PRINTX        
               ING'                                                             
         FOUT  CONHEADH                                                         
         OI    GENSTAT2,USMYOK                                                  
         MVI   ELCODE,X'15'                                                     
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         USING PUSRPELM,R6                                                      
         MVC   PUSRPLEN,LINRM+2                                                 
         B     EXIT                                                             
**                                                                L01           
OPERERR  MVC   CONHEAD(L'OPER),OPER   OPERATOR ERROR              L01           
         LA    R2,PRTSEL1H            FOR CURSOR ERROR POSITONING L01           
         B     TSERRX2A                                           L01           
         DROP  R6                                                               
         EJECT                                                                  
*****                                                                           
CHKCOM   DS    0H                                                               
         SR    R0,R0                                                            
         CLC   12(2,R4),=C'26'                                                  
         BE    CHKC10                                                           
         CLC   12(2,R4),=C'19'      IF COMMENT FIELD SELECTED                   
         BNE   CHKCEX                                                           
CHKC10   CLI   14(R4),C'('          AND LENGTH SEPCIFIED                        
         BNE   CHKCEX               ELSE DEFAULT TO TABLE LENGTH                
         CLI   16(R4),C')'                                                      
         BNE   CHKC15                                                           
         CLI   15(R4),C'0'       MUST BE NUMERIC CHARACTERS                     
         BNH   SCANERR                                                          
         PACK  HALF,15(1,R4)        CONVERT CHARACTER TO BINARY                 
         ZAP   DUB,HALF                                                         
         CVB   R0,DUB                                                           
         STC   R0,BYTE                                                          
         B     CHKC18                                                           
*                                                                               
CHKC15   CLI   17(R4),C')'                                                      
         BNE   SCANERR                                                          
         CLI   15(R4),C'0'        MUST BE NUMERIC CHARACTERS                    
         BL    SCANERR                                                          
         CLI   16(R4),C'0'        MUST BE NUMERIC CHARACTERS                    
         BL    SCANERR                                                          
         CLC   15(2,R4),=C'00'                                                  
         BE    SCANERR                                                          
         PACK  HALF,15(2,R4)        CONVERT CHARACTER TO BINARY                 
         ZAP   DUB,HALF                                                         
         CVB   R0,DUB                                                           
         STC   R0,BYTE              SPECIFIED LENGTH                            
*                                                                               
CHKC18   CLC   12(2,R4),=C'26'                                                  
         BE    CHKC20                                                           
         CLI   BYTE,X'09'                                                       
         BL    SCANERR                                                          
         CLI   BYTE,X'30'           CANNOT BE GREATER THAN 48                   
         BH    SCANERR                                                          
CHKC20   AH    R0,=H'1'                                                         
CHKCEX   BR    RE                                                               
*****                                                                           
         EJECT                                                                  
* DISPLAY RECORD *                                                              
         SPACE                                                                  
DR       DS    0H                                                               
         XC    P4(22),P4         CLEAR 93 DESCRIPTION               L04         
         XC    ELEM,ELEM                                            L04         
         L     R6,AIO                                               L04         
         MVI   ELCODE,93                                            L04         
         BAS   RE,GETEL                                             L04         
         BNE   NO93DESC                                             L04         
         MVC   P4(21),1(R6) MOVE W/O ID                             L04         
NO93DESC DS    0H                                                   L04         
         XC    PRTTITL,PRTTITL     CLEAR SCREEN                                 
         XC    PRTSEL1,PRTSEL1                                                  
         XC    PRTSEL2,PRTSEL2                                                  
         LA    R2,PRTTITLH                                                      
         FOUT  (R2)                                                             
         LA    R2,PRTSEL1H                                                      
         FOUT  (R2)                                                             
         LA    R2,PRTSEL2H                                                      
         FOUT  (R2)                                                             
         XC    SELSW,SELSW                                                      
         SPACE                                                                  
         LA    R2,PRTTITLH                                                      
         MVI   ELCODE,X'10'                                                     
         L     R6,AIO                                                           
         USING PUSRTELM,R6                                                      
         BAS   RE,GETEL                                                         
         BE    DR3                                                              
         MVC   8(26,R2),=C'** TITLE ELEM NOT FOUND **'                          
         B     *+10                                                             
DR3      MVC   8(L'PUSRTNM,R2),PUSRTNM                                          
         FOUT  (R2)                                                             
         SPACE                                                                  
*                                                                               
         MVI   ELCODE,X'15'                                                     
         L     R6,AIO                                                           
         LA    R2,PRTSUPRH                                                      
         USING PUSRPELM,R6                                                      
         BAS   RE,GETEL                                                         
         BE    DR3C                                                             
         MVI   PRTLSPA,0                                          L08           
         FOUT  PRTLSPAH                                           L08           
         LA    R2,PRTSUPRH                                        L08           
         XC    PRTSUPR,PRTSUPR       NO X'15' ELEM - CLEAR FIELDS L05           
         FOUT  (R2)                                               L05           
         XC    PRTSCM1,PRTSCM1                                    L05           
         FOUT  PRTSCM1H                                           L05           
         XC    PRTSCM2,PRTSCM2                                    L05           
         FOUT  PRTSCM2H                                           L05           
         B     DR5                                                L05           
*                                                                 L05           
DR3C     MVC   PRTSUPR,PUSRPSUP                                                 
DR3F     FOUT  (R2)                                                             
**                                                                L01           
         LA    R2,PRTSCM1H       FIRST COMMENT RECORD             L01           
         FOUT  (R2)                                               L01           
         XC    8(6,R2),8(R2)                                      L01           
         OC    PUSRCOM1,PUSRCOM1                                  L01           
         BZ    *+10                                               L01           
         MVC   8(6,R2),PUSRCOM1                                   L01           
         LA    R2,PRTSCM2H      SECOND COMMENT RECORD             L01           
         FOUT  (R2)                                               L01           
         XC    8(6,R2),8(R2)                                      L01           
         OC    PUSRCOM2,PUSRCOM2                                  L01           
         BZ    *+10                                               L01           
         MVC   8(6,R2),PUSRCOM2                                   L01           
         MVC   PRTLSPA,PUSRSPAC                                  L01            
         OI    PRTLSPA,X'F0'                                     L01            
         CLI   PRTLSPA,X'F0'                                     L08            
         BNE   *+8                                                L08           
         MVI   PRTLSPA,C'1'                                      L08            
         FOUT  PRTLSPAH                                          L08            
*                                                                 L08           
*                                                                 L08           
DR5      MVI   ELCODE,X'20'                                                     
         LA    R2,PRTSEL1H                                                      
         ST   R2,FULL     START OF LINE ADDRESS                                 
         XC    LINRM,LINRM                                                      
         L     R6,AIO                                                           
         USING PUSRSELM,R6                                                      
         BAS   RE,GETEL                                                         
         BE    DR7                                                              
         DC    H'0'                                                             
*                                                                               
DR7      TM    PUSRSTS,X'10'              X'10' IS OPEN RATES.                  
         BNO   DR9                                                              
         ZIC   R1,PUSRSSEL                                                      
         CVD   R1,DUB                                                           
         SP    DUB,=P'100'                OPEN RATES ARE STORED AT 100          
         OI    DUB+7,X'0F'                     GREATER SO MUST SUBTRACT         
         UNPK  8(2,R2),DUB                     100 BEFORE DISPLAYING.           
         MVI   10(R2),C'O'                DISPLAY O FOR OPEN RATES.             
         LA    R2,3(R2)                                                         
         B     DR12                                                             
DR9      TM    PUSRSTS,X'20'              X'20' IS DIFFERENCE OF OPEN           
         BNO   DR10                             AND CONTRACT.                   
         ZIC   R1,PUSRSSEL                                                      
         CVD   R1,DUB                                                           
         SP    DUB,=P'160'                DIFFERENCE IS STORED AT 160           
         OI    DUB+7,X'0F'                    GREATER SO SUBTRACT 160           
         UNPK  8(2,R2),DUB                    BEFORE DISPLAYING.                
         MVI   10(R2),C'D'                DISPLAY D FOR DIFFERENCE.             
         LA    R2,3(R2)                                                         
         B     DR12                                                             
*                                                                               
*****                                                                           
DR10     DS    0H                                                               
         CLI   PUSRSSEL,X'1A'        IS IT BLANK COLUMN                         
         BNE   DR10C                                                            
         CLI   PUSRSDSP,X'02'        IS IT DEFAULT LENGTH                       
         BE    DR10M                                                            
         B     DR10J                                                            
*                                                                               
DR10C    CLI   PUSRSSEL,X'13'        IS IT COMMENT FIELD  #19                   
         BNE   DR10M                                                            
         CLI   PUSRSDSP,X'30'        IS IT DEFAULT LENGTH                       
         BE    DR10M                                                            
DR10J    ZIC   R1,PUSRSSEL                                                      
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  8(2,R2),DUB                                                      
         MVI   10(R2),C'('                                                      
         MVI   13(R2),C')'                                                      
         ZIC   R1,PUSRSDSP                                                      
         SH    R1,=H'1'                                                         
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  11(2,R2),DUB                                                     
         LA    R2,6(R2)                                                         
         B     DR10P                                                            
*****                                                                           
DR10M    ZIC   R1,PUSRSSEL                                                      
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  8(2,R2),DUB                                                      
         LA    R2,2(R2)                                                         
         CLI   PUSRSDSP,X'F0'     NO PRINT INDICATOR                L04         
         BNE   NOPRTIND                                             L04         
         MVC   8(2,R2),=C'NP'                                       L04         
         LA    R2,2(R2)                                             L04         
NOPRTIND DS    0H                                                   L04         
*                                                                   L04         
**                                                                L01           
         CLI   PUSRSSEL,93    CALCULATION COLUMN                  L01           
         BNE   DR10P                                              L01           
         CLI   PUSRSLN,PUSRSLNE  EXTENDED 93 (MULTIPLIER)           L03         
         BNE   NOTXTND                                              L03         
         CLI   PUSROPT,C'A'  IF = COMBINATION OF SUMS AND MULTIPLY  L03         
         BNE   DISPMULT                                             L03         
NOTXTND  DS    0H                                                   L03         
         MVI   8(R2),C'+'                                         L01           
         TM    PUSRSSRT,X'80'    THREE COLUMNS                    L01           
         BNO   *+8                                                L01           
         MVI   9(R2),C'+'                                         L01           
         TM    PUSRSSRT,X'40'     SUBTRACT COLS 1 &2              L01           
         BNO   *+8                                                L01           
         MVI   8(R2),C'-'                                         L01           
         TM    PUSRSSRT,X'20'    SUB COLS 2 & 3                   L01           
         BNO   *+8                                                L01           
         MVI   9(R2),C'-'                                         L01           
         LA    R2,1(R2)         ASSUME ONLY TWO COLUMNS           L01           
         CLI   8(R2),X'4D'                                        L01           
         BL    *+8                                                L01           
         LA    R2,1(R2)                                           L01           
*                                                                   L03         
DISPMULT CLI   PUSRSLN,PUSRSLNE  EXTENDED 93 (MULTIPLIER)           L03         
         BNE   NOTXTNDA                                             L03         
         OC    PUSRMULT,PUSRMULT                                    L03         
         BZ    NOTXTNDA                                             L03         
         MVI   8(R2),C'('                                           L03         
         MVC   WORK(12),=X'40202021204B20202020205D'              BUG01         
         XC    WORK+12(6),WORK+12                                   L03         
         ED    WORK(11),PUSRMULT                                    L03         
         LA    RF,WORK+1                                            L03         
CHKEF    CLI   0(RF),X'4B' SEE IF  DECIMAL POINT ENCOUNTERED        L03         
         BE    MOVEPCT                                              L03         
         CLI   0(RF),X'F0'                                          L03         
         BH    MOVEPCT                                              L03         
         LA    RF,1(RF)                                            L03          
         B     CHKEF                                                L03         
MOVEPCT  MVC   9(12,R2),0(RF)  MOVE MULTIPLIER AND LEFT PAREN       L03         
*                                                                   L03         
         LA    R1,WORK                                              L03         
         SR    RF,R1          GET LENGTH OF INDENTATION             L03         
         LA    R2,13(R2)                                            L03         
         SR    R2,RF                                                L03         
*                                                                 BUG01         
COMPARE  CLC   6(2,R2),=C'0)'                                     BUG01         
         BNE   NOTXTNDA                                           BUG01         
         MVI   6(R2),C')'                                         BUG01         
         BCTR  R2,0                                               BUG01         
         B     COMPARE                                            BUG01         
*                                                                 BUG01         
*                                                                 BUG01         
NOTXTNDA DS    0H                                                   L03         
*                                                                   L04         
*  LOOK TO SEE  IF THERE IS A DESCRIPTION FOR 93                    L04         
*                                                                   L04         
         CLI   P4,0        ANY LENGTH PRESENT                       L04         
         BE    NODESC93                                             L04         
         MVC   8(3,R2),=C',D='                                      L04         
         LA    R2,3(R2)                                             L04         
         ZIC   RF,P4                                                L04         
*                                                                   L04         
*  SEE IF DESCRIPTION FITS ON LINE                                  L04         
*                                                                   L04         
         L     RE,FULL         BEGINING OF LINE                     L04         
         AR    RF,R2           POTENTIAL END OF LINE                L04         
         SR    RF,RE                                                L04         
         CH    RF,=H'60'                                            L04         
         BNH   NONEWL                                               L04         
         BCTR  R2,0                                                 L04         
         BCTR  R2,0                                                 L04         
         BCTR  R2,0                                                 L04         
         MVC   8(3,R2),=C'   ' BLANK OUT ,D=                        L04         
         LA    R2,PRTSEL1H                                          L04         
         FOUT  (R2)                                                 L04         
         CLI   SELSW,1                                              L04         
         BNE   *+6                                                  L04         
         DC    H'0'                                                 L04         
         LA    R2,PRTSEL2H                                          L04         
         ST    R2,FULL                                              L04         
         SR    R3,R3                                                L04         
         MVC   8(2,R2),=C'D='                                       L04         
         LA    R2,2(R2)                                             L04         
NONEWL   ZIC   RF,P4                                                L04         
*                                                                   L04         
*                                                                   L04         
*                                                                   L04         
         SH    RF,=H'3'                                             L04         
         EX    RF,*+8                                               L04         
         B     *+10                                                 L04         
         MVC   8(0,R2),P4+1                                         L04         
         LA    R2,1(RF,R2)                                          L04         
*                                                                   L04         
*                                                                   L04         
NODESC93 DS    0H                                                   L03         
         B     DR12                                               L01           
DR10P    TM    PUSRSTS,X'06'  X'06'=MONTH ACCUMS+SUB-TOTAL ON MONTH BRK         
         BNO   DRNXTX                                             L02           
         MVI   8(R2),C'A'                                                       
LAR2BY1  DS    0H                                                 L02           
         LA    R2,1(R2)                                                         
         B     DR12                                                             
DRNXTX   TM    PUSRSTS,8        WEEKLY OPTION                     L02           
         BNO   DRNXT                                              L02           
         MVI   8(R2),C'W'                                         L02           
         B     LAR2BY1                                            L02           
*                                                                 L02           
*                                                                 L02           
DRNXT    TM    PUSRSTS,X'04'       X'04'=MONTH ACCUMS ON DATE FIELDS            
         BZ    DR11                                                             
         MVI   8(R2),C'M'                                                       
         LA    R2,1(R2)                                                         
         B     DR12                                                             
DR11     TM    PUSRSTS,X'02'       X'02'=TOTALS                                 
         BZ    DR11A5                                                           
         MVI   8(R2),C'T'                                                       
         LA    R2,1(R2)                                                         
         B     DR12                                                             
DR11A5   TM    PUSRSTS,X'01'       X'01'=SORT                                   
         BZ    DR12                                                             
         MVI   8(R2),C'S'                                                       
         LA    R2,1(R2)                                                         
DR12     MVI   8(R2),C','                                                       
         LA    R2,1(R2)                                                         
         LR    R1,R2                                                            
         L     RE,FULL                                                          
         SR    R1,R3            HOW MANY BYTES USED IN LINE                     
         CH    R1,=H'60'                                                        
         BH    DR15                                                             
         BAS   RE,NEXTEL                                                        
         BNE   DR17                                                             
         B     DR7                                                              
         SPACE                                                                  
DR15     DS    0H                                                               
         SH    R2,=H'1'            BLANK LAST COMMA OF 1ST                      
         MVI   8(R2),X'40'                 SELECT TWA LINE                      
         LA    R2,PRTSEL1H                                                      
         FOUT  (R2)                                                             
         CLI   SELSW,1                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         BAS   RE,NEXTEL                                                        
         BNE   DR20                                                             
         MVI   SELSW,1                                                          
         LA    R2,PRTSEL2H                                                      
         ST    R2,FULL          BEGINING ADDRESS FOR HOW MANY BYTES             
         XC    LINRM,LINRM                                                      
         SR    R3,R3                                                            
         B     DR7                                                              
         SPACE                                                                  
DR17     DS    0H                                                               
         SH    R2,=H'1'                                                         
         MVI   8(R2),X'40'                                                      
         LA    R2,1(R2)            RESET R2 TO HEADER                           
         LA    R2,PRTSEL1H                                                      
         CLI   SELSW,1                                                          
         BNE   *+8                                                              
         LA    R2,PRTSEL2H                                                      
         FOUT  (R2)                                                             
         B     DR20                                                             
*                                                                               
DR20     DS    0H                                                               
         MVI   ELCODE,X'15'                                                     
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   EXIT                                                             
         USING PUSRPELM,R6                                                      
         CLC   PUSRPLEN,=H'132'                                                 
         BNH   EXIT                                                             
         MVC   CONHEAD(45),=C'** NOTE **  -  REPORT REQUIRES WIDE PRINTX        
               ING'                                                             
         FOUT  CONHEADH                                                         
         OI    GENSTAT2,USMYOK                                                  
         B     EXIT                                                             
         EJECT                                                                  
         SPACE                                                                  
* DISPLAY KEY *                                                                 
DK       DS    0H                                                               
         L     R6,AIO                                                           
         USING PUSERREC,R6                                                      
         MVC   PRTMED(1),PUSRKMED                                               
         LA    R2,PRTMEDH                                                       
         FOUT  (R2)                                                             
         MVC   PRTCODE,PUSRKTYP                                                 
         LA    R2,PRTCODEH                                                      
         FOUT  (R2)                                                             
         B     EXIT                                                             
         EJECT                                                                  
         SPACE                                                                  
* LIST RECORDS *                                                                
LR       DS    0H                                                               
         CLI   MODE,PRINTREP                                                    
         BNE   LR02                                                             
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         LA    R1,HDRTN                                                         
         ST    R1,HEADHOOK                                                      
LR02     MVI   NLISTS,X'0E'                                                     
         MVI   ELCODE,X'10'                                                     
         LA    R2,LISTAR                                                        
         USING PLINE,R2                                                         
         OC    KEY(25),KEY                                                      
         BNZ   LR05                                                             
         MVC   KEY(25),SVKEY                                                    
LR05     GOTO1 HIGH                                                             
         B     LR12                                                             
         SPACE                                                                  
LR10     GOTO1 SEQ                                                              
LR12     CLC   KEY(3),KEYSAVE              AGY/MED                              
         BNE   LRX                                                              
         CLI   KEY+3,X'30'                 REC ID                               
         BNE   LR10                                                             
         L     R6,AIO                                                           
         USING PUSERREC,R6                                                      
         GOTO1 GETREC                                                           
         MVC   PCDE,PUSRKTYP                                                    
         BAS   RE,GETEL                                                         
         USING PUSRTELM,R6                                                      
         BE    *+14                                                             
         MVC   PTITLE(19),=C'** NOT AVAILABLE **'                               
         B     LR20                                                             
         MVC   PTITLE,PUSRTNM                                                   
LR20     DS    0H                                                               
LR30     GOTO1 LISTMON                                                          
         B     LR10                                                             
         SPACE                                                                  
LRX      B     EXIT                                                             
         EJECT                                                                  
* LIST RECORDS *                                                                
PR       DS    0H                                                               
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         LA    R1,HDRTN                                                         
         ST    R1,HEADHOOK                                                      
PR02     DS    0H                                                               
         MVI   ELCODE,X'10'                                                     
         LA    R2,LISTAR                                                        
         USING PLINE,R2                                                         
         OC    KEY(25),KEY                                                      
         BNZ   PR05                                                             
         MVC   KEY(25),SVKEY                                                    
PR05     GOTO1 HIGH                                                             
         B     PR12                                                             
         SPACE                                                                  
PR10     GOTO1 SEQ                                                              
PR12     CLC   KEY(3),KEYSAVE              AGY/MED                              
         BNE   PRX                                                              
         CLI   KEY+3,X'30'                 REC ID                               
         BNE   PR10                                                             
         CLC   SVKEY+4(4),=4X'00'          IS THERE REC CODE                    
         BE    *+14                                                             
         CLC   SVKEY+4(4),KEY+4            TEST FOR REQUIRED REC CODE           
         BNE   PR10                                                             
         L     R6,AIO                                                           
         USING PUSERREC,R6                                                      
         GOTO1 GETREC                                                           
*                                                                               
         LA    R2,LISTAR                                                        
         MVC   PCDE,PUSRKTYP                                                    
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         USING PUSRTELM,R6                                                      
         BE    *+14                                                             
         MVC   PTITLE(19),=C'** NOT AVAILABLE **'                               
         B     PR20                                                             
         MVC   PTITLE,PUSRTNM                                                   
PR20     DS    0H                                                               
         MVC   P+5(L'PLINE),PLINE                                               
*                                                                               
         LA    R2,P2+5                                                          
*                                                                               
         LA    R4,PTITLE                                                        
*                                                                               
         MVI   ELCODE,X'20'                                                     
         USING PUSRSELM,R6                                                      
PR21     BAS   RE,NEXTEL                                                        
         BNE   PR25                                                             
         LA    R5,SELTBL                                                        
         ZIC   R1,PUSRSSEL                                                      
         CVD   R1,DUB                                                           
*                                                                               
         TM    PUSRSTS,X'10'       SEE IF OPEN                                  
         BZ    PR21B                                                            
         SP    DUB,=P'100'                                                      
         B     PR21X                                                            
*                                                                               
PR21B    TM    PUSRSTS,X'20'       SEE IF DIFFERENCE                            
         BZ    PR21X                                                            
         SP    DUB,=P'160'                                                      
*                                                                               
PR21X    DS    0H                                                               
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(2),DUB                                                      
PR22     CLC   0(2,R5),WORK                                                     
         BE    PR23                                                             
         ZIC   R1,4(R5)            L'OF ENTRY IN SELTABLE                       
         AR    R5,R1                                                            
         CLI   0(R5),0                                                          
         BNE   PR22                                                             
         DC    H'0'                NO MATCH, END OF TABLE                       
PR23     ZIC   R1,5(R5)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),6(R5)                                                    
         AR    R4,R1               INCREMENT P LINE                             
         CLI   PUSRSSEL,X'1A'      SEE IF BLANK COLUMN                          
         BE    PR24                                                             
*                                                                               
         TM    PUSRSTS,X'10'        SEE IF OPEN                                 
         BZ    PR23C                                                            
         MVC   0(6,R4),=C'(OPEN)'                                               
         LA    R4,6(R4)                                                         
         B     PR23F                                                            
*                                                                               
PR23C    TM    PUSRSTS,X'20'        SEE IF DIFFERENCE                           
         BZ    PR23F                                                            
         MVC   0(6,R4),=C'(DIFF)'                                               
         LA    R4,6(R4)                                                         
*                                                                               
PR23F    MVI   0(R4),C','                                                       
PR24     LA    R4,1(R4)                                                         
         B     PR21                                                             
         SPACE                                                                  
PR25     DS    0H                                                               
*                                                                               
         BCTR  R4,0                                                             
         MVI   0(R4),C' '          BLANK OUT LAST COMMA                         
*                                                                               
         LA    R2,P3+5                                                          
         LA    R4,PTITLE-8                                                      
         LR    R2,R4                                                            
         MVC   8(4,R2),=C'SEL='                                                 
         LA    R2,4(R2)                                                         
*                                                                 L08           
DISR5    DS    0H                                                               
         XC    P4(22),P4         CLEAR 93 DESCRIPTION               L04         
         XC    ELEM,ELEM                                            L04         
         L     R6,AIO                                               L04         
         MVI   ELCODE,93                                            L04         
         BAS   RE,GETEL                                             L04         
         BNE   DISR6                                                L04         
         MVC   P4(21),1(R6) MOVE W/O ID                             L04         
*                                                                               
DISR6    MVI   ELCODE,X'20'                                                     
         ST    R2,FULL     START OF LINE ADDRESS                                
         L     R6,AIO                                                           
         USING PUSRSELM,R6                                                      
         BAS   RE,GETEL                                                         
         BE    DISR7                                                            
         DC    H'0'                                                             
*                                                                               
DISR7    TM    PUSRSTS,X'10'              X'10' IS OPEN RATES.                  
         BNO   DISR9                                                            
         ZIC   R1,PUSRSSEL                                                      
         CVD   R1,DUB                                                           
         SP    DUB,=P'100'                OPEN RATES ARE STORED AT 100          
         OI    DUB+7,X'0F'                     GREATER SO MUST SUBTRACT         
         UNPK  8(2,R2),DUB                     100 BEFORE DISPLAYING.           
         MVI   10(R2),C'O'                DISPLAY O FOR OPEN RATES.             
         LA    R2,3(R2)                                                         
         B     DISR12                                                           
DISR9    TM    PUSRSTS,X'20'              X'20' IS DIFFERENCE OF OPEN           
         BNO   DISR10                           AND CONTRACT.                   
         ZIC   R1,PUSRSSEL                                                      
         CVD   R1,DUB                                                           
         SP    DUB,=P'160'                DIFFERENCE IS STORED AT 160           
         OI    DUB+7,X'0F'                    GREATER SO SUBTRACT 160           
         UNPK  8(2,R2),DUB                    BEFORE DISPLAYING.                
         MVI   10(R2),C'D'                DISPLAY D FOR DIFFERENCE.             
         LA    R2,3(R2)                                                         
         B     DISR12                                                           
*                                                                               
*****                                                                           
DISR10   DS    0H                                                               
         CLI   PUSRSSEL,X'1A'        IS IT BLANK COLUMN                         
         BNE   DISR10C                                                          
         CLI   PUSRSDSP,X'02'        IS IT DEFAULT LENGTH                       
         BE    DISR10M                                                          
         B     DISR10J                                                          
*                                                                               
DISR10C  CLI   PUSRSSEL,X'13'        IS IT COMMENT FIELD  #19                   
         BNE   DISR10M                                                          
         CLI   PUSRSDSP,X'30'        IS IT DEFAULT LENGTH                       
         BE    DISR10M                                                          
DISR10J  ZIC   R1,PUSRSSEL                                                      
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  8(2,R2),DUB                                                      
         MVI   10(R2),C'('                                                      
         MVI   13(R2),C')'                                                      
         ZIC   R1,PUSRSDSP                                                      
         SH    R1,=H'1'                                                         
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  11(2,R2),DUB                                                     
         LA    R2,6(R2)                                                         
         B     DISR10P                                                          
*****                                                                           
DISR10M  ZIC   R1,PUSRSSEL                                                      
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  8(2,R2),DUB                                                      
         LA    R2,2(R2)                                                         
         CLI   PUSRSDSP,X'F0'     NO PRINT INDICATOR                L04         
         BNE   DISR10Q                                              L04         
         MVC   8(2,R2),=C'NP'                                       L04         
         LA    R2,2(R2)                                             L04         
DISR10Q  DS    0H                                                   L04         
*                                                                   L04         
**                                                                L01           
         CLI   PUSRSSEL,93    CALCULATION COLUMN                  L01           
         BNE   DISR10P                                            L01           
         CLI   PUSRSLN,PUSRSLNE  EXTENDED 93 (MULTIPLIER)           L03         
         BNE   DISR10S                                              L03         
         CLI   PUSROPT,C'A'  IF = COMBINATION OF SUMS AND MULTIPLY  L03         
         BNE   DISR10W                                              L03         
DISR10S  DS    0H                                                   L03         
         MVI   8(R2),C'+'                                         L01           
         TM    PUSRSSRT,X'80'    THREE COLUMNS                    L01           
         BNO   *+8                                                L01           
         MVI   9(R2),C'+'                                         L01           
         TM    PUSRSSRT,X'40'     SUBTRACT COLS 1 &2              L01           
         BNO   *+8                                                L01           
         MVI   8(R2),C'-'                                         L01           
         TM    PUSRSSRT,X'20'    SUB COLS 2 & 3                   L01           
         BNO   *+8                                                L01           
         MVI   9(R2),C'-'                                         L01           
         LA    R2,1(R2)         ASSUME ONLY TWO COLUMNS           L01           
         CLI   8(R2),X'4D'                                        L01           
         BL    *+8                                                L01           
         LA    R2,1(R2)                                           L01           
*                                                                   L03         
DISR10W  CLI   PUSRSLN,PUSRSLNE  EXTENDED 93 (MULTIPLIER)           L03         
         BNE   DISR10XX                                             L03         
         OC    PUSRMULT,PUSRMULT                                    L03         
         BZ    DISR10XX                                             L03         
         MVI   8(R2),C'('                                           L03         
         MVC   WORK(12),=X'40202021204B20202020205D'              BUG01         
         XC    WORK+12(6),WORK+12                                   L03         
         ED    WORK(11),PUSRMULT                                    L03         
         LA    RF,WORK+1                                            L03         
*                                                                               
DISR10Y  CLI   0(RF),X'4B' SEE IF  DECIMAL POINT ENCOUNTERED        L03         
         BE    DISR10Z                                              L03         
         CLI   0(RF),X'F0'                                          L03         
         BH    DISR10Z                                              L03         
         LA    RF,1(RF)                                            L03          
         B     DISR10Y                                              L03         
*                                                                               
DISR10Z  MVC   9(12,R2),0(RF)  MOVE MULTIPLIER AND LEFT PAREN       L03         
*                                                                   L03         
         LA    R1,WORK                                              L03         
         SR    RF,R1          GET LENGTH OF INDENTATION             L03         
         LA    R2,13(R2)                                            L03         
         SR    R2,RF                                                L03         
*                                                                 BUG01         
DISR10Z5 CLC   6(2,R2),=C'0)'                                     BUG01         
         BNE   DISR10XX                                           BUG01         
         MVI   6(R2),C')'                                         BUG01         
         BCTR  R2,0                                               BUG01         
         B     DISR10Z5                                           BUG01         
*                                                                 BUG01         
*                                                                 BUG01         
DISR10XX DS    0H                                                   L03         
*                                                                   L04         
*  LOOK TO SEE  IF THERE IS A DESCRIPTION FOR 93                    L04         
*                                                                   L04         
         CLI   P4,0        ANY LENGTH PRESENT                       L04         
         BE    DRDESC93                                             L04         
         MVC   8(3,R2),=C',D='                                      L04         
         LA    R2,3(R2)                                             L04         
*                                                                               
DRNEWL   ZIC   RF,P4                                                L04         
*                                                                   L04         
         SH    RF,=H'3'                                             L04         
         EX    RF,*+8                                               L04         
         B     *+10                                                 L04         
         MVC   8(0,R2),P4+1                                         L04         
         LA    R2,1(RF,R2)                                          L04         
*                                                                   L04         
*                                                                   L04         
DRDESC93 DS    0H                                                   L03         
         B     DISR12                                             L01           
DISR10P  TM    PUSRSTS,X'06'  X'06'=MONTH ACCUMS+SUB-TOTAL ON MONTH BRK         
         BNO   DISRNXTX                                           L02           
         MVI   8(R2),C'A'                                                       
         DS    0H                                                 L02           
         LA    R2,1(R2)                                                         
         B     DISR12                                                           
DISRNXTX TM    PUSRSTS,8        WEEKLY OPTION                     L02           
         BNO   DISRNXT                                            L02           
         MVI   8(R2),C'W'                                         L02           
         B     LAR2BY1                                            L02           
*                                                                 L02           
*                                                                 L02           
DISRNXT  TM    PUSRSTS,X'04'       X'04'=MONTH ACCUMS ON DATE FIELDS            
         BZ    DISR11                                                           
         MVI   8(R2),C'M'                                                       
         LA    R2,1(R2)                                                         
         B     DISR12                                                           
DISR11   TM    PUSRSTS,X'02'       X'02'=TOTALS                                 
         BZ    DISR11A5                                                         
         MVI   8(R2),C'T'                                                       
         LA    R2,1(R2)                                                         
         B     DISR12                                                           
DISR11A5 TM    PUSRSTS,X'01'       X'01'=SORT                                   
         BZ    DISR12                                                           
         MVI   8(R2),C'S'                                                       
         LA    R2,1(R2)                                                         
DISR12   MVI   8(R2),C','                                                       
         LA    R2,1(R2)                                                         
         BAS   RE,NEXTEL                                                        
         BNE   DISR17                                                           
         B     DISR7                                                            
         SPACE                                                                  
         SPACE                                                                  
DISR17   DS    0H                                                               
         SH    R2,=H'1'                                                         
         MVI   8(R2),X'40'                                                      
*                                                                               
DISR20   DS    0H                                                               
         MVI   ELCODE,X'15'                                                     
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   PR100                                                            
         USING PUSRPELM,R6                                                      
         CLC   PUSRPLEN,=H'132'                                                 
         BNH   PR100                                                            
         LA    R4,P4                                                            
         LA    R4,PTITLE-PLINE(R4)                                              
         MVC   0(45,R4),=C'** NOTE **  -  REPORT REQUIRES WIDE PRINTINGX        
               '                                                                
*                                                                               
PR100    GOTO1 SPOOL,DMCB,(R8)                                                  
         B     PR10                                                             
         SPACE                                                                  
PRX      B     EXIT                                                             
         EJECT                                                                  
         SPACE                                                                  
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
         SPACE                                                                  
*                                                                               
* CHECK SCAN OUTPUT FOR ERRORS                                                  
SCANCHK  NTR1                                                                   
         LA    R3,1                                                             
         LR    R1,R4               VALID FIELD=NN(T/S)                          
SCAN10   CLI   0(R1),2                                                          
         BL    SCANERR                                                          
         CLC  12(2,R1),=C'D=' 93 TITLE DESCRIPTION                  L04         
         BNE  CLIR18                                                L04         
         CLI  0(R1),21                                              L04         
         BL   LA132                                                 L04         
         MVC   CONHEAD(16),=C'COMMENT M/B < 20'                     L04         
         B     LAR2PRT                                              L04         
*                                                                   L04         
CLIR18   CLI   0(R1),8        LEN M/B LT 8                          L03         
         BL    LESSTH8                                              L03         
         CLC   12(2,R1),=C'93'   IF 93 LEN COULD BE UP TO 16        L03         
         BNE   SCANERR                                              L03         
         CLI   0(R1),16                                             L03         
         BH    SCANERR                                              L03         
LESSTH8  DS    0H                                                   L03         
         LA    R3,1(R3)                                                         
LA132    DS    0H                                                   L03         
         LA    R1,32(R1)                                                        
         CLI   0(R1),0                                                          
         BNE   SCAN10                                                           
* CHECK FOR DUPLICATE CODE ENTRIES                                              
         LA    R1,12(R4)                                                        
         LA    R5,32(R1)                                                        
SCAN20   CLC   0(2,R1),0(R5)                                                    
         BNE   SCAN30                                                           
*                                                                               
         CLI   2(R1),C'O'                                                       
         BE    SCAN25                                                           
         CLI   2(R1),C'D'                                                       
         BE    SCAN25                                                           
         CLI   2(R5),C'O'                                                       
         BE    SCAN25                                                           
         CLI   2(R5),C'D'                                                       
         BNE   SCAN28                                                           
SCAN25   CLC   2(1,R1),2(R5)                                                    
         BNE   SCAN30                                                           
*                                                                               
SCAN28   DS    0H                                                               
*****                                                                           
         CLC   0(2,R1),=C'26'     SEE IF BLANK COLUMN - ALLOW DUPS              
         BE    SCAN30                                                           
*****                                                                           
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(32),=C'** ERROR - DUPLICATE CODE #   **'                 
         MVC   CONHEAD+27(2),0(R5)                                              
         LA    R2,CONHEADH                                                      
         FOUT  (R2)                                                             
         LA    R2,PRTSEL1H                                                      
         OI    6(R2),X'40'                                                      
         GOTO1 ERREX2                                                           
         SPACE                                                                  
SCAN30   LA    R5,32(R5)                                                        
         CLC   0(2,R5),=X'0000'                                                 
         BNE   SCAN20                                                           
         LA    R1,32(R1)                                                        
         CLC   0(2,R1),=X'0000'                                                 
         BE    COMNTCHK                                                         
         LA    R5,32(R1)                                                        
         B     SCAN20                                                           
         EJECT                                                                  
* COMMENT CHECK - COMMENT NEEDS INSERTION DATE     *                            
*               - BUT NO M/A TOTALS ON INERTION DATE *                          
COMNTCHK DS    0H                                                               
         LR    R5,R4                                                            
         LR    R1,R4               SAVE FOR REG/DST CHK                         
         SPACE                                                                  
COM10    CLC   12(2,R5),=C'19'     IS IT A COMMENT ID                           
         BNE   COM25                                                            
COM15    CLC   12(2,R4),=C'30'     IS IT AN INSERTION DATE                      
         BNE   COM20                                                            
         CLI   14(R4),C'M'         IS IT M TOTALS                               
         BE    CMNTERR1            #1 NO M TOTS IF COMMENTS                     
         CLI   14(R4),C'A'         IS IT A TOTALS                               
         BE    CMNTERR1                                                         
         B     COMX                                                             
COM20    LA    R4,32(R4)                                                        
         CLI   0(R4),0                                                          
         BE    CMNTERR2            #2 COMMENT NEEDS INSERTION DATE              
         B     COM15                                                            
COM25    LA    R5,32(R5)                                                        
         CLI   0(R5),0                                                          
         BNE   COM10                                                            
COMX     B     RDCHK                                                            
         SPACE 2                                                                
* REG/DST CHECK - REG/DST NAME NEEDS REG/DST CODE  *                            
         SPACE                                                                  
RDCHK    DS    0H                                                               
         LR    R4,R1                                                            
         LR    R5,R4                                                            
         LA    R3,1                                                             
RDC10    CLC   12(2,R5),=C'04'     IS IT REGION NAME                            
         BNE   RDC25                                                            
RDC15    CLC   12(2,R4),=C'02'     IS IT REGION CODE                            
         BE    RDCX                                                             
RDC20    LA    R4,32(R4)                                                        
         CLI   0(R4),0                                                          
         BE    REGNMERR            REG NAME NEEDS REG CODE                      
         B     RDC15                                                            
RDC25    LA    R5,32(R5)                                                        
         LA    R3,1(R3)                                                         
         CLI   0(R5),0                                                          
         BNE   RDC10                                                            
         SPACE                                                                  
RDCX     LR    R4,R1                                                            
         LR    R5,R4                                                            
         LA    R3,1                                                             
RDC30    CLC   12(2,R5),=C'05'     IS IT DISTRICT NAME                          
         BNE   RDC45                                                            
RDC35    CLC   12(2,R4),=C'03'     IS IT DISTRICT CODE                          
         BE    RDCXX                                                            
RDC40    LA    R4,32(R4)                                                        
         CLI   0(R4),0                                                          
         BE    DSTNMERR            DST NAME NEEDS REG CODE                      
         B     RDC35                                                            
RDC45    LA    R5,32(R5)                                                        
         LA    R3,1(R3)                                                         
         CLI   0(R5),0                                                          
         BNE   RDC30                                                            
         SPACE                                                                  
RDCXX    B     EXIT                                                             
         EJECT                                                                  
**  CHECK FOR DUPLICATE COMMAS, SCANNER DOES NOT **                             
CCHK     DS    0H                                                               
         LA    R3,8(R2)                                                         
CC10     CLI   0(R3),C','                                                       
         BNE   CC20                                                             
         CLI   1(R3),C','                                                       
         BNE   CC20                                                             
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(30),=C'** ERROR - DUPLICATE COMMAS **'                   
         LA    R2,PRTSEL1H                                                      
         OI    6(R2),X'40'                                                      
         GOTO1 ERREX2                                                           
CC20     LA    R3,1(R3)                                                         
         CLI   0(R3),0                                                          
         BER   RE                                                               
         B     CC10                                                             
*********                                                                       
GETCOMNT NTR1                     SAVE REG                        L01           
         MVC   WORK(6),=6C' '                                    L01            
         MVC   WORK+6(6),8(R2)   COMMENT                         L01            
         OC    WORK+6(6),=6C' '  RIGHT JUSTIFY COMMENT NUMBER    L01            
         LA    RE,WORK+6                                         L01            
         CLI   5(RE),C' '                                         L01           
         BNE   *+8                                                L01           
         BCT   RE,*-8                                             L01           
         MVC   SVKEY,KEY                                          L01           
         XC    KEY+3(22),KEY+3                                    L01           
         MVI   KEY+3,X'40'        COMMENT ID                      L01           
         MVC   KEY+4(6),0(RE)     COMMENT NO                      L01           
         MVC   WORK(6),KEY+4      ACCESSABILITY TO CALLING PGM    L01           
         STC   RF,COMERR+8         FORCE COMMENT NUMBER           L01           
         GOTO1 HIGH               READ FOR ADDRESS                L01           
         CLC   KEY(20),KEYSAVE    GOOD READ                       L01           
         BE    OUTCMNT                                            L01           
         XC    CONHEAD,CONHEAD                                    L01           
         MVC   CONHEAD(L'COMERR),COMERR                           L01           
         CLI   *,0     FORCE UNEQUAL                              L01           
         B     EXIT                                               L01           
OUTCMNT  MVC   KEY,SVKEY          RENORMALIZE D/M                 L01           
         GOTO1 HIGH                                               L01           
         CLI   *,X'95' FORCE EQUAL                                L01           
         B     EXIT                                               L01           
*                                                                 L01           
*                                                                 L01           
*                                                                 L01           
COMERR   DC    C'COMMENT 1 RECORD DOES NOT EXIST'                 L01           
ELMNT    DC    C'PREVIOUS 2/3 FIELDS NOT TOTALS'                  L01           
OPER     DC    C'VALID OPERATORS FOR #93 ARE +-'                  L01           
JUST1    DC    X'0'                                               L01           
*****                                                             L01           
         SPACE                                                                  
*                                                                               
SCANERR  DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(32),=C'** ERROR IN SELECTION LIST #    '                 
         LA    R5,CONHEAD+29                                                    
         EDIT  (R3),(2,(R5))                                                    
         B     TSERRX2                                                          
*                                                                               
INVSEL   DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(38),=C'** ERROR-    IS INVALID SELECTION CODE'           
         MVC   CONHEAD+10(2),12(R4)                                             
         B     TSERRX2                                                          
*                                                                               
TMERR    DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(52),=C'** ERROR-SELECTION #    M CODE ON DATE FIX        
               ELDS ONLY **'                                                    
         B     TSERRX1                                                          
*                                                                               
CMNTERR1 DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(57),=C'** ERROR - IF COMMENT(19), THEN NO M/A ONX        
                INS DATE(30) **'                                                
         B     TSERRX2                                                          
*                                                                               
CMNTERR2 DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(44),=C'** ERROR - COMMENT(19) NEEDS INS DATE(30)X        
                **'                                                             
         B TSERRX2                                                              
*                                                                               
TSERR    DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(55),=C'** ERROR-SELECTION #    NO TOTALS/SORT ONX        
                DOLLAR FIELDS'                                                  
TSERRX1  LA    R5,CONHEAD+20                                                    
         EDIT  (R3),(2,(R5)),ALIGN=LEFT                                         
TSERRX2  LA    R2,PRTSEL1H                                                      
TSERRX2A DS    0H                                                 L01           
         OI    6(R2),X'40'                                                      
         GOTO1 ERREX2                                                           
*                                                                               
TSERR2   DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(38),=C'** ERROR-SELECTION #    NO TOT/SORT **'           
         LA    R5,CONHEAD+20                                                    
         EDIT  (R3),(2,(R5)),ALIGN=LEFT                                         
         LA    R2,PRTSEL1H                                                      
         OI    6(R2),X'40'                                                      
         GOTO1 ERREX2                                                           
*                                                                               
REGNMERR DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(48),=C'** ERROR-SELECTION #   REG NAME NEEDS REGX        
                CODE **'                                                        
         LA    R5,CONHEAD+20                                                    
         EDIT  (R3),(2,(R5)),ALIGN=LEFT                                         
         LA    R2,PRTSEL1H                                                      
         OI    6(R2),X'40'                                                      
         GOTO1 ERREX2                                                           
*                                                                               
DSTNMERR DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(48),=C'** ERROR-SELECTION #   DST NAME NEEDS DSTX        
                CODE **'                                                        
         LA    R5,CONHEAD+20                                                    
         EDIT  (R3),(2,(R5)),ALIGN=LEFT                                         
         LA    R2,PRTSEL1H                                                      
         OI    6(R2),X'40'                                                      
         GOTO1 ERREX2                                                           
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
HDRTN    NTR1                                                                   
         MVC   H2+10(L'QMED),QMED                                               
         MVC   H2+15(L'MEDNM),MEDNM                                             
         B     EXIT                                                             
         SPACE 2                                                                
HEADING  SSPEC H1,3,REQUESTOR                                                   
         SSPEC H2,3,C'MEDIA'                                                    
         SSPEC H1,46,C'USER PRINT REPORT'                                       
         SSPEC H2,46,C'-----------------'                                       
         SSPEC H1,73,AGYNAME                                                    
         SSPEC H2,73,AGYADD                                                     
         SSPEC H3,73,REPORT                                                     
         SSPEC H4,73,RUN                                                        
         SSPEC H5,103,PAGE                                                      
         SSPEC H7,6,C'CODE'                                                     
         SSPEC H8,6,C'----'                                                     
         SSPEC H7,15,C'REPORT TITLE'                                            
         SSPEC H8,15,C'------------'                                            
         DC    X'00'                                                            
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
         SPACE                                                                  
*  TABLE OF SELECTION CODES AND LENGTHS                                         
*        CL2  SELECTION CODE                                                    
*        XL1  SORT LENGTH - 255=NO SORT ON THIS FIELD                           
*        XL1  DISPLAY LENGTH                                                    
*        XL1  ENTRY LENGTH                                                      
*        XL1  FIELD NAME LENGTH (*THIS DATA IS NOT USED BY ESB RECS)            
*                               (*NAMES IN PPREPL102 PRGRM ARE USED)            
*        SELECTION CODES                                                        
*        30-39   FOR DATE FIELDS                                                
*        40-89   FOR COST FIELDS                                                
         SPACE 2                                                                
SELTBL   DS    0C                                                               
         DC    C'01',AL1(03,7,13,07),C'CLIENT '            CLIENT               
         DC    C'02',AL1(11,7,13,07),C'REGION '            REGION               
* NOTE THAT SORT LEN OF REGION =11 (DIV/REG/DST/SHR/SUM)                        
* THIS MEANS THAT DISTRICT WILL BE CARRIED UNDER REGION ID                      
* IN THE SORTREC.  DISTRICT RTN IN TLIN (IN PPREPL102)                          
* WILL BE A DUMMY RTN.                                                          
         DC    C'03',AL1(03,9,15,09),C'DISTRICT '       DISTRICT                
         DC    C'04',AL1(255,21,17,11),C'REGION NAME'   REGION NAME             
         DC    C'05',AL1(255,21,19,13),C'DISTRICT NAME'  DISTRICT NAME          
         DC    C'10',AL1(06,18,10,04),C'PUB '           PUB                     
         DC    C'11',AL1(20,21,15,09),C'PUB NAME '      PUB NAME                
         DC    C'12',AL1(03,4,15,09),C'PRD CODE '       PRODUCT CODE            
         DC    C'13',AL1(20,21,15,09),C'PRD NAME '      PRODUCT NAME            
         DC    C'14',AL1(02,4,10,04),C'EST '            ESTIMATE                
         DC    C'15',AL1(17,18,16,10),C'SPACE DES '     SPACE DESCR             
         DC    C'16',AL1(03,4,10,04),C'DIV '            DIVISION                
         DC    C'17',AL1(06,8,14,08),C'AD CODE '        AD CODE                 
         DC    C'18',AL1(17,18,15,09),C'COPY NUM '      COPY NUMBER             
         DC    C'19',AL1(10,48,15,09),C'COMMENTS '      COMMENTS                
         DC    C'20',AL1(17,21,21,14),C'ESTIMATE DESCR '                        
         DC    C'21',AL1(09,10,15,09),C'ASPO NUM '      ASPO NUMBER             
         DC    C'22',AL1(03,05,16,10),C'LIST CODE '     LIST CODE               
         DC    C'23',AL1(05,13,15,09),C'LAST I/O '      LAST INSER ORDR         
         DC    C'24',AL1(25,26,14,08),C'CAPTION '       CAPTION                 
         DC    C'25',AL1(03,12,15,09),C'LAST AD  '      LAST ADCODE USE         
         DC    C'26',AL1(255,02,07,01),C' '             BLANK COLUMN            
         DC    C'27',AL1(05,08,14,08),C'AD CODE '       5 CHAR SORT             
         DC    C'28',AL1(20,21,21,15),C'DIVISION NAME  ' DIVISION NAME          
         DC    C'30',AL1(05,12,15,09),C'INS DATE '      INSERTION DATE          
         DC    C'31',AL1(03,9,16,10),C'CLOS DATE '      CLOSING DATE            
         DC    C'32',AL1(03,9,19,13),C'ON-SALE DATE '   ON-SALE DATE            
         DC    C'33',AL1(03,9,15,09),C'PAY DATE '       PAYABLE DATE            
         DC    C'34',AL1(03,9,15,09),C'BIL DATE '       BILLABLE DATE           
         DC    C'35',AL1(03,9,20,09),C'MAT CLOSE DATE'  MAT CLOSE DTE           
         DC    C'36',AL1(02,5,14,08),C'OAN CODE'        OAN CODE                
         DC    C'37',AL1(33,33,14,08),C'OAN NAME'        OAN NAME               
         DC    C'38',AL1(35,38,21,15),C'OAN NAME + CODE' OAN NAME& CODE         
         DC    C'40',AL1(255,15,16,09),C'ORD GROSS '    ORDERED GROSS           
         DC    C'41',AL1(255,15,14,08),C'ORD NET '      ORDERED NET             
         DC    C'42',AL1(255,15,19,13),C'ORD GROSS-CD ' ORDRED GROSS-CD         
         DC    C'43',AL1(255,15,17,11),C'ORD NET-CD '   ORDERED NET-CD          
         DC    C'44',AL1(255,15,19,13),C'ORD AGY-COMM ' ORDRED AGY-COMM         
         DC    C'45',AL1(255,15,13,07),C'ORD CD '       ORDERED CD              
         DC    C'46',AL1(255,15,11,05),C'COST '                   L01           
         DC    C'50',AL1(255,15,17,11),C'PAID GROSS '   PAID GROSS              
         DC    C'51',AL1(255,15,15,09),C'PAID NET '     PAID NET                
         DC    C'52',AL1(255,15,20,14),C'PAID GROSS-CD ' PAID GROSS-CD          
         DC    C'53',AL1(255,15,18,12),C'PAID NET-CD '   PAID NET-CD            
         DC    C'54',AL1(255,15,20,14),C'PAID AGY-COMM ' PAID AGY-COMM          
         DC    C'55',AL1(255,15,14,08),C'PAID CD '      PAID CD                 
         DC    C'60',AL1(255,15,16,10),C'BIL GROSS '    BILLED GROSS            
         DC    C'61',AL1(255,15,14,08),C'BIL NET '      BILLED NET              
         DC    C'62',AL1(255,15,19,13),C'BIL GROSS-CD ' BILLED GROSS-CD         
         DC    C'63',AL1(255,15,17,11),C'BIL NET-CD '   BILLED NET-CD           
         DC    C'64',AL1(255,15,19,13),C'BIL AGY-COMM ' BILLED AGY-COMM         
         DC    C'65',AL1(255,15,13,07),C'BIL CD '       BILLED CD               
         DC    C'70',AL1(255,15,17,11),C'UNPD GROSS '   UNPAID GROSS            
         DC    C'71',AL1(255,15,15,09),C'UNPD NET '     UNPAID NET              
         DC    C'72',AL1(255,15,20,14),C'UNPD GROSS-CD ' UNPAD GROSS-CD         
         DC    C'73',AL1(255,15,18,12),C'UNPD NET-CD '  UNPAID NET-CD           
         DC    C'74',AL1(255,15,20,14),C'UNPD AGY-COMM ' UNPAD AGY-COMM         
         DC    C'75',AL1(255,15,14,08),C'UNPD CD '      UNPAID CD               
         DC    C'80',AL1(255,15,18,12),C'BILBL GROSS '  BILLABLE GROSS          
         DC    C'81',AL1(255,15,16,10),C'BILBL NET '    BILLABLE NET            
         DC    C'82',AL1(255,15,19,13),C'BILBL GRS-CD ' BLBL GROSS-CD           
         DC    C'83',AL1(255,15,19,13),C'BILBL NET-CD ' BILLABLE NET-CD         
         DC    C'84',AL1(255,15,20,14),C'BILBL AGY-COM ' BILBL AGY-COMM         
         DC    C'85',AL1(255,15,15,09),C'BILBL CD '     BILLABLE CD             
         DC    C'86',AL1(255,06,20,13),C'NUM OF INSERTS' NUM OF INSERTS         
         DC    C'90',AL1(255,15,17,11),C'PLAND COST '   PLANNED COST            
         DC    C'91',AL1(255,15,21,15),C'ACT LESS PLAND ' ACTUAL-PLAND          
         DC    C'92',AL1(255,15,10,04),C'TAX '            TAX                   
         DC    C'93',AL1(255,15,15,09),C'CALC COL '               L01           
         DC    X'0000'                                                          
         EJECT                                                                  
         SPACE                                                                  
       ++INCLUDE DDSPOOLD                                                       
ARITH    DS    CL1                                                  L03         
OPERA    DS    CL1                                                  L03         
         EJECT                                                                  
       ++INCLUDE DDFLDIND                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE PRSFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFMF2D                                          L09          
         EJECT                                                                  
       ++INCLUDE PRSFMWORKD                                                     
       EJECT                                                                    
*                           ***** T41C51 SAVED WORK AREAS *****                 
*                                                                               
         ORG   SYSSPARE                                                         
SELSW    DS    CL1                                                              
LINRM    DS    F                                                                
*                                                                               
PLINED   DSECT               ***** PRINT LINE DSECT *****                       
PLINE    DS    0CL75                                                            
PCDE     DS    CL4                                                              
         DS    CL5                                                              
PTITLE   DS    CL30                                                             
         DS    CL39                                                             
         SPACE 2                                                                
         PRINT ON                                                               
PUSRECD  DSECT                                                                  
       ++INCLUDE PUSEREC                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'039PRSFM51   06/22/10'                                      
         END                                                                    
