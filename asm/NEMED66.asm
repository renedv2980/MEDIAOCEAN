*          DATA SET NEMED66    AT LEVEL 003 AS OF 07/16/04                      
*          DATA SET NEMED66    AT LEVEL 005 AS OF 08/20/99                      
*          DATA SET NEMED66    AT LEVEL 231 AS OF 07/12/99                      
*PHASE T31E66A,+0                                                               
         TITLE 'T31E66 - NETWORK POST-BUY EVALUATION'                           
*                                                                               
         PRINT NOGEN                                                            
**                                                                              
         MACRO                                                                  
&NAME    LKPRD &P1,&P2                                                          
* LOOK UP PRODUCT. &2 IS CODE TO LOOKUP, &1 IS OUTPUT AREA (3-BYTE)             
* ASSUMES CLIENT RECORD IS IN ANETWS1                                           
&NAME    L     R1,ANETWS1                                                       
         USING CLTHDR,R1                                                        
         LA    R1,CLIST                                                         
         DROP  R1                                                               
LKP&SYSNDX  CLI   0(R1),0             CK FOR END OF LIST                        
         BE    XKP&SYSNDX                                                       
         CLC   3(1,R1),&P2                                                      
         BE    XKP&SYSNDX                                                       
         LA    R1,4(R1)                                                         
         B     LKP&SYSNDX                                                       
XKP&SYSNDX  MVC   &P1.(3),0(R1)                                                 
         MEND                                                                   
*                                                                               
         MACRO                                                                  
&NAME    CPP   &P1,&P2,&P3                                                      
         L     R1,ACCOST                                                        
         M     R0,=F'20'                                                        
         CLI   &P3,C'R'                                                         
         BE    *+8                                                              
         M     R0,=F'100'                                                       
         OC    &P1,&P1                                                          
         BZ    XCP&SYSNDX                                                       
         D     R0,&P1                                                           
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         ST    R1,&P2                                                           
XCP&SYSNDX DS  0H                                                               
         MEND                                                                   
*                                                                               
*******************************************************************             
* NETWORK POST BUY EVALUATION AND SUMMARY                                       
*                                                                               
*                                                                               
* GLOBALS:  R5 - A(CURRENT PRINT LINE)                                          
*           R6 - CURRENT MONTH SET IN MONTHLIST                                 
*           R7 - WORKING STORAGE                                                
*           R8 - A(DSECT FOR SPOOL PRINTING)                                    
*           R9 - NETWORK SYSTEM DSECT                                           
*           RA - SECOND BASE REGISTER!!! TWA ADDR IN ATWA                       
*           RC - GEND                                                           
*                                                                               
*  INPUTS: NETBLOCK SET UP BY EDIT.                                             
*                       *******************************                         
*       ANETWS1 ->      * WORKING STORAGE BLOCK 1     *                         
*                       * ---- CLIENT RECORD          *                         
*       ANETWS2 -> R7-> * WORKING STORAGE BLOCK 2     *                         
*                       * ---- NET DEMO BLOCK         *                         
*                       * ---- DEMO BLOCK                                       
*                       * ---- ARGUMENTS PASSED FROM EDIT                       
*                       * ----    FLAVOR- 'C' OR 'A'                            
*                       * ----    DATEOPT- 'W' OR 'M'                           
*                       * ---- LOCAL W/S              *                         
*                       *******************************                         
*                                                                               
*   CONTROLS:                                                                   
*       PERTYPE - BYTE1 - W- WEEKS, M- MONTHS, Q-QUARTER                        
*                 BYTE2 - 1 - USE MONTHS IF TOO MANY WEEKS                      
*                 BYTE3 - 1 - USE QUARTERS IF TOO MANY MONTHS                   
*                                                                               
*       USEASSGN  - SET TO Y IF ASSIGNED COSTS ARE DESIRED                      
*       USEINTEG - SET TO Y IF INTEGRATION COSTS SHOULD BE INCLUDED             
*       PRNZFLG -  SET IF ZERO LINES ARE TO BE PRINTED                          
************************************************************                    
*                                                                               
T31E66   CSECT                                                                  
         NMOD1 0,**NTPO**,RA,RR=R4                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         USING T31E66+4096,RA                                                   
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         LA    R2,HOOK                                                          
         ST    R2,HEADHOOK                                                      
         USING COLINFO,R5                                                       
         L     R7,ANETWS2          LOCAL W/S                                    
         USING NDDEMBLK,R7                                                      
***      LA    R1,STATIONL                                                      
         L     R1,=A(STATIONL)                                                  
         AR    R1,R4               RELO                                         
         ST    R1,NBCNVNTI                                                      
*                                                                               
********                                                                        
*                                                                               
***** INITIALIZE CONTROLS                                                       
*                                                                               
         XC    PERTYPE,PERTYPE                                                  
         MVC   PERTYPE(1),DATEOPT     GET REQUEST TYPE FROM TWA                 
         MVI   PERTYPE+1,1            USE MONTHS IF TOO MANY WEEKS              
         MVI   PERTYPE+2,0            NEVER USE QUARTERS                        
*                                                                               
         CLI   NDDEMOS+2,X'01'     IF FIRST DEMO NOT ALREADY HOMES              
         BE    INC2                                                             
         MVC   WORK(60),NDDEMOS       FORCE 1ST DEMO TO HOMES                   
         MVC   NDDEMOS(3),=X'00E301'       (T01)                                
         MVC   NDDEMOS+3(57),WORK                                               
*                                                                               
INC2     MVC   USEINTEG,NBUSER+15                                               
         MVC   USEASSGN,NBUSER+8                                                
         MVI   PRNZFLG,0                                                        
***********************                                                         
PROCDAT  NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBERROR,0                                                        
         BNE   PROCERR                                                          
         CLI   NBMODE,NBVALDAT     PROCESS DATES                                
         BE    IN6                                                              
         B     PROCDAT             OTHER MODES ARE IGNORED                      
*                                                                               
IN6      LA    R1,MAXMONTS         SET UP MAX SIZE OF MONTH (WEEK) LIST         
         ST    R1,NUMMONS          GET LIST INTO MONLIST. NUMMONS IS            
*                                    NEW SIZE OF LIST                           
         NETGO NVWKLST,DMCB,NUMMONS,MONLIST,PERTYPE                             
         MVC   ALPHPER,=C'MONTH   '   SET ALPHA PERIOD FOR HEADERS              
         CLI   PERTYPE,C'W'                                                     
         BNE   IN8                                                              
         MVC   ALPHPER,=C'WEEK    '                                             
*                                                                               
IN8      LA    RF,NUMNTRYS*MAXMONTS*MAXNETS    STOTLEN                          
         SLL   RF,2                            = ABOVE * 4                      
         XCEF  SUMTOTL,(RF)        INITIALIZE TOTALS                            
         LA    RF,MTOTLEN                                                       
         XCEF  MONTOTL,(RF)                                                     
         LA    RF,NTOTLEN                                                       
         XCEF  NETTOTL,(RF)                                                     
         LA    RF,ATOTLEN                                                       
         XCEF  ACTTOTL,(RF)                                                     
         LA    RF,TTOTLEN                                                       
         XCEF  TOTTOTL,(RF)                                                     
         LA    RF,FAKELEN                                                       
         XCEF  FAKELINE,(RF)                                                    
         MVI   NETCOUNT,0                                                       
         BAS   RE,DEMHEADS         GET ALPHA DEMO HEADERS                       
*                                                                               
         LA    R5,P1               FIRST PRINT LINE                             
         LA    R6,MONLIST          USE BEGINNING OF MONTH LIST                  
         MVI   RCSUBPRG,1          START WITH EVALUATION REPORT                 
*                                                                               
         MVC   NBAFFOPT,AFFIDOPT   SET AFFID OPTION                             
         MVI   NBDATA,C'U'         SELECT UNIT RECORDS                          
         MVI   NBSEQ,C'N'          READ IN NETWORK,DATE ORDER                   
         OI    NBSPLOPT,X'80'      TURN ON SPLIT OPTION                         
         MVI   NBHUNOPT,C'Y'       PROGRAM DEALS WITH HUNDREDS                  
         MVC   NBREROPT,REROPT     PASS RERATE OPTION                           
         MVI   NBACTOPT,C'Y'       TO GET ACTUAL DEMOS                          
         MVI   NBESTOPT,C'A'       GET EST DEMOS FOR M/G AS LONG AS             
*                                  THE MISSED UNIT IS NOT PFB                   
         CLI   FLAVOR,C'A'         IF AUDIT TRAIL                               
         BNE   *+8                                                              
         MVI   NBESTOPT,C'Y'       GET ESTIMATED DEMOS                          
         SPACE 1                                                                
GETFIRST NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBERROR,0                                                        
         BNE   PROCERR                                                          
         CLI   NBMODE,NBPROCUN     IF FIRST UNIT RECORD                         
         BE    GOTFIRST                                                         
         CLI   NBMODE,NBREQLST     IF NO UNITS                                  
         BE    TOTALS                                                           
         B     GETFIRST            OTHER MODES ARE IGNORED                      
         SPACE 1                                                                
GOTFIRST MVC   NETLIST(6),SPACES   SET UP FIRST ENTRY IN NETLIST                
         MVC   NETLIST+2(4),NBACTNET                                            
         MVC   CURNET,NBACTNET                                                  
         MVC   NVGDNET,CURNET      PASS NETWORK THROUGH FOR DEMO EDIT           
         MVI   ESTFLAG,0           NO ESTIMATES FOUND                           
         B     CKDATE              SKIP NETWORK CHECKS                          
*                                                                               
CKNEWNET TM    NBSUBMSK,NBSBMNET   IF NEW NETWORK                               
         BZ    CKDATE                                                           
         BAS   RE,MONTOTS          FILL IN MONTH TOTAL.                         
         BAS   RE,NETTOTS          FILL IN NETWORK TOTALS                       
         MVI   FORCEHED,C'Y'       FORCE NEW PAGE                               
         LA    R6,MONLIST          RESET R6 TO BEG OF MONLIST                   
         MVI   ESTFLAG,0           RESET ESTIMATE FLAG                          
         ZIC   R1,NETCOUNT                                                      
         MVC   CURNET,NBACTNET                                                  
         CLI   NETCOUNT,MAXNETS-1  SET UP NETLIST                               
         BL    INCNET                                                           
         MH    R1,=H'6'            ALL NETWORK SLOTS USED UP                    
         LA    R1,NETLIST(R1)      LAST SLOT IN NETLIST                         
         MVC   0(6,R1),=C'OTHERS'                                               
         B     CKDATE                                                           
INCNET   LA    R1,1(R1)                                                         
         STC   R1,NETCOUNT         INCREMENT NETCOUNT                           
         MH    R1,=H'6'                                                         
         LA    R1,NETLIST(R1)      NEXT SLOT IN NETLIST                         
         MVC   0(6,R1),SPACES                                                   
         MVC   2(4,R1),NBACTNET                                                 
*                                                                               
CKDATE   CLI   ESTFLAG,0           IF ACT DEMS FROM EST FOR 1ST TIME            
         BNE   CKD2                                                             
         CLI   NBRESULT,C'E'                                                    
         BNE   CKD2                                                             
***      CLI   NBSURVEY,C'N'       MUST BE FOR MAIN NETWORK                     
***      BNE   CKD2                                                             
         BAS   RE,ACTTOTS          PRINT ACTUAL TOTAL LINE                      
         MVI   ESTFLAG,1           SET FLAG                                     
         SPACE 1                                                                
CKD2     CLC   NBACTDAT,2(R6)      IF IN CURRENT MONTH SET                      
         BH    NEWMONTH                                                         
CKD3     BAS   RE,DOMAINLN         FILL IN UNIT LINE.                           
         B     GETNEXT                                                          
*                                                                               
NEWMONTH BAS   RE,MONTOTS          FILL IN MONTH TOTAL                          
         OC    4(4,R6),4(R6)       WILL WE GO OVER TABLE?                       
         BZ    CKD3                YES/PRINT IT NOW                             
         LA    R6,4(R6)            NO/POINT TO NEXT DATE-SET                    
         B     CKDATE                                                           
*                                                                               
GETNEXT  NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBERROR,0                                                        
         BNE   PROCERR                                                          
         CLI   OFFLINE,C'Y'                                                     
         BE    *+8                                                              
         BAS   RE,CHKMAXIO                                                      
         CLI   NBMODE,NBPROCUN     IF NEXT UNIT RECORD                          
         BE    CKNEWNET              DO NETWORK CHECKS                          
         CLI   NBMODE,NBREQLST     IF NO MORE UNITS                             
         BE    TOTALS                                                           
         B     GETNEXT             OTHER MODES ARE IGNORED                      
*                                                                               
TOTALS   BAS   RE,MONTOTS          FINISH TOALS                                 
         BAS   RE,NETTOTS                                                       
         BAS   RE,DOSUMMRY                                                      
         XMOD1                                                                  
*                                                                               
PROCERR  DC    F'0'                                                             
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
*****************************************************************               
         EJECT                                                                  
*****************************************************************               
NEXTLIN  NTR1                                                                   
         CLC   P1,SPACES           IS ANYTHING TO BE PRINTED                    
         BE    XITNL               IF NOT THEN EXIT                             
         CLI   FORCEPRN,0          IF PRINT IS FORCED                           
         BNE   PRINIT                PRINT NO MATTER WHAT                       
         LA    R1,P4                                                            
         CR    R5,R1               IF USED LAST PRINT LINE                      
         BL    INCLINE                                                          
PRINIT   GOTO1 SPOOL,DMCB,(R8)        PRINT WHATS THERE                         
         LA    R5,P1                  POINT R5 TO FIRST PRINT LINE              
         B     XITNL                                                            
*                                    ELSE                                       
INCLINE  LA    R5,132(R5)               NEXT PRINTLINE                          
*                                    FI                                         
XITNL    MVI   FORCEPRN,0                                                       
R5XIT    XIT1  REGS=(R5)                                                        
*                                                                               
***************************************************************8                
         EJECT                                                                  
****************************************************************                
* DOMAINLN - PROCESS AND PRINT A UNIT LINE                                      
*                                                                               
*   FOR FLAVOR=CLIENT:                                                          
*       IGNORE ALL MISSED SPOTS                                                 
*       USE BOTH ESTIMATE AND ACTUAL FIGURES FROM THE MAKE GOOD SPOT            
*                                                                               
*   FOR FLAVOR=AUDIT TRAIL:                                                     
*       NOTE MISSED SPOTS, MAKE GOODS                                           
*       USE ESTIMATE FROM MISSED, ACTUAL FROM MAKE GOOD                         
*                                                                               
*  NOTE: A UNIT MAY BE A MAKEGOOD AND MISSED!!                                  
*        THE ESTIMATE DEMOS ON A MAKEGOOD COME FROM THE MISSED SPOT,            
*        SO NO EXTRA LOOKUP IS NEEDED                                           
**************************************************************                  
*                                                                               
DOMAINLN NTR1                                                                   
         OC    NBMGFPCD,NBMGFPCD   IF THIS IS NOT A MAKEGOOD                    
         BNZ   DOMGF                                                            
         CLI   FLAVOR,C'A'           IF THIS IS CLIENT FORMAT                   
         BE    DMFE                                                             
         OC    NBMGBPCD,NBMGBPCD      AND A MISSED SPOT,                        
         BNZ   XITMNLN                  THEN EXIT                               
DMFE     BAS   RE,FILLEST            FILL ESTIMATE PART OF FAKE LINE            
         B     CKMGB                                                            
*                                  ELSE                                         
DOMGF    MVC   CLESTMGF,SPACES                                                  
         CLI   FLAVOR,C'A'           IF FLAVOR IS AUDIT TRAIL                   
         BNE   EFLAVC                                                           
         L     R3,NBAIO                                                         
         USING NURECD,R3                                                        
         LA    R2,NUMAINEL                                                      
         MVI   SRCHEL,6                                                         
         BAS   RE,NEXTEL                                                        
         BNE   *+8                                                              
         BAS   RE,DOMGFRTN                                                      
         B     CKMGB                                                            
*                                                                               
DOMGFRTN NTR1                                                                   
         USING NUMGD,R2                                                         
         MVI   MGF,C'Y'                                                         
         MVC   CLESTMGF,SPACES                                                  
         MVC   CLESTMGF(9),=C'(M/G FOR '   PRINT MAKE GOOD FOR LINE             
         MVC   CLESTMGF+9(16),NUMGPNM                                           
         MVC   CLESTMGF+26(3),=C'ON '                                           
         GOTO1 DATCON,DMCB,(2,NUMGDATE),(5,CLESTMGF+29)                         
         MVI   CLESTMGF+34,C'-'                                                 
         LA    R4,CLESTMGF+35                                                   
         EDIT  (B1,NUMGSUB),(2,0(R4)),ALIGN=LEFT                                
         GOTO1 SQUASHER,DMCB,CLESTMGF,37                                        
         L     R0,4(R1)                                                         
         LA    R1,CLESTMGF                                                      
         AR    R1,R0                                                            
         MVI   0(R1),C')'                                                       
XIT      B     EXIT                                                             
*                                                                               
EFLAVC   OC    NBMGBPCD,NBMGBPCD      IF BOTH MISSED AND MADE GOOD              
         BNZ   XITMNLN                  THEN EXIT (CLIENT FLAVOR)               
         BAS   RE,FILLEST            ELSE USE ESTIMATED                         
*                                                                               
CKMGB    OC    NBMGBPCD,NBMGBPCD   IF THIS IS A MISSED UNIT                     
         BZ    DOACT                                                            
         CLI   FLAVOR,C'C'          IF FLAVOR IS CLIENT                         
         BNE   FILMGB                                                           
DOACT    BAS   RE,FILLACT           FILL ACTUAL PART OF FAKE LINE               
         B     PRINMNLN                                                         
*                                                                               
FILMGB   L     R3,NBAIO                                                         
         USING NURECD,R3                                                        
         LA    R2,NUMAINEL                                                      
         MVI   SRCHEL,7                                                         
         BAS   RE,NEXTEL                                                        
         BNE   PRINMNLN                                                         
         BAS   RE,DOMGBRTN                                                      
         B     PRINMNLN                                                         
*                                                                               
DOMGBRTN NTR1                                                                   
         USING NUMGD,R2                                                         
         MVI   MGB,C'Y'                                                         
         MVC   CLACTMGB,SPACES                                                  
         MVC   CLACTMGB(9),=C'(M/G BY  '  PRINT MAKE GOOD BY LINE               
         MVC   CLACTMGB+9(16),NUMGPNM                                           
         MVC   CLACTMGB+26(3),=C'ON '                                           
         GOTO1 DATCON,DMCB,(2,NUMGDATE),(5,CLACTMGB+29)                         
         MVI   CLACTMGB+34,C'-'                                                 
         LA    R4,CLACTMGB+35                                                   
         EDIT  (B1,NUMGSUB),(2,0(R4)),ALIGN=LEFT                                
         GOTO1 SQUASHER,DMCB,CLACTMGB,37                                        
         L     R0,4(R1)                                                         
         LA    R1,CLACTMGB                                                      
         AR    R1,R0                                                            
         MVI   0(R1),C')'                                                       
         B     XIT                                                              
*                                                                               
PRINMNLN GOTO1 DATCON,DMCB,(2,NBACTDAT),(4,CLDATE)                              
         MVC   CLPROGNM,NBPROGNM                                                
         CLC   NBSELPRD,=C'POL'    DONT PRINT BRAND IF 1 SELECTED               
         BE    PL1                                                              
         OC    NBSELPRD,NBSELPRD                                                
         BZ    PL2                                                              
*                                                                               
PL1      LKPRD CLPROD,NBPRD                                                     
         CLI   NBPRD2,0            CK IF A PIGGY                                
         BE    PL2                                                              
         LKPRD CLPROD2,NBPRD2                                                   
         MVI   CLPSLASH,C'/'                                                    
*                                                                               
PL2      BAS   RE,NEXTLIN                                                       
*                                                                               
         LA    R3,FAKELINE                                                      
         BAS   RE,CPPTOTS           SET UP CPP LINE IN CASE NEEDED              
*                                                                               
         L     R4,ATWA                                                          
         USING T31EFFD,R4                                                       
         CLI   TWNCPPU,C'Y'         IF SHOULD PRINT CPMS FOR UNIT               
         BNE   PL8                                                              
         BAS   RE,CPPFORM           PRINT CPPS                                  
         MVI   FORCEPRN,1                                                       
         BAS   RE,NEXTLIN                                                       
PL8      CLI   TWNINDU,C'Y'         IF SHOULD PRINT DIFFS FOR UNIT              
         BNE   PL10                                                             
         BAS   RE,DIFFFORM          PRINT DIFFS                                 
         MVI   FORCEPRN,1                                                       
         BAS   RE,NEXTLIN                                                       
         DROP  R4                                                               
*                                                                               
PL10     DS    0H                                                               
         CLI   MGF,C'Y'            TEST MULTIPLE MAKEGOODS                      
         BNE   PL14                                                             
         L     R3,NBAIO                                                         
         USING NURECD,R3                                                        
         LA    R2,NUMAINEL                                                      
         MVI   SRCHEL,6                                                         
         BAS   RE,NEXTEL                                                        
         BNE   PL14                                                             
         BAS   RE,NEXTEL                                                        
         BNE   PL14                                                             
PL10C    BAS   RE,DOMGFRTN                                                      
         MVI   FORCEPRN,1                                                       
         BAS   RE,NEXTLIN                                                       
         BAS   RE,NEXTEL                                                        
         BE    PL10C                                                            
*                                                                               
PL14     CLI   MGB,C'Y'            TEST MULTIPLE MAKEGOODS                      
         BNE   PL17                                                             
         L     R3,NBAIO                                                         
         USING NURECD,R3                                                        
         LA    R2,NUMAINEL                                                      
         MVI   SRCHEL,7                                                         
         BAS   RE,NEXTEL                                                        
         BNE   PL17                                                             
         BAS   RE,NEXTEL                                                        
         BNE   PL17                                                             
PL15     BAS   RE,DOMGBRTN                                                      
         MVI   FORCEPRN,1                                                       
         BAS   RE,NEXTLIN                                                       
         BAS   RE,NEXTEL                                                        
         BE    PL15                                                             
*                                                                               
PL17     MVI   LIN2FLG,0           WILL BE SET IF A 2ND LINE IS NEEDED          
         CLI   FLAVOR,C'A'         IF AUDIT TRAIL                               
         BNE   XITMNLN                                                          
*                                                                               
         MVI   SRCHEL,X'04'        LOOK FOR COMMENT ELEMENT                     
         L     R3,NBAIO            A(UNIT RECORD)                               
         USING NURECD,R3                                                        
         LA    R2,NUMAINEL         A(FIRST ELEMENT)                             
         DROP  R3                                                               
*                                                                               
COMLOOP  BAS   RE,NEXTEL           IF ONE EXISTS                                
         BNE   XITMNLN                                                          
         MVI   LIN2FLG,1                                                        
         USING NUCOMD,R2           R2 POINTS TO COMMENT ELEMENT                 
         ZIC   R4,NUCOMLEN         LENGTH OF COMMENT                            
         SH    R4,=H'5'            SUBTRACT 4 FOR EL HEADER, 1 FOR EX           
         EXMVC R4,CLESTMGF,NUCOMMNT                                             
         BAS   RE,NEXTLIN          PRINT THE COMMENT                            
         B     COMLOOP             NEXT COMMENTT                                
         DROP  R2                                                               
*                                                                               
*XITMNLN  XIT1  REGS=(R5)                                                       
XITMNLN  B     R5XIT                                                            
****************************************************************                
         EJECT                                                                  
****************************************************************                
* MONTOTS - PERFORMS END OF MONTH TOTALS                                        
*             FOR EVAL REPORT:                                                  
*                STORES CURR MONTH TOTAL LINE IN THE SUMMARY TOTALS             
*             FOR SUMMARY REPORT:                                               
*                GETS CURR MONTH TOTAL LINE FROM SUMMARY TOTALS                 
*                                                                               
*             ADDS CURR MONTH TOTAL LINE TO NETWORK TOTAL LINE                  
*             PRINTS THE CURR MONTH TOTAL LINE                                  
*             CLEARS THE CURR MONTH TOTAL LINE                                  
****************************************************************                
MONTOTS  NTR1                                                                   
*                                                                               
*                         **** GET OFFSET OF CURR MONTH INTO SUMTOTL            
         CLI   NETCOUNT,MAXNETS-1  IF WE HAVE THE MAX NUM OF NETWORKS           
         BL    MT2                                                              
         MVI   NETCOUNT,MAXNETS-1    USE ALL OTHERS CATEGORY                    
MT2      ZIC   R2,NETCOUNT           GET OFFSET FOR THIS NETWORK                
         SLL   R2,2                    FROM SUMTOTL INTO R2                     
         LA    R1,NUMNTRYS*MAXMONTS    (MULT BY 4 FOR FULLWORDS)                
         STH   R1,HALF                                                          
         MH    R2,HALF             R2=(NUMNETS*4)*NUMNTRYS*MAXMONTS             
         LA    R1,MONLIST                                                       
         LR    R4,R6               GET OFFSET PAST R2 FOR THIS MONTH            
         SR    R4,R1                 INTO R4                                    
*                              DIVIDE R4 BY 4 TO GET MONTH NUMBER,              
*                              BUT MULT R4 BY 4 FOR FULLWORDS SO NOP            
         LA    R1,NUMNTRYS                                                      
         STH   R1,HALF                                                          
         MH    R4,HALF             R4=(MONTHNUMBER*4)*NUMNTRYS                  
         AR    R4,R2               OFFSET OF THIS LINE FROM SUMTOTL             
         LA    R4,SUMTOTL(R4)                                                   
*                                                                               
         CLI   RCSUBPRG,1          IF EVALUATION REPORT                         
         BNE   MT4                                                              
         LA    R3,MONTOTL          R4 ALREADY CONTAINS A(SUMTOTL LINE)          
         BAS   RE,CPPTOTS           SET UP CPP VALUES IN CASE NEEDED            
         BAS   RE,ADDLINES         ADD MON TOTS TO SUMM TOTS                    
         LA    R3,ENDOFF(R3)                                                    
         LA    R4,ENDOFF(R4)                                                    
         BAS   RE,ADDLINES                                                      
         B     ADDMTON                                                          
MT4      MVC   MONTOTL(NUMNTRYS*4),0(R4)  IF SUMMARY MOVE SUM TO NET            
*                                                                               
*****                            **** ADD MONTOTL TO NETTOTL                    
ADDMTON  LA    R3,MONTOTL          POINT TO ESTIMATE HALFLINES                  
         LA    R4,NETTOTL                                                       
         BAS   RE,ADDLINES         ADD EST HALF OF MONTOTL,NETTOTL              
         LA    R3,ENDOFF(R3)       POINT TO ACTUAL HALFLINES                    
         LA    R4,ENDOFF(R4)                                                    
         BAS   RE,ADDLINES         ADD ACT HALF OF MONTOTL,NETTOTL              
*                                                                               
*                           ******* PRINT THE MONTH TOTAL LINE                  
         OC    MONTOTL(MTOTLEN),MONTOTL   CK FOR ZERO LINE                      
         BNZ   OKPRIN                                                           
         CLI   PRNZFLG,0           ZERO LINE. DO WE PRINT IT ANYWAY?            
         BE    ENDMONT                                                          
*                                                                               
OKPRIN   CLI   RCSUBPRG,1          FOR EVALUATION, PRINT BLANK LINE             
         BNE   OKP2                                                             
         MVI   FORCEPRN,1          CLEAR THE PRINT BUFFER                       
         BAS   RE,NEXTLIN                                                       
         GOTO1 SPOOL,DMCB,(R8)     AND PRINT A BLANK LINE                       
OKP2     LA    R3,MONTOTL                                                       
         LA    R4,CLESTLIN                                                      
         BAS   RE,FORMAT           FORMAT ESTIMATE PART OF PRINT                
         LA    R3,ENDOFF(R3)                                                    
         LA    R4,CLACTLIN         NOW FORMAT ACTUALS                           
         BAS   RE,FORMAT                                                        
         BAS   RE,MONHEAD          FILL IN LINE IDENTIFICATION                  
*                                                                               
PRINMON  MVI   FORCEPRN,1          FORCE PRINTING                               
         BAS   RE,NEXTLIN          PRINT IT                                     
         CLI   RCSUBPRG,1                                                       
         BNE   ENDMONT                                                          
*                                                                               
         L     R4,ATWA                                                          
         USING T31EFFD,R4                                                       
         CLI   TWNCPPU,C'Y'         IF SHOULD PRINT CPMS FOR UNIT               
         BNE   MT8                                                              
         BAS   RE,CPPFORM           PRINT CPPS                                  
         MVI   FORCEPRN,1                                                       
         BAS   RE,NEXTLIN                                                       
MT8      CLI   TWNINDU,C'Y'         IF SHOULD PRINT DIFFS FOR UNIT              
         BNE   MT10                                                             
         BAS   RE,DIFFFORM          PRINT DIFFS                                 
         MVI   FORCEPRN,1                                                       
         BAS   RE,NEXTLIN                                                       
MT10     GOTO1 SPOOL,DMCB,(R8)     FOR EVALUATION PRINT A BLANK LINE            
         DROP  R4                                                               
*                                                                               
ENDMONT  LA    RF,MTOTLEN                                                       
         XCEF MONTOTL,(RF)         CLEAR MONTH TOTAL LINE                       
*        XIT1 REGS=(R5)                                                         
         B    R5XIT                                                             
         EJECT                                                                  
****************************************************************                
* MONHEAD - FILLS IN THE LINE IDENTIFICATION INFO                               
*             FOR EVALUATION: 'WEEK TOTAL', 'MONTH TOTAL',                      
*                OR  MONTHNAME 'TOTAL'                                          
*             FOR SUMMARY: NETWORK AND WEEKNAME OR MONTHNAME                    
****************************************************************                
MONHEAD  NTR1                                                                   
         CLI   RCSUBPRG,1          EVALUATION REPORT                            
         BE    EVALMHED                                                         
         B     SUMMMHED            SUMMARY REPORT                               
*                                                                               
EVALMHED CLI   PERTYPE,C'W'        ** FOR EVALUATION                            
         BNE   USEMONS                                                          
         MVC   CLPROGNM,=CL16'WEEK TOTALS'    FOR WEEKS                         
         B     XITMHED                                                          
USEMONS  NETGO NDGETMON,DMCB,2(R6),(9,CLPROGNM)  GET MONTH NAME                 
*                            '''' - USE SECOND DATE, TO HANDLE BMONS            
         MVI   CLPROGNM+9,C' '                                                  
         MVC   CLPROGNM+10(6),=C'TOTALS'                                        
         GOTO1 SQUASHER,DMCB,CLPROGNM,16 SQUASH IT                              
         B     XITMHED                                                          
*                                                                               
SUMMMHED CLI   PERTYPE,C'W'        FOR WEEKS                                    
         BNE   USEMONTH                                                         
         GOTO1 DATCON,DMCB,(2,0(R6)),(4,CLPER)    USE MMMDD FORMAT              
         B     XITMHED                                                          
USEMONTH GOTO1 DATCON,DMCB,(2,2(R6)),(6,CLPER)      USE MMM/YY FORMAT           
*                                                                               
*XITMHED  XIT1  REGS=(R5)                                                       
XITMHED  B    R5XIT                                                             
****************************************************************                
         EJECT                                                                  
****************************************************************                
* NETTOTS - PERFORMS END OF NETWORK TOTALS                                      
*             ADDS NETWORK TOTAL LINE TO REPORT TOTAL LINE                      
*             PRINTS THE NETWORK TOTAL LINE                                     
*             CLEARS THE NETWORK TOTAL LINE                                     
****************************************************************                
NETTOTS  NTR1                                                                   
         MVI   FORCEPRN,1          CLEAR THE PRINT BUFFER                       
         BAS   RE,NEXTLIN                                                       
*                                                                               
         LA    R3,NETTOTL          POINT TO ESTIMATE HALFLINES                  
         LA    R4,TOTTOTL                                                       
         BAS   RE,CPPTOTS          FILL IN COST-PER-POINT LINE                  
         BAS   RE,ADDLINES         ADD EST HALF OF NETTOTL,TOTTOTL              
         LA    R3,ENDOFF(R3)       POINT TO ACTUAL HALFLINES                    
         LA    R4,ENDOFF(R4)                                                    
         BAS   RE,ADDLINES         ADD ACT HALF OF NETTOTL,TOTTOTL              
*                                                                               
*                           ******* PRINT THE MONTH TOTAL LINE                  
         OC    NETTOTL(NTOTLEN),NETTOTL   CK FOR ZERO LINE                      
         BNZ   OKPRNET                                                          
         CLI   PRNZFLG,0           ZERO LINE. DO WE PRINT IT ANYWAY?            
         BE    ENDNET                                                           
*                                                                               
OKPRNET  GOTO1 SPOOL,DMCB,(R8)     PRINT A BLANK LINE                           
         LA    R5,P1               RESET                                        
         LA    R3,NETTOTL                                                       
         LA    R4,CLESTLIN                                                      
         BAS   RE,FORMAT           FORMAT ESTIMATE PART OF PRINT                
         LA    R3,ENDOFF(R3)                                                    
         LA    R4,CLACTLIN         NOW FORMAT ACTUALS                           
         BAS   RE,FORMAT                                                        
*                                                                               
         BAS   RE,NETHEAD          FILL IN LINE IDENTIFICATION                  
*                                                                               
         MVI   FORCEPRN,1          FORCE PRINTING                               
         BAS   RE,NEXTLIN          PRINT IT                                     
         BAS   RE,CPPFORM          FORMAT CPP LINE                              
         BAS   RE,NEXTLIN                                                       
         BAS   RE,DIFFFORM         FORMAT DIFFERENCE LINE                       
         MVI   FORCEPRN,1                                                       
         BAS   RE,NEXTLIN          AND PRINT IT                                 
         GOTO1 SPOOL,DMCB,(R8)     PRINT BLANK LINE                             
         LA    R5,P1                                                            
*                                                                               
ENDNET   LA    RF,NTOTLEN                                                       
         XCEF  NETTOTL,(RF)        CLEAR NET TOTAL LINE                         
         LA    RF,ATOTLEN                                                       
         XCEF  ACTTOTL,(RF)        NEW ACTUAL TOTAL FOR EACH NETWORK            
*        XIT1  REGS=(R5)                                                        
         B     R5XIT                                                            
****************************************************************                
         EJECT                                                                  
****************************************************************                
* NETHEAD - FILLS IN THE LINE IDENTIFICATION INFO FOR NETWORK TOTALS            
*           FOR EVALUATION: NETWORK 'TOTALS' IN PROGRAM NAME COLUMN             
*           FOR SUMMARY: SPACES IN NETWORK COL, 'TOTALS' IN PERIOD COL          
****************************************************************                
NETHEAD  NTR1                                                                   
         CLI   RCSUBPRG,1                                                       
         BE    EVALNHED                                                         
         B     SUMMNHED                                                         
*                                                                               
EVALNHED DS    0H                  *** FOR EVALUATION REPORT                    
         MVC   CLPROGNM,SPACES        FILL TOTAL HEADER                         
         MVC   CLPROGNM(4),CURNET     (NBACTNET CONTAINS NEXT NETWORK)          
         MVC   CLPROGNM+5(6),=C'TOTALS'                                         
         GOTO1 SQUASHER,DMCB,CLPROGNM,16 SQUASH IT                              
         B     XITNHED                                                          
*                                                                               
SUMMNHED MVC   CLNET,SPACES                                                     
         MVC   CLPER(9),=CL9'TOTALS'                                            
*                                                                               
*XITNHED  XIT1  REGS=(R5)                                                       
XITNHED  B     R5XIT                                                            
****************************************************************                
         EJECT                                                                  
****************************************************************                
* REPTOTS - PERFORMS END OF REPORT TOTALS                                       
*           PERFORMS SUMMARY REPORT                                             
****************************************************************                
REPTOTS  NTR1                                                                   
         MVI   FORCEPRN,1          CLEAR THE PRINT BUFFER                       
         BAS   RE,NEXTLIN                                                       
*                           ******* PRINT THE REPORT TOTAL LINE                 
         OC    TOTTOTL(NTOTLEN),TOTTOTL   CK FOR ZERO LINE                      
         BNZ   OKPRTOT                                                          
         CLI   PRNZFLG,0           ZERO LINE. DO WE PRINT IT ANYWAY?            
         BE    XITREP                                                           
*                                                                               
OKPRTOT  GOTO1 SPOOL,DMCB,(R8)     PRINT A BLANK LINE                           
         LA    R5,P1               RESET R5                                     
         LA    R3,TOTTOTL                                                       
         LA    R4,CLESTLIN                                                      
         BAS   RE,CPPTOTS                                                       
         BAS   RE,FORMAT           FORMAT ESTIMATE PART OF PRINT                
         LA    R3,ENDOFF(R3)                                                    
         LA    R4,CLACTLIN         NOW FORMAT ACTUALS                           
         BAS   RE,FORMAT                                                        
         BAS   RE,REPHEAD                                                       
*                                                                               
         MVI   FORCEPRN,1          FORCE PRINTING                               
         BAS   RE,NEXTLIN          PRINT IT                                     
         BAS   RE,CPPFORM          FORMAT CPP LINE                              
         BAS   RE,NEXTLIN                                                       
         BAS   RE,DIFFFORM          FORMAT DIFFERENCE LINE                      
         MVI   FORCEPRN,1                                                       
         BAS   RE,NEXTLIN          PRINT THEM                                   
         GOTO1 SPOOL,DMCB,(R8)      PRINT BLANK LINE                            
*                                                                               
*XITREP   XIT1  REGS=(R5)                                                       
XITREP   B     R5XIT                                                            
****************************************************************                
         EJECT                                                                  
****************************************************************                
* CPPTOTS - CALCULATE COST PER POINTS AND FILL CPP LINE                         
*  INPUTS - R3 - THE TOTAL LINE WHICH CALCS SHOULD COME FROM                    
*  OUTPUTS - CPPLIST - THE COST PER POINTS.                                     
***************************************************************                 
CPPTOTS  NTR1                                                                   
         XC    CPPLIST(32),CPPLIST                                              
         USING ACD,R3                                                           
         CPP   ACGRPS,ESTGRPCP,=C'R'       CALC ESTIMATES                       
         NETGO NVDEMTYP,DMCB,(0,NDDEMBLK),BYTE  1ST DEMO TYPE IN BYTE           
         CPP   ACDEM1,ESTDM1CP,BYTE                                             
         NETGO NVDEMTYP,DMCB,(1,NDDEMBLK),BYTE  2ND DEMO TYPE IN BYTE           
         CPP   ACDEM2,ESTDM2CP,BYTE                                             
         NETGO NVDEMTYP,DMCB,(2,NDDEMBLK),BYTE  3RD DEMO TYPE IN BYTE           
         CPP   ACDEM3,ESTDM3CP,BYTE                                             
*                                                                               
         LA    R3,ENDOFF(R3)                                                    
         CPP   ACGRPS,ACTGRPCP,=C'R'       CALC ACTUALS                         
         NETGO NVDEMTYP,DMCB,(0,NDDEMBLK),BYTE  1ST DEMO TYPE IN BYTE           
         CPP   ACDEM1,ACTDM1CP,BYTE                                             
         NETGO NVDEMTYP,DMCB,(1,NDDEMBLK),BYTE  2ND DEMO TYPE IN BYTE           
         CPP   ACDEM2,ACTDM2CP,BYTE                                             
         NETGO NVDEMTYP,DMCB,(2,NDDEMBLK),BYTE  3RD DEMO TYPE IN BYTE           
         CPP   ACDEM3,ACTDM3CP,BYTE                                             
         B     EXIT                                                             
****************************************************************                
         EJECT                                                                  
****************************************************************                
* REPHEAD - FILLS IN THE LINE IDENTIFICATION INFO FOR REPORT TOTALS             
*           FOR EVALUATION: 'REPORT TOTALS' IN PROGRAM NAME COLUMN              
*           FOR SUMMARY: SPACES IN NETWORK COL, 'TOTALS' IN PERIOD COL          
****************************************************************                
REPHEAD  NTR1                                                                   
         CLI   RCSUBPRG,1                                                       
         BE    EVALRHED                                                         
         B     SUMMRHED                                                         
*                                                                               
EVALRHED DS    0H                  *** FOR EVALUATION REPORT                    
         MVC   CLPROGNM,SPACES        FILL TOTAL HEADER                         
         MVC   CLPROGNM(4),CURNET     (NBACTNET CONTAINS NEXT NETWORK)          
         MVC   CLPROGNM(16),=CL16'REPORT TOTALS'                                
         B     XITRHED                                                          
*                                                                               
SUMMRHED MVC   CLNET,SPACES                                                     
         MVC   CLPER(9),=CL9'TOTALS'                                            
*                                                                               
*XITRHED  XIT1  REGS=(R5)                                                       
XITRHED  B     R5XIT                                                            
****************************************************************                
         EJECT                                                                  
****************************************************************                
* ACTTOTS - WRITE ACTUAL TOTAL LINE                                             
****************************************************************                
ACTTOTS  NTR1                                                                   
         MVI   FORCEPRN,1          CLEAR THE PRINT BUFFER                       
         BAS   RE,NEXTLIN                                                       
*                           ******* PRINT THE CURRENT REPORT TOTAL LINE         
         OC    ACTTOTL(ATOTLEN),ACTTOTL   CK FOR ZERO LINE                      
         BNZ   OKPRACT                                                          
         CLI   PRNZFLG,0           ZERO LINE. DO WE PRINT IT ANYWAY?            
         BE    XITACT                                                           
*                                                                               
OKPRACT  GOTO1 SPOOL,DMCB,(R8)     PRINT A BLANK LINE                           
         LA    R5,P1                                                            
         LA    R3,ACTTOTL                                                       
         LA    R4,CLESTLIN                                                      
         BAS   RE,FORMAT           FORMAT ESTIMATE PART OF PRINT                
         LA    R3,ENDOFF(R3)                                                    
         LA    R4,CLACTLIN         NOW FORMAT ACTUALS                           
         BAS   RE,FORMAT                                                        
         MVC   CLPROGNM(16),=CL16'ACTUAL TOTALS'                                
*                                                                               
         MVI   FORCEPRN,1          FORCE PRINTING                               
         BAS   RE,NEXTLIN          PRINT IT                                     
         GOTO1 SPOOL,DMCB,(R8)     PRINT BLANK LINE                             
*                                                                               
*XITACT   XIT1  REGS=(R5)                                                       
XITACT   B     R5XIT                                                            
****************************************************************                
         EJECT                                                                  
****************************************************************                
* DOSUMMRY - NOW PRINT THE SUMMARY INFO                                         
*                                                                               
DOSUMMRY NTR1                                                                   
         LA    RF,TTOTLEN                                                       
         XCEF  TOTTOTL,(RF)        RESET TOTALS                                 
         MVI   LINE,99             FORCE PAGE BREAK                             
         MVI   RCSUBPRG,2          FLAG SUMMARY REPORT                          
FRSTNET  MVC   ACTNUMNT,NETCOUNT   SAVE NUM OF NETS IN REPORT                   
         MVI   NETCOUNT,0          DO FOR ALL NETWORKS                          
*                                                                               
NETLOOP  LA    R6,MONLIST          FIRST IN DATE SET                            
         CLI   NETCOUNT,MAXNETS-1  GET NETWORK NAME INTO PRINTLINE              
         BL    NL2                                                              
         MVC   CLNET(6),=C'OTHERS'                                              
         B     DOMONS                                                           
NL2      ZIC   R1,NETCOUNT                                                      
         MH    R1,=H'6'            OFFSET INTO NETLIST                          
         LA    R1,NETLIST(R1)                                                   
         MVC   CLNET(6),0(R1)                                                   
DOMONS   L     R2,NUMMONS                                                       
MONLOOP  BAS   RE,MONTOTS           DO FOR EACH IN DATE SET LIST                
         LA    R6,4(R6)            NEXT DATE SET                                
         BCT   R2,MONLOOP                                                       
*                                                                               
NEXTNET  BAS   RE,NETTOTS          PRINT NET SUMMARY                            
         CLC   NETCOUNT,ACTNUMNT   CK FOR LAST NETWORK                          
         BNL   NETSDONE                                                         
         ZIC   R1,NETCOUNT         NEXT NETWORK                                 
         LA    R1,1(R1)                                                         
         STC   R1,NETCOUNT                                                      
         B     NETLOOP                                                          
*                                                                               
****   NOW DO FOR MONTHS ACROSS NETWORKS                                        
****    DO THIS BY SUMMING ALL NETWORKS INTO THE FIRST NETWORK AREA             
****     OF SUMTOTS. THEN ONLY PRINT TOTALS FOR THIS FIRST NETWORK.             
****   BORROW R8 FOR LOOP CONTROL (NUMBER OF NETS)                              
****          R2 IS NUMBER OF DATE SETS                                         
NETSDONE DS    0H                  SUM ALL INTO FIRST NETWORK SPACE             
         ZIC   R8,ACTNUMNT                                                      
         LTR   R8,R8               DONT SUM IF ONLY 1 NET                       
         BNH   MTOTPRIN                                                         
         LA    R3,NUMNTRYS*MAXMONTS*4    OFFSET TO 2ND NETWORK                  
         LA    R3,SUMTOTL(R3)      A(2ND NETWORK IN SUMTOTL)                    
*                                  DO FOR NETWORKNUM (R8) = 2 TO LAST           
ADDNXTNT LA    R4,SUMTOTL           SET R4 TO FIRST NETWORK                     
         LA    R2,MAXMONTS          SET R2 TO NUMMONTHS*2.                      
         SLL   R2,1                 (DO FOR ACT AND EST EACH MONTH)             
*                                   DO FOR EACH MONTH IN TOTALS (R2)            
ADDNXTMN BAS   RE,ADDLINES           ADD MONS OF NET(R8) TO 1ST NET             
         LA    R3,ENDOFF(R3)                                                    
         LA    R4,ENDOFF(R4)         SET UP FOR NET MONTH                       
         BCT   R2,ADDNXTMN          NEXT MONTH                                  
         BCT   R8,ADDNXTNT         NEXT NET                                     
*                                                                               
MTOTPRIN L     R8,ASPOOLD          RESTORE R8                                   
*                                                                               
         MVI   NETCOUNT,0          PRINT THE TOTALS                             
         MVC   CLNET(6),=C'TOTALS'    PRETEND ONLY 1 NET. (TOTALS)              
         LA    R6,MONLIST             WITH CALL LETTERS: 'TOTALS'               
         L     R2,NUMMONS                                                       
PRNTSLIN BAS   RE,MONTOTS          DO FOR EACH DATE SET                         
         LA    R6,4(R6)            NEXT DATE SET                                
         BCT   R2,PRNTSLIN                                                      
*                                                                               
NDEND    BAS   RE,REPTOTS                                                       
         B     EXIT                                                             
****************************************************************                
         EJECT                                                                  
****************************************************************                
* FILLEST - FILLS ESTIMATE PART OF FAKE LINE                                    
*           ADDS THIS TO MONT TOTALS                                            
*           FORMATS IT TO ESTIMATE PORTION OF PRINT LINE                        
*                                                                               
*   CALLS TO: GETCOST - CALCULATES COST AND RETURNS IT IN FULL.                 
*             ADDLINES- ADDS TWO HALFLINES. ARGS: R3,R4                         
*             FORMAT -  FORMATS A HALF LINE ONTO PRINT LINE ARGS: R3,R4         
****************************************************************                
*                                                                               
FILLEST  NTR1                                                                   
         LA    R3,FAKELINE                                                      
         BAS   RE,GETCOST          CALC COST. RESULT IN FULL                    
         OC    NBESTUN,NBESTUN     ZERO EST COST IF NO DEMOS RETURNED           
         BNZ   FE2                                                              
         XC    FULL,FULL                                                        
FE2      MVC   COSTOFF(4,R3),FULL                                               
         MVC   UNITOFF+2(2,R3),NBESTUN                                          
         MVC   GRPSOFF+2(2,R3),NBESTHOM+2                                       
         NETGO NVGETDEM,DMCB,(0,NDDEMBLK),(C'E',DEM1OFF(R3))                    
         NETGO NVGETDEM,DMCB,(1,NDDEMBLK),(C'E',DEM2OFF(R3))                    
         NETGO NVGETDEM,DMCB,(2,NDDEMBLK),(C'E',DEM3OFF(R3))                    
*                                                                               
         LA    R4,MONTOTL          ESTIMATE HALF OF MON TOTAL LINE              
         BAS   RE,ADDLINES         ADD FAKELINE INTO MON TOTAL LINE             
*                                                                               
         LA    R4,ACTTOTL          ESTIMATE HALF OF ACT TOTAL LINE              
         BAS   RE,ADDLINES         ADD FAKELINE INTO ACT TOTAL LINE             
*                                                                               
         LA    R4,CLESTLIN         ADDR OF ESTIMATE OUTPUT AREA                 
         BAS   RE,FORMAT           FORMAT ESTIMATE HALF LINE                    
*                                                                               
*        XIT1  REGS=(R5)                                                        
         B     R5XIT                                                            
***************************************************************                 
         EJECT                                                                  
****************************************************************                
* FILLACT - FILLS ACTUAL PART OF FAKE LINE                                      
*           ADDS THIS TO MONTH TOTALS                                           
*           FORMATS IT TO ACTUAL PORTION OF PRINT LINE                          
*                                                                               
*   CALLS TO: GETCOST - CALCULATES COST AND RETURNS IT IN FULL.                 
*             ADDLINE - ADDS TWO HALFLINES. R3,R4                               
*             FORMAT -  FORMATS A HALF LINE ONTO PRINT LINE R3,R4               
****************************************************************                
*                                                                               
FILLACT  NTR1                                                                   
         LA    R3,FAKELINE+ENDOFF  ACTUAL PORTION OF HALF LINE                  
         BAS   RE,GETCOST          CALC COST. RESULT IN FULL                    
         TM    NBUNITST,X'42'      ZERO COST FOR MISSED OR PRE-EMPT             
         BZ    FILA2                                                            
         XC    FULL,FULL                                                        
FILA2    MVC   COSTOFF(4,R3),FULL                                               
         MVC   UNITOFF+2(2,R3),NBACTUN                                          
         MVC   GRPSOFF+2(2,R3),NBACTHOM+2                                       
         NETGO NVGETDEM,DMCB,(0,NDDEMBLK),(C'A',DEM1OFF(R3))                    
         NETGO NVGETDEM,DMCB,(1,NDDEMBLK),(C'A',DEM2OFF(R3))                    
         NETGO NVGETDEM,DMCB,(2,NDDEMBLK),(C'A',DEM3OFF(R3))                    
*                                                                               
         LA    R4,MONTOTL+ENDOFF   ACTUAL HALF OF MON TOTAL LINE                
         BAS   RE,ADDLINES         ADD FAKELINE INTO MON TOTAL LINE             
*                                                                               
         LA    R4,ACTTOTL+ENDOFF   ACTUAL HALF OF ACT TOTAL LINE                
         BAS   RE,ADDLINES         ADD FAKELINE INTO ACT TOTAL LINE             
*                                                                               
         LA    R4,CLACTLIN         ADDR OF ACTUAL OUTPUT AREA                   
         BAS   RE,FORMAT           FORMAT ACTUAL HALF LINE                      
         CLI   FLAVOR,C'A'         FOR FLAVOR AUDIT TRAIL                       
         BNE   XITFA                 PUT NBRESULT IN UNIT COL                   
         USING CLDEMLIN,R4                                                      
         MVC   CLUNITS,SPACES                                                   
         MVC   CLUNITS+2(1),NBRESULT                                            
*                                                                               
         CLI   NBRESULT,C'E'       IF DEMOS ARE ACTUAL                          
         BE    XITFA                                                            
*                                                                               
         MVI   SRCHEL,X'DE'        LOOK FOR MANUAL OVERRIDE                     
         L     R3,NBAIO            A(UNIT RECORD)                               
         USING NURECD,R3                                                        
         LA    R2,NUMAINEL         A(FIRST ELEMENT)                             
         DROP  R3                                                               
         BAS   RE,NEXTEL           IF ONE EXISTS                                
         BNE   XITFA                                                            
         MVI   CLUNITS+3,C'M'        ID IT AS M                                 
         SPACE 1                                                                
*XITFA    XIT1  REGS=(R5)                                                       
XITFA    B     R5XIT                                                            
         DROP  R4                                                               
***************************************************************                 
         EJECT                                                                  
*              CALCULATES COSTS AND SETS ZERO FLAG                              
         SPACE 3                                                                
* OUTPUT:   COST (IN DOLLARS)  RETURNED IN FULL                                 
* ZEROFLAG: SET TO SPACE OR ZERO                                                
* LOCALS: R2,R3 - REGISTER PAIR FOR DIVIDE. R3 CONTAINS COST IN CENTS           
         SPACE 1                                                                
GETCOST  NTR1                                                                   
         MVI   ZEROFLAG,C' '       SET FLAG TO SPACE                            
         CLI   USEASSGN,C'Y'      IF ASSIGNED COST FLAG SET                     
         BNE   GC2                                                              
         L     R3,NBASSIGN           USE ASSIGNED COST                          
         TM    NBUNITST,X'08'      HAS IT BEEN ASSIGNED                         
         BNO   *+8                                                              
         MVI   ZEROFLAG,C'0'                                                    
         B     XITGC                                                            
         SPACE 1                                                                
GC2      TM    NBUNITST,X'20'      HAS IT BEEN ASSIGNED                         
         BNO   *+8                                                              
         MVI   ZEROFLAG,C'0'                                                    
         CLI   USEINTEG,C'Y'       ELSEIF INTEGRATION FLAG SET                  
         BNE   GC4                                                              
         L     R3,NBINTEG            USE ACTUAL COST + INTEGRATION COST         
         A     R3,NBACTUAL                                                      
         B     XITGC                                                            
         SPACE 1                   ELSE                                         
GC4      L     R3,NBACTUAL           USE ACTUAL COST                            
         SPACE 1                                                                
XITGC    LR    R2,R3           CONVERT CENTS TO DOLLARS AND ROUND               
         SR    R3,R3                                                            
         SRDA  R2,31                                                            
         D     R2,=F'100'                                                       
         LTR   R3,R3                                                            
         BM    *+8                                                              
         AH    R3,=H'1'                                                         
         SRA   R3,1                                                             
         ST    R3,FULL                                                          
         B     EXIT                                                             
         EJECT                                                                  
***************************************************************                 
* ADDLINES - ADDS HALFLINE1 INTO HALFLINE2                                      
*        ARGS: R3 - HALFLINE1                                                   
*              R4 - HALFLINE2                                                   
*                                                                               
ADDLINES NTR1                                                                   
         AF    COSTOFF(R4),COSTOFF(R3)                                          
         AF    UNITOFF(R4),UNITOFF(R3)                                          
         AF    GRPSOFF(R4),GRPSOFF(R3)                                          
         AF    DEM1OFF(R4),DEM1OFF(R3)                                          
         AF    DEM2OFF(R4),DEM2OFF(R3)                                          
         AF    DEM3OFF(R4),DEM3OFF(R3)                                          
         B     EXIT                                                             
****************************************************************                
         EJECT                                                                  
*              PUTS HALFLINE INTO PRINTABLE FORMAT                              
         SPACE 3                                                                
*        ARGS: R3 - HALFLINE                                                    
*              R4 - FORMADDR                                                    
         SPACE 1                                                                
FORMAT   NTR1                                                                   
         USING CLDEMLIN,R4                                                      
         USING ACD,R3                                                           
         EDIT  (4,ACCOST),(9,CLCOST),FLOAT=-    COST                            
         OC    CLCOST+8(1),ZEROFLAG ENSURE ZERO COST PRINTS                     
         EDIT  (4,ACUNITS),(4,CLUNITS)       UNITS                              
         CLI   CLUNITS,C' '                                                     
         BNE   PF2                                                              
         MVC   CLUNITS(3),CLUNITS+1          SHIFT LEFT IF ROOM                 
         MVI   CLUNITS+3,C' '                 AND CLEAR LEFTOVER BYTE           
         SPACE 1                                                                
PF2      EDIT  (4,ACGRPS),(7,CLGRPS),1,ZERO=BLANK                               
         CLI   CLGRPS,C' '                                                      
         BE    PF8                           GRPS (1 DEC IF ROOM)               
         L     R1,ACGRPS                                                        
         LA    R1,5(R1)                                                         
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
         EDIT  (R1),(7,CLGRPS)                                                  
PF8      NETGO NVPRDEM,DMCB,(0,NDDEMBLK),ACDEM1,CLDEMO1     DEMO1               
         NETGO NVPRDEM,DMCB,(1,NDDEMBLK),ACDEM2,CLDEMO2     DEMO2               
         NETGO NVPRDEM,DMCB,(2,NDDEMBLK),ACDEM3,CLDEMO3     DEMO3               
*        XIT1  REGS=(R5)                                                        
         B     R5XIT                                                            
         EJECT                                                                  
***************************************************************                 
* CPPFORM  - PUTS CPP LINE IN PRINTABLE FORMAT                                  
*            RATINGS ARE WITH NO DEC PLACES. OTHERS HAVE 2.                     
*                                                                               
CPPFORM  NTR1                                                                   
         LA    R4,CLESTLIN         DO ESTIMATES                                 
         USING CLDEMLIN,R4                                                      
         EDIT  (4,ESTGRPCP),(5,CLGRPS+2),FLOAT=$                                
*                                                                               
         OC    ESTDM1CP,ESTDM1CP                                                
         BZ    CPPF2                                                            
         EDIT  (4,ESTDM1CP),(7,CLDEMO1),2,FLOAT=$                               
         NETGO NVDEMTYP,DMCB,(0,NDDEMBLK),BYTE  1ST DEMO TYPE IN BYTE           
         CLI   BYTE,C'R'                                                        
         BNE   CPPF2                                                            
         EDIT  (4,ESTDM1CP),(7,CLDEMO1),FLOAT=$                                 
*                                                                               
CPPF2    OC    ESTDM2CP,ESTDM2CP                                                
         BZ    CPPF4                                                            
         EDIT  (4,ESTDM2CP),(7,CLDEMO2),2,FLOAT=$                               
         NETGO NVDEMTYP,DMCB,(1,NDDEMBLK),BYTE  2ND DEMO TYPE IN BYTE           
         CLI   BYTE,C'R'                                                        
         BNE   CPPF4                                                            
         EDIT  (4,ESTDM2CP),(7,CLDEMO2),FLOAT=$                                 
*                                                                               
CPPF4    OC    ESTDM3CP,ESTDM3CP                                                
         BZ    CPPF6                                                            
         EDIT  (4,ESTDM3CP),(7,CLDEMO3),2,FLOAT=$                               
         NETGO NVDEMTYP,DMCB,(2,NDDEMBLK),BYTE  3RD DEMO TYPE IN BYTE           
         CLI   BYTE,C'R'                                                        
         BNE   CPPF6                                                            
         EDIT  (4,ESTDM3CP),(7,CLDEMO3),FLOAT=$                                 
*                                                                               
CPPF6    LA    R4,CLACTLIN                                                      
         EDIT  (4,ACTGRPCP),(5,CLGRPS+2),FLOAT=$                                
*                                                                               
         OC    ACTDM1CP,ACTDM1CP                                                
         BZ    CPPF8                                                            
         EDIT  (4,ACTDM1CP),(7,CLDEMO1),2,FLOAT=$                               
         NETGO NVDEMTYP,DMCB,(0,NDDEMBLK),BYTE  1ST DEMO TYPE IN BYTE           
         CLI   BYTE,C'R'                                                        
         BNE   CPPF8                                                            
         EDIT  (4,ACTDM1CP),(7,CLDEMO1),FLOAT=$                                 
*                                                                               
CPPF8    OC    ACTDM2CP,ACTDM2CP                                                
         BZ    CPPF10                                                           
         EDIT  (4,ACTDM2CP),(7,CLDEMO2),2,FLOAT=$                               
         NETGO NVDEMTYP,DMCB,(1,NDDEMBLK),BYTE  2ND DEMO TYPE IN BYTE           
         CLI   BYTE,C'R'                                                        
         BNE   CPPF10                                                           
         EDIT  (4,ACTDM2CP),(7,CLDEMO2),FLOAT=$                                 
*                                                                               
CPPF10   OC    ACTDM3CP,ACTDM3CP                                                
         BZ    CPPFXIT                                                          
         EDIT  (4,ACTDM3CP),(7,CLDEMO3),2,FLOAT=$                               
         NETGO NVDEMTYP,DMCB,(2,NDDEMBLK),BYTE  3RD DEMO TYPE IN BYTE           
         CLI   BYTE,C'R'                                                        
         BNE   CPPFXIT                                                          
         EDIT  (4,ACTDM3CP),(7,CLDEMO3),FLOAT=$                                 
*CPPFXIT  XIT1  REGS=(R5)                                                       
CPPFXIT  B     R5XIT                                                            
****************************************************************                
         EJECT                                                                  
***************************************************************                 
* DIFFFORM  - PUTS DIFFERENCE LINE IN PRINTABLE FORMAT                          
*             RATIO OF EST/ACT CPPS *100                                        
DIFFFORM  NTR1                                                                  
         LA    R4,CLACTLIN         PUT THEM IN ACTUAL LINE                      
         USING CLDEMLIN,R4                                                      
*                                                                               
*                                                                               
         L     R3,ESTGRPCP                                                      
         L     R1,ACTGRPCP                                                      
         LTR   R1,R1                                                            
         BZ    DF1                                                              
         M     R2,=F'200'                                                       
         DR    R2,R1                                                            
         AH    R3,=H'1'                                                         
         SRA   R3,1                                                             
         EDIT  (R3),(7,CLGRPS)                                                  
*                                                                               
DF1      L     R3,ESTDM1CP                                                      
         L     R1,ACTDM1CP                                                      
         LTR   R1,R1                                                            
         BZ    DF2                                                              
         M     R2,=F'200'                                                       
         DR    R2,R1                                                            
         AH    R3,=H'1'                                                         
         SRA   R3,1                                                             
         EDIT  (R3),(7,CLDEMO1)                                                 
*                                                                               
DF2      L     R3,ESTDM2CP                                                      
         L     R1,ACTDM2CP                                                      
         LTR   R1,R1                                                            
         BZ    DF4                                                              
         M     R2,=F'200'                                                       
         DR    R2,R1                                                            
         AH    R3,=H'1'                                                         
         SRA   R3,1                                                             
         EDIT  (R3),(7,CLDEMO2)                                                 
*                                                                               
DF4      L     R3,ESTDM3CP                                                      
         L     R1,ACTDM3CP                                                      
         LTR   R1,R1                                                            
         BZ    DF6                                                              
         M     R2,=F'200'                                                       
         DR    R2,R1                                                            
         AH    R3,=H'1'                                                         
         SRA   R3,1                                                             
         EDIT  (R3),(7,CLDEMO3)                                                 
*                                                                               
DF6      DS    0H                                                               
*        XIT1  REGS=(R5)                                                        
         B     R5XIT                                                            
****************************************************************                
         EJECT                                                                  
* DEMHEADS - GET ALPHANUMERIC DEMO HEADERS                                      
*                                                                               
DEMHEADS NTR1                                                                   
         MVC   ALPHDEM1,SPACES     RESET TO 0                                   
         MVC   ALPHDEM2,SPACES                                                  
         MVC   ALPHDEM3,SPACES                                                  
         NETGO NVDEMCON,DMCB,(0,NDDEMBLK),(C'C',DBLOCK),(7,ALPHDEM1)            
         NETGO NVDEMCON,DMCB,(1,NDDEMBLK),(C'C',DBLOCK),(7,ALPHDEM2)            
         CLC   NDDEMOS+6(3),=X'FF0000' TEST IF NO MORE DEMOS                    
         BE    XITDH                                                            
         NETGO NVDEMCON,DMCB,(2,NDDEMBLK),(C'C',DBLOCK),(7,ALPHDEM3)            
XITDH    B     EXIT                                                             
**************************************************************                  
* HOOK - BUILDS THE HEADER. SUPPLEMENTS THE SSPECS IN NEMED50                   
*                                                                               
*  LOCALS -R4 -A(TWA)                                                           
************************************************************                    
HOOK     NTR1                                                                   
*                                                                               
         L     R4,ATWA                                                          
         USING T31EFFD,R4                                                       
*                                                                               
* MAIN HEADER INFO                                                              
         LA    R5,HEAD1            BASE ADDRESS FOR OFFSETS                     
         USING PHEAD,R5                                                         
         MVC   PHCLI,NBSELCLI                                                   
         MVC   PHCLNM,TWNCLIN                                                   
         MVC   PHPRD,TWNPRO                                                     
         MVC   PHPRNM,TWNPRON                                                   
         MVC   PHNET,CURNET                                                     
         CLI   RCSUBPRG,2          FOR SUMMARY                                  
         BNE   MHED2               PAGE                                         
         OC    NBSELNET,NBSELNET                                                
         BNZ   MHED2                                                            
         MVC   PHNET(4),=C'ALL '                                                
MHED2    EDIT  NBSELEST,(4,PHEST),ALIGN=LEFT                                    
         CLI   NBSELESE,0                                                       
         BNE   MHED5                                                            
         CLI   NBSELEFL,0                                                       
         BE    *+10                                                             
MHED5    MVC   PHEST(7),TWNEST                                                  
         MVC   PHESTNM,TWNESTN                                                  
         MVC   PHDAYP,TWNDPTN                                                   
*                                                                               
* COLUMN HEADERS                                                                
         LA    R5,H9               BASE ADDRESS FOR PROPER OFFSETS              
         USING COLINFO,R5                                                       
         MVC   CLESTLIN+5(36),=C'-------------ESTIMATED--------------'          
         MVC   CLACTLIN+5(36),=C'---------------ACTUAL---------------'          
*                                                                               
         LA    R5,132(R5)          SECOND LINE                                  
         USING CLDEMLIN,R6         FILL IN EST,ACT LINES                        
         LA    R6,CLESTLIN                                                      
         BAS   RE,HOOKSUB1                                                      
         LA    R6,CLACTLIN                                                      
         BAS   RE,HOOKSUB1                                                      
*                                                                               
         LA    R5,132(R5)          THIRD LINE                                   
         LA    R6,CLESTLIN                                                      
         BAS   RE,HOOKSUB2                                                      
         LA    R6,CLACTLIN                                                      
         BAS   RE,HOOKSUB2                                                      
*                                                                               
****** NOW DO SPECIFICS TO REPORT TYPE ******                                   
         CLI   RCSUBPRG,1                                                       
         BE    EVALHEAD                                                         
         B     SUMMHEAD                                                         
*                                                                               
EVALHEAD DS    0H                  EVALUATION REPORT HEADINGS                   
         LA    R5,H9               FIRST LINE                                   
         MVC   CLDATE(4),=C'DATE'                                               
         MVC   CLPROGNM(12),=C'PROGRAM NAME'                                    
         CLC   NBSELPRD,=C'POL'    PRINT BRAND IF POL                           
         BE    EH1                                                              
         OC    NBSELPRD,NBSELPRD   NO NEED TO PRINT BRAND IF SELECTED           
         BNZ   EH2                                                              
EH1      MVC   CLPROD-1(5),=C'BRAND'                                            
EH2      LA    R5,132(R5)          SECOND LINE                                  
         MVC   CLDATE(4),=C'----'                                               
         MVC   CLPROGNM(12),=C'------------'                                    
         CLC   NBSELPRD,=C'POL'    PRINT BRAND IF POL                           
         BE    EH3                                                              
         OC    NBSELPRD,NBSELPRD                                                
         BNZ   EH4                                                              
EH3      MVC   CLPROD-1(5),=C'-----'                                            
EH4      B     XITHOOK                                                          
*                                                                               
SUMMHEAD DS    0H                  SUMMARY REPORT HEADINGS                      
         LA    R5,H9               FIRST LINE                                   
         MVC   CLNET(7),=C'NETWORK'                                             
         MVC   CLPER(L'CLPER),ALPHPER                                           
         LA    R5,132(R5)          SECOND LINE                                  
         MVC   CLNET(7),=C'-------'                                             
         GOTO1 UNDERLIN,DMCB,(6,ALPHPER),CLPER                                  
*                                                                               
XITHOOK  B     EXIT                                                             
*****                                                                           
*  HOOKSUB1 - FILL IN HEADER LINE 2 FOR CLDEMLIN                                
HOOKSUB1 NTR1                                                                   
         MVC   CLCOST+5(4),=C'COST'                                             
         MVC   CLUNITS(5),=C'UNITS'                                             
         MVC   CLGRPS+3(4),=C'HOME'                                             
         MVC   CLDEMO1+1(6),ALPHDEM1                                            
         CLI   ALPHDEM1,X'40'                                                   
         BNH   HSB100                                                           
         LA    R3,NDDEMOS                                                       
         CLI   0(R3),0                                                          
         BE    HSB100                                                           
         LA    R4,CLDEMO1+133                                                   
*        LA    R4,135(R4)                                                       
         BAS   RE,EDTRTN                                                        
HSB100   MVC   CLDEMO2+1(6),ALPHDEM2                                            
         CLI   ALPHDEM2,X'40'                                                   
         BNH   HSB102                                                           
         LA    R3,3(R3)                                                         
         CLI   0(R3),0                                                          
         BE    HSB102                                                           
         LA    R4,CLDEMO2+133                                                   
*        LA    R4,135(R4)                                                       
         BAS   RE,EDTRTN                                                        
HSB102   MVC   CLDEMO3+1(6),ALPHDEM3                                            
         CLI   ALPHDEM3,X'40'                                                   
         BNH   HSBX                                                             
         LA    R3,3(R3)                                                         
         CLI   0(R3),0                                                          
         BE    HSBX                                                             
         LA    R4,CLDEMO3+133                                                   
*        LA    R4,135(R4)                                                       
         BAS   RE,EDTRTN                                                        
HSBX     B     EXIT                                                             
EDTRTN   DS    0H                                                               
         LA    R4,135(R4)                                                       
         EDIT  (B1,0(R3)),(2,0(R4))                                             
         BR    RE                                                               
*                                                                               
*  HOOKSUB2 - FILL IN HEADER LINE 3 FOR CLDEMLIN. R6 IS CLDEMLIN.               
HOOKSUB2 EQU   *                                                                
         MVC   CLGRPS+3(4),=C'GRPS'                                             
         MVC   CLDEMO1+2(5),AD1DESC                                             
         MVC   CLDEMO2+2(5),AD2DESC                                             
         MVC   CLDEMO3+2(5),AD3DESC                                             
         BR    RE                                                               
         DROP  R4                                                               
         DROP  R5                                                               
         DROP  R6                                                               
***********************************************************                     
         EJECT                                                                  
***********************************************************                     
* GETEL,NEXTEL MACRO DEFINITION                                                 
*                                                                               
         GETEL R2,NBDTADSP,SRCHEL                                               
***********************************************************                     
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
***************************************************************                 
* SET MAXIOCTR TO 90% OF MAX IO'S SHOWN BY GETFACT                              
* CHECK IF I/O OVER                                                             
***************************************************************                 
*                                                                               
CHKMAXIO NTR1                                                                   
         L     R5,ACOMFACS         GET MAX IO                                   
         USING COMFACSD,R5                                                      
         GOTO1 CGETFACT,DMCB,0                                                  
         L     R1,0(R1)                                                         
                                                                                
         CLI   MAXIOCTR,0          HAVE WE SET MAXIO ?                          
         JNE   MAX20                           YES                              
         MVC   MAXIOCTR,FATMAXIO-FACTSD(R1)    NO                               
         SR    R2,R2                                                            
         LH    R3,MAXIOCTR                                                      
         LA    R4,9                MULTIPLY MAX IO BY 9                         
         MR    R2,R4                                                            
         LA    R4,10               DIVIDE MAXIMUM BY 10                         
         DR    R2,R4                                                            
         STH   R3,MAXIOCTR                                                      
         J     MAXOK                                                            
MAX20    DS    0H                                                               
         CLC   MAXIOCTR,FATIOCNT-FACTSD(R1)   MAXED OUT ?                       
         JH    MAXOK                                                            
                                                                                
         L     R8,ATWA                                                          
         USING T31EFFD,R8                                                       
         MVC   CONHEAD(38),=C'*-IO TIMEOUT REDUCE REQUEST PARAMETERS'           
         OI    CONHEADH+6,X'80'                                                 
         GOTO1 ERREX2,DMCB                                                      
         B     MAXOK                                                            
                                                                                
MAXOK    J     EXIT                                                             
         DROP  R5,R8                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
* * LOOKUP TABLES * *                                                           
EFFEFFS  DC    50XL1'FF'           HEX FF'S TO COMPARE VS. END OF TBL           
ZEROES   DC    50XL1'00'           HEX ZEROES                                   
*                                                                               
STATIONL DS    CL4000                                                           
*                                                                               
         EJECT                                                                  
*OFFSETS FOR PAGE HEADER INFORMATION                                            
PAGED    DSECT                                                                  
PHEAD    EQU   *                                                                
PHLENGTH EQU   132                                                              
*                                                                               
         ORG   PHEAD+3*PHLENGTH+11                                              
PHCLI    DS    CL3                 CLIENT ABBR                                  
         DS    CL1                                                              
PHCLNM   DS    CL20                CLIENT NAME                                  
         ORG   PHEAD+4*PHLENGTH+11                                              
PHPRD    DS    CL3                 PRODUCT ABBR                                 
         DS    CL1                                                              
PHPRNM   DS    CL20                PRODUCT NAME                                 
         ORG   PHEAD+4*PHLENGTH+85                                              
PHNET    DS    CL4                 NETWORK                                      
         DS    CL2                                                              
PHDAYP   DS    CL8                                                              
         ORG   PHEAD+5*PHLENGTH+11                                              
PHEST    DS    CL1                 ESTIMATE ABBR                                
         DS    CL7                                                              
PHESTNM  DS    CL24                ESTIMATE NAME                                
*                                                                               
*DSECT FOR PRINT LINES                                                          
COLD     DSECT                                                                  
COLINFO  EQU   *                                                                
*                                                                               
CLDATE   DS    CL5                 DATE (MMMDD)                                 
         DS    CL1                                                              
CLPROGNM DS    CL16                PROGRAM NAME                                 
         DS    CL1                                                              
CLPROD   DS    CL3                 PRODUCT                                      
CLPSLASH DS    CL1                 SLASH IF 2ND PROD                            
CLPROD2  DS    CL3                 2ND PRODUCT                                  
         DS    CL1                                                              
*                                  ****ESTIMATED LINE                           
CLESTLIN DS    0CL42                                                            
         DS    CL5                                                              
CLESTMGF DS    CL37                                                             
*                                  ****ACTUAL LINE                              
CLACTLIN DS    0CL42                                                            
         DS    CL5                                                              
CLACTMGB DS    CL37                                                             
*                                                                               
CLSUMM   ORG   COLINFO             ID INFO FOR SUMMARY REPORT                   
         DS    CL6                                                              
CLNET    DS    CL7                 NETWORK                                      
         DS    CL4                                                              
CLPER    DS    CL6                 PERIOD                                       
*                                                                               
*              ******** BREAKDOWN OF EST, ACT LINES                             
CDEMD    DSECT                                                                  
CLDEMLIN EQU   *                                                                
CLCOST   DS    CL9                 COST                                         
         DS    CL1                                                              
CLUNITS  DS    CL4                                                              
CLGRPS   DS    CL7                                                              
CLDEMO1  DS    CL7                                                              
CLDEMO2  DS    CL7                                                              
CLDEMO3  DS    CL7                                                              
*                                                                               
         EJECT                                                                  
***** COMMON WITH EDIT                                                          
WSD      DSECT                                                                  
*        NETDEMOT                                                               
*        DEDBLOCK                                                               
         PRINT OFF                                                              
       ++INCLUDE NETDEMOT                                                       
       ++INCLUDE DEDBLOCK                                                       
         PRINT ON                                                               
         SPACE 1                                                                
*****  ARGS FROM EDIT                                                           
FLAVOR   DS    CL1                                                              
DATEOPT  DS    CL1                                                              
REROPT   DS    CL1                                                              
AFFIDOPT DS    CL1                                                              
*                                                                               
*                                                                               
*****   LOCAL W/S                                                               
*                                                                               
MGF      DS    CL1                                                              
MGB      DS    CL1                                                              
SRCHEL   DS    CL1                                                              
ALPHPER  DS    CL8                 ALPHA FOR DATE-SET TYPE (E.G. MONTH          
*                                                                               
ADEMLIST DS    0C                  ALPHA OUTPUT LIST FROM DEMOCON               
ALPHDEM1 DS    CL7                 ALPHA HEADER FOR DEMO1                       
AD1DESC  DS    CL5                                                              
         DS    CL7                 EXTENDED NAD DESCRIPTION                     
ALPHDEM2 DS    CL7                 ALPHA HEADER FOR DEMO2                       
AD2DESC  DS    CL5                                                              
         DS    CL7                 EXTENDED NAD DESCRIPTION                     
ALPHDEM3 DS    CL7                                                              
AD3DESC  DS    CL5                                                              
         DS    CL7                 EXTENDED NAD DESCRIPTION                     
*                                                                               
MAXMONTS EQU   16                  MAXIMUM MONTHS ALLOWED                       
MONLIST  DS    CL(4*MAXMONTS)      LIST OF MONTHS (WEEKS)                       
NUMMONS  DS    F                   NUMBER OF DATE-SETS USED IN OMNLIST          
MAXIOCTR DS    H                   MAXIOS                                       
*                                                                               
*** INTERNAL CONTROL ******                                                     
LIN2FLG  DS    CL1                 FLAG SET IF A 2ND LINE TO BE PRINTED         
FORCEPRN DS    CL1                 FLAG SET IF PRINT IS TO BE FORCED            
NETCOUNT DS    CL1                 CURRENT NUMBER OF NETWORKS - 1               
ACTNUMNT DS    CL1                 ACTUAL NUM OF NETS USED                      
ESTFLAG  DS    CL1                 FLAG SET AFTER ESTIMATED DEMOS BEGIN         
CURNET   DS    CL4                 THE CUURENT NETWORK                          
*                                                                               
*** PRINT CONTROLS ******                                                       
PERTYPE  DS    0CL3                DESCRIBES PERIOD FOR MONTH LIST              
         DS    CL1                 W-WKS,M-MONS,Q-QRTRS (Q NOT USED YET         
         DS    CL1                 1- USE MONTHS IF TOO MANY WKS                
         DS    CL1                 1-USE QUARTERS IF TOO MANY MONS              
USEASSGN DS    CL1                 SET (TO Y) IF ASSIGNED COSTS WANTED          
USEINTEG DS    CL1                 SET (TO Y) IF INTEG COSTS INCLUDED           
PRNZFLG  DS    CL1                 SET IF ZERO LINES ARE TO BE PRINTED          
ZEROFLAG DS    CL1                 SET TO SPACE OR ZERO                         
*                                                                               
*** TOTALS *******                                                              
NUMNTRYS EQU   12                  NUMBER OF ENTRIES PER TOTAL LINE             
MAXNETS  EQU   8                   MAX NUM OF NETWORKS IN SUMMARY +1            
*                                                                               
COSTOFF  EQU   0                   OFFSETS INTO TOTALS                          
UNITOFF  EQU   4                   NOTE: EACH TOTAL LINE CONSISTS OF            
GRPSOFF  EQU   8                     TWO HALF LINES, EACH HAVING                
DEM1OFF  EQU   12                    THESE OFFSETS.                             
DEM2OFF  EQU   16                  THESE OFFSETS MUST CORRESPOND                
DEM3OFF  EQU   20                    WITH THE ACD DSECT                         
ENDOFF   EQU   24                  END OF HALF LINE                             
*                                                                               
CPPLIST  EQU   *                   PLACE WHERE COST PER POINTS STORED           
ESTGRPCP DS    F                                                                
ESTDM1CP DS    F                                                                
ESTDM2CP DS    F                                                                
ESTDM3CP DS    F                                                                
ACTGRPCP DS    F                                                                
ACTDM1CP DS    F                                                                
ACTDM2CP DS    F                                                                
ACTDM3CP DS    F                                                                
*                                                                               
*                                                                               
NETLIST  DS    (MAXNETS)CL6                                                     
*** EACH LINE CONSISTS                                                          
FAKELINE DS    (NUMNTRYS)F         HOLDS UNIT RECORD INFO                       
FAKELEN  EQU   *-FAKELINE                                                       
MONTOTL  DS    (NUMNTRYS)F         CURRENT MONTH TOTALS                         
MTOTLEN  EQU   *-MONTOTL                                                        
NETTOTL  DS    (NUMNTRYS)F         CURRENT NETWORK TOTALS                       
NTOTLEN  EQU   *-NETTOTL                                                        
ACTTOTL  DS    (NUMNTRYS)F         CURRENT ACTUAL TOTALS                        
ATOTLEN  EQU   *-ACTTOTL                                                        
TOTTOTL  DS    (NUMNTRYS)F         CURRENT REPORT TOTALS                        
TTOTLEN  EQU   *-TOTTOTL                                                        
SUMTOTL  DS    (NUMNTRYS*MAXMONTS*MAXNETS)F     SUMMARY TOTALS                  
STOTLEN  EQU   *-SUMTOTL                                                        
         DS    0C                                                               
WORKX    EQU   *                                                                
*                                                                               
*                                                                               
*                                                                               
ACD      DSECT                                                                  
ACCOST   DS    F                             COST                               
ACUNITS  DS    F                             UNITS                              
ACGRPS   DS    F                             HOME GRPS                          
ACDEM1   DS    F                             DEMO 1                             
ACDEM2   DS    F                             DEMO 2                             
ACDEM3   DS    F                             DEMO 3                             
         SPACE 2                                                                
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE NETINCLS                                                       
         PRINT ON                                                               
*                                                                               
         EJECT                                                                  
       ++INCLUDE NEMEDFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDE6D                                                       
         SPACE 1                                                                
*        NEGENUNIT                                                              
*        SPGENCLT                                                               
         PRINT OFF                                                              
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003NEMED66   07/16/04'                                      
         END                                                                    
