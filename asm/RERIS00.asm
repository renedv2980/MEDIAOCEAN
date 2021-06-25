*          DATA SET RERIS00    AT LEVEL 046 AS OF 11/04/13                      
*          DATA SET RERIS00    AT LEVEL 043 AS OF 02/20/98                      
*PHASE T80D00A                                                                  
         TITLE 'RERIS00 --- T80D00 --- REP RIS BASE MODULE'                     
*                                                                               
***********************************************************************         
*                                                                     *         
*  RERIS00 -- PHASE T80D00 -- RIS (REP ON-LINE FINANCIAL INFORMATION) *         
*                              SUB-SYSTEM CONTROLLER                  *         
*                                                                     *         
* ------------------------------------------------------------------- *         
*                                                                     *         
*  MOD LOG                                                            *         
*  -------                                                            *         
*  08/03/89  PJS  CHANGE TO PRINT NUMBERS WITHOUT PENNIES             *         
*                                                                     *         
*  01/15/90  PJS  READ REP PROGRAM PROFILES FOR RIS AND SAVE IN TWA   *         
*                                                                     *         
*  JAN23/90 (MRR) --- SET ALL PROFILE BITS TO '1' IF  DDS TERMINAL    *         
*                                                                     *         
*  FEB24/92 (BU ) --- SET UP FOR ALTERNATE DOLLAR DISPLAY...          *         
*                                                                     *         
*  FEB25/92 (BU ) --- SAVE PROFILE BIT 4 IF DDS TERMINAL (SEE JAN23/90*         
*                     NOTE ABOVE)                                     *         
*                                                                     *         
*  AUG05/94 (BU ) --- DON'T SET ALL PROFILE BITS....                  *         
*                                                                     *         
*  SEP17/96 (SKU) --- UPGRADE SWAP TO CONTRACT TO USE GLOBBER XFERCTRL*         
*                                                                     *         
*  NOV11/96 (RHV) --- SWAPPING TO BROWSE                              *         
*                                                                     *         
*  APR24/97 (RHV) --- SUPPORT PF12 GENERIC RETURN FROM CONTRACT PROG  *         
*                                                                     *         
*  SEP09/97 (JRD) --- USE BROWSE INTERFACE MODULE                     *         
*                                                                     *         
*  OCT24/97 (BU ) --- RERISWRKB --> RERISWRKC                         *         
*                                                                     *         
*  DEC10/97 (JRD) --- FIX GLOBBER BUG, CONTRACT IS ONLY 7 CHARS       *         
*                                                                     *         
*  JUN00/00 (BU ) --- REMOVE REFERENCES TO GLV1GOTO PER MEL HERTZIG   *         
*                                                                     *         
*  JUL19/00 (BU ) --- MASTER REP RECOGNITION                          *         
*                                                                     *         
*  FEB22/01 (ABO) --- ADDED PF5 FUNCTION (MGL).                       *         
*                                                                     *         
*  MAR14/01 (BU ) --- DAILY PACING DISPLAY (TENTATIVE)                *         
*                                                                     *         
*  JUN16/04 (BU ) --- GLOBBER IN: RESET A(CURSOR) TO LINE RATHER      *         
*                     THAN HOME                                       *         
*                                                                     *         
*  AUG15/13 (BOB) --- ADD LISTS FOR MAKEGOOD OFFERS                   *         
*                                                                     *         
*                                                                     *         
*                    RGENEROL INCLUDED EXPLICITLY                     *         
***********************************************************************         
*                                                                               
T80D00   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 600,**0D00*,CLEAR=YES,RR=RE                                      
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                NOTE R9                               
         USING T80D00,RB,R9                                                     
         USING GENOLD,RC                                                        
         ST    RD,BASERD                                                        
*                                                                               
         L     RF,16(R1)           A(COMFACS)                                   
         ST    RF,ACOMFACS                                                      
         ST    R1,AFACILS                                                       
         L     RF,0(R1)            A(FATIOB)                                    
         ST    RF,ATIOB                                                         
         USING TIOBD,RF                                                         
         ZIC   R0,TIOBAID          NUMBER OF PFKEY PRESSED                      
         C     R0,=F'12'                                                        
         BNH   *+8                                                              
         S     R0,=F'12'                                                        
         STC   R0,PFKEY            PFKEY ADJUSTED TO 1..12 (0 = ENTER)          
         DROP  RF                                                               
                                                                                
         BAS   RE,INITL                                                         
         USING T80DFFD,RA                                                       
         ST    RB,BASERB                                                        
*                                                                               
         L     RF,ACOMFACS                                                      
         USING COMFACSD,RF                                                      
         GOTO1 CCALLOV,DMCB,0,X'D9000AAC',0                                     
         DROP  RF                                                               
         MVC   VREPFACS,0(R1)      EXTERNAL ROUTINE, MISC. SUBROUTINES          
         MVC   RFBLOCK(4),ACOMFACS                                              
         MVC   RFBLOCK+4(2),REPALPHA                                            
*                                                                               
         GOTO1 (RFBROWSE,VREPFACS),DMCB,ACOMFACS,BASERD,0                       
         BAS   RE,CHKGLOB          CHECK GLOBBER                                
                                                                                
*                                                                               
*- IF NOT ALREADY DONE, READ IN REP RECORD FOR PGM PROFILE                      
MAIN0010 CLI   SVPGP#,RREPQRIS                                                  
         BE    MAIN0160            IN TWA FROM PRIOR HIT                        
*                                                                               
MAIN0040 EQU   *                                                                
         XC    KEY,KEY                                                          
         MVI   KEY,X'01'                                                        
         MVC   KEY+25(2),REPALPHA                                               
         GOTO1 READ                                                             
         CLI   DMCB+8,X'10'                                                     
         BNE   *+6                                                              
         DC    H'0'                REP RECORD NOT ON FILE?  HOW?                
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         CLC   RREPMAST,=C'  '     MASTER OR SUBSIDIARY?                        
         BNH   MAIN0060            NO MASTER/SUBSIDIARY                         
         CLC   RREPMAST,=X'FFFF'   MASTER?                                      
         BE    MAIN0060            YES                                          
         MVC   MASTRREP,RREPMAST   NO  - SUBSIDIARY: SAVE MASTER                
MAIN0060 EQU   *                                                                
         MVI   PREVALT$,X'FF'      SET PREVIOUS ALT$ VALUE 1ST PASS             
         XC    SVPGMPRF,SVPGMPRF   ASSUME NO ELEMENT                            
         MVI   SVPGP#,RREPQRIS                                                  
*                                                                               
         L     RE,AIOAREA          RECORD IS HERE                               
         USING RREPREC,RE                                                       
         MVC   REPPRFSV,RREPPROF+8   SAVE PROFILE FOR DIRECT RESPONSE           
         MVC   DAILYPAC,RREPPROF+27  SAVE PROFILE FOR DAILY PACING              
         MVC   CFLAG,RREPPROF+10     SAVE 0 INVOICE $ FLAG                      
         DROP  RE                                                               
         SR    RF,RF                                                            
         ICM   RF,3,RREPLEN                                                     
         AR    RF,RE                                                            
         MVI   0(RF),0             FORCE 0 AT END OF RECORD                     
*                                                                               
         LA    RE,34(RE)           A(1ST ELEMENT)                               
MAIN0080 EQU   *                                                                
         CLI   0(RE),0             END OF RECORD W/O MATCH?                     
         BE    MAIN0160                                                         
*                                                                               
         CLI   0(RE),X'04'         PROGRAM PROFILE ELEMENT?                     
         BE    MAIN0100                                                         
*                                                                               
         ZIC   RF,1(RE)            GET NEXT ELEMENT                             
         AR    RE,RF                                                            
         B     MAIN0080                                                         
*                                                                               
*- FIND RIS PROGRAM UNIT WITHIN PROGRAM PROFILE ELEMENT                         
MAIN0100 EQU   *                                                                
         USING RREPPGMP,RE                                                      
         ZIC   R0,RREPPGM#         NUMBER OF PROGRAM UNITS                      
         LA    RE,RREPPGM1         A(1ST UNIT)                                  
         DROP  RE                                                               
*                                                                               
MAIN0120 CLI   0(RE),RREPQRIS      LOOKING FOR RIS                              
         BE    MAIN0140                                                         
         LA    RE,RREPPGML(RE)     NEXT UNIT                                    
         BCT   R0,MAIN0120                                                      
         B     MAIN0160            NO MATCH. USE DEFAULTS                       
*                                                                               
MAIN0140 MVC   SVPGMPRF,0(RE)      SAVE UNIT IN TWA.                            
         SPACE                                                                  
MAIN0160 EQU   *                                                                
*                                                                               
*   AT THIS POINT, THE PROFILE HAS BEEN LOADED.  IF THIS IS A DDS               
*      TERMINAL, ALL BITS ARE TO BE TURNED ON.  THE 4TH BIT MUST                
*      RETAIN ITS VALUE FROM THE PROFILE, HOWEVER.  THIS IS THE                 
*      'ALTERNATE $ DISPLAY BIT'.                                               
*                                                                               
         LR    R8,RA                                                            
         USING TWAD,R8                                                          
         CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
*****>>  BNE   MAIN0200                                                         
*                                                                               
**    DDS TERMINAL TEST IS IGNORED!  THIS MAKES IT IMPOSSIBLE TO TEST           
*       NEW REQUEST BITS, BECAUSE THEY ARE CONSTANTLY OVERRIDDEN!               
*                                                                               
         B     MAIN0200                                                         
                                                                                
***      LA    RE,1                SET FLAG TO 'NO RESET'                       
***      TM    SVPGPBIT,X'10'      IS 4TH BIT ON?                               
***      BO    MAIN0180            YES - OKAY TO OVERRIDE IT                    
***      SR    RE,RE               NO  - SET FLAG TO 'RESET'                    
MAIN0180 EQU   *                                                                
***      MVI   SVPGP#,RREPQRIS                                                  
***      MVC   SVPGPBIT,=8X'FF'                                                 
***      LTR   RE,RE               RESET 4TH BIT?                               
***      BNZ   MAIN0200            NO  - SET TO 'NO RESET'                      
***      XI    SVPGPBIT,X'10'      YES - SET TO 'RESET'                         
*                                                                               
         DROP  R8                                                               
*                                                                               
MAIN0200 EQU   *                                                                
         LA    RE,GETMDAY                                                       
         ST    RE,VMONDAY                                                       
         MVC   LIST,=8X'FF'        SET CONSTANTS                                
         XC    ALL,ALL                                                          
         XC    RISMESS,RISMESS                                                  
         OI    RISSTAH+1,X'01'     TURN ON MODIFIED BIT                         
         OI    RISSTAH+6,X'80'     TURN ON TRANSMIT BIT                         
         EJECT                                                                  
                                                                                
* - CHECK IF PFKEY SWITCHING                                                    
         CLI   PFKEY,0             WAS ENTER PRESSED?                           
         BE    CHKOVL              YES CONTINUE NORMAL PROCESSING               
                                                                                
***      CLI   PFKEY,12            12=SWITCH TO SELLERS WORKSHEET ?             
***      BE    PF12                                                             
*                                                                               
MAIN0220 EQU   *                                                                
         CLI   PFKEY,4             4=SCROLL BACK                                
         BE    CHKOVL                LET IT CONTINUE/HANDLED IN 05              
                                                                                
         CLI   PFKEY,2             2=SWITCH TO CONTRACT DISPLAY                 
         BE    PF02                                                             
                                                                                
         CLI   PFKEY,3             2=SWITCH TO CONTRACT CHANGE                  
         BE    PF02                                                             
*ABOB ADDED                                                                     
         CLI   PFKEY,5             5=SWITCH TO CONTRACT CHANGE                  
         BE    PF02                                                             
                                                                                
         CLI   PFKEY,6             5=SWITCH TO DARE FOR LISTO                   
         BE    PF02                                                             
                                                                                
         CLI   PFKEY,1             ,,1=PRINT REPORT                             
         BNE   CHKOVL                                                           
         CLI   PFJUMP,2            ,,ONLY IF LISTD                              
         BE    *+8                                                              
         CLI   PFJUMP,4            ,,ONLY IF LISTO                              
         BNE   CHKOVL                                                           
         MVI   PRNT,1                                                           
         B     INVAL                                                            
                                                                                
* SWITCH TO CONTRACT DISPLAY                                                    
PF02     CLI   PFJUMP,1            FOR LIST$                                    
         BE    PF10                                                             
         CLI   PFJUMP,3            FOR LISTP (PRODUCT LIST)                     
         BE    PF10                                                             
         CLI   PFJUMP,4            FOR LISTO (MAKEGOOD LIST)                    
         BE    PF10                                                             
         CLI   PFJUMP,2            FOR LISTD                                    
         BNE   CHKOVL                                                           
         BAS   RE,CHKCURS2         IS CURSOR ON VALID LINE                      
         B     *+8                                                              
PF10     BAS   RE,CHKCURSR         IS CURSOR POSITIONED ON VALID LINE?          
         BNE   CURSERR             NO                                           
         BAS   RE,SWAP             YES/SET GLOBBER 'N SWAP                      
         B     XIT                                                              
                                                                                
**PF12     BAS   RE,SELJUMP           IF FROM SELLERS, GO BACK                  
**         BNE   CHKVOL                  NOT FROM SELLERS/IGNORE                
**         B     XIT                     YES/FROM SELLERS JUMP                  
                                                                                
XIT      XIT1                                                                   
                                                                                
                                                                                
*                                  TEST WHETHER TO CALL OVERLAY                 
CHKOVL   TM    RISSTAH+4,X'20'                                                  
         BNO   INVAL                                                            
         TM    RISOFFH+4,X'20'                                                  
         BNO   INVAL                                                            
         TM    RISAGYH+4,X'20'                                                  
         BNO   INVAL                                                            
         TM    RISADVH+4,X'20'                                                  
         BNO   INVAL                                                            
         TM    RISSLSH+4,X'20'                                                  
         BNO   INVAL                                                            
         TM    RISCTGH+4,X'20'                                                  
         BNO   INVAL                                                            
         TM    RISTARH+4,X'20'                                                  
         BNO   INVAL                                                            
         TM    RISCRTH+4,X'20'                                                  
         BNO   INVAL                                                            
         TM    RISCSTH+4,X'20'                                                  
         BNO   INVAL                                                            
         TM    RISMNTSH+4,X'20'                                                 
         BNO   INVAL                                                            
         TM    RISDATEH+4,X'20'                                                 
         BNO   INVAL                                                            
         TM    RISFILTH+4,X'20'                                                 
         BNO   INVAL                                                            
         TM    RISALTDH+4,X'20'                                                 
         BNO   INVAL                                                            
* REQUEST SCREEN HAS NOT CHANGED                                                
         CLI   PFKEY,4            DO WE WANT TO SCROLL BACK?                    
         BE    LOADREAD            YES                                          
* ARE WE COMING FROM 05 AND ARE AT END OF SCREEN                                
         CLI   NEXTBYTE,X'FF'                                                   
         BNE   LOADREAD            NO                                           
         MVI   NEXTBYTE,0          YES CLEAR BYTE                               
         B     INVAL               AND START FROM THE TOP                       
                                                                                
                                                                                
INVAL    NI    RISOFFH+4,X'DF'                                                  
         NI    RISSTAH+4,X'DF'                                                  
         NI    RISAGYH+4,X'DF'                                                  
         NI    RISADVH+4,X'DF'                                                  
         NI    RISSLSH+4,X'DF'                                                  
         NI    RISCTGH+4,X'DF'                                                  
         NI    RISTARH+4,X'DF'                                                  
         NI    RISCRTH+4,X'DF'                                                  
         NI    RISCSTH+4,X'DF'                                                  
         NI    RISMNTSH+4,X'DF'                                                 
         NI    RISDATEH+4,X'DF'                                                 
         NI    RISFILTH+4,X'DF'                                                 
         NI    RISALTDH+4,X'DF'                                                 
                                                                                
*                                                                               
         XC    WOPSSAVE,WOPSSAVE   INIT FIELDS FOR LISTO                        
         XC    OOPSSAVE,OOPSSAVE   INIT FIELDS FOR LISTO                        
         XC    DOPSSAVE,DOPSSAVE   INIT FIELDS FOR LISTO                        
         XC    SAVEKEY,SAVEKEY                                                  
*                                                                               
         XC    MYP,MYP       CLEAR THIS AREA USED BY 05 TO STORE                
         XC    MYP2,MYP2     DISK ADDRESSES FOR SCROLL BACK                     
*                                                                               
*                                  VALIDATE OVERLAY                             
         GOTO1 VCALLOV,DMCB,(1,0),(RA)                                          
         CLI   DMCB+4,X'FF'        TEST ERROR                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB             A(OVERLAY)                                   
         GOTO1 (RF),DMCB,(RC)                                                   
         CLI   ERRAREA,0                                                        
         BNE   EXXMOD                                                           
         EJECT                                                                  
LOADREAD DS    0H                                                               
         GOTO1 VCALLOV,DMCB,(10,0),(RA)                                         
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VREAD,DMCB          SET READ OVERLAY ADDRESS                     
         B     TESTLIST                                                         
         SPACE 2                                                                
TESTLIST CLI   NEXTBYTE,0                                                       
         BNE   LISTER                                                           
         CLI   LISTBYTE,0                                                       
         BNE   LISTER                                                           
         EJECT                                                                  
*                                  BUILD GRID                                   
         LA    RE,GRID             CLEAR FIRST                                  
         LA    RF,L'GRID-1         CLEAR FIRST                                  
         XCEF                                                                   
         SPACE 1                                                                
         MVC   HALF,TBLBGN                                                      
         LA    RE,GRID                                                          
*                                                                               
GRID10   MVC   0(2,RE),HALF                                                     
         SR    RF,RF                                                            
         IC    RF,HALF+1           UPDATE MONTH                                 
         LA    RF,1(RF)                                                         
         STC   RF,HALF+1                                                        
         CH    RF,=H'13'                                                        
         BL    GRID20                                                           
         IC    RF,HALF             INCREMENT YEAR                               
         LA    RF,1(RF)                                                         
         STC   RF,HALF                                                          
         MVI   HALF+1,1            RESET MONTH                                  
*                                                                               
GRID20   LA    RE,14(RE)                                                        
         CLC   TBLEND(2),HALF                                                   
         BNL   GRID10                                                           
         MVC   GRID+12*14(2),=X'FFFF'                                           
         EJECT                                                                  
*                                  GO TO TABULATING MODULE                      
TABULATE GOTO1 VCALLOV,DMCB,(2,0),(RA)                                          
         CLI   DMCB+4,X'FF'        TEST ERROR                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB             A(MODULE)                                    
         GOTO1 (RF),DMCB,(RC)                                                   
         CLI   ERRAREA,0                                                        
         BNE   EXXMOD                                                           
*                                  NO ERROR RETURN, FOUT TABLE                  
         LA    R2,RISTITLH                                                      
         USING LINE1,R2                                                         
         XC    RISTITL,RISTITL                                                  
         CLI   RISALTD,C'I'        INVOICE DOLLARS ONLY                         
         BNE   *+14                                                             
         MVC   RISTITL(L'TITLE3),TITLE3                                         
         B     ESTDOLRS                                                         
*                                                                               
         CLI   RISALTD,C'E'                                                     
         BNE   *+14                                                             
         MVC   RISTITL(L'TITLE1),TITLE1                                         
         B     ESTDOLRS                                                         
*                                                                               
         CLI   RISALTD,C'B'                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   RISTITL(L'TITLE2),TITLE2                                         
         B     ESTDOLRS                                                         
                                                                                
ESTDOLRS EQU   *                                                                
         FOUT  (R2)                                                             
         ZIC   RE,0(R2)            ** PXZ FOR STEREO                            
         LA    R2,0(RE,R2)         **                                           
         BAS   RE,CLRLINE          **                                           
         SPACE 1                                                                
         LA    R4,GRID                                                          
         LA    R6,13                                                            
OUTPUT   SR    RE,RE                                                            
         IC    RE,0(R2)                                                         
         LA    R2,0(RE,R2)                                                      
         BAS   RE,CLRLINE                                                       
         FOUT  (R2)                                                             
OUTPUT1A DS    0H                                                               
         CLI   0(R4),0                                                          
         BNE   OUTPUT1B                                                         
         BCTR  R6,R0                                                            
         LA    R4,14(R4)                                                        
         B     OUTPUT1A                                                         
OUTPUT1B CLI   0(R4),X'FF'         TEST FOR TOTALS                              
         BNE   OUTPUT2                                                          
         FOUT  (R2)                                                             
         SR    RE,RE                                                            
         IC    RE,0(R2)                                                         
         LA    R2,0(RE,R2)                                                      
         BAS   RE,CLRLINE                                                       
         MVC   LMNTH,=C'TOTALS'                                                 
         B     OUTPUT4                                                          
OUTPUT2  GOTO1 VDATCON,DMCB,(3,(R4)),(6,LMNTH)                                  
OUTPUT4  MVC   FULL,2(R4)                                                       
         LA    R5,LCOL1                                                         
         BAS   RE,EDIT1                                                         
         MVC   FULL,6(R4)                                                       
         LA    R5,LCOL2                                                         
         BAS   RE,EDIT1                                                         
         MVC   FULL,10(R4)                                                      
         LA    R5,LCOL3                                                         
         BAS   RE,EDIT1                                                         
         FOUT  (R2)                                                             
OUTPUT8  LA    R4,14(R4)                                                        
         BCT   R6,OUTPUT                                                        
         XC    RISMESS,RISMESS                                                  
         MVC   RISMESS(L'MSG1),MSG1                                             
         FOUT  RISMESSH                                                         
         B     FTCLEAR                                                          
*                                                                               
         DROP  R2                                                               
*                                                                               
*  COMMAS=YES TAKEN OUT OF EDIT -- DEAL WITH LARGER DOLLAR FIGURES              
*  REMOVED PENNIES FROM EDIT MASK                                               
EDIT1    L     R0,FULL                                                          
         EDIT  (R0),(13,(R5)),FLOAT=-                                           
         BR    RE                                                               
         EJECT                                                                  
FTCLEAR  SR    RE,RE                                                            
         IC    RE,0(R2)                                                         
         LA    R2,0(R2,RE)                                                      
FTCLEAR2 CLI   0(R2),0                                                          
         BE    FTCLEAR4            FINISHED                                     
         IC    RE,0(R2)                                                         
         SH    RE,=H'9'                                                         
         EX    RE,ORFLD                                                         
         BZ    FTCLEAR3                                                         
         EX    RE,XCFLD                                                         
         FOUT  (R2)                                                             
FTCLEAR3 LA    R2,9(RE,R2)                                                      
         B     FTCLEAR2                                                         
*                                                                               
FTCLEAR4 NI    RISSTAH+4,X'DF'                                                  
         NI    RISOFFH+4,X'DF'                                                  
         NI    RISADVH+4,X'DF'                                                  
         NI    RISAGYH+4,X'DF'                                                  
         NI    RISSLSH+4,X'DF'                                                  
         NI    RISMNTSH+4,X'DF'                                                 
         NI    RISDATEH+4,X'DF'                                                 
         NI    RISFILTH+4,X'DF'                                                 
         LA    R2,RISSTAH                                                       
         B     EXIT                                                             
         SPACE 2                                                                
CLRLINE  SR    R8,R8                                                            
         IC    R8,0(R2)                                                         
         SH    R8,=H'9'                                                         
         EX    R8,XCFLD                                                         
         BR    RE                                                               
         SPACE 2                                                                
ORFLD    OC    8(0,R2),8(R2)                                                    
*                                                                               
XCFLD    XC    8(0,R2),8(R2)                                                    
         EJECT                                                                  
* INPUT=YYMMDD; OUTPUT=COMPRESSED DATE (2 BYTE) FOR MONDAY OF THAT WEEK         
*                                                                               
GETMDAY  NTR1  BASE=BASERB                                                      
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                NOTE R9                               
         L     R7,0(R1)            INPUT                                        
         L     R8,4(R1)            OUTPUT                                       
         GOTO1 VGETDAY,DMCB,(R7),FULL                                           
         CLC   FULL(3),=CL3' '     TEST ERROR                                   
         LA    R3,DATERR                                                        
         LA    R2,RISDATE                                                       
         BE    ERROR                                                            
*                                                                               
         SR    R5,R5                                                            
         IC    R5,DMCB                                                          
         BCTR  R5,0                                                             
         LNR   R5,R5                                                            
         CLI   DAILYPAC,C'Y'       DAILY PACING?                                
         BNE   GETM0020            NO                                           
         SR    R5,R5               YES - ZERO DAY ADJUSTMENT                    
GETM0020 EQU   *                                                                
         ST    R5,FULL                                                          
         GOTO1 VADDAY,DMCB,(R7),DUB,(R5)                                        
         GOTO1 VDATCON,DMCB,DUB,(2,(R8))                                        
         B     EXXMOD                                                           
         EJECT                                                                  
LISTER   CLI   LISTBYTE,LISTPNAM      IS IT PROD LIST READ?                     
         BE    LISTER2                                                          
         CLI   LISTBYTE,LISTMKG       IS IT MAKEGOOD LIST?                      
         BE    LISTERMG                                                         
         CLI   NEXTBYTE,2             ARE WE IN MIDDLE OF PROD LIST?            
         BE    LISTER2                                                          
         CLI   NEXTBYTE,3             ARE WE IN MIDDLE OF LISTO LIST?           
         BE    LISTERMG                                                         
                                                                                
         GOTO1 VCALLOV,DMCB,(5,0),(RA)                                          
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         L     R4,FULL                                                          
         GOTO1 (RF),DMCB,(RC),(R4)                                              
         B     EXXMOD                                                           
*                                                                               
* PRODUCT LIST READS X'9D' KEY                                                  
LISTER2  DS    0H                                                               
*                                  BUILD GRID                                   
         LA    RE,GRID             CLEAR FIRST                                  
         LA    RF,L'GRID-1                                                      
         XCEF                                                                   
                                                                                
***      MVC   HALF,TBLBGN                                                      
         MVC   HALF,MOSTRT                                                      
         LA    RE,GRID                                                          
*                                                                               
LSTGRD10 MVC   0(2,RE),HALF                                                     
         SR    RF,RF                                                            
         IC    RF,HALF+1           UPDATE MONTH                                 
         LA    RF,1(RF)                                                         
         STC   RF,HALF+1                                                        
         CH    RF,=H'13'                                                        
         BL    LSTGRD20                                                         
         IC    RF,HALF             INCREMENT YEAR                               
         LA    RF,1(RF)                                                         
         STC   RF,HALF                                                          
         MVI   HALF+1,1            RESET MONTH                                  
*                                                                               
LSTGRD20 LA    RE,12(RE)                                                        
***      CLC   TBLEND(2),HALF                                                   
         CLC   MOSEND,HALF                                                      
         BNL   LSTGRD10                                                         
         MVI   0(RE),X'FF'         MARK END OF TABLE                            
*                                                                               
* NOW COPY GRID -> GRID2                                                        
         MVC   GRID2,GRID                                                       
         LA    RE,GRID2            PREPARE GRID2 FOR PRIOR                      
LSTGRD22 IC    RF,0(RE)            GET YEAR                                     
         BCTR  RF,0                -1                                           
         STC   RF,0(RE)            SET NEW YEAR                                 
         LA    RE,12(RE)                                                        
         CLI   0(RE),X'FF'                                                      
         BNE   LSTGRD22                                                         
*                                                                               
         GOTO1 VCALLOV,DMCB,(6,0),(RA)                                          
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         L     R4,FULL                                                          
         GOTO1 (RF),DMCB,(RC),(R4)                                              
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
* MAKEGOOD LIST READS 'A0' KEYS                                                 
*                                                                               
LISTERMG DS    0H                                                               
*                                                                               
         GOTO1 VCALLOV,DMCB,(7,0),(RA)                                          
*                                                                               
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,DMCB                                                          
         L     R4,FULL                                                          
         GOTO1 (RF),DMCB,(RC),(R4)                                              
*                                                                               
         B     EXXMOD                                                           
*                                                                               
         EJECT                                                                  
       ++INCLUDE REGENINT                                                       
         EJECT                                                                  
* -                                                                             
CHKGLOB  NTR1                                                                   
* CHECK FOR GLOBBER LOADER VARIABLES - USE MYP                                  
*                                                                               
         L     R2,ACOMFACS                                                      
         USING COMFACSD,R2                                                      
                                                                                
         GOTO1 CGLOBBER,DMCB,=C'GETD',GLOBSV,24,GLVXCTL   XCTL ELEM?            
         TM    DMCB+8,X'10'        NO VARIABLES FOUND, SKIP                     
         BNZ   CKGLOBX                                                          
         GOTO1 (RF),DMCB,=C'DELE',,,GLVXCTL                                     
*                                                                               
         GOTO1 CGLOBBER,DMCB,=C'GETD',MYP,50,GLRKEY                             
         TM    DMCB+8,X'10'        NO VARIABLES FOUND, SKIP                     
         BNZ   CKGL150                                                          
         GOTO1 (RF),DMCB,=C'DELE',,,GLRKEY                                      
         DROP  R2                                                               
*                                                                               
*                                                                               
         LA    R2,MYP                                                           
         LA    R3,MYP                                                           
         BAS   R5,GETCOMMA                                                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   RISSTA(0),0(R3)                                                  
         LA    R1,1(R1)                                                         
         STC   R1,RISSTAH+5        SET INPUT LENGTH                             
         OI    RISSTAH+6,X'80'     FOUT                                         
                                                                                
         LA    R2,2(R2)            BUMP OVER COMMA TO NXT FIELD                 
         LR    R3,R2               SET R3 TO START OF NEXT FIELD                
         BAS   R5,GETCOMMA                                                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   RISOFF(0),0(R3)                                                  
         LA    R1,1(R1)                                                         
         STC   R1,RISOFFH+5                                                     
         OI    RISSTAH+6,X'80'                                                  
                                                                                
         LA    R2,2(R2)                                                         
         LR    R3,R2                                                            
         BAS   R5,GETCOMMA                                                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   RISAGY(0),0(R3)                                                  
         LA    R1,1(R1)                                                         
         STC   R1,RISAGYH+5                                                     
         OI    RISAGYH+6,X'80'                                                  
                                                                                
         LA    R2,2(R2)                                                         
         LR    R3,R2                                                            
         BAS   R5,GETCOMMA                                                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   RISADV(0),0(R3)                                                  
         LA    R1,1(R1)                                                         
         STC   R1,RISADVH+5                                                     
         OI    RISADVH+6,X'80'                                                  
                                                                                
         LA    R2,2(R2)                                                         
         LR    R3,R2                                                            
         BAS   R5,GETCOMMA                                                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   RISMNTS(0),0(R3)                                                 
         OI    RISMNTSH+6,X'80'                                                 
         LA    R1,1(R1)                                                         
         STC   R1,RISMNTSH+5                                                    
         OI    RISMNTSH+6,X'80'                                                 
                                                                                
         LA    R2,2(R2)                                                         
         LR    R3,R2                                                            
         MVC   RISDATE(5),0(R3)    HARD FOR NOW                                 
         MVI   RISDATEH+5,5        PASSING 'LISTD' ONLY                         
         OI    RISDATEH+6,X'80'                                                 
*                                                                               
CKGL150  DS    0H                                                               
*                                                                               
*   TEST                                                                        
         LR    RF,RA               SET DISPLACEMENT IN TWA                      
         A     RF,=A(CURSDISD)                                                  
         LH    RE,0(RF)                                                         
         LTR   RE,RE               ANY VALUE IN DISPLACEMENT?                   
         BZ    CKGL180             NO                                           
         LR    RF,RA               RESET DISPLACEMENT IN TWA                    
         AR    RF,RE               CALCULATE CURSOR POSITION                    
         OI    6(RF),X'40'         SET CURSOR TO THIS HEADER                    
         LA    R0,0                                                             
*   TEST END                                                                    
*                                                                               
CKGL180  DS    0H                                                               
         LA    R2,RISMESSH                                                      
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                 POINT TO SERVICE REQUEST FIELD             
         MVC   8(4,R2),=C'=RE '      FORCE SCREEN REFRESH                       
         MVI   5(R2),4                                                          
         OI    6(R2),X'80'                                                      
         L     RD,4(RD)              POP BACK TWICE                             
         B     CKGLOBX                                                          
*                                                                               
CKGLOBX  XIT1                                                                   
*                                                                               
GETCOMMA DS    0H                                                               
         SR    R1,R1                                                            
COMMA10  CLI   1(R2),C','                                                       
         BER   R5                                                               
         LA    R2,1(R2)                                                         
         LA    R1,1(R1)                                                         
         B     COMMA10                                                          
         EJECT                                                                  
* THIS ROUTINE USED FOR LIST$  **** HARD CODED ***                              
* BUMP DOWN SCREEN OUT LINES TO DETERMINE IF CURSOR ON VALID LINE               
* RETURN SCREEN LINE ADDR IN FULL                                               
* R3 USED FOR CURSOR ADDR  R4 SCREEN OUT LINE ADDR                              
                                                                                
CHKCURSR NTR1                                                                   
         L     R2,ATIOB                                                         
         USING TIOBD,R2                                                         
         LH    R3,TIOBCURD         PICK UP RELATIVE DISPLACEMENT                
         AR    R3,RA               RA=TWA                                       
         LA    R4,RISOUTH          FIRST OUT LINE ON SCREEN                     
         CR    R3,R4                                                            
         BL    CCNO                CURSOR CAN'T BE LESS THAN 1ST LINE           
         BE    CC20                FOUND                                        
         BH    CC15                GET NEXT LINE                                
*                                                                               
CC10     CR    R3,R4                                                            
         BE    CC20                                                             
         BL    CCYES                                                            
         BH    CC15                                                             
                                                                                
CC15     ST    R4,FULL             STORE ADDR OF OUT LINE                       
         ZIC   R1,0(R4)                                                         
         C     R1,=F'9'            END OF SCREEN                                
         BNH   CCNO                                                             
         AR    R4,R1               NO/BUMP TO NEXT SCREEN OUTLINE               
* NEEDED TO REMOVE CHECKS FOR BLANK LINE BELOW SINCE LISTP LEAVES               
* A BLANK LINE BETWEEN CHANGE OF STATIONS IN THE LIST                           
*                                                                               
***      CLI   8(R4),X'40'      NO DATA = PASSED VALID OUTLINES                 
***      BNH   CCNO                                                             
         B     CC10                                                             
                                                                                
* - IF CURSOR POINTS TO START OF LINE                                           
CC20     ST    R4,FULL             PASS SCREEN LINE ADDR IN FULL                
*                                                                               
CCYES    EQU   *                                                                
         L     RE,FULL             SET A(SCREEN LINE POSITION)                  
         LA    R3,T80DFFD                                                       
         SR    RE,R3               CALCULATE DISPLACEMENT                       
         LR    RF,RA               SET DISPLACEMENT IN TWA                      
         A     RF,=A(CURSDISD)                                                  
         STH   RE,0(RF)            SAVE CURSOR DISPLACEMENT IN TWA              
*                                                                               
         SR    R3,R3                                                            
CCNO     LTR   R3,R3                                                            
CCX      XIT1                                                                   
         DROP  R2                                                               
                                                                                
                                                                                
                                                                                
* THIS ROUTINE USED FOR LISTD  ****HARD CODED****                               
* BUMP DOWN SCREEN OUT LINES TO DETERMINE IF CURSOR ON VALID LINE               
* RETURN SCREEN LINE ADDR IN FULL                                               
* R3 USED FOR CURSOR ADDR  R4 SCREEN OUT LINE ADDR                              
                                                                                
CHKCURS2 NTR1                                                                   
         L     R2,ATIOB                                                         
         USING TIOBD,R2                                                         
         LR    RF,RA               SET DISPLACEMENT IN TWA                      
         A     RF,=A(CURSDISD)                                                  
         MVC   0(2,RF),TIOBCURD      SAVE CURSOR DISPLACEMENT IN TWA            
*                                                                               
         LH    R3,TIOBCURD         PICK UP RELATIVE DISPLACEMENT                
         AR    R3,RA               RA=TWA                                       
         LA    R4,RISBOTMH         LAST OUT LINE ON SCRREN                      
         CR    R3,R4                                                            
         BNL   CCNO                CURSOR > THAN LAST OUT LINE                  
         LA    R4,RISOUTH          FIRST OUT LINE ON SCRREN                     
         CR    R3,R4                                                            
         BL    CCNO                                                             
                                                                                
CHKC10   LR    R5,R4               GET NEXT SET OF LINES INTO R5                
         ZIC   R1,0(R4)                                                         
         AR    R5,R1               R5 POINTS TO 2ND OUT LINE OF LISTD           
         ZIC   R1,0(R5)                                                         
         AR    R5,R1               R5 POINTS TO BLANK  BETWEEN SETS             
         ZIC   R1,0(R5)                                                         
         AR    R5,R1               R5 POINTS TO NEXT SET                        
                                                                                
         CR    R3,R5               IF CURSOR < NEXT SET OF OUT LINES            
         BL    CC20                OK-R4 HAS START OF PREVIOUS OUTLINES         
         LR    R4,R5               PUT CURRENT START INTO R4                    
         B     CHKC10                                                           
                                                                                
         DROP  R2                                                               
*                                                                               
CURSERR  DS    0H                                                               
         XC    RISMESS,RISMESS                                                  
         MVC   RISMESS(L'MSG2),MSG2                                             
         FOUT  RISMESSH                                                         
         B     EXIT                                                             
         EJECT                                                                  
* IF WE CAME FROM SELLER'S WORKSHEET, RETURN                                    
*                                                                               
SELJUMP  NTR1                                                                   
**       L     R5,ACOMFACS                                                      
**       USING COMFACSD,R5                                                      
**       LA    R1,GLOBSV                                                        
**       USING GLVXFRSY                                                         
**       CLC   =C'REP',GLVXFRSY    IF FROM SELLERS WORKSHEET                    
**       BNE   SELX                                                             
**       CLC   =C'SEL',GLVXFRPR                                                 
**       BNE   SELX                                                             
**       XC    GLOBSV(14),GLOBSV   RETURN THERE                                 
**       MVC   GLVXFRSY,=C'REP'                                                 
**       MVC   GLVXFRPR,=C'RIS'                                                 
**       MVC   GLVXTOSY,=C'REP'                                                 
**       MVC   GLVXTOPR,=C'SEL'                                                 
**       OI    GLVXFLG1,GLV1RETN+GLV1SEPS                                       
**       DROP  R1                                                               
**       GOTO1 CGLOBBER,DMCB,=C'PUTD',GLOBSV,14,GLVXCTL                         
SELX     XIT1                                                                   
         EJECT                                                                  
* GLOBBER PASSES DATA TO CONTRACT PROGRAM                                       
* INPUT ADDRESS OF SCREEN LINE IN FULL                                          
*                                                                               
SWAP     NTR1                                                                   
         L     R5,ACOMFACS                                                      
         USING COMFACSD,R5                                                      
         L     R2,FULL             ADDR OF OUT LINE                             
*---------------------*                                                         
* GET CONTRACT NUMBER *                                                         
*---------------------*                                                         
         CLI   PFJUMP,1            LIST$                                        
         BNE   SWP10                                                            
         USING LINE2,R2                                                         
*                                                                               
         MVC   WORK(8),L2CON                                                    
         BAS   R1,CHKNUM                                                        
         LA    R2,L2CON                                                         
         B     SWP15                                                            
                                                                                
SWP10    CLI   PFJUMP,2            LISTD                                        
         BNE   SWPLSTO                                                          
         USING LINE3A,R2                                                        
*                                                                               
         MVC   WORK(8),L3CNT                                                    
         BAS   R1,CHKNUM                                                        
         LA    R2,L3CNT                                                         
         B     SWP15                                                            
                                                                                
SWPLSTO  CLI   PFJUMP,4            LISTO                                        
         BNE   SWPLSTOX                                                         
         USING LSMKGD,R2                                                        
*                                                                               
         MVC   WORK(8),LSMKGCON                                                 
         BAS   R1,CHKNUM                                                        
         LA    R2,LSMKGCON                                                      
         B     SWP15                                                            
*                                                                               
SWPLSTOX DS    0H                                                               
*                                                                               
SWP12    CLI   PFJUMP,3            LISTP                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PLPNMD,R2                                                        
*                                                                               
         MVC   WORK(8),PLPNCON                                                  
         BAS   R1,CHKNUM                                                        
         LA    R2,PLPNCON                                                       
         B     SWP15                                                            
         DROP  R2                                                               
                                                                                
********************************************                                    
* BUG CATCHER                                                                   
CHKNUM   LA    RE,WORK                                                          
         LA    RF,8                                                             
***      CLI   PFJUMP,3                                                         
***      BNE   CHKNUM0                                                          
***      LA    RF,8                                                             
CHKNUM0  CLI   0(RE),X'40'                                                      
         BE    CHKNUM3                                                          
         CLI   0(RE),X'F0'                                                      
         BL    CURSERR                                                          
         CLI   0(RE),X'F9'                                                      
         BH    CURSERR                                                          
CHKNUM3  LA    RE,1(RE)                                                         
         BCT   RF,CHKNUM0                                                       
         BR    R1                                                               
***********************************************                                 
                                                                                
SWP15    DS    0H                                                               
*                                                                               
         CLI   PFJUMP,4            SKIP IF NOT LISTO                            
         BNE   SWP15A                                                           
*                                                                               
         CLI   PFKEY,6             IF SWITCHING TO DARE                         
         BNE   *+20                                                             
         L     R2,FULL                POINT TO LIST LINE                        
         BRAS  RE,GODARE              GO DO IT                                  
         BNZ   EXIT                      ERROR                                  
*                                                                               
         B     STGX                                                             
*                                                                               
SWP15A   DS    0H                                                               
*                                                                               
         XC    WORK,WORK                                                        
         LA    RE,WORK                                                          
         USING GLCONNUM,RE                                                      
         LA    RF,7                ALL CON#S ARE NOW 8 CHARS                    
***      CLI   PFJUMP,3                                                         
***      BNE   *+8                                                              
***      LA    RF,7                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   GLCONNUM(0),0(R2)                                                
*                                                                               
         CLI   PFKEY,2             DISPLAY?                                     
         BNE   SWP16               NO                                           
         MVC   GLCONCA(3),=C'DIS'                                               
         MVC   GLCONBA(3),=C'DSM'                                               
         B     SWP30                                                            
*                                                                               
SWP16    DS    0H                                                               
         CLI   PFKEY,3             CHANGE?                                      
         BNE   SWP17               NO                                           
         MVC   GLCONCA(3),=C'CHA'                                               
         B     SWP30                                                            
*                      ABOB ADDED                                               
SWP17    DS    0H                                                               
         CLI   PFKEY,5             DISPLAY?                                     
         BNE   SWP18               NO                                           
         CLI   PFJUMP,4            LISTO                                        
         BE    SWP17A                                                           
         MVC   GLCONCA(3),=C'DIS'                                               
         MVC   GLCONBA(3),=C'MGL'                                               
         B     SWP30                                                            
*                                                                               
SWP17A   DS    0H                                                               
*                                                                               
         L     R2,FULL             POINT TO LIST LINE                           
         USING LSMKGD,R2                                                        
*                                                                               
         MVC   GLCONCA(3),=C'MGL'                                               
         MVI   GLCONCA+3,C','                                                   
         MVC   GLCONCA+4(2),LSMKGOFR                                            
         B     SWP30                                                            
*                                                                               
SWP18    DS    0H                                                               
         DC    H'0'                HOW DID WE GET HERE?                         
         DROP  RE                                                               
*                                                                               
SWP30    DS    0H                                                               
         GOTO1 CGLOBBER,DMCB,=C'PUTD',WORK,GLCONLNQ,GLRKACT                     
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
*                                                                               
*        LA    R2,RISMESSH                                                      
*        ZIC   R1,0(R2)            BUMP TO SERVICE REQUEST FIELD                
*        AR    R2,R1                                                            
*        OI    6(R2),X'C0'         XMIT FIELD                                   
*        ZIC   R1,0(R2)                                                         
*        S     R1,=F'8'            GET LENGTH OF FIELD                          
*        BCTR  R1,0                                                             
*        EX    R1,*+8                                                           
*        B     *+10                                                             
*        XC    8(0,R2),8(R2)                                                    
*        MVC   8(10,R2),=C'+C,SV     '                                          
*                                                                               
         XC    WORK,WORK                                                        
         LA    R1,WORK                                                          
         USING GLVXFRSY,R1                                                      
         MVC   GLVXFRSY,=C'REP'    FROM THE REP SYSTEM                          
         MVC   GLVXFRPR,=C'RIS'    RIS PROGRAM                                  
         MVC   GLVXTOSY,=C'REP'    TO THE REP SYSTEM                            
         MVC   GLVXTOPR,=C'CON'    CONTRACT PROGRAM                             
***>>>   OI    GLVXFLG1,GLV1GOTO+GLV1SEPS   CALL BASE ON TRANSFER               
         OI    GLVXFLG1,GLV1SEPS   CALL BASE ON TRANSFER                        
         DROP  R1                                                               
*                                  SET UP THE TRANSFER CONTROL BLOCK            
         GOTO1 CGLOBBER,DMCB,=C'PUTD',WORK,14,GLVXCTL                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
STGX     XIT1                                                                   
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
MYERR    DS    0H    **ERROR ROUTINE FOR ERR MSGS > #255**                      
         L     RF,ACOMFACS                                                      
         USING COMFACSD,RF                                                      
         L     RF,CGETTXT                                                       
         DROP  RF                                                               
         GOTO1 (RF),DMCB+12,(R3),0,(C'E',DMCB),0,0,0                            
         LTR   R3,R3               IF DATAMGR ERROR, DON'T SET CURPOS           
         BZ    EXIT                                                             
         OI    6(R2),X'40'                                                      
         B     EXIT                                                             
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*                                                                               
*        GLOBBER TO DARE PROGRAM                                                
*                                                                               
GODARE   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R4,ACOMFACS         ESTABLISH CONFACS                            
         USING COMFACSD,R4                                                      
*                                                                               
         L     R2,FULL             POINT TO LIST LINE                           
         USING LSMKGD,R2                                                        
*                                                                               
         CLC   LSMKGDRS,=CL8' '    ERROR IF NOT DARE OFFER                      
         BNH   GODAREER                                                         
*                                                                               
*        SEND CONTRACT NUMBER                                                   
*                                                                               
         GOTO1 CGLOBBER,DMCB,=C'PUTD',LSMKGCON,L'LSMKGCON,GLRCONNO              
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*        SEND GROUP CODE                                                        
*                                                                               
         GOTO1 CGLOBBER,DMCB,=C'PUTD',LSMKGOFR,L'LSMKGOFR,GLRSTAT               
*                                                                               
*        SEND TRANSFER BLOCK                                                    
*                                                                               
         XC    WORK,WORK                                                        
         LA    R1,WORK                                                          
         USING GLVXFRSY,R1                                                      
         MVC   GLVXFRSY,=C'REP'    FROM THE REP SYSTEM                          
         MVC   GLVXFRPR,=C'RIS'    RIS PROGRAM                                  
         MVC   GLVXTOSY,=C'REP'    TO THE REP SYSTEM                            
         MVC   GLVXTOPR,=C'DAR'    DARE     PROGRAM                             
         OI    GLVXFLG1,GLV1SEPS   CALL BASE ON TRANSFER                        
*                                                                               
         DROP  R1                                                               
*                                  SET UP THE TRANSFER CONTROL BLOCK            
         GOTO1 CGLOBBER,DMCB,=C'PUTD',WORK,14,GLVXCTL                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
GODAREX  DS    0H                                                               
         CR    RB,RB               SET EQ CC                                    
         XIT1                                                                   
*                                                                               
GODAREER DS    0H                                                               
         XC    RISMESS,RISMESS     INIT MESSAGE ERROR                           
         MVC   RISMESS(L'DARERR),DARERR SET ERROR MESSAGE                       
         LTR   RB,RB               SET NE CC                                    
         XIT1                                                                   
DARERR   DC    C'SELECTED OFFER IS NOT A DARE OFFER'                            
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*                                                                               
MSG1     DC    C'ACTION COMPLETED - ENTER NEXT REQUEST'                         
MSG2     DC    C'CURSOR NOT POINTING TO CONTRACT LINE '                         
TITLE1   DC    C' PERIOD  PRIOR ESTIMATE   PRIOR ACTUAL   CURRENT EST'          
TITLE2   DC    C' PERIOD    PRIOR BOOKED   PRIOR ACTUAL    CUR BOOKED'          
TITLE3   DC    C' PERIOD   PRIOR INVOICE   PRIOR ACTUAL   CUR INVOICE'          
         SPACE 2                                                                
*                                                                               
       ++INCLUDE RGENEROL                                                       
* LITERAL POOL                                                                  
         LTORG                                                                  
       ++INCLUDE RERISWRK                                                       
       ++INCLUDE FATIOB                                                         
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE REGLBRW                                                        
       ++INCLUDE REGLCON                                                        
       ++INCLUDE REPIOBLK                                                       
       ++INCLUDE DMPRTQL                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'046RERIS00   11/04/13'                                      
         END                                                                    
