*          DATA SET ACREPRA02  AT LEVEL 004 AS OF 09/10/20                      
*PHASE ACRA02C,+0                                                               
         TITLE 'INTERNAL TRANSACTION AGING REPORT'                              
*---------------------------------------------------------------------*         
* USR  LVL DATE    TICKET     DESCRIPTION                             *         
* ---  --- ----    ------     -----------                             *         
* RKEJ 004 26AUG20 SPEC-48399 FIX TO DISPLAY 2 CHAR ACC FILE NAMES,   *         
*                             ALPHA ID ON HEADER AND SUPPORT 2 CHAR   *         
*                             FILE NAMES ON PARM= CARD THRU JCL       *         
*---------------------------------------------------------------------*         
*---------------------------------------------------------------------*         
* LATEST CHANGES:- THIS MODULE NOW SUPPORTS 1 OR 2 CHAR FILE NAMES IN *         
*                  THE PARM= JCL CARD.                                *         
*                                                                     *         
* PARM=1 2 N14 --> PARM CARD CAN HAVE MAX OF 8 CHARS WITH 4 FILES     *         
*                  1 CHAR FILENAME SHOULD HAVE THE 2ND CHAR AS BLANK  *         
*                  2 CHAR FILENAME SHOULD HAVE 2 CHAR NAME            *         
*                  E.G. ABOVE -> ACC1, ACC2, ACCN1, ACC4 INCLUDED     *         
* PARM=*1 2 N1 --> PARM CARD CAN HAVE MAX OF 8 CHARS WITH 3 FILES WHEN*         
*                  1ST CHAR IS A * THAT DENOTES EXCLUSION OF THE FILES*         
*                  E.G. ABOVE -> ACC1, ACC2, ACCN1 EXCLUDED           *         
*---------------------------------------------------------------------*         
ACRA02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACRA**,R9       R9=2ND BASE REGISTER                         
         L     RA,0(R1)                                                         
         USING ACWORKD,RA          RA=A(GLOBAL W/S)                             
         LA    RC,SPACEND                                                       
         USING LWSD,RC                                                          
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
XIT      XIT   1                                                                
         EJECT                                                                  
***********************************************************************         
* RUN FIRST                                                           *         
***********************************************************************         
RUNF     MVI   FCSUPOFC,C'Y'                                                    
         XC    DATE1,DATE1                                                      
*                                                                               
         L     R2,VEXTRAS                                                       
         USING RUNXTRAD,R2         GET A(ISDDS)                                 
         MVC   ISDDS,AISDDS-RUNXTRAD(R2)                                        
         L     R2,AMONACC                                                       
         USING ACMD,R2                                                          
         MVC   ADDCB,ACMADDIR      GET A(ACCDIR DCB)                            
*                                                                               
         L     R2,ADMASTC                                                       
         USING MASTD,R2                                                         
         MVC   ACCSE#,MCIDSENO     GET SE NUMBER                                
         L     R1,MCUTL                                                         
         ST    R1,AUTL             A(UTL)                                       
         MVC   SVSE,4(R1)          SAVE CURRENT SE                              
*                                                                               
         L     R1,MCBXAREA                                                      
         ST    R1,ADBOX                                                         
*                                                                               
         L     R1,=A(BOXRC)        SET UP BOX ROUTINE                           
         ST    RC,0(R1)                                                         
         L     R1,=A(BXHOOK)                                                    
         ST    R1,HEADHOOK                                                      
*                                                                               
         LA    R2,DKEY                                                          
         USING CTWREC,R2           READ SYSTEM LIST RECORD (FOR FILE)           
         XC    CTWKEY,CTWKEY                                                    
         MVI   CTWKTYP,CTWKTYPQ                                                 
         MVI   CTWKREC,CTWKRSYS                                                 
         MVI   CTWKSYSN,CTWKACC                                                 
         GOTO1 DATAMGR,DMCB,DMREAD,CTFILE,DKEY,AIO1                             
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                ERROR IN READING CONTROL FILE                
*                                                                               
         LA    R1,FILELST          KEEP LIST OF ACC FILES                       
         MVI   FILELST,X'FF'                                                    
         L     R2,AIO1                                                          
         LA    R2,CTWDATA                                                       
         USING SYSELD,R2           LOCATE SYSTEM ELEMENT FOR SE#                
RUNF3    CLI   SYSEL,0                                                          
         BE    RUNF13                                                           
         CLI   SYSEL,SYSELQ        TEST SYSTEM ELEMENT                          
         BNE   RUNF11                                                           
         CLI   SYSSYS,6            6 = ACCOUNT SYSTEM                           
         BNE   RUNF11                                                           
         CLC   ACCSE#,SYSSEN       SAME SE #?                                   
         BNE   RUNF5                                                            
         MVC   ACCFIL#,SYSNAME+3   SAVE  ACCFILE #                              
         B     RUNF11                                                           
*                                                                               
RUNF5    MVC   BYTE,RCFFPARM                                                    
         CLI   RCFFPARM,C' '       TEST FILE FILTERS                            
         BNH   RUNF9               NO FILTERS                                   
         LA    RF,RCFFPARM                                                      
         LA    R0,RCFFPARM+L'RCFFPARM-1                                         
         CLI   RCFFPARM,C'*'                                                    
         BNE   *+8                                                              
         LA    RF,1(RF)                                                         
*                                                                               
RUNF7    CLC   SYSNAME+3(2),0(RF)  MATCH FILTER ?                               
         BNE   RUNF8               NO, LOOK AT NEXT                             
         CLI   BYTE,C'*'           EXCLUDE ?                                    
         BE    RUNF11              YES, EXCLUDE THIS ONE                        
         B     RUNF9               ADD TO LIST                                  
*                                                                               
RUNF8    LA    RF,2(RF)                                                         
         CR    RF,R0               TEST PASSED RCFFPARM                         
         BH    RUNF11                                                           
         CLI   0(RF),C' '          NEXT FILTER                                  
         BH    RUNF7               YES, TRY AGAIN                               
         CLI   BYTE,C'*'           NO MATCH AND EXCLUDING ?                     
         BE    RUNF9               YES, TAKE IT                                 
         B     RUNF11              SKIP THIS FILE                               
                                                                                
*                                                                               
RUNF9    MVC   0(1,R1),SYSSEN      LIST OF ACC SE# & FILE #                     
         MVC   1(2,R1),SYSNAME+3                                                
         LA    R1,3(R1)                                                         
         MVI   0(R1),X'FF'                                                      
*                                                                               
RUNF11   LLC   R0,SYSLEN                                                        
         AR    R2,R0                                                            
         B     RUNF3                                                            
*                                  BUILD ISDDS PARAMTER LIST                    
RUNF13   LA    R1,ISREAD                                                        
         ST    R1,ISPARM1          PARM 1 A(READ)                               
         LA    R1,DIR                                                           
         ST    R1,ISPARM2          PARM 2 A(IO AREA)                            
         XC    ISPARM3,ISPARM3     PARM 3 NULLS                                 
         L     R1,ADDCB                                                         
         ST    R1,ISPARM4          PARM 4 A(DCB)                                
         LA    R1,DKEY                                                          
         ST    R1,ISPARM5          PARM 5 A(KEY ARGUMENT)                       
         L     R1,ATRKBUF                                                       
         ST    R1,ISPARM6          PARM 6 A(TRACK BUFFER)                       
         MVI   ISPARM6,X'FF'       FULL TRACK READ                              
*                                                                               
         BAS   RE,INIT             INITIALIZE START OF FILE                     
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* REQUEST FIRST                                                       *         
***********************************************************************         
REQF     CLI   INITSW,C'Y'         FIRST FOR NEXT FILE                          
         BNE   *+8                                                              
         BAS   RE,INIT             INITIALIZE FOR NEW FILE                      
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCREQREP,C'N'                                                    
         MVI   RCREQSUM,C'N'                                                    
         OC    DATE1,DATE1         ALREADY HAVE DATES ?                         
         BNZ   REQF11                                                           
*                                  BUILD DATE TABLE                             
         LA    RF,5                SET DEFAULT COLUMN WIDE TO 5 YEARS           
         CLC   QSELECT(2),SPACES                                                
         BE    REQF3                                                            
         SR    R1,R1                                                            
         CLI   QSELECT+2,C' '                                                   
         BE    *+8                                                              
         LA    R1,1                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,QSELECT(0)                                                   
         CVB   RF,DUB                                                           
*                                                                               
REQF3    LNR   RF,RF               SET YEARS TO DECREMENT                       
         ST    RF,FULL                                                          
         GOTO1 DATCON,DMCB,(4,RCDATE),(0,TODAY)                                 
         GOTO1 DATCON,DMCB,(0,TODAY),(1,DATE5)                                  
         MVC   WORK,TODAY                                                       
         LA    R0,4                                                             
         LA    R3,DATE4                                                         
*                                                                               
REQF5    MVC   DMCB+8(4),FULL                                                   
         GOTO1 ADDAY,DMCB,(C'Y',WORK),WORK+6                                    
         GOTO1 DATCON,DMCB,(0,WORK+6),(1,0(R3))                                 
         MVC   WORK(6),WORK+6                                                   
         SHI   R3,L'DATE1                                                       
         BCT   R0,REQF5                                                         
*                                                                               
         MVC   HED1,SPACES         BUILD HEADLINES                              
         MVC   HED2,SPACES                                                      
         MVC   HED3,SPACES                                                      
*                                                                               
         LA    R5,HED1                                                          
         USING PLD,R5                                                           
         MVC   PTOT+4(15),=C' TOTAL #       '                                   
         LA    R5,HED2                                                          
         MVC   PTOT+4(15),=C'TRNS PER   VERT'                                   
         LA    R5,HED3                                                          
         MVC   PTOT+4(15),=C' LEDGER       %'                                   
*                                                                               
         LA    R5,HED2                                                          
         LA    R6,HED3                                                          
         GOTO1 DATCON,DMCB,(1,DATE1),(6,PCOL1+2)                                
         MVC   PCOL1+9(7),=C'&& PRIOR'                                          
         MVI   PCOL1+11-PLD(R6),C'#'                                            
         MVI   PCOL1+15-PLD(R6),C'%'                                            
*                                                                               
         LA    R0,4                SET DATE RANGED MMM/YY-MMM/YY                
         LA    R2,DATE1                                                         
         LA    R3,PCOL2                                                         
         LA    R6,132(R3)                                                       
*                                                                               
REQF7    GOTO1 DATCON,DMCB,(1,0(R2)),(0,WORK)                                   
         GOTO1 ADDAY,DMCB,(C'M',WORK),WORK+6,F'1'                               
         GOTO1 DATCON,DMCB,(0,WORK+6),(6,3(R3))                                 
         MVI   9(R3),C'-'                                                       
         GOTO1 DATCON,DMCB,(1,3(R2)),(6,10(R3))                                 
         MVI   11(R6),C'#'                                                      
         MVI   15(R6),C'%'                                                      
         LA    R2,L'DATE2(R2)                                                   
         LA    R3,PBXLNQ(R3)                                                    
         LA    R6,PBXLNQ(R6)                                                    
         BCT   R0,REQF7                                                         
         DROP  R5                                                               
*                                                                               
         CLI   QOPT2,C'D'          CREATE DOWNLOAD FILE ?                       
         BNE   REQF11                                                           
         MVI   QOPT3,C'N'          SUPPRESS SUMMARY                             
         OPEN  (AGEFIL,(OUTPUT))                                                
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         OPEN  (IDFIL,(OUTPUT))                                                 
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   OUTREC,SPACES                                                    
         LA    R3,OUTREC                                                        
         MVC   0(6,R3),=C'ACCESS'                                               
         MVI   6(R3),TABCHRQ                                                    
         LA    R3,7(R3)                                                         
         MVC   0(4,R3),=C'FILE'    DOWNLOAD HEADLINES                           
         MVI   4(R3),TABCHRQ                                                    
         LA    R3,5(R3)                                                         
         MVC   0(6,R3),=C'AGENCY'                                               
         MVI   6(R3),TABCHRQ                                                    
         LA    R3,7(R3)                                                         
         MVC   0(6,R3),=C'LEDGER'                                               
         MVI   6(R3),TABCHRQ                                                    
         LA    R3,7(R3)                                                         
         LA    R0,5                                                             
         LA    RE,HED2+(PCOL1-PLD)                                              
*                                                                               
REQF9    MVC   0(L'PCOL1,R3),0(RE)                                              
         MVI   L'PCOL1+1(R3),TABCHRQ                                            
         LA    R3,L'PCOL1+2(R3)                                                 
         LA    RE,L'PCOL1+1(RE)                                                 
         BCT   R0,REQF9                                                         
         MVC   0(5,R3),=C'TOTAL'                                                
         MVI   6(R3),TABCHRQ                                                    
                                                                                
         GOTO1 ADSQUASH,DMCB,OUTREC,L'OUTREC                                    
         PUT   AGEFIL,OUTREC                                                    
*                                                                               
REQF11   L     RF,ALDGTOT          -TOP OF LEDGER TABLE                         
         MVI   0(RF),X'FF'                                                      
         ST    RF,ACURLDG                                                       
*                                                                               
         BAS   RE,GETID            GET COMPANY ID INFO                          
*                                                                               
         L     R4,ACURCPY          CLEAR COMPANY TOTALS                         
         USING LDGRD,R4                                                         
         LA    R4,LDGRLNQ(R4)                                                   
         ST    R4,ACURCPY                                                       
         L     R6,ACURCMP                                                       
         USING CMPD,R6                                                          
         MVC   LDGRCPY,CMPHEX                                                   
         MVC   LDGRUL,SPACES                                                    
         LA    R1,LDGRCOL                                                       
         LA    R0,LDGRCLN                                                       
         ZAP   0(L'LDGRCOL,R1),PZERO                                            
         LA    R1,L'LDGRCOL(R1)                                                 
         BCT   R0,*-10                                                          
*                                                                               
         LA    R4,LDGRLNQ(R4)      BUMP TO NEXT TABLE ENTRY                     
         MVI   0(R4),X'FF'         SET EOT                                      
*                                                                               
         BAS   RE,READ             READ DIRECTORY                               
         MVI   RCSUBPRG,0                                                       
         BAS   RE,RPT              PRINT OUT REPORT                             
*                                                                               
         B     XIT                                                              
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
* RUN LAST                                                            *         
***********************************************************************         
RUNL     MVI   RCSUBPRG,1          SUMMARY REPORT                               
         MVI   FORCEHED,C'Y'                                                    
         CLI   QOPT3,C'S'          FILE SUMMARY                                 
         BNE   *+8                                                              
         BAS   RE,RPT                                                           
*                                                                               
         LA    R1,FILELST                                                       
*                                                                               
RUNL3    CLI   0(R1),X'FF'         TEST END OF LIST                             
         BE    RUNL7               YES                                          
         CLI   0(R1),0             TEST ALREADY PROCESSED                       
         BNE   *+12                NO                                           
         LA    R1,3(R1)                                                         
         B     RUNL3                                                            
*                                                                               
         MVC   ACCSE#,0(R1)        SET ACC SE#                                  
         MVC   ACCFIL#,1(R1)       ACCFILE #                                    
         XC    0(3,R1),0(R1)                                                    
         L     RF,AUTL                                                          
         MVC   4(1,RF),ACCSE#                                                   
*                                                                               
         L     R2,AMONACC                                                       
         USING ACMD,R2                                                          
         GOTO1 DATAMGR,DMCB,OPEN,ACCOUNT,ACCFILEL                               
         GOTO1 DATAMGR,DMCB,ACMDMDTF,ACMACDIR                                   
         MVC   ADDCB,12(R1)                                                     
         L     R1,ADDCB                                                         
         ST    R1,ISPARM4                                                       
*                                                                               
         MVI   ACMMODE,REQFRST     RESET TO FIRST MODE                          
         MVI   RCCOMPFL,C' '       RESET COMPANY                                
         OI    ACMINDS3,ACMIMULT                                                
         MVI   INITSW,C'Y'         INITIALIZE FOR NEW FILE                      
         B     XIT                                                              
*                                                                               
RUNL7    L     RF,AUTL                                                          
         MVC   4(1,RF),SVSE                                                     
         CLI   QOPT2,C'D'          CREATE DOWNLOAD FILE                         
         BNE   XIT                                                              
         CLOSE (AGEFIL)                                                         
         CLOSE (IDFIL)                                                          
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* READ DIRECTORY - BUILD C/U/L TABLE                                  *         
***********************************************************************         
READ     NTR1  ,                                                                
         LA    R6,RUNTOTS          RUN TOTALS                                   
RUN      USING LDGRD,R6                                                         
*                                                                               
         L     R5,ACURCPY          COMPANY TOTAL                                
CPY      USING LDGRD,R5                                                         
*                                                                               
         L     R4,ACURLDG          LEDGER TOTALS                                
         USING LDGRD,R4                                                         
*                                                                               
         MVC   DKEY,SPACES                                                      
         LA    R2,DKEY                                                          
         USING CPYRECD,R2                                                       
         L     R8,ACURCMP                                                       
         USING CMPD,R8                                                          
         MVC   CPYKCPY,CMPHEX      START WITH COMPANY RECORD                    
*                                                                               
         LA    R1,ISREAD                                                        
         ST    R1,ISPARM1          READ                                         
         GOTO1 ISDDS,ISPARM1                                                    
         CLC   DKEY,DIR                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R1,ISRDSEQ                                                       
         ST    R1,ISPARM1          READSEQ                                      
*                                                                               
         LA    R2,DIR                                                           
READ3    GOTO1 ISDDS,ISPARM1                                                    
         CLC   CPYKCPY,CMPHEX      TEST SAME COMPANY RECORD                     
         BNE   XIT                                                              
         USING TRNRECD,R2                                                       
         CLI   TRNKDATE,C' '       TEST TRANSACTION                             
         BNH   READ3               MIGHT BE TRANSACTION KEY/RECORD              
         TM    TRNKSTA,TRNSDELT+TRNSDRFT+TRNSARCH                               
         BNZ   READ3                                                            
         TM    TRNKSTA2,TRNSPEEL   IGNORE PEELED ITEMS                          
         BNZ   READ3                                                            
         CLC   TRNKCPY(3),LDGRCPY  SAME CUL ?                                   
         BE    READ5                                                            
         CLI   LDGRCPY,X'FF'       FIRST TIME ?                                 
         BE    *+8                 YES                                          
         LA    R4,LDGRLNQ(R4)      BUILD NEW ENTRY                              
         ST    R4,ACURLDG                                                       
         MVC   LDGRCPY(3),TRNKCPY                                               
         MVI   LDGRCPY+LDGRLNQ,X'FF'    MARK END OF TABLE                       
         LA    R1,LDGRCOL                                                       
         LA    R0,LDGRCLN                                                       
         ZAP   0(L'LDGRCOL,R1),PZERO                                            
         LA    R1,L'LDGRCOL(R1)                                                 
         BCT   R0,*-10                                                          
*                                                                               
READ5    LA    R3,DATE1           POINT TO DATE TABLE                           
         LA    RF,LDGRCOL                                                       
         LA    RE,CPY.LDGRCOL                                                   
         LA    R7,RUN.LDGRCOL                                                   
         LA    R0,4                                                             
*                                                                               
READ7    CLC   TRNKSMOS,0(R3)      GET CORRECT COLUMN                           
         BNH   READ9                                                            
         LA    R3,L'DATE1(R3)                                                   
         LA    RF,L'LDGRCOL(RF)                                                 
         LA    RE,L'LDGRCOL(RE)                                                 
         LA    R7,L'LDGRCOL(R7)                                                 
         BCT   R0,READ7                                                         
*                                                                               
READ9    AP    0(L'LDGRCOL,RF),PONE COLUMN TOTAL                                
         AP    LDGRTOT,PONE         LEDGER TOTAL                                
         AP    0(L'LDGRCOL,RE),PONE                                             
         AP    CPY.LDGRTOT,PONE     COMPANY TOTAL                               
         AP    0(L'LDGRCOL,R7),PONE                                             
         AP    RUN.LDGRTOT,PONE     RUN TOTAL                                   
         B     READ3                                                            
         DROP  R2,R4,R8,RUN,CPY                                                 
         EJECT                                                                  
***********************************************************************         
* PRINT REPORT                                                        *         
***********************************************************************         
RPT      NTR1  ,                                                                
         LA    R5,P                                                             
         USING PLD,R5                                                           
*                                                                               
         L     R4,ALDGTOT          LEDGER TOTALS                                
         USING LDGRD,R4                                                         
         L     R6,ACURCPY          CURRENT COMPANT TOTALS                       
         CLI   RCSUBPRG,0          TEST LEDGER REPORT                           
         BE    *+12                YES                                          
         L     R4,ACPYTOT          COMPANY TOTALS                               
         LA    R6,RUNTOTS          RUN TOTALS                                   
*                                                                               
         ZAP   TOTAL,LDGRTOT-LDGRD(L'LDGRTOT,R6)                                
         CP    TOTAL,PZERO                                                      
         BE    XIT                                                              
                                                                                
RPT3     CLI   RCSUBPRG,0                                                       
         BNE   RPT5                                                             
         MVC   PUL,LDGRUL          UNIT/LEDGER                                  
         B     RPT9                                                             
*                                                                               
RPT5     L     RF,ACMPIDS          COMPANY ID'S                                 
         USING CMPD,RF                                                          
RPT6     CLI   LDGRCPY,X'FF'                                                    
         BNE   *+6                                                              
         DC    H'0'                MISSING COMPANY ENTRY                        
         CLC   LDGRCPY,CMPHEX                                                   
         BE    RPT7                                                             
         LA    RF,CMPLNQ(RF)                                                    
         B     RPT6                                                             
*                                                                               
RPT7     MVC   PDATA(2),CMPCODE    HEX CODE                                     
         MVC   PDATA+3(7),CMPLOGO  AND NAME                                     
         DROP  RF                                                               
*                                                                               
RPT9     BAS   RE,FROW             FORMAT A ROW                                 
         CLI   RCSUBPRG,0          DETAIL LINE ?                                
         BNE   PRT11                                                            
         CLI   QOPT2,C'D'          TEST DOWNLOAD                                
         BNE   PRT11                                                            
         BAS   RE,DWNL             DOWNLOAD A LINE                              
*                                                                               
PRT11    LA    R4,LDGRLNQ(R4)                                                   
         CLI   LDGRCPY,X'FF'       END OF TABLE ?                               
         BNE   RPT3                                                             
*                                                                               
         GOTO1 ACREPORT            SKIP A LINE                                  
*                                                                               
         L     R4,ACURCPY          COMPANY TOTALS                               
         CLI   RCSUBPRG,0                                                       
         BE    *+8                                                              
         LA    R4,RUNTOTS                                                       
         MVC   PDATA+2(6),=C'TOTALS'                                            
         BAS   RE,FROW             COMPANY/RUN TOTALS                           
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         DROP  R4,R5                                                            
         EJECT                                                                  
***********************************************************************         
* FORMAT A ROW                                                        *         
***********************************************************************         
         USING PLD,R5                                                           
         USING LDGRD,R4                                                         
FROW     ST    RE,SAVRE            PRINT A ROW                                  
         MVC   OUTREC,SPACES                                                    
         CP    LDGRTOT,PZERO       PRINT YEAR COLUMNS #/%                       
         BE    FROW5                                                            
         LA    R7,PCOL1                                                         
         LA    R3,LDGRCOL                                                       
         LA    R2,NCOL                                                          
*                                                                               
FROW3    ZAP   PACK12,0(L'LDGRCOL,R3)                                           
         SRP   PACK12,4,5                                                       
         DP    PACK12,LDGRTOT                                                   
         SRP   PACK12(L'LDGRTOT),64-2,5                                         
         EDIT  (P6,PACK12),(3,13(R7))                                           
         EDIT  (P6,0(R3)),(12,0(R7)),COMMAS=YES                                 
         LA    R7,PBXLNQ(R7)                                                    
         LA    R3,L'LDGRCOL(R3)                                                 
         BCT   R2,FROW3                                                         
*                                  PRINT TOT #TRNS/VERTICAL PERCENT             
FROW5    EDIT  (P6,LDGRTOT),(12,PTOT),COMMAS=YES                                
         CP    TOTAL,PZERO                                                      
         BE    FROW7                                                            
         ZAP   PACK12,LDGRTOT      CALCULATE VERTICAL PERCENT                   
         SRP   PACK12,5,5          MULTIPLY BY 100000                           
         DP    PACK12,TOTAL                                                     
         SRP   PACK12(6),64-2,5                                                 
         EDIT  (P6,PACK12),(5,PTOT+14),1,ZERO=BLANK                             
*                                                                               
FROW7    GOTO1 ACREPORT                                                         
         L     RE,SAVRE                                                         
         BR    RE                                                               
         DROP  R4,R5                                                            
         EJECT                                                                  
***********************************************************************         
* DOWNLOAD A LINE                                                     *         
***********************************************************************         
         USING LDGRD,R4                                                         
DWNL     ST    RE,SAVRE                                                         
         CP    LDGRTOT,PZERO         TEST ALL ZERO                              
         BER   RE                                                               
         L     R6,ACURCMP                                                       
         USING CMPD,R6                                                          
         MVC   OUTREC,SPACES                                                    
         LA    R7,OUTREC                                                        
         MVC   0(2,R7),CMPALPHA                                                 
         MVI   2(R7),TABCHRQ                                                    
         LA    R7,3(R7)                                                         
         MVC   0(2,R7),ACCFIL#       FILE NUMBER                                
         MVI   1(R7),TABCHRQ                                                    
         LA    R7,3(R7)                                                         
         MVC   0(2,R7),CMPCODE       COMPANY                                    
         MVI   2(R7),TABCHRQ                                                    
         LA    R7,3(R7)                                                         
         MVC   0(2,R7),LDGRUL        LEDGER                                     
         MVI   2(R7),TABCHRQ                                                    
         LA    R7,3(R7)                                                         
*                                                                               
         LA    R3,LDGRCOL            COLUMN TOTALS                              
         LA    R2,LDGRCLN                                                       
*                                                                               
DWNL3    EDIT  (P6,0(R3)),(12,0(R7)),ZERO=NOBLANK                               
         MVI   12(R7),TABCHRQ                                                   
         LA    R3,L'LDGRCOL(R3)                                                 
         LA    R7,13(R7)                                                        
         BCT   R2,DWNL3                                                         
*                                                                               
         GOTO1 ADSQUASH,DMCB,OUTREC,L'OUTREC                                    
         PUT   AGEFIL,OUTREC                                                    
         L     RE,SAVRE                                                         
         BR    RE                                                               
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
* GET COMPANY ID INFO                                                 *         
*  DON'T RETURN  IF  FILTER OUT                                       *         
***********************************************************************         
GETID    ST    RE,SAVRE                                                         
*                                                                               
         L     R5,ADCMPEL          COMPANY ELEMENT                              
         USING CPYELD,R5                                                        
         CLI   QOPT1,C' '          FILTER 1 OR 2 CHARACTER                      
         BE    GETID3                                                           
         MVI   BYTE,C'1'                                                        
         TM    CPYSTAT4,CPYSOFF2                                                
         BNO   *+8                                                              
         MVI   BYTE,C'2'                                                        
         CLC   QOPT1,BYTE                                                       
         BNE   XIT                 SKIP THIS COMPANY                            
*                                                                               
GETID3   L     R6,ACURCMP                                                       
         USING CMPD,R6                                                          
         LA    R6,CMPLNQ(R6)       BUMP TO NEXT                                 
         ST    R6,ACURCMP                                                       
         L     RF,ADCOMP                                                        
         MVC   CMPHEX,0(RF)       SAVE CURRENT COMPANY CODE                     
         GOTO1 HEXOUT,DMCB,CMPHEX,CMPCODE,L'CMPCODE,=C'TOG'                     
         MVC   CMPALPHA,CPYALPHA                                                
         MVC   CMPLOGO,CPYLOGO                                                  
         MVI   CMPSTAT,0                                                        
         TM    CPYSTAT4,CPYSOFF2                                                
         BNO   *+8                                                              
         OI    CMPSTAT,CMPSOFF2                                                 
         MVC   CMPNAME,SPACES                                                   
         L     R5,ADCMPNAM         COMPANY NAME                                 
         USING NAMELD,R5                                                        
         XR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SHI   R1,3                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CMPNAME(0),NAMEREC                                               
         MVI   CMPD+CMPLNQ,X'FF'    MARK NEW END OF TABLE                       
         CLI   QOPT2,C'D'                                                       
         BNE   GETIDX                                                           
*                                                                               
         MVC   OUTREC,SPACES                                                    
         LA    R7,OUTREC                                                        
         MVC   0(2,R7),CMPALPHA      ALPHA                                      
         MVI   2(R7),TABCHRQ                                                    
         LA    R7,3(R7)                                                         
         MVC   0(2,R7),ACCFIL#       FILE NUMBER                                
         MVI   1(R7),TABCHRQ                                                    
         LA    R7,3(R7)                                                         
         MVC   0(2,R7),CMPCODE       COMPANY                                    
         MVI   2(R7),TABCHRQ                                                    
         LA    R7,3(R7)                                                         
         MVC   0(7,R7),CMPLOGO       LOGO                                       
         MVI   7(R7),TABCHRQ                                                    
         LA    R7,8(R7)                                                         
         MVC   0(36,R7),CMPNAME                                                 
         LA    R1,35(R7)                                                        
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         MVI   1(R1),TABCHRQ                                                    
*                                                                               
         PUT   IDFIL,OUTREC                                                     
*                                                                               
GETIDX   L     RE,SAVRE                                                         
         BR    RE                                                               
         DROP  R5,R6                                                            
         EJECT                                                                  
***********************************************************************         
* INITIALIZE START OF NEW FILE                                        *         
***********************************************************************         
INIT     DS    0H                                                               
         L     RF,ACMPIDS          COMPANY ID'S                                 
         MVI   0(RF),X'FF'                                                      
         SHI   RF,CMPLNQ                                                        
         ST    RF,ACURCMP                                                       
*                                                                               
         L     RF,ACPYTOT          COMPANY TOTALS                               
         MVI   0(RF),X'FF'                                                      
         SHI   RF,LDGRLNQ                                                       
         ST    RF,ACURCPY                                                       
*                                                                               
         LA    R4,RUNTOTS          CLEAR RUN TOTALS                             
         USING LDGRD,R4                                                         
         MVI   LDGRCPY,X'FF'       SET OVERALL TOTAL                            
         MVC   LDGRUL,SPACES                                                    
         LA    R1,LDGRCOL                                                       
         LA    R0,LDGRCLN                                                       
         ZAP   0(L'LDGRCOL,R1),PZERO                                            
         LA    R1,L'LDGRCOL(R1)                                                 
         BCT   R0,*-10                                                          
         MVI   INITSW,C'N'                                                      
         BR    RE                                                               
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* CONSTANTS                                                           *         
***********************************************************************         
AIO1     DC    A(IO1)                                                           
ATRKBUF  DC    A(TRKBUF)                                                        
*                                                                               
ACMPIDS  DC    A(CMPIDS)           COMPANY ID'S                                 
ACPYTOT  DC    A(CPYTOT)           COMPANY TOTALS                               
ALDGTOT  DC    A(LDGTOT)           LEDGER TOTALS                                
*                                                                               
CTFILE   DC    CL8'CTFILE  '                                                    
OPEN     DC    CL8'OPEN'                                                        
ACCOUNT  DC    CL8'ACCOUNT'                                                     
ACCFILEL DC    C'NACCDIR NACCMST NACCARC X '                                    
*                                                                               
PZERO    DC    P'0'                                                             
PONE     DC    P'1'                                                             
         EJECT                                                                  
***********************************************************************         
* LITERAL POOL                                                        *         
***********************************************************************         
         LTORG                                                                  
*                                                                               
AGEFIL   DCB   DDNAME=AGEFIL,DSORG=PS,MACRF=(PM),                      X        
               RECFM=FB,LRECL=L'OUTREC                                          
*                                                                               
IDFIL    DCB   DDNAME=IDFIL,DSORG=PS,MACRF=(PM),                       X        
               RECFM=FB,LRECL=L'OUTREC                                          
         EJECT                                                                  
***********************************************************************         
* BOX HOOK                                                            *         
***********************************************************************         
BXHOOK   DS    0D                                                               
         NMOD1 0,*BHOOK                                                         
         L     RC,BOXRC                                                         
*                                                                               
         MVC   HEAD2+1(9),=C'ACC FILE='                                         
         MVC   HEAD2+10(2),ACCFIL#                                              
         CLI   RCSUBPRG,0                                                       
         BNE   BXHOOK3                                                          
         L     R6,ACURCMP                                                       
         USING CMPD,R6                                                          
         MVC   HEAD2+13(4),=C'HEX='                                             
         MVC   HEAD2+17(2),CMPCODE                                              
         MVC   HEAD2+21(3),=C'ID='                                              
         MVC   HEAD2+24(7),CMPLOGO                                              
         LA    RF,HEAD2+30                                                      
         CLI   0(RF),C' '                                                       
         BH    *+10                                                             
         BCTR  RF,0                                                             
         B     *-10                                                             
         LA    RF,3(RF)                                                         
         MVC   0(8,RF),=C'OFFICE=1'                                             
         TM    CMPSTAT,CMPSOFF2                                                 
         BNO   *+10                                                             
         MVC   0(8,RF),=C'OFFICE=2'                                             
*                                                                               
         MVC   HEAD2+41(8),=C'ALPHAID='                                         
         MVC   HEAD2+49(2),CMPALPHA                                             
*                                                                               
         MVC   HEAD3+1(7),=C'COMPANY'                                           
         MVC   HEAD3+13(36),CMPNAME                                             
         DROP  R6                                                               
*                                                                               
BXHOOK3  MVC   HEAD6,HED1                                                       
         MVC   HEAD7,HED2                                                       
         MVC   HEAD8,HED3                                                       
*                                                                               
         L     R4,ADBOX                                                         
         USING BOXD,R4                                                          
         MVC   BOXCOLS,SPACES                                                   
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+4,C'T'                                                   
         MVI   BOXROWS+8,C'M'                                                   
*                                                                               
         LA    R5,BOXCOLS                                                       
         USING PLD,R5                                                           
         MVI   PBXL,C'L'                                                        
         MVI   PBX1,C'C'                                                        
         MVI   PBX2,C'C'                                                        
         MVI   PBX3,C'C'                                                        
         MVI   PBX4,C'C'                                                        
         MVI   PBX5,C'C'                                                        
         MVI   PBX6,C'C'                                                        
         MVI   PBXR,C'R'                                                        
*                                                                               
BXHOOK5  MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXBLANK,C'N'                                                    
         DROP  R4,R5                                                            
*                                                                               
         XMOD1 1                                                                
BOXRC    DC    A(0)                                                             
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* STORAGE AND BUFFERS                                                 *         
***********************************************************************         
MAXCPY   EQU   50                                                               
MAXLDG   EQU   50                                                               
NCOL     EQU   5                                                                
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
TABCHRQ  EQU   X'5E'                                                            
*                                                                               
IO1      DS    0D                  IOAREA #1                                    
         DS    2000C                                                            
*                                                                               
CMPIDS   DS    0D                  COMPANY ID TABLE                             
         DS    (MAXCPY*CMPLNQ)C                                                 
*                                                                               
LDGTOT   DS    0D                  LEDGER TOTALS                                
         DS    (MAXLDG*LDGRLNQ)C                                                
*                                                                               
CPYTOT   DS    0D                  COMPANY TOTALS                               
         DS    (MAXCPY*LDGRLNQ)C                                                
*                                                                               
TRKBUF   DS    0D                                                               
         DS    (1024*60)X          60K TRACK BUFFER                             
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE                                                     *         
***********************************************************************         
LWSD     DSECT                                                                  
ADBOX    DS    A                   ADDRESS OF BOX ROUTINE                       
AIO      DS    A                   A(CURRENT IO)                                
ACURCMP  DS    A                   A(CURRENT COMPANY ID)                        
ACURCPY  DS    A                   A(CURRENT COMPANY TOTALS)                    
ACURLDG  DS    A                   A(CURRENT LEDGER TOTALS                      
*                                                                               
SAVRE    DS    F                   REGISTER RE SAVE AREA                        
TODAY    DS    CL6                                                              
*                                                                               
ACCSE#   DS    XL1                 ACC SE NUMBER                                
ACCFIL#  DS    CL2                 ACCFIL NUMBER                                
*                                                                               
FILELST  DS    25XL3               LIST OF ACC SE#, FILE #                      
*                                                                               
INITSW   DS    CL1                                                              
OUTREC   DS    CL(L'P)                                                          
*                                                                               
         DS    0D                                                               
DKEY     DS    XL(L'ACCKEY)                                                     
         DS    0D                                                               
DIR      DS    XL(ACCKDA-ACCKEY+L'ACCKDA)                                       
DA       DS    F                                                                
DMBYTE   DS    XL1                                                              
*                                                                               
ISDDS    DS    A                                                                
ADDCB    DS    A                                                                
ISPARM1  DS    A                   A(COMMAND)                                   
ISPARM2  DS    A                   A(IO)                                        
ISPARM3  DS    A                   NULLS                                        
ISPARM4  DS    A                   A(DCB)                                       
ISPARM5  DS    A                   A(KEY ARGUMENT)                              
ISPARM6  DS    A                   X'FF' FULL TRACK,AL3(TRACK BUFFER)           
*                                                                               
DATE1    DS    XL3                                                              
DATE2    DS    XL3                                                              
DATE3    DS    XL3                                                              
DATE4    DS    XL3                                                              
DATE5    DS    XL3                                                              
*                                                                               
PACK12   DS    PL12                12 BYTE PACKED NUMBER                        
*                                                                               
TOTAL    DS    PL6                                                              
*                                                                               
AUTL     DS    F                                                                
SVSE     DS    XL1                                                              
*                                                                               
RUNTOTS  DS    XL(LDGRLNQ)                                                      
*                                                                               
HED1     DS    CL132                                                            
HED2     DS    CL132                                                            
HED3     DS    CL132                                                            
*                                                                               
         EJECT                                                                  
***********************************************************************         
* DSECT FOR LEDGER/COMPANY TOTALS                                     *         
***********************************************************************         
LDGRD    DSECT                                                                  
LDGRCPY  DS    CL1                 COMPANY                                      
LDGRUL   DS    CL2                 UNIT/LEDGER                                  
LDGRCOL  DS    (NCOL)PL6                                                        
LDGRTOT  DS    PL6                                                              
LDGRLNQ  EQU   *-LDGRD                                                          
LDGRCLN  EQU   (*-LDGRCOL)/L'LDGRCOL                                            
                                                                                
                                                                                
***********************************************************************         
* DSECT FOR COMPANY ID TABLE                                          *         
***********************************************************************         
CMPD     DSECT                                                                  
CMPALPHA DS    CL2                 ALPHA CODE                                   
CMPHEX   DS    XL1                 COMPANY HEX CODE                             
CMPCODE  DS    CL2                 CHARACTER HEX CODE                           
CMPLOGO  DS    CL7                 COMPANY ABBREVIATION                         
CMPNAME  DS    CL36                COMPANY NAME                                 
CMPSTAT  DS    XL1                 STATUS                                       
CMPSOFF2 EQU   X'80'               2 CHARACTER OFFICE                           
CMPLNQ   EQU   *-CMPD                                                           
         EJECT                                                                  
***********************************************************************         
* DSECT FOR THE  PRINT LINE                                           *         
***********************************************************************         
PLD      DSECT                                                                  
PL       DS    XL1                                                              
PBXL     DS    XL1                 LEFT BOX                                     
PDATA    DS    XL10                                                             
         ORG   PDATA+5                                                          
PUL      DS    CL2                                                              
         ORG   PDATA+L'PDATA                                                    
PBX1     DS    XL1                 BOX 1                                        
PCOL1    DS    CL17                COLUMN 1                                     
         ORG   PCOL1                                                            
PNUM     DS    CL12                NUMBER OF RECORDS                            
         DS    CL1                                                              
PPCT     DS    CL3                                                              
         DS    CL1                                                              
PBX2     DS    XL1                 BOX 2                                        
PCOL2    DS    CL17                COLUMN 2                                     
PBX3     DS    XL1                 BOX 3                                        
PCOL3    DS    CL17                COLUMN 3                                     
PBX4     DS    XL1                 BOX 4                                        
PCOL4    DS    CL17                COLUMN 4                                     
PBX5     DS    XL1                 BOX 5                                        
PCOL5    DS    CL17                COLUMN 5                                     
PBX6     DS    XL1                 BOX 6                                        
PTOT     DS    CL20                TOTAL COLUMN                                 
PBXR     DS    XL1                 RIGHT BOX                                    
PBXLNQ   EQU   L'PBX1+L'PCOL1                                                   
         ORG                                                                    
         EJECT                                                                  
* ACBIGPRNTD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACBIGPRNTD                                                     
         PRINT ON                                                               
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* ACGENMODES                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
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
* DDBOXEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDBOXEQUS                                                      
         PRINT ON                                                               
* DDCNTRL                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDCNTRL                                                        
         PRINT ON                                                               
* DDLOGOD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDLOGOD                                                        
         PRINT ON                                                               
* DDMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
* DDSYSELD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSYSELD                                                       
         PRINT ON                                                               
* DDREMOTED                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDREMOTED                                                      
         PRINT ON                                                               
* DDREPXTRAD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDREPXTRAD                                                     
         PRINT ON                                                               
* DMGREQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMGREQUS                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004ACREPRA02 09/10/20'                                      
         END                                                                    
