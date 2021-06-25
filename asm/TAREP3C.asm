*          DATA SET TAREP3C    AT LEVEL 012 AS OF 12/05/12                      
*PHASE T7033CE,*                                                                
         TITLE 'T7033C - NYTAPE REPORT'                                         
T7033C   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T7033C,R6                                                      
         L     RC,0(R1)                                                         
         USING GEND,RC             RC=A(CONTROLLER W/S)                         
         L     RA,ATWA                                                          
         USING T703FFD,RA          RA=A(SCREEN)                                 
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          R9=A(SYSTEM W/S)                             
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8           R8=A(SPOOL DSECT)                            
         LA    R7,BUFF                                                          
         LA    R7,8(R7)                                                         
         USING TUD,R7              R7=A(LOCAL W/S)                              
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
*                                                                               
         GOTO1 INITIAL,DMCB,0      INITIALIZE                                   
*                                                                               
         CLI   MODE,VALREC         VALIDATE SCREEN                              
         BE    *+12                                                             
         CLI   MODE,DISPREC                                                     
         BNE   *+12                                                             
         BAS   RE,VKEY                                                          
         B     XIT                                                              
*                                                                               
         CLI   MODE,PRINTREP       PROCESS REPORT                               
         BNE   XIT                                                              
         BAS   RE,PREP                                                          
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*--------------------------*                                                    
*  VKEY - VALIDATE SCREEN  *                                                    
*--------------------------*                                                    
*                                                                               
VKEY     NTR1                                                                   
         LR    RE,R7               A(LOCAL W/S)                                 
         LA    RF,TULNQ                                                         
         XCEFL ,                   CLEAR LOCAL W/S                              
         XC    TIFILTS,TIFILTS     CLEAR SYSIO FILTERS                          
         XC    SCREMPN,SCREMPN     CLEAR EMPLOYER NAME                          
         OI    SCREMPNH+6,X'80'                                                 
*                                                                               
* VALIDATE EMPLOYER                                                             
*                                                                               
         XC    TGEMP,TGEMP                                                      
         LA    R2,SCREMPH                                                       
         GOTO1 RECVAL,DMCB,TLEMCDQ,(X'08',SCREMPH),SCREMPNH                     
         MVC   TIFEMP,TGEMP        EMPLOYER REQUESTED                           
*                                                                               
         MVC   TUEIN,SPACES        READ EMTAX RECORD                            
         GOTO1 RECVAL,DMCB,TLEXCDQ,(X'A0',TGEMP)                                
         BNE   MISSID                                                           
         MVI   ELCODE,TATIELQ      FOR NY TAX ID NUMBER                         
         MVI   FULL,TATITYTX                                                    
         MVC   FULL+1(3),=C'NY '                                                
         GOTO1 GETL,DMCB,(4,FULL)                                               
         BNE   MISSID                                                           
         L     R1,TGELEM                                                        
         USING TATID,R1                                                         
         MVC   TUEIN,TATIID        SAVE EIN NUMBER                              
         DROP  R1                                                               
         SPACE                                                                  
*                                                                               
* VALIDATE PERIOD                                                               
*                                                                               
VALPER   LA    R2,SCRPERH                                                       
         LA    R3,BLOCK                                                         
         USING PERVALD,R3                                                       
         GOTO1 PDVAL,DMCB,(R3)                                                  
         MVC   TUPER,PVALCPER     SAVE PRINTABLE PERIOD                         
         MVC   TIQPSTR,PVALPSTA   SET DATES FOR SYSIO                           
         MVC   TIQPEND,PVALPEND                                                 
         MVI   TIQDTYPE,TIQDCHK   SET FILTERING ON CHECK DATE                   
         DROP  R3                                                               
*                                                                               
* VALIDATE OPTIONS                                                              
*                                                                               
         BAS   RE,VALOPT           VALIDATE OPTIONS                             
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------*                                       
*  VALOPT - ROUTINE TO VALIDATE OPTIONS *                                       
*---------------------------------------*                                       
*                                                                               
VALOPT   NTR1                                                                   
         LA    R2,SCROPTH          VALIDATE OPTIONS                             
         CLI   5(R2),0                                                          
         BE    VOPTX                                                            
         LA    R3,BLOCK            R3=A(SCAN BLOCK)                             
         USING SCAND,R3                                                         
         GOTO1 SCANNER,DMCB,(R2),(X'80',(R3))                                   
         CLI   4(R1),0                                                          
         BE    FLDINV                                                           
         ZIC   R0,4(R1)            NUMBER OF SCANNER ENTRIES                    
*                                                                               
VOPT10   CLC   =C'TRACE',SCDATA1                                                
         BNE   VOPT15                                                           
         CLI   SCDATA2,C'N'                                                     
         BE    VOPTNEXT                                                         
         CLI   SCDATA2,C'Y'                                                     
         BNE   FLDINV                                                           
         OI    TUOPTS,TUTRACE      TRACE OUT SORT RECORDS                       
         B     VOPTNEXT                                                         
*                                                                               
VOPT15   CLC   =C'BOX',SCDATA1                                                  
         BNE   VOPT20                                                           
         CLI   SCDATA2,C'Y'                                                     
         BE    VOPTNEXT                                                         
         CLI   SCDATA2,C'N'                                                     
         BNE   FLDINV                                                           
         OI    TUOPTS,TUNOBOX      NO BOXES IN REPORT                           
         B     VOPTNEXT                                                         
*                                                                               
VOPT20   CLC   =C'TAPE',SCDATA1                                                 
         BNE   FLDINV                                                           
         CLI   SCDATA2,C'Y'                                                     
         BE    VOPTNEXT                                                         
         CLI   SCDATA2,C'N'                                                     
         BNE   FLDINV                                                           
         OI    TUOPTS,TUNOTAPE     DON'T GENERATE A TAPE                        
         B     VOPTNEXT                                                         
*                                                                               
VOPTNEXT LA    R3,SCANNEXT         BUMP TO NEXT                                 
         BCT   R0,VOPT10                                                        
*                                                                               
VOPTX    B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*-------------------------------------------*                                   
*  PREP - REPORT GENERATION - CALL TO SYSIO *                                   
*-------------------------------------------*                                   
*                                                                               
PREP     NTR1                                                                   
         LA    R1,MYSPECS          SET A(SPECS) FOR PRINTING                    
         ST    R1,SPECS                                                         
         LA    R1,HDHOOK           SET A(HEADLINE HOOK)                         
         ST    R1,HEADHOOK                                                      
         ZAP   TUWAGES,=P'0'       WAGES FOR AN EMPLOYER                        
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD  INITIALIZE                         
         OPEN  (TMPFIL,OUTPUT)     OPEN TMPFIL FOR OUTPUT                       
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   TIACOMFC,ACOMFACS   SET UP SYSIO BLOCK                           
         LA    R1,IOHOOK           A(I/O HOOK)                                  
         ST    R1,TIHOOK                                                        
         MVC   TIACCS,TWAACCS      LIMIT ACCESS                                 
         MVC   TIAUTH,TWAAUTH      AUTHORIZATION                                
         MVC   TIUSERID,TWAORIG    REQUESTING ID                                
         MVI   TIFCUR,C'U'         US DOLLARS                                   
         OI    TIFPDPN,TAPDPCRD    NO CREDITS                                   
         OI    TIQFLAG2,TIQFPGRY   PASS GREY RECORDS                            
         MVI   TIREAD,TLCKCDQ      READ CHECK RECORDS                           
         GOTO1 TASYSIO,DMCB,TASYSIOD  OFF TO SYSIO TO DO I/O                    
*                                                                               
         CLI   TUSACTV,C'Y'        ANY RECORDS WRITEN OUT?                      
         BNE   PREP10                                                           
         BAS   RE,GETSORT          GET SORT REC & PUT OUT TO WKFILE             
         OPEN  (TMPFIL,INPUT)      OPEN TMPFIL TO GET RECORDS                   
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,OPENTAPE         OPEN TAPE                                    
         BAS   RE,PRNTREP          GET FROM FILE, PRINT,& PUT TO TAPE           
         TM    TUOPTS,TUNOTAPE     IF TAPE REQUESTED - CLOSE IT                 
         BO    PREP10                                                           
         L     R2,=A(NYTAPE)                                                    
         CLOSE ((2))                                                            
*                                                                               
PREP10   GOTO1 SORTER,DMCB,=C'END' CLOSE SORT                                   
         B     XIT                                                              
         EJECT                                                                  
*------------------------------------------------------*                        
* GETSORT - GET A REC FROM SORTER & PUT TO A TEMP FILE *                        
*           WHILE COUNTING THE NUMBER OF EMPLOYEES     *                        
*------------------------------------------------------*                        
*                                                                               
GETSORT  NTR1                                                                   
         BAS   RE,PUTAFIL          ADD EMPLOYER RECORD TO FILE                  
         MVI   TUFRST,C'Y'                                                      
         XC    TUNUM,TUNUM         NUMBER OF EMPLOYEES                          
         XC    TUSVSSN,TUSVSSN                                                  
         ZAP   TUWAGE2,=P'0'       EMPLOYEE WAGE                                
*                                                                               
GETS5    GOTO1 SORTER,DMCB,=C'GET' GET A SORT RECORD                            
         USING SORTD,R3                                                         
         ICM   R3,15,4(R1)         IF NO MORE RECORDS - PRINT TOTAL             
         BNZ   GETS10                                                           
         BAS   RE,PUTBFIL          PUT LAST EMPLOYEE TO FILE                    
         CLOSE (TMPFIL,)           CLOSE TMPFIL                                 
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         B     XIT                                                              
*                                                                               
GETS10   MVC   TUSRTREC,0(R3)      SAVE CURRENT SORT RECORD                     
         ST    R3,FULL             ADDRESS OF RECORD TO TRACE                   
         MVC   HALF,=AL2(SORTLNQ)  LENGTH TO SORT                               
         GOTO1 MYTRACE,DMCB,=C'SORTOUT'                                         
*                                                                               
         CLI   TUFRST,C'Y'                                                      
         BNE   GETS20                                                           
         MVI   TUFRST,C'N'                                                      
         MVC   TUSVSSN,SORTSSN     SAVE FIRST SSN                               
         B     GETS30              ADD WAGE TO TOTAL                            
*                                                                               
GETS20   CLC   TUSVSSN,SORTSSN                                                  
         BE    GETS30                                                           
         BAS   RE,PUTBFIL          PUT EMPLOYEE TO FILE                         
         MVC   TUSVSSN,SORTSSN     SAVE NEW SSN                                 
         ZAP   TUWAGE2,=P'0'       RESET WAGES -ADD NEW SSN TO WAGES            
*                                                                               
GETS30   ICM   R1,15,SORTWAGE      ACCUMULATE WAGES                             
         CVD   R1,DUB                                                           
         AP    TUWAGE2,DUB                                                      
         B     GETS5                                                            
         DROP  R3                                                               
         EJECT                                                                  
*----------------------------------------------------*                          
* PUTAFIL- PUT EMPLOYER RECORD W/ TOTAL WAGES TO FILE*                          
*----------------------------------------------------*                          
*                                                                               
PUTAFIL  NTR1                                                                   
         XC    TUREC,TUREC                                                      
         LA    R2,TUREC                                                         
         USING RECD,R2                                                          
*                                                                               
         MVI   RECATYP,C'A'       INDICATE EMPLOYER RECORD                      
         MVC   RECAEIN,SPACES     SET EIN W/O ANY DASHES                        
         LA    R0,L'TUEIN                                                       
         LA    R1,TUEIN                                                         
         LA    RE,RECAEIN                                                       
PUTAF10  CLI   0(R1),0                                                          
         BE    PUTAF20            END OF NUMBER                                 
         CLI   0(R1),C'-'         SKIP DASHES AND BLANKS                        
         BE    PUTAF15                                                          
         CLI   0(R1),C' '                                                       
         BE    PUTAF15                                                          
         MVC   0(1,RE),0(R1)                                                    
         LA    RE,1(RE)                                                         
PUTAF15  LA    R1,1(R1)                                                         
         BCT   R0,PUTAF10                                                       
*                                                                               
PUTAF20  OC    RECAEIN,=9C'0'     PAD WITH CHARACTER ZERO'S                     
         MVC   RECASFX,SPACES     NO SUFFIX                                     
         MVC   RECANM(L'SCREMPN),SCREMPN                                        
         OC    RECANM,SPACES                                                    
         MVC   RECABNK,SPACES                                                   
*                                                                               
         GOTO1 DATCON,DMCB,(1,TIQPEND),(0,WORK)                                 
         MVC   RECAQYR+1(1),WORK+1 LAST DIGIT OF YEAR                           
         SR    RE,RE                                                            
         PACK  DUB,WORK+2(2)                                                    
         CVB   RF,DUB                                                           
         AH    RF,=H'2'                                                         
         D     RE,=F'3'                                                         
         STC   RF,BYTE                                                          
         EDIT  BYTE,(1,RECAQYR)    QUARTER 1,2,3,4                              
*                                                                               
         EDIT  TUWAGES,(12,RECAWG),FILL=0 TOTAL WAGES                           
         MVI   RECAFIL,C'X'                                                     
         MVC   RECAFIL+1(L'RECAFIL-1),RECAFIL                                   
         BAS   RE,PUTFILE         PUT EMPLOYER RECORD TO FILE                   
*                                                                               
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*--------------------------------------*                                        
* PUTBFIL- PUT EMPLOYEE RECORD TO FILE *                                        
*--------------------------------------*                                        
*                                                                               
PUTBFIL  NTR1                                                                   
         LA    R3,TUSRTREC                                                      
         USING SORTD,R3                                                         
         LA    R2,TUREC                                                         
         USING RECD,R2                                                          
         MVI   RECBTYP,C'B'                                                     
         MVC   RECBSSN,TUSVSSN                                                  
         MVC   RECBNM,SPACES      GET IT ON OUTPUT FROM FILE                    
         CP    TUWAGE2,=P'0'      DON'T SHOW EMPLOYEES W/ NO EARNINGS           
         BE    PUTBFILX                                                         
         EDIT  TUWAGE2,(12,RECBWG),FILL=0                                       
         MVC   RECBBLNK,SPACES                                                  
         BAS   RE,PUTFILE         PUT EMPLOYEE RECORD OUT TO FILE               
PUTBFILX B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
*---------------------------------------------------*                           
* PUTFILE - PUT A RECORD TO FILE- IF EMPLOYEE COUNT *                           
*---------------------------------------------------*                           
*                                                                               
PUTFILE  NTR1                                                                   
         LA    R2,TUREC                                                         
         USING RECD,R2                                                          
         CLI   RECBTYP,C'B'                                                     
         BNE   PUTFIL5                                                          
         L     R1,TUNUM           INCREMENT EMPLOYEE COUNT                      
         LA    R1,1(R1)                                                         
         ST    R1,TUNUM                                                         
*                                                                               
PUTFIL5  PUT   TMPFIL,TUREC       PUT RECORD TO TMFIL                           
*                                                                               
         LA    R1,TUREC                                                         
         ST    R1,FULL            ADDRESS OF RECORD TO TRACE                    
         MVC   HALF,=AL2(RECLNQ)  LENGTH OF FILE RECORD                         
         GOTO1 MYTRACE,DMCB,=C'FILEIN'                                          
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*--------------------------------------*                                        
* OPENTAPE- DYNAMICALLY OPENS THE TAPE *                                        
*--------------------------------------*                                        
*                                                                               
OPENTAPE NTR1                                                                   
         TM    TUOPTS,TUNOTAPE    NO TAPE REQUESTED?                            
         BO    OPENTX                                                           
         L     R4,TWADCONS                                                      
         USING TWADCOND,R4                                                      
         L     R1,TSPFUSER        SAVE FILE NUMBER                              
         USING GENTBLD,R1                                                       
OPENT20  OC    GENEMP,GENEMP      ANY EMPLOYERS?                                
         BNZ   OPENT25                                                          
         MVC   GENEMP,TIFEMP      SAVE EMPLOYER -TAPE GENERATED FOR             
         B     OPENT30                                                          
OPENT25  CLC   GENEMP,TIFEMP      GENERATING NEW TAPE FOR SAME EMP?             
         BE    OPENT30                                                          
         LA    R1,GENTBLL(R1)                                                   
         B     OPENT20                                                          
OPENT30  ZIC   RF,GENNUM          BUMP GENERATION NUMBER FOR NEW TAPE           
         LA    RF,1(RF)                                                         
         STC   RF,GENNUM                                                        
         DROP  R1                                                               
*                                                                               
         MVC   WORK(35),=CL35'TALTAPE.TA0NY  1'                                 
         MVC   WORK+13(2),TIFEMP                                                
         CLC   =C'P+',TIFEMP       TAPES DON'T LIKE THE "+"                     
         BNE   *+10                                                             
         MVC   WORK+13(2),=C'PL'                                                
         MVC   DUB,=CL8'NYTAPE'                                                 
         GOTO1 TDYNALLO,DMCB,(0,DUB),((RF),WORK)                                
         L     R2,=A(NYTAPE)                                                    
         OPEN  ((2),OUTPUT)                                                     
OPENTX   B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*------------------------------------------------------*                        
* PRNTREP - GET RECORD FROM TMPFILE, PRINT IT AND      *                        
*           PUT TO TAPE                                *                        
*------------------------------------------------------*                        
*                                                                               
PRNTREP  NTR1                                                                   
         MVI   TUFRST,C'Y'                                                      
         LA    R3,NCHUNKS         NUMBER OF SECTIONS PER LINE                   
         LA    R5,P                                                             
         USING PRNTD,R5                                                         
         LA    R2,TUREC                                                         
         USING RECD,R2                                                          
PRNT5    GET   TMPFIL,TUREC        GET RECORD FROM FILE                         
         CLI   TUFRST,C'Y'                                                      
         BNE   PRNT10                                                           
         MVI   TUFRST,C'N'                                                      
         CLI   RECATYP,C'A'        FIRST RECORD SHOULD BE EMPLOYER              
         BE    *+6                                                              
         DC    H'0'                                                             
         EDIT  TUNUM,(6,RECANUM),FILL=0 TOTAL NUM OF EMPLOYEES                  
         B     PRNT50              FOR TAPE                                     
*                                                                               
PRNT10   MVC   PRNTSSN,RECBSSN                                                  
         MVC   TGSSN,RECBSSN                                                    
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'28',0),0                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,AIO                                                           
         MVI   ELCODE,TAW4ELQ     W4 NAME ELEMENT                               
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAW4D,R4                                                         
         CLI   TAW4TYPE,TAW4TYIN FOR INDIVIDUAL PUT DASH IN                     
         BE    PRNT20                                                           
         MVC   PRNTNAME,TGNAME                                                  
         B     PRNT25                                                           
*                                                                               
PRNT20   MVC   PRNTNAME(L'TAW4NAM2),TAW4NAM2                                    
         LA    RF,PRNTNAME+L'PRNTNAME-1                                         
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'-'         SEP LAST FROM FIRST BY A DASH                 
         LA    RF,2(RF)                                                         
         LA    RE,PRNTNAME+L'PRNTNAME-1                                         
         SR    RE,RF                                                            
         LA    R1,L'TAW4NAM1-1                                                  
         CR    RE,R1                                                            
         BNH   *+6                                                              
         LR    RE,R1                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),TAW4NAM1   MOVE AS MUCH OF 1ST NAME AS POSSIBLE          
*                                                                               
         CLI   TAW4LEN,TAW4LN2Q   NEW ELEMENT?                                  
         BNE   PRNT25                                                           
         LA    RF,PRNTNAME+L'PRNTNAME-1                                         
         LA    RE,PRNTNAME+L'PRNTNAME-1                                         
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         CR    RF,RE                                                            
         BNL   *+10                 IF THERE IS ENOUGH SPACE,                   
         MVC   2(1,RF),TAW4MIDN     MOVE IN MIDDLE INITIAL                      
*                                                                               
PRNT25   MVC   RECBNM,PRNTNAME    ADD IT TO RECORD - FOR TAPE PURPOSES          
         EDIT  (C12,RECBWG),(12,PRNTWAGE),2                                     
         BCT   R3,PRNT30                                                        
         BAS   RE,PRNTIT          GO PRINT THE PREVIOUS LINE                    
         LA    R5,P               PT TO BEGINNING OF LINE                       
         LA    R3,NCHUNKS                                                       
         B     PRNT50                                                           
*                                                                               
PRNT30   LA    R5,PRNTNEXT        PT TO SECOND HALF OF LINE                     
*                                                                               
PRNT50   BAS   RE,PUTTAPE         PUT TO TAPE                                   
         B     PRNT5                                                            
         DROP  R2,R5                                                            
         EJECT                                                                  
*-------------------------------------------*                                   
* ENDFIL  - NO MORE RECORDS FROM TMPFIL     *                                   
*-------------------------------------------*                                   
*                                                                               
ENDFIL   BAS   RE,PRNTIT           INCASE ANY THING LEFT IN PRINT LINE          
         BAS   RE,PRNTOT           PRINT TOTALS                                 
         CLOSE (TMPFIL,)           CLOSE FILE                                   
         B     XIT                                                              
         SPACE                                                                  
*----------------------------*                                                  
* PRNTIT  - PRINTS A LINE    *                                                  
*----------------------------*                                                  
*                                                                               
PRNTIT   NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)    PRINT IT                                      
         B     XIT                                                              
         SPACE                                                                  
*------------------------------------------------------*                        
* PRNTOT - PRINT TOTAL WAGES AND TOTAL EMPLOYEES       *                        
*------------------------------------------------------*                        
*                                                                               
PRNTOT   NTR1                                                                   
         BAS   RE,PRNTIT          PRINT BLANK LINE                              
         LA    R5,P                                                             
         USING PRNTD,R5                                                         
         MVC   PRNTNAME(15),=CL15'EMPLOYER TOTALS'                              
         EDIT  TUWAGES,(12,PRNTWAGE),2                                          
         LA    R5,L'P(R5)                                                       
         MVC   PRNTNAME(19),=CL19'NUMBER OF EMPLOYEES'                          
         EDIT  TUNUM,(12,PRNTWAGE)                                              
         BAS   RE,PRNTIT                                                        
         B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
*--------------------------------------*                                        
* PUTTAPE - PUTS REC FROM FILE TO TAPE *                                        
*--------------------------------------*                                        
*                                                                               
PUTTAPE  NTR1                                                                   
         TM    TUOPTS,TUNOTAPE    NO TAPE REQUESTED?                            
         BO    PUTTX                                                            
         L     R2,=A(NYTAPE)                                                    
         LA    R3,TUREC                                                         
         PUT   (R2),(R3)                                                        
PUTTX    B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------*                           
*  IOHOOK - HOOK FROM SYSIO - WRITE RECORDS TO SORT *                           
*---------------------------------------------------*                           
*                                                                               
IOHOOK   NTR1                                                                   
         CLI   TIMODE,PROCREC      PROCESS RECORD MODE?                         
         BNE   IOHOOKX                                                          
         L     R5,TIAREC                                                        
         USING TLCKD,R5                                                         
         CLI   0(R5),TLCKCDQ       CHECK RECORD?                                
         BNE   IOHOOKX                                                          
*                                                                               
         LR    R4,R5                                                            
         USING TAPDD,R4                                                         
         MVI   ELCODE,TAPDELQ     PAYMENT DETAILS ELEMENT                       
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   TAPDW4TY,TAW4TYIN  FOR INDIVIDUALS OR                            
         BE    IOHOOK10                                                         
         CLI   TAPDW4TY,TAW4TYES  ESTATES ONLY                                  
         BNE   IOHOOKX                                                          
         DROP  R4                                                               
*                                                                               
IOHOOK10 ST    R5,AIO                                                           
         MVI   ELCODE,TACWELQ                                                   
         GOTO1 GETL,DMCB,=C'NY '                                                
         BNE   IOHOOKX                                                          
         L     R4,TGELEM                                                        
         USING TACWD,R4                                                         
         TM    TACWSTAT,TACWSTAX  TAXABLE STATE NY                              
         BNO   IOHOOKX                                                          
*                                                                               
         LR    R4,R5                                                            
         MVI   ELCODE,TATUELQ     P+?                                           
         BAS   RE,GETEL                                                         
         BNE   IOHOOK30                                                         
*                                                                               
         GOTO1 GETL,DMCB,=C'NY '                                                
         BNE   IOHOOKX                                                          
         L     R4,TGELEM                                                        
         USING TATUD,R4                                                         
         ICM   RF,15,TATUWAGE     TAXABLE WAGES                                 
         ICM   RE,15,TATUTNWA     TAXABLE NON WAGES                             
         AR    RF,RE                                                            
         LTR   RF,RF              ANY TAXABLE WAGES?                            
         BZ    IOHOOKX            NO - SKIP IT                                  
         ST    RF,TGFULL                                                        
         B     IOHOOK40                                                         
*                                                                               
IOHOOK30 XC    FULL,FULL          INDIVIDUALS TAXABLE EARNINGS                  
         LR    R4,R5                                                            
         USING TACDD,R4                                                         
         MVI   ELCODE,TACDELQ     CHECK DETAILS ELEMENT                         
         BAS   RE,GETEL                                                         
         BNE   IOHOOKX                                                          
         OC    TACDEARN,TACDEARN  ANY TAXABLE EARNINGS?                         
         BZ    IOHOOKX                                                          
         MVC   TGFULL,TACDEARN                                                  
*                                                                               
IOHOOK40 BAS   RE,PUTSORT                                                       
*                                                                               
         MVC   FULL,TIAREC        TRACE CHECK RECORD                            
         XC    HALF,HALF          LENGTH TO SORT                                
         GOTO1 MYTRACE,DMCB,=C'CHECK REC'                                       
*                                                                               
IOHOOKX  MVC   AIO,AIO1           RE-SET AIO                                    
         B     XIT                                                              
         EJECT                                                                  
*-----------------------------------------------*                               
* PUTSORT - PUT ONLY EMPLOYEE RECORDS TO SORTER *                               
*-----------------------------------------------*                               
*                                                                               
PUTSORT  NTR1                                                                   
         XC    TUSRTREC,TUSRTREC                                                
         LA    R2,TUSRTREC         R2=A(SORT RECORD)                            
         USING SORTD,R2                                                         
*                                                                               
         MVC   SORTSSN,TLCKSSN   SOCIAL SECURITY NUMBER                         
         MVC   SORTWAGE,TGFULL   EMPLOYEES WAGES                                
         L     R1,TGFULL                                                        
         CVD   R1,DUB                                                           
         AP    TUWAGES,DUB        ADD EMPLOYEES WAGES TO TOTAL WAGES            
         MVI   TUSACTV,C'Y'       RECORD WRITTEN OUT TO SORT                    
         GOTO1 SORTER,DMCB,=C'PUT',(R2)                                         
*                                                                               
         LA    R1,TUSRTREC                                                      
         ST    R1,FULL            ADDRESS OF RECORD TO TRACE                    
         MVC   HALF,=AL2(SORTLNQ) LENGTH TO SORT                                
         GOTO1 MYTRACE,DMCB,=C'SORTIN'                                          
*                                                                               
PUTSORTX B     XIT                                                              
         DROP  R2,R4,R5                                                         
         EJECT                                                                  
*-------------------------------------------*                                   
* MYTRACE - ROUTINE TO HANDLE RECORD TRACES *                                   
*-------------------------------------------*                                   
*                                                                               
MYTRACE  NTR1                                                                   
         TM    TUOPTS,TUTRACE                                                   
         BNO   MYTRACEX                                                         
         L     R4,FULL            WHAT TO TRACE                                 
         LH    R5,HALF            LENGTH TO TRACE                               
         L     R2,0(R1)           LITERAL                                       
         ZIC   R3,0(R1)           LENGTH OF LITERAL                             
         GOTO1 TRACE,DMCB,(R4),(R5),(R2),(R3)                                   
MYTRACEX B     XIT                                                              
         EJECT                                                                  
*------------------------------------*                                          
* HDHOOK -  HEADLINE HOOK (HEADHOOK) *                                          
*------------------------------------*                                          
*                                                                               
HDHOOK   NTR1                                                                   
         TM    TUOPTS,TUNOBOX                                                   
         BO    *+8                                                              
         BAS   RE,INTBOX                                                        
*                                                                               
         LA    R2,TUSRTREC        R2=A(SORT RECORD)                             
         USING SORTD,R2                                                         
         LA    R3,BLOCK                                                         
         USING PERVALD,R3                                                       
*                                                                               
         MVC   H1+53(18),=CL18'NYTAPE WAGE REPORT'                              
         MVC   H2+53(18),=18X'BF'                                               
*                                                                               
         MVC   H3+10(3),SCREMP    EMPLOYER & EMPLOYER NAME                      
         MVC   H3+15(L'SCREMPN),SCREMPN                                         
         MVC   H4+108(L'TUPER),TUPER                                            
         B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
*------------------------------------*                                          
* INTBOX - BOXES REQUESTED - SET UP  *                                          
*------------------------------------*                                          
*                                                                               
INTBOX   NTR1                                                                   
         L     R4,ABOX                                                          
         USING BOXD,R4                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+6,C'T'                                                   
         MVI   BOXROWS+8,C'M'                                                   
         MVI   BOXROWS+60,C'B'                                                  
*                                                                               
         MVC   BOXCOLS,SPACES                                                   
         LA    RF,BOXCOLS                                                       
         USING PRNTD,RF                                                         
         LA    R0,NCHUNKS                                                       
INTBOX5  MVI   BL,C'L'                                                          
         MVI   BC1,C'C'                                                         
         MVI   BC2,C'C'                                                         
         MVI   BR,C'R'                                                          
         LA    RF,PRNTNEXT                                                      
         BCT   R0,INTBOX5                                                       
         B     XIT                                                              
         DROP  RF,R4                                                            
         EJECT                                                                  
*              ERRORS, EXITS, CONSTANTS, ETC.                                   
*                                                                               
FLDINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         GOTO1 ERREX                                                            
         SPACE 2                                                                
MISSERR  MVI   ERROR,MISSING       MISSING                                      
         GOTO1 ERREX                                                            
         SPACE 2                                                                
MISSID   MVI   ERROR,EREXNFND      TAX ID NOT FOUND                             
         GOTO1 ERREX                                                            
         SPACE 2                                                                
         GETEL R4,DATADISP,ELCODE                                               
         SPACE 2                                                                
SORTCARD DC    CL80'SORT FIELDS=(1,9,A),FORMAT=BI,WORK=1'                       
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=13'                                    
         SPACE 2                                                                
TMPFIL   DCB   DDNAME=TMPFIL,DSORG=PS,RECFM=FB,LRECL=96,               X        
               BLKSIZE=9600,MACRF=(GM,PM),EODAD=ENDFIL                          
         SPACE 2                                                                
NYTAPE   DCB   DDNAME=NYTAPE,DSORG=PS,RECFM=FB,LRECL=96,               X        
               BLKSIZE=9600,MACRF=PM                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              REPORT SPECS                                                     
*                                                                               
MYSPECS  SSPEC H1,2,RUN                                                         
         SSPEC H1,100,REPORT                                                    
         SSPEC H1,120,PAGE                                                      
         SSPEC H2,100,REQUESTOR                                                 
         SSPEC H3,2,C'EMPLOYER'                                                 
         SSPEC H4,100,C'PERIOD'                                                 
         SSPEC H8,7,C'SS NUMBER         EMPLOYEE NAME'                          
         SSPEC H8,49,C'GROSS WAGES'                                             
         SSPEC H8,68,C'SS NUMBER         EMPLOYEE NAME'                         
         SSPEC H8,110,C'GROSS WAGES'                                            
         DC    X'00'                                                            
         EJECT                                                                  
*              DSECT TO COVER LOCAL WORKING STORAGE                             
*                                                                               
TUD      DSECT                                                                  
         DS    0D                                                               
*                                                                               
NCHUNKS  EQU   2                   PRINT 2 ACROSS                               
TUNUM    DS    F                   TOTAL WAGES FOR EMPLOYER                     
TUSACTV  DS    CL1                 Y= RECORD WAS PUT TO SORT                    
TUFRST   DS    CL1                 FIRST TIME THROUGH FLAG                      
TUWAGES  DS    PL6                 TOTAL WAGES FOR EMPLOYER                     
TUWAGE2  DS    PL6                 EMPLOYEE WAGE                                
TUSVSSN  DS    CL9                                                              
TUEIN    DS    CL14                EMPLOYER EIN NUMBER                          
TUPER    DS    CL17                REQUESTED PERIOD MMMDD/YY-MMMDD/YY           
*                                                                               
TUOPTS   DS    XL1                 OPTIONS                                      
TUNOTAPE EQU   X'80'               DON'T GENERATE TAPE                          
TUNOBOX  EQU   X'40'               NO BOXES                                     
TUTRACE  EQU   X'20'               TRACE SORT RECORDS                           
*                                                                               
TUSRTREC DS    CL(SORTLNQ)         SORT RECORD AREA                             
TUREC    DS    CL(RECLNQ)          FILE RECORD AREA                             
*                                                                               
TULNQ    EQU   *-TUD                                                            
         EJECT                                                                  
*              DSECT TO COVER TSPFUSER                                          
*                                                                               
GENTBLD  DSECT                                                                  
GENEMP   DS    CL2                EMPLOYER                                      
GENNUM   DS    XL1                LATEST GENERATION NUMBER FOR EMPLOYER         
GENTBLL  EQU   *-GENTBLD                                                        
         EJECT                                                                  
*              DSECT TO COVER SORT RECORD                                       
*                                                                               
SORTD    DSECT                                                                  
SORTSSN  DS    CL9                SOCAL SECURITY NUMBER                         
SORTWAGE DS    XL4                EMPLOYEE WAGE THIS QUARTER                    
SORTLNQ  EQU   *-SORTD                                                          
         EJECT                                                                  
*              DSECT TO COVER FILE RECORD                                       
*                                                                               
RECD     DSECT                                                                  
RECATYP  DS    C                   A= EMPLOYER                                  
RECAEIN  DS    CL9                 EIN                                          
RECASFX  DS    CL2                 SUFFIX NOT USED                              
RECANM   DS    CL39                EMPLOYER NAME                                
RECAQYR  DS    XL2                 QUARTER AND LAST DIGIT OF YEAR               
RECABNK  DS    CL3                 BLANK                                        
RECANUM  DS    XL6                 NUMBER OF EMPLOYEES                          
RECAWG   DS    CL12                TOTAL WAGES PAID                             
RECAFIL  DS    CL22                X FILLED                                     
RECLNQ   EQU   *-RECD                                                           
*                                                                               
         ORG   RECATYP                                                          
RECBTYP  DS    C                  B= EMPLOYEE                                   
RECBSSN  DS    CL9                SOCAL SECURITY NUMBER                         
RECBNM   DS    CL30               EMPLOYEE NAME LAST-FIRST-MIDDLE               
RECBWG   DS    CL12               EMPLOYEE WAGE THIS QUARTER                    
RECBBLNK DS    CL44               BLANK                                         
         EJECT                                                                  
*              DSECT TO COVER PRINT LINE                                        
*                                                                               
PRNTD    DSECT                                                                  
         DS    CL5                SPARE                                         
BL       DS    C                                                                
PRNTSSN  DS    CL9                SS NUMBER                                     
BC1      DS    C                                                                
PRNTNAME DS    CL30               LAST-FIRST-MIDDLE                             
BC2      DS    C                                                                
PRNTWAGE DS    XL12               GROSS WAGES PAID THIS QUARTER                 
BR       DS    C                                                                
         DS    CL1                                                              
PRNTNEXT EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE TAREPFFD                                                       
         SPACE                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPDCD                                                       
         EJECT                                                                  
* DDGENTWA   (MUST FOLLOW LAST SCREEN)                                          
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* DDBIGBOX                                                                      
* TAGENFILE                                                                     
* TASYSDSECT                                                                    
* TASYSEQUS                                                                     
* DDPERVALD                                                                     
* DDTWADCONS                                                                    
* TAREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE DDPERVALD                                                      
TWADCOND DSECT                                                                  
       ++INCLUDE DDTWADCONS                                                     
       ++INCLUDE TAREPWORKD                                                     
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012TAREP3C   12/05/12'                                      
         END                                                                    
