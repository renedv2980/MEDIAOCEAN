*          DATA SET TAREP3A    AT LEVEL 084 AS OF 12/09/13                      
*PHASE T7033AB,*                                                                
         TITLE 'T7033A - DUE COMPANY REPORT'                                    
T7033A   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T7033A,R6                                                      
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
         XC    DUCCURN,DUCCURN     CLEAR CURRENCY NAME                          
         OI    DUCCURNH+6,X'80'                                                 
         XC    DUCEMPN,DUCEMPN     CLEAR EMPLOYER NAME                          
         OI    DUCEMPNH+6,X'80'                                                 
         XC    DUCPIDN,DUCPIDN     CLEAR SS NAME                                
         OI    DUCPIDNH+6,X'80'                                                 
*                                                                               
* VALIDATE YEAR                                                                 
*                                                                               
         LA    R2,DUCYRH                                                        
         CLI   5(R2),0                                                          
         BE    MISSERR             YEAR REQUIRED                                
         MVC   WORK(4),=C'1231'                                                 
         MVC   WORK+4(2),DUCYR                                                  
         GOTO1 DATVAL,DMCB,WORK,TUENDYR                                         
         OC    DMCB(4),DMCB                                                     
         BZ    FLDINV                                                           
*                                                                               
* VALIDATE CURRENCY                                                             
*                                                                               
         XC    TUCURR,TUCURR                                                    
         LA    R2,DUCCURH                                                       
         CLI   5(R2),0             ANY CURRENCY FILTER?                         
         BNE   VK02                                                             
         TM    WHEN,X'20'          FIELD REQUIRED FOR SOON                      
         BO    MISSERR                                                          
         B     VK20                                                             
*                                                                               
VK02     CLI   DUCCUR,C'U'         VALID CURRENCIES ARE U & C                   
         BNE   VK05                                                             
         MVC   DUCCURN,=C'US$ '                                                 
         B     VK10                                                             
VK05     CLI   DUCCUR,C'C'                                                      
         BNE   VK06                                                             
         MVC   DUCCURN,=C'CAN$'                                                 
         B     VK10                                                             
VK06     CLI   DUCCUR,C'E'                                                      
         BNE   FLDINV                                                           
         MVC   DUCCURN,=C'EURO'                                                 
VK10     MVC   TUCURR,DUCCUR                                                    
*                                                                               
* VALIDATE EMPLOYER                                                             
*                                                                               
VK20     XC    TGEMP,TGEMP                                                      
         LA    R2,DUCEMPH                                                       
         CLI   5(R2),0             ANY EMPLOYER FILTER?                         
         BNE   VK25                                                             
         TM    WHEN,X'20'          FIELD REQUIRED FOR SOON                      
         BO    MISSERR                                                          
         B     VK30                                                             
*                                                                               
VK25     GOTO1 RECVAL,DMCB,TLEMCDQ,(X'08',DUCEMPH),DUCEMPNH                     
*                                                                               
* VALIDATE SOCIAL SECURITY NUMBER                                               
*                                                                               
VK30     XC    TGSSN,TGSSN                                                      
         XC    TGPID,TGPID                                                      
                                                                                
         LA    R2,DUCPIDH                                                       
         CLI   5(R2),0             SS#/PID INPUT?                               
         BE    VK50                                                             
                                                                                
         CLI   5(R2),9             IF SS# INPUT, SKIP RIGHT TO RECVAL           
         BNE   VK30A                                                            
         MVC   TGSSN,8(R2)                                                      
         B     VK30C                                                            
                                                                                
VK30A    CLI   5(R2),6             IF PID INPUT                                 
         BNE   FLDINV                                                           
         MVC   TGPID,8(R2)         MOVE TO GLOBAL PID FIELD                     
VK30B    GOTO1 SSNUNPK,DMCB,TGPID,TGSSN                                         
                                                                                
VK30C    GOTO1 RECVAL,DMCB,TLW4CDQ,(X'08',0),DUCPIDNH                           
         BNE   ERREXIT                                                          
                                                                                
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   8(9,R2),SPACES                                                   
         MVC   8(L'TGPID,R2),TGPID                                              
         MVI   5(R2),L'TGPID                                                    
         OI    6(R2),X'80'                                                      
*                                                                               
* VALIDATE OPTIONS                                                              
*                                                                               
VK50     BAS   RE,VALOPT           VALIDATE OPTIONS                             
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------*                                       
*  VALOPT - ROUTINE TO VALIDATE OPTIONS *                                       
*---------------------------------------*                                       
*                                                                               
VALOPT   NTR1                                                                   
         LA    R2,DUCOPTH          VALIDATE OPTIONS                             
         CLI   5(R2),0                                                          
         BE    VOPTX                                                            
         LA    R3,BLOCK            R3=A(SCAN BLOCK)                             
         USING SCAND,R3                                                         
         GOTO1 SCANNER,DMCB,(R2),(X'80',(R3))                                   
         CLI   4(R1),0                                                          
         BE    FLDINV                                                           
         ZIC   R0,4(R1)            NUMBER OF SCANNER ENTRIES                    
*                                                                               
VOPT10   CLC   =C'EARNINGS',SCDATA1                                             
         BNE   VOPT20                                                           
         CLI   SCDATA2,C'Y'                                                     
         BNE   *+12                                                             
         OI    TUOPTS,TUEARN       SHOW EMPLOYEES W/ EARNINGS                   
         B     VOPTNEXT                                                         
         CLI   SCDATA2,C'N'                                                     
         BNE   FLDINV                                                           
         OI    TUOPTS,TUNOEARN     SHOW EMPLOYEES W/O EARNINGS                  
         B     VOPTNEXT                                                         
*                                                                               
VOPT20   CLC   =C'BOX',SCDATA1                                                  
         BNE   VOPT30                                                           
         CLI   SCDATA2,C'Y'                                                     
         BE    VOPTNEXT                                                         
         CLI   SCDATA2,C'N'                                                     
         BNE   FLDINV                                                           
         OI    TUOPTS,TUNOBOX      TURN OFF BOXES                               
         B     VOPTNEXT                                                         
*                                                                               
VOPT30   CLC   =C'TRACE',SCDATA1                                                
         BNE   VOPT40                                                           
         CLI   SCDATA2,C'N'                                                     
         BE    VOPTNEXT                                                         
         CLI   SCDATA2,C'Y'                                                     
         BNE   FLDINV                                                           
         OI    TUOPTS,TUTRACE      TRACE OUT SORT RECORDS                       
         B     VOPTNEXT                                                         
*                                                                               
VOPT40   CLC   =C'TYPE',SCDATA1                                                 
         BNE   FLDINV                                                           
         CLI   SCDATA2,C'A'                                                     
         BNE   *+12                                                             
         OI    TUOPTS,TUDAGY       SHOW AGENCY ERROR TYPE ONLY                  
         B     VOPTNEXT                                                         
         CLI   SCDATA2,C'D'                                                     
         BNE   FLDINV                                                           
         OI    TUOPTS,TUDCOM       SHOW COMPANY ERROR TYPE ONLY                 
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
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD  INITIALIZE                         
*                                                                               
         MVI   TUFRST,C'Y'         SET FIRST TIME THROUGH FLAG                  
         MVI   TUFYTD,C'Y'         SET FIRST TIME THROUGH YTD                   
         MVI   TUSKIP,C'N'         DON'T SKIP THIS SSN                          
         XC    TUSVVALS(TUSVVALX-TUSVVALS),TUSVVALS                             
         MVC   TIACOMFC,ACOMFACS   SET UP SYSIO BLOCK                           
         LA    R1,IOHOOK           A(I/O HOOK)                                  
         ST    R1,TIHOOK                                                        
         MVC   TIACCS,TWAACCS      LIMIT ACCESS                                 
         MVC   TIAUTH,TWAAUTH      AUTHORIZATION                                
         MVC   TIUSERID,TWAORIG    REQUESTING ID                                
         MVI   TIREAD,TLDUCDQ      READ DUE COMPANY RECORDS                     
         MVC   TIFEMP,TGEMP        EMPLOYER REQUESTED                           
         MVC   TIFSSN,TGSSN        SOCIAL SECURITY NUMBER REQUESTED             
         MVC   TIFCUR,TUCURR       CURRENCY REQUESTED                           
         GOTO1 TASYSIO,DMCB,TASYSIOD  OFF TO SYSIO TO DO I/O                    
*                                                                               
         MVI   TUSSNCNT,0          COUNT FOR LINES PER SSN                      
         BAS   RE,GETSORT          GET SORT RECORDS AND PRINT REPORT            
         GOTO1 SORTER,DMCB,=C'END' CLOSE SORT                                   
         B     XIT                                                              
         EJECT                                                                  
*----------------------------------*                                            
* GETSORT - GET A REC FROM SORTER  *                                            
*----------------------------------*                                            
*                                                                               
GETSORT  NTR1                                                                   
         LA    RE,TUACCN          CLEAR TOTAL ACCUMULATORS                      
         LA    RF,TUACCS                                                        
GETS2    XC    0(TUACCLQ,RF),0(RF)                                              
         LA    RF,TUACCLQ(RF)                                                   
         BCT   RE,GETS2                                                         
         MVI   TUFRST,C'Y'        SET FIRST TIME & CLEAR SAVED VALUES           
         XC    TUSVVALS(TUSVVALX-TUSVVALS),TUSVVALS                             
*                                                                               
GETS5    MVI   TUFORHED,C'N'      SET MY FORCEHED TO N                          
         CLI   TUSACTV,C'Y'       ANY RECORD PUT TO SORT                        
         BNE   XIT                                                              
         GOTO1 SORTER,DMCB,=C'GET' GET A SORT RECORD                            
         USING SORTD,R3                                                         
         ICM   R3,15,4(R1)        IF NO MORE RECORDS - PRINT TOTAL              
         BNZ   GETS10                                                           
         GOTO1 PRNTACC,DMCB,SSNEQU,=C'TOTALS'                                   
         GOTO1 PRNTACC,DMCB,EMPEQU,=C'EMPLOYER TOTALS'                          
         GOTO1 PRNTACC,DMCB,CUREQU,=C'CURRENCY TOTALS'                          
         GOTO1 PRNTACC,DMCB,TYPEQU,=C'TYPE TOTALS'                              
         CLI   TUSVERN,0                                                        
         BNE   GETS8                                                            
         GOTO1 PRNTACC,DMCB,EARNEQU,=C'EARNINGS TOTALS'                         
         B     XIT                                                              
GETS8    GOTO1 PRNTACC,DMCB,EARNEQU,=C'NO EARNINGS TOTALS'                      
         B     XIT                                                              
*                                                                               
GETS10   MVC   TUSRTREC,0(R3)      SET SORT RECORD                              
         ST    R3,FULL             ADDRESS OF RECORD TO TRACE                   
         MVC   HALF,=AL2(SORTKYLQ) LENGTH TO SORT                               
         GOTO1 MYTRACE,DMCB,=C'SORTOUT'                                         
         CLI   TUFRST,C'Y'         FIRST TIME - SAVE VALUES                     
         BNE   GETS20                                                           
         MVI   TUFRST,C'N'                                                      
         B     GETS60                                                           
         EJECT                                                                  
*                                  TEST EMPLOYEE CHANGE                         
GETS20   CLC   TUSVVALS(TUSVKEY-TUSVERN),SORTEARN                               
         BE    GETS25                                                           
         GOTO1 PRNTACC,DMCB,SSNEQU,=C'TOTALS'                                   
*                                  TEST EMPLOYER CHANGE                         
GETS25   CLC   TUSVVALS(TUSVSSN-TUSVERN),SORTEARN                               
         BE    GETS30                                                           
         GOTO1 PRNTACC,DMCB,EMPEQU,=C'EMPLOYER TOTALS'                          
         MVI   TUFORHED,C'Y'                                                    
*                                  TEST CURRENCY CHANGE                         
GETS30   CLC   TUSVVALS(TUSVEMP-TUSVERN),SORTEARN                               
         BE    GETS40                                                           
         GOTO1 PRNTACC,DMCB,CUREQU,=C'CURRENCY TOTALS'                          
         MVI   TUFORHED,C'Y'                                                    
*                                  TEST TYPE CHANGE                             
GETS40   CLC   TUSVVALS(TUSVCUR-TUSVERN),SORTEARN                               
         BE    GETS50                                                           
         GOTO1 PRNTACC,DMCB,TYPEQU,=C'TYPE TOTALS'                              
         MVI   TUFORHED,C'Y'                                                    
*                                                                               
GETS50   CLC   TUSVERN,SORTEARN                                                 
         BE    GETS60                                                           
         CLI   TUSVERN,0                                                        
         BNE   GETS52                                                           
         GOTO1 PRNTACC,DMCB,EARNEQU,=C'EARNINGS TOTALS'                         
         B     GETS58                                                           
GETS52   GOTO1 PRNTACC,DMCB,EARNEQU,=C'NO EARNINGS TOTALS'                      
GETS58   MVI   TUFORHED,C'Y'                                                    
*                                                                               
GETS60   BAS   RE,SVSORT           SAVE CURRENT SORT INFO                       
         CLI   TUFORHED,C'Y'                                                    
         BNE   GETS70                                                           
         MVI   FORCEHED,C'Y'                                                    
         XC    TUSVSSN,TUSVSSN     FORCE PRINTING SSN                           
GETS70   BAS   RE,PRNTIT           YES -- PRINT A LINE OF INFO                  
         B     GETS5                                                            
         EJECT                                                                  
*----------------------------------------*                                      
* SVSORT - SAVE CURRENT SORT RECORD INFO *                                      
*----------------------------------------*                                      
*                                                                               
SVSORT   NTR1                                                                   
         MVC   TUSVERN,SORTEARN                                                 
         MVC   TUSVTYP,SORTTYPE                                                 
         MVC   TUSVCUR,SORTCUR                                                  
         CLC   TUSVEMP,SORTEMP                                                  
         BE    SVSORT10                                                         
         MVC   TUSVEMP,SORTEMP                                                  
         GOTO1 RECVAL,DMCB,TLEMCDQ,(X'88',SORTEMP),0                            
         MVC   TUSVEMPN,TGNAME                                                  
SVSORT10 MVC   TUSVYTD,SORTYTD                                                  
         MVC   TUSVNONT,SORTNONT                                                
SVSORTX  B     XIT                                                              
         EJECT                                                                  
*---------------------------------*                                             
* PRNTIT - PRINT A LINE OF INFO   *                                             
*---------------------------------*                                             
*                                                                               
PRNTIT   NTR1                                                                   
         LA    R5,P                                                             
         USING PRNTD,R5                                                         
*                                                                               
         MVI   TUNEWSSN,C'N'      NOT NEW SSN                                   
         CLC   SORTSSN,TUSVSSN    SAME SS# = SAME NAME                          
         BE    *+8                                                              
         MVI   TUNEWSSN,C'Y'                                                    
*                                                                               
         BAS   RE,PRNT2ND         PRINT 2ND LINE -W/CONT'D STAT OR SSN          
         CLI   TUNEWSSN,C'Y'                                                    
         BNE   PRNT10                                                           
         XC    TUSVAGY,TUSVAGY    REPRINT AGENCY FOR EACH NEW SSN               
         MVC   PRNTSSN,SORTSSN    SOCIAL SECURITY NUMBER                        
         MVC   TUSVSSN,SORTSSN    SAVE NEW SS NUMBER                            
         MVC   TGSSN,SORTSSN      GET SSN NAME                                  
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'88',0),0                                  
         MVC   TUSVSSNM,TGNAME                                                  
         GOTO1 CHOPPER,DMCB,(L'TUSVSSNM,TUSVSSNM),                     X        
               (L'PRNTNAME,PRNTNAME),(C'P',2),DATCON                            
         CLC   DMCB+8(4),=F'2'                                                  
         BNE   PRNT11                                                           
         LA    R1,PRNTNAME+132                                                  
         MVC   TUSVNM2,0(R1)      SAVE 2ND HALF OF NAME                         
         MVC   0(L'PRNTNAME,R1),SPACES                                          
         B     PRNT11                                                           
*                                                                               
PRNT10   MVC   PRNTNAME,TUSVNM2   PRINT 2ND HALF OF NAME                        
         XC    TUSVNM2,TUSVNM2                                                  
*                                                                               
PRNT11   MVC   PRNTREF,SORTREF    DUE COMPANY REFERENCE NUMBER                  
         CLC   TUSVAGY,SORTAGY                                                  
         BE    PRNT12                                                           
         MVC   TUSVAGY,SORTAGY                                                  
         MVC   PRNTAGY,SORTAGY    AGENCY CODE                                   
PRNT12   MVC   PRNTCINV,SORTCINV  CREDIT INVOICE NUMBER                         
PRNT14   OC    SORTPCT,SORTPCT                                                  
         BZ    PRNT15                                                           
         EDIT  SORTPCT,(6,PRNTPCT),2                                            
*                                                                               
PRNT15   BAS   RE,PRNTST          PRINT STATUS TYPES                            
         EDIT  SORTDUE,(13,PRNTDUE),2,FLOAT=-                                   
         EDIT  SORTCOL,(13,PRNTCOL),2,FLOAT=-                                   
         ICM   R2,15,SORTDUE                                                    
         ICM   R1,15,SORTCOL                                                    
         SR    R2,R1              (R2) = BALANCE                                
         EDIT  (R2),(13,PRNTBAL),2,FLOAT=-                                      
*                                                                               
         CLI   TUNEWSSN,C'N'      IF NEW SSN - PRINT YTD FOR IT                 
         BE    PRNT20                                                           
         ICM   R2,15,TUSVYTD                                                    
         ICM   R1,15,TUSVNONT                                                   
         AR    R2,R1              (R2) = TOTAL YTD EARNINGS                     
         EDIT  (R2),(13,PRNTYTD),2,FLOAT=-                                      
*                                                                               
PRNT20   BAS   RE,PRNTLINE        PRINT THE LINE                                
*                                                                               
         BAS   RE,ADDACC          ADD TO ACCUMULATORS                           
         ZIC   R1,TUSSNCNT        COUNT OF LINES PER SSN                        
         LA    R1,1(R1)                                                         
         STC   R1,TUSSNCNT                                                      
PRNTX    B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------*                           
*  PRNTST   - PRINT STATUS ON P1,AND P2 IF NECESS   *                           
*---------------------------------------------------*                           
*                                                                               
PRNTST   NTR1                                                                   
         LA    RE,STATTBL         STATUS TABLE                                  
         LA    R2,PRNTSTAT                                                      
         LA    R0,3                                                             
PRNS20   MVC   BYTE,SORTSTAT                                                    
         NC    BYTE,1(RE)                                                       
         BZ    PRNS25                                                           
         CH    R0,=H'3'           IF NOT FIRST STAT LABEL PRINTING              
         BE    *+12                                                             
         MVI   0(R2),C','         PUT A C',' BEFORE LABEL                       
         LA    R2,1(R2)                                                         
*                                                                               
         ZIC   R1,0(RE)                                                         
         SH    R1,=H'2'                                                         
         BCTR  R1,0               LENGTH OF LABEL ONLY                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),2(RE)      MOVE STAT LABEL                               
         LA    R1,1(R1)           RESTORE LENGTH                                
         AR    R2,R1              BUMP PRINT LINE POINTER                       
         BCTR  R0,0                                                             
         LTR   R0,R0              PRINTED ALL WE CAN FIT ON THIS LINE?          
         BNZ   PRNS25                                                           
         LA    R2,TUSVST2         SAVE 2ND LINE STATUS FOR LATER PRNTG          
         LA    R0,3                                                             
*                                                                               
PRNS25   ZIC   R1,0(RE)           BUMP TO NEXT ROW IN TABLE                     
         AR    RE,R1                                                            
         CLI   0(RE),X'FF'                                                      
         BNE   PRNS20                                                           
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------*                           
*  PRNT2ND  - PRINT 2ND LINE IF NECESSARY           *                           
*---------------------------------------------------*                           
*                                                                               
PRNT2ND  NTR1                                                                   
         MVI   TUSV2NDP,C'N'      NO 2ND LINE TO PRINT                          
         OC    TUSVST2,TUSVST2    2ND PART OF STATUS TO PRNT?                   
         BZ    PRNT2ND5                                                         
         MVI   TUSV2NDP,C'Y'      YES                                           
         MVC   PRNTSTAT,TUSVST2   PRINT IT                                      
         XC    TUSVST2,TUSVST2                                                  
*                                                                               
PRNT2ND5 OC    TUSVNM2,TUSVNM2    2ND PART OF SSN NAME STILL TO PRNT?           
         BZ    PRNT2ND8                                                         
         CLI   TUSV2NDP,C'Y'      IF NO STATUS & NOT NEW SSN                    
         BE    PRNT2ND6                                                         
         CLI   TUNEWSSN,C'Y'                                                    
         BNE   PRNT2NDX           DON'T PRINT NOW                               
         MVI   TUSV2NDP,C'Y'                                                    
PRNT2ND6 MVC   PRNTNAME,TUSVNM2   OTHERWISE, PRINT IT                           
         XC    TUSVNM2,TUSVNM2                                                  
*                                                                               
PRNT2ND8 CLI   TUSV2NDP,C'Y'      IS THERE SOMETHING TO PRINT?                  
         BNE   PRNT2NDX                                                         
         BAS   RE,PRNTLINE        YES -PRINT THE LINE                           
         CLI   TUNEWSSN,C'Y'      NEW PERSON?                                   
         BNE   PRNT2NDX                                                         
         MVI   SPACING,1          YES - PRINT A BLANK LINE NOW                  
         BAS   RE,PRNTLINE                                                      
*                                                                               
PRNT2NDX B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
*---------------------------------------------------*                           
*  ADDACC - ADD TOTAL LINE PRINTED TO TOTALS        *                           
*---------------------------------------------------*                           
*                                                                               
ADDACC   NTR1                                                                   
         LA    R1,TUACCN                                                        
         LA    R2,TUACCS                                                        
         USING TUACCD,R2                                                        
*                                                                               
ADDACC10 ICM   RE,15,TUADUE                                                     
         ICM   RF,15,SORTDUE                                                    
         AR    RE,RF                                                            
         STCM  RE,15,TUADUE       TOTAL DUE                                     
*                                                                               
         ICM   RE,15,TUACOL                                                     
         ICM   RF,15,SORTCOL                                                    
         AR    RE,RF                                                            
         STCM  RE,15,TUACOL       TOTAL COLLECTED                               
*                                                                               
         ICM   RF,15,SORTDUE                                                    
         ICM   RE,15,SORTCOL                                                    
         SR    RF,RE              RF= BALANCE                                   
         ICM   RE,15,TUABAL                                                     
         AR    RE,RF                                                            
         STCM  RE,15,TUABAL       TOTAL BALANCE                                 
*                                                                               
         LA    R2,TUACCLQ(R2)                                                   
         BCT   R1,ADDACC10                                                      
         B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
*---------------------------------------------------*                           
*  PRNTACC - PRINTS TOTAL FOR EQUATED ROW PASSED    *                           
*---------------------------------------------------*                           
*                                                                               
PRNTACC  NTR1                                                                   
         LA    R5,P                                                             
         USING PRNTD,R5                                                         
         MVC   BYTE,3(R1)         EQUATED ROW                                   
         L     R2,4(R1)           LITERAL                                       
         ZIC   R3,4(R1)           LENGTH OF LITERAL                             
*                                                                               
         MVI   TUNEWSSN,C'Y'      FORCE TO ALWAYS PRINT                         
         BAS   RE,PRNT2ND         2ND LINE BEFORE TOTALS                        
*                                                                               
         ZIC   R4,BYTE            EQUATED ROW                                   
         BCTR  R4,0                                                             
         MH    R4,=Y(TUACCLQ)                                                   
         LA    R1,TUACCS          START OF TABLE                                
         AR    R4,R1              + DISPLACEMENT                                
         USING TUACCD,R4                                                        
*                                                                               
         LA    R1,PRNTNAME        PRINT TOTAL LABEL IN NAME COLUMN              
         CLI   BYTE,SSNEQU                                                      
         BNE   PRNTA05                                                          
         CLI   TUSSNCNT,1         IF ONLY ONE LINE FOR SSN                      
         BE    PRNTA20            DON'T BOTHER W/TOTAL-JUST SKIP LINE           
         LA    R1,PRNTREF         PRINT TOTAL LABEL IN REF COLUMN               
         B     PRNTA08                                                          
*                                                                               
PRNTA05  MVI   SPACING,1          SKIP A LINE BEFORE TOTAL                      
         BAS   RE,PRNTLINE        PRINT THE LINE                                
*                                                                               
PRNTA08  BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),0(R2)      SET LABEL FOR TOTAL LINE                      
         EDIT  TUADUE,(13,PRNTDUE),2,FLOAT=-                                    
         EDIT  TUACOL,(13,PRNTCOL),2,FLOAT=-                                    
         EDIT  TUABAL,(13,PRNTBAL),2,FLOAT=-                                    
         BAS   RE,PRNTLINE        PRINT THE LINE                                
*                                                                               
PRNTA20  CLI   BYTE,SSNEQU                                                      
         BNE   PRNTA30                                                          
         OC    TUSVNM2,TUSVNM2    IF 2ND HALF TO PRINT -DON'T SKIP LINE         
         BNZ   PRNTA30                                                          
         MVI   SPACING,1          SKIP A LINE                                   
         BAS   RE,PRNTLINE                                                      
*                                 CLEAR CURRENT +ALL LOWER LEVEL TOTS           
PRNTA30  XC    0(TUACCLQ,R4),0(R4)                                              
         LA    R4,TUACCLQ(R4)                                                   
         CLI   0(R4),X'FF'        END OF TABLE?                                 
         BNE   PRNTA30                                                          
         MVI   TUSSNCNT,0         ALSO RESET SSN COUNT                          
PRNTAX   B     XIT                                                              
         DROP  R4,R5                                                            
         EJECT                                                                  
*---------------------------------------------------*                           
*  IOHOOK - HOOK FROM SYSIO - WRITE RECORDS TO SORT *                           
*---------------------------------------------------*                           
*                                                                               
IOHOOK   NTR1                                                                   
         CLI   TIMODE,PROCREC      PROCESS RECORD MODE?                         
         BNE   IOHOOKX                                                          
         L     R5,TIAREC                                                        
         USING TLDUD,R5                                                         
         CLI   0(R5),TLDUCDQ       DUE COMPANY RECORD?                          
         BNE   IOHOOKX                                                          
*                                                                               
         LR    R4,R5                                                            
         MVI   ELCODE,TADUELQ      DUE COMPANY DETAILS ELEMENT                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TADUD,R4                                                         
         ICM   R2,15,TADUDUE                                                    
         ICM   R1,15,TADUCOL                                                    
         SR    R2,R1                                                            
         LTR   R2,R2              ANY BALANCE?                                  
         BZ    IOHOOKX            NOPE - NOT INTERESTED                         
*                                                                               
         TM    TUOPTS,TUDAGY                                                    
         BNO   IOHK5                                                            
         CLI   TADUTYPE,TADUTYAY                                                
         BNE   IOHOOKX                                                          
         B     IOHK10             SHOW AGENCY ERROR TYPE ONLY                   
IOHK5    TM    TUOPTS,TUDCOM                                                    
         BNO   IOHK10             SHOW BOTH ERROR TYPES                         
         CLI   TADUTYPE,TADUTYDU                                                
         BNE   IOHOOKX                                                          
         B     IOHK10             SHOW COMPANY ERROR TYPE ONLY                  
*                                                                               
IOHK10   MVC   FULL,TIAREC        TRACE DUE COMPANY RECORDS                     
         XC    HALF,HALF          LENGTH TO SORT                                
         GOTO1 MYTRACE,DMCB,=C'DUECOMPANY'                                      
*                                                                               
         CLI   TUFRST,C'Y'                                                      
         BE    IOHK35                                                           
         CLI   TUSKIP,C'Y'        SKIP ADDING THIS SSN TO SORT?                 
         BNE   IOHK30                                                           
         CLC   TUSVSSN,TLDUSSN    YES - IF SAME SS #                            
         BE    IOHOOKX                                                          
         MVI   TUSKIP,C'N'        NEW SSN - GET YTD                             
         B     IOHK35                                                           
IOHK30   BAS   RE,CHKVALS         CHECK CHANGE IN SSN,EMP,OR CURRENCY           
         BE    IOHK40                                                           
*                                                                               
IOHK35   BAS   RE,SVVALS          SAVE NEW VALUES                               
         BAS   RE,GETYTD                                                        
         CLI   TUSKIP,C'Y'        SKIP ADDING THIS SSN TO SORT?                 
         BE    IOHOOKX                                                          
*                                                                               
IOHK40   BAS   RE,PUTSORT         PUT DUE COMPANY RECORD TO SORT                
         CLI   TUFYTD,C'Y'        CALL TO GETYTD                                
         BNE   IOHOOKX                                                          
         MVI   TUFYTD,C'N'                                                      
         MVC   KEY,TIKEY          RE-ESTABLISH SYSIO'S READ SEQUENCE            
         GOTO1 HIGH               ONLY ON THE FIRST TIME THROUGH                
*                                                                               
IOHOOKX  B     XIT                                                              
         EJECT                                                                  
*-----------------------------------------------------------------*             
* SVVALS - SAVES CURRENT S.S. #, EMPLOYER , ERROR TYPE & CURRENCY *             
*-----------------------------------------------------------------*             
*                                                                               
SVVALS   NTR1                                                                   
         MVI   TUFRST,C'N'                                                      
         MVC   TUSVSSN,TLDUSSN    SAVE SS #                                     
         MVC   TUSVEMP,TADUEMP    SAVE EMPLOYER                                 
         MVI   TUSVCUR,C'C'       SAVE CURRENCY                                 
         TM    TADUSTAT,TADUSCAN                                                
         BO    SV10                                                             
         MVI   TUSVCUR,C'E'                                                     
         TM    TADUSTAT,TADUSEUR                                                
         BO    SV10                                                             
         MVI   TUSVCUR,C'U'                                                     
SV10     MVC   TUSVTYP,TADUTYPE   SAVE ERROR TYPE                               
         B     XIT                                                              
         SPACE                                                                  
*------------------------------------------------------*                        
* CHKVALS- CHECKS CURRENT VALUES AGAINST PREVIOUS      *                        
*  XIT   - CC SET EQUAL IF VALUES ARE SAME             *                        
*------------------------------------------------------*                        
*                                                                               
CHKVALS  NTR1                                                                   
         CLC   TUSVSSN,TLDUSSN                                                  
         BNE   CHKVNO                                                           
         CLC   TUSVEMP,TADUEMP                                                  
         BNE   CHKVNO                                                           
         CLC   TUSVTYP,TADUTYPE                                                 
         BNE   CHKVNO                                                           
*                                                                               
         CLI   TUSVCUR,C'C'                                                     
         BNE   CHKV10                                                           
         TM    TADUSTAT,TADUSCAN                                                
         BO    CHKVYES                                                          
         B     CHKVNO                                                           
CHKV10   TM    TADUSTAT,TADUSCAN                                                
         BNO   CHKVYES                                                          
*                                                                               
         CLI   TUSVCUR,C'E'                                                     
         BNE   CHKV20                                                           
         TM    TADUSTAT,TADUSEUR                                                
         BO    CHKVYES                                                          
         B     CHKVNO                                                           
CHKV20   TM    TADUSTAT,TADUSEUR                                                
         BNO   CHKVYES                                                          
*                                                                               
CHKVNO   LTR   RB,RB                                                            
         B     CHKVX                                                            
*                                                                               
CHKVYES  CR    RB,RB                                                            
CHKVX    B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------*                                   
*  GETYTD - CALL TAYTD TO GET YTD EARNINGS  *                                   
*-------------------------------------------*                                   
*                                                                               
GETYTD   NTR1                                                                   
         MVI   TUFYTD,C'Y'        GET                                           
         LA    R3,TUYTDBLK                                                      
         USING TYD,R3                                                           
         MVC   TYASYSIO,TASYSIO   A(SYSIO)                                      
         LA    R1,MYYTDTBL        A(YTD TABLE)                                  
         ST    R1,TYATAB                                                        
         MVC   TYEMP,TUSVEMP      CURRENT EMPLOYER                              
         MVC   TYSSN,TUSVSSN      CURRENT SOCIAL SECURITY #                     
         MVC   TYCUR,TUSVCUR      CURRENT CURRENCY                              
         TM    TUOPTS,TUTRACE                                                   
         BNO   *+8                                                              
         MVI   TYTRACE,C'Y'       TRACE YTD RECORDS                             
         GOTO1 DATCON,DMCB,(0,TUENDYR),(1,TYPEND)                               
         GOTO1 TGTAYTD,DMCB,(RC),SYSCOMM,(R3)                                   
*                                                                               
         TM    TUOPTS,TUEARN                                                    
         BNO   GETYTD10                                                         
         OC    TYCYTD(TYCYLNQ),TYCYTD                                           
         BNZ   *+8                                                              
         MVI   TUSKIP,C'Y'        SHOW EMPLOYEES W/EARNINGS ONLY                
         B     GETYTDX                                                          
*                                                                               
GETYTD10 TM    TUOPTS,TUNOEARN                                                  
         BNO   GETYTDX            SHOW ALL EMPLOYEES                            
         OC    TYCYTD(TYCYLNQ),TYCYTD                                           
         BZ    *+8                                                              
         MVI   TUSKIP,C'Y'        SHOW EMPLOYESS W/O EARNINGS ONLY              
GETYTDX  B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*----------------------------------*                                            
* PUTSORT - PUT A RECORD TO SORTER *                                            
*----------------------------------*                                            
*                                                                               
PUTSORT  NTR1                                                                   
         LA    R3,TUYTDBLK                                                      
         USING TYD,R3                                                           
         XC    TUSRTREC,TUSRTREC                                                
         LA    R2,TUSRTREC         R2=A(SORT RECORD)                            
         USING SORTD,R2                                                         
*                                                                               
         OC    TYCYTD(TYCYLNQ),TYCYTD                                           
         BNZ   *+8                                                              
         MVI   SORTEARN,1         NO EARNINGS YTD                               
         MVC   SORTTYPE,TUSVTYP   ERROR TYPE - A/D                              
         MVC   SORTCUR,TUSVCUR    CURRENT CURRENCY                              
         MVC   SORTEMP,TUSVEMP    CURRENT EMPLOYER                              
         MVC   SORTREF,TLDUDUC    DUE COMPANY REFERENCE NUMBER                  
         CLI   TLDUDUC,X'FA'      IS THIS A YEAR 2000 DATE?                     
         BL    PUTS05             NO                                            
         MVC   WORK(4),TLDUDUC    YES, CONVERT TO DISPLAY FORMAT                
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(X'20',WORK)                                
         MVC   SORTREF(4),WORK                                                  
         MVC   SORTREF+4(2),TLDUDUC+4                                           
*                                                                               
PUTS05   MVC   SORTAGY,TADUAGY    AGENCY CODE                                   
         MVC   SORTCINV,SPACES                                                  
         OC    TADUCINV,TADUCINV  CREDIT INVOICE NUMBER                         
         BZ    PUTS10                                                           
         GOTO1 TINVCON,DMCB,TADUCINV,SORTCINV,DATCON                            
PUTS10   MVC   SORTPCT,TADUPCT    DEDUCTION PERCENTAGE                          
         MVC   SORTSTAT,TADUSTAT  STATUS                                        
         MVC   SORTSSN,TUSVSSN    CURRENT SOCIAL SECURITY #                     
         MVC   SORTDUE,TADUDUE    AMOUNT DUE                                    
         MVC   SORTCOL,TADUCOL    AMOUNT COLLECTED                              
         MVC   SORTYTD,TYCEARN    YTD EARNINGS                                  
         MVC   SORTNONT,TYCNTAX   YTD NON TAXABLE EARNINGS                      
         MVI   TUSACTV,C'Y'       PUT A RECORD TO SORT                          
*                                                                               
         GOTO1 SORTER,DMCB,=C'PUT',(R2)     WRITE OUT SORT RECORD               
*                                                                               
         LA    R1,TUSRTREC                                                      
         ST    R1,FULL            ADDRESS OF RECORD TO TRACE                    
         MVC   HALF,=AL2(SORTKYLQ) LENGTH TO SORT                               
         GOTO1 MYTRACE,DMCB,=C'SORTIN'                                          
*                                                                               
PUTSORTX B     XIT                                                              
         DROP  R2,R3,R4,R5                                                      
         EJECT                                                                  
*----------------------------*                                                  
* PRNTLINE- PRINTS A LINE    *                                                  
*----------------------------*                                                  
*                                                                               
PRNTLINE NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)    PRINT IT                                      
         B     XIT                                                              
         SPACE 3                                                                
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
         BAS   RE,INTBOX          TURN ON BOXES                                 
*                                                                               
         LA    R2,TUSRTREC        R2=A(SORT RECORD)                             
         USING SORTD,R2                                                         
*                                                                               
         MVC   H3+10(10),=C'DUE AGENCY'                                         
         CLI   SORTTYPE,C'A'                                                    
         BE    *+10                                                             
         MVC   H3+10(11),=C'DUE COMPANY'                                        
*                                                                               
         MVC   H4+10(3),=C'US$'   CURRENCY                                      
         CLI   SORTCUR,C'C'                                                     
         BNE   *+10                                                             
         MVC   H4+10(4),=C'CAN$'                                                
         CLI   SORTCUR,C'E'                                                     
         BNE   *+10                                                             
         MVC   H4+10(4),=C'EURO'                                                
*                                                                               
         MVC   H5+10(3),SORTEMP   EMPLOYER & EMPLOYER NAME                      
         MVC   H5+15(L'TUSVEMPN),TUSVEMPN                                       
*                                                                               
         CLI   SORTEARN,1                                                       
         BE    HDHK10                                                           
         MVC   H1+51(23),=C'EMPLOYEES WITH EARNINGS'                            
         MVC   H2+51(23),=26X'BF'                                               
         B     HDHK20                                                           
HDHK10   MVC   H1+49(26),=C'EMPLOYEES WITHOUT EARNINGS'                         
         MVC   H2+49(26),=26X'BF'                                               
*                                                                               
HDHK20   GOTO1 DATCON,DMCB,(0,TUENDYR),(X'20',WORK)                             
         MVC   H8+119(2),WORK      DISPLAY YEAR FOR YTD EARNINGS                
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*------------------------------------*                                          
* SETBOX - BOXES REQUESTED - SET UP  *                                          
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
         MVI   BL,C'L'                                                          
         MVI   BC1,C'C'                                                         
         MVI   BC2,C'C'                                                         
         MVI   BC3,C'C'                                                         
         MVI   BC4,C'C'                                                         
         MVI   BC5,C'C'                                                         
         MVI   BC6,C'C'                                                         
         MVI   BC7,C'C'                                                         
         MVI   BC8,C'C'                                                         
         MVI   BC9,C'C'                                                         
         MVI   BC10,C'C'                                                        
         MVI   BR,C'R'                                                          
         B     XIT                                                              
         DROP  RF,R4                                                            
         EJECT                                                                  
*              ERRORS, EXITS, CONSTANTS, ETC.                                   
*                                                                               
FLDINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     ERREXIT                                                          
         SPACE 2                                                                
MISSERR  MVI   ERROR,MISSING       MISSING                                      
ERREXIT  GOTO1 ERREX                                                            
         SPACE 2                                                                
         GETEL R4,DATADISP,ELCODE                                               
         SPACE 2                                                                
SORTCARD DC    CL80'SORT FIELDS=(1,15,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=66'                                    
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              REPORT SPECS                                                     
*                                                                               
MYSPECS  SSPEC H1,2,RUN                                                         
         SSPEC H1,100,REPORT                                                    
         SSPEC H1,120,PAGE                                                      
         SSPEC H2,100,REQUESTOR                                                 
         SSPEC H3,2,C'TYPE'                                                     
         SSPEC H4,2,C'CURRENCY'                                                 
         SSPEC H5,2,C'EMPLOYER'                                                 
         SSPEC H8,1,C' SS NUMBER'                                               
         SSPEC H8,20,C'NAME'                                                    
         SSPEC H8,33,C'REF #  AGENCY CR INV DED %     STATUS'                   
         SSPEC H8,77,C'AMT DUE      COLLECTED      BALANCE'                     
         SSPEC H8,123,C'YTD'                                                    
         DC    X'00'                                                            
         EJECT                                                                  
TUACCN   EQU   5                   MAX NUMBER OF ACCUMULATORS                   
EARNEQU  EQU   1                   ROW # 1 = EARNING TOTALS                     
TYPEQU   EQU   2                   ROW # 2 = TYPE TOTALS                        
CUREQU   EQU   3                   ROW # 3 = CURRENCY TOTALS                    
EMPEQU   EQU   4                   ROW # 4 = EMPLOYER TOTALS                    
SSNEQU   EQU   5                   ROW # 5 = EMPLOYEE TOTALS                    
         SPACE                                                                  
STATTBL  DS    0C                 STATUS TABLE                                  
         DC    AL1(05),AL1(TADUSAGY),C'AGY'                                     
         DC    AL1(05),AL1(TADUSCLI),C'CLI'                                     
         DC    AL1(06),AL1(TADUSAUT),C'AUTO'                                    
         DC    AL1(06),AL1(TADUSCAN),C'CAN$'                                    
         DC    AL1(06),AL1(TADUSHLD),C'HOLD'                                    
         DC    AL1(06),AL1(TADUSLCK),C'LOCK'                                    
         DC    AL1(06),AL1(TADUSEUR),C'EURO'                                    
         DC    X'FF'                                                            
         SPACE                                                                  
         DS    0D                                                               
         DC    C'*ACCUMS*'        ACCUMULATORS                                  
TUACCS   DS    (TUACCN)XL(TUACCLQ)                                              
         DC    X'FF'                                                            
         SPACE                                                                  
MYNYTD   EQU   60                  MAXIMUM NUMBER OF YTD ENTRIES                
*                                                                               
         DS    0D                  YTD TABLE                                    
         DC    C'*YTDTBL*'                                                      
MYYTDTBL DC    (MYNYTD*YTDLNQ)X'00'                                             
         DC    X'00'                                                            
         SPACE                                                                  
         EJECT                                                                  
*              DSECT TO COVER LOCAL WORKING STORAGE                             
*                                                                               
TUD      DSECT                                                                  
*                                                                               
TUCURR   DS    CL1                 CURRENCY                                     
TUENDYR  DS    CL6                 1231YY -YTD EARNINGS (INTERNAL FMT)          
TUFRST   DS    CL1                 FIRST TIME FLAG INDICATOR                    
TUFYTD   DS    CL1                 FIRST TIME FLAG INDICATOR                    
TUSKIP   DS    CL1                 YTD SKIP FLAG                                
TUFORHED DS    CL1                 FIRST TIME FLAG INDICATOR                    
TUSACTV  DS    CL1                 Y= RECORD WAS PUT TO SORT                    
TUSSNCNT DS    XL1                 COUNT OF LINES PER SSN                       
TUNEWSSN DS    XL1                 Y= NEW SSN FOR YTD PRINTING                  
*                                                                               
TUOPTS   DS    XL1                 OPTIONS                                      
TUEARN   EQU   X'80'               EARNINGS ONLY                                
TUNOEARN EQU   X'40'               NO EARNINGS ONLY                             
TUNOBOX  EQU   X'20'               NO BOXES                                     
TUTRACE  EQU   X'10'               TRACE SORT RECORDS                           
TUDAGY   EQU   X'08'               SHOW AGENCY ERROR TYPE ONLY                  
TUDCOM   EQU   X'04'               SHOW COMPANY ERROR TYPE ONLY                 
*                                                                               
TUSV2NDP DS    CL1                 Y=2ND PRINT LINE                             
TUSVVALS DS    0C                                                               
TUSVERN  DS    CL1                 SAVED EMPLOYEE W/ OR W/O EARNINGS            
TUSVTYP  DS    CL(L'TADUTYPE)      SAVED ERROR TYPE                             
TUSVCUR  DS    CL(L'TGCUR)         SAVED CURRENCY                               
TUSVEMP  DS    CL(L'TGEMP)         SAVED EMPLOYER                               
TUSVSSN  DS    CL(L'TGSSN)         SAVED SOCIAL SECURITY NUMBER                 
TUSVKEY  EQU   *                                                                
*                                                                               
TUSVYTD  DS    XL4                 YTD EARNINGS PER SSN                         
TUSVNONT DS    XL4                 YTD NON TAXABLE EARNINGS PER SSN             
TUSVAGY  DS    CL6                 AGENCY                                       
TUSVSSNM DS    CL(L'TGNAME)        SAVED NAME                                   
TUSVEMPN DS    CL(L'TGNAME)        SAVED EMPLOYER NAME                          
TUSVVALX EQU   *                                                                
*                                                                               
TUSVNM2  DS    CL(L'PRNTNAME)      2ND HALF OF CHOPPED NAME                     
TUSVST2  DS    CL(L'PRNTSTAT)      2ND HALF OF STATUS COLUMN                    
*                                                                               
TUYTDBLK DS    CL(TYLNQ)           YTD TABLE                                    
*                                                                               
TUSRTREC DS    CL(SORTLNQ)         SORT RECORD AREA                             
*                                                                               
TULNQ    EQU   *-TUD                                                            
         EJECT                                                                  
*              DSECT TO COVER SORT RECORD                                       
*                                                                               
SORTD    DSECT                                                                  
SORTEARN DS    X                   EMPLOYEES W/EARN=0, W/O EARN=1               
SORTTYPE DS    X                   A= AGENCY, D= COMPANY                        
SORTCUR  DS    C                   CURRENCY                                     
SORTEMP  DS    CL(L'TIFEMP)        EMPLOYER ID                                  
SORTSSN  DS    CL(L'TIFSSN)        SOCIAL SECURITY NUMBER                       
SORTKYLQ EQU   *-SORTD                                                          
*                                                                               
SORTREF  DS    CL6                 DUE COMPANY REFERENCE NUMBER                 
SORTAGY  DS    CL6                 AGENCY CODE                                  
SORTCINV DS    CL6                 CREDIT INVOICE NUMBER                        
SORTPCT  DS    XL2                 PERCENTAGE                                   
SORTSTAT DS    XL1                 STATUS                                       
SORTDUE  DS    XL4                 GROSS AMOUNT DUE                             
SORTCOL  DS    XL4                 AMOUNT COLLECTED                             
SORTYTD  DS    XL4                 YTD EARNINGS                                 
SORTNONT DS    XL4                 NON TAXABLE EARNINGS                         
         DS    CL14                SPARE                                        
SORTLNQ  EQU   *-SORTD                                                          
         SPACE 2                                                                
*              DSECT TO COVER TOTAL ACCUMULATORS                                
*                                                                               
TUACCD   DSECT                                                                  
TUADUE   DS    F                                                                
TUACOL   DS    F                                                                
TUABAL   DS    F                                                                
TUACCLQ  EQU   (*-TUACCD)                                                       
         EJECT                                                                  
*              DSECT TO COVER PRINT LINE                                        
*                                                                               
PRNTD    DSECT                                                                  
BL       DS    C                                                                
PRNTSSN  DS    CL9                SS NUMBER                                     
BC1      DS    C                                                                
PRNTNAME DS    CL20               NAME                                          
BC2      DS    C                                                                
PRNTREF  DS    CL6                REFERENCE NUMBER                              
BC3      DS    C                                                                
PRNTAGY  DS    CL6                AGENCY CODE                                   
BC4      DS    C                                                                
PRNTCINV DS    CL6                CREDIT INVOICE NUMBER                         
BC5      DS    C                                                                
PRNTPCT  DS    XL6                PERCENTAGE                                    
BC6      DS    C                                                                
PRNTSTAT DS    CL13               STATUS                                        
BC7      DS    C                                                                
PRNTDUE  DS    XL13               DUE COMPANY                                   
BC8      DS    C                                                                
PRNTCOL  DS    XL13               AMOUNT COLLECTED                              
BC9      DS    C                                                                
PRNTBAL  DS    XL13               BALANCE DUE                                   
BC10     DS    C                                                                
PRNTYTD  DS    XL13               YEAR TO DATE EARNINGS                         
BR       DS    C                                                                
         EJECT                                                                  
       ++INCLUDE TAREPFFD                                                       
         SPACE                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPDAD                                                       
         EJECT                                                                  
* DDGENTWA   (MUST FOLLOW LAST SCREEN)                                          
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* DDBIGBOX                                                                      
* TAGENFILE                                                                     
* TASYSDSECT                                                                    
* TASYSEQUS                                                                     
* TAYTDD                                                                        
* TAREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAYTDD                                                         
       ++INCLUDE TAREPWORKD                                                     
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'084TAREP3A   12/09/13'                                      
         END                                                                    
