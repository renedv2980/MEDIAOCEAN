*          DATA SET ACPRO23    AT LEVEL 044 AS OF 04/10/15                      
*PHASE T60B23A                                                                  
*INCLUDE KHDUMMY                                                                
         TITLE 'T60B23 - PRODUCTION LEDGER REPORT'                              
T60B23   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T60B23,RA,RR=R2                                                
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          SYSTEM SPECIFIC WORK                         
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
         ST    R2,RELO                                                          
         SPACE 1                                                                
         CLI   MODE,VALKEY                                                      
         BNE   MODE2                                                            
         LA    RE,LOCAL            CLEAR LOCAL STORAGE                          
         LA    RF,LOCALLN                                                       
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         BAS   RE,VKEY                                                          
*                                                                               
         CLI   QWCOPT,C'Y'                                                      
         BNE   *+8                                                              
         BAS   RE,INITIAL                                                       
*                                                                               
         B     XIT                                                              
         SPACE 1                                                                
MODE2    CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
         BAS   RE,PREP                                                          
         BAS   RE,WRAP             WRAP-UP AFTER REPORT                         
         B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO VALIDATE KEY FIELDS                                            
*                                                                               
VKEY     NTR1                                                                   
         XC    QOGROUP,QOGROUP                                                  
         XC    QOFFICE,QOFFICE                                                  
         MVC   QCLI,SPACES                                                      
         MVC   QPROD,SPACES                                                     
         MVC   QJOB,SPACES                                                      
         MVI   OPTION,C'Y'                                                      
         LA    R2,CONRECH                                                       
         GOTO1 SETHEIR             NEED KEY LENGTHS                             
         SR    R3,R3                                                            
         SPACE 1                                                                
         LA    R2,PROOFGH                                                       
         CLI   5(R2),0                                                          
         BE    VKEY1                                                            
         GOTO1 VALOG                                                            
         MVC   QOGROUP,EFFOFG                                                   
         SPACE 1                                                                
VKEY1    LA    R2,PROOFFH                                                       
         CLI   5(R2),0                                                          
         BE    VKEY2                                                            
         MVI   ERROR,NOTOFNOG                                                   
         CLI   PROOFGH+5,0                                                      
         BNE   ERREND                                                           
         GOTO1 VALOFF                                                           
         MVC   QOFFICE,EFFOFFC                                                  
         SPACE 1                                                                
VKEY2    LA    R2,PROCLIH          CLIENT OPTIONAL                              
         CLI   5(R2),0                                                          
         BE    VKEY4               NO-IMPLIES 'ALL' CLIENTS                     
         MVI   ERROR,NOTCLNOF                                                   
         CLI   PROOFFH+5,0         TEST IF OFFICE INPUT                         
         BNE   ERREND              YES-CANNOT HAVE BOTH                         
         MVI   ERROR,NOTCLNOG                                                   
         CLI   PROOFGH+5,0         TEST IF OFFICE GROUP INPUT                   
         BNE   ERREND              YES-CANNOT HAVE BOTH                         
         ZIC   R3,LCLI             SET KEY COMPARE LENGTH                       
         GOTO1 VALCLI                                                           
         MVC   QCLI,CLICODE                                                     
*                                                                               
VKEY4    LA    R2,PROPROH          PRODUCT OPTIONAL IF OVERNIGHT                
         CLI   5(R2),0                                                          
         BE    VKEY6                                                            
         ZIC   R3,LCLIPRO                                                       
         GOTO1 VALPROD                                                          
         MVC   QPROD,PRODCODE                                                   
         B     VKEY8                                                            
*                                                                               
VKEY6    CLI   OFFLINE,C'Y'                                                     
         BE    VKEY8                                                            
         TM    WHEN,X'20'          TEST FOR 'SOON' REQUEST                      
         BZ    VKEY8               NO                                           
         MVI   ERROR,MISSING       YES-FORCE A PRODUCT TO BE INPUT              
         B     ERREND                                                           
*                                                                               
VKEY8    LA    R2,PROJOBH          JOB OPTIONAL                                 
         CLI   5(R2),0                                                          
         BE    VKEY10                                                           
         LA    R3,12                                                            
         GOTO1 VALJOB                                                           
         MVI   ERROR,JOBVSOPT      INCOMPATIBLE MESSAGE                         
         L     R6,AIO                                                           
         TM    ACTRSTAT-ACTRECD(R6),ACTSDRFT                                    
         BO    VKEY9                                                            
         CLI   PRONOD,C'O'                                                      
         BE    ERREND                                                           
         B     VKEY9A                                                           
*                                                                               
VKEY9    CLI   PRONOD,C'Y'         CAN ONLY REQUEST BY DRAFT IF                 
         BE    VKEY9A                                                           
         CLI   PRONOD,C'O'         Y OR O                                       
         BNE   ERREND                                                           
*                                                                               
VKEY9A   MVC   QJOB,JOBNUM                                                      
*                                                                               
VKEY10   LA    R3,2(R3)            SET CONTROL BREAK LENGTH-1                   
         STC   R3,LCONBRK                                                       
*                                            VALIDATE FILTER 1                  
VKEY10A  LA    R2,PROFIL1H                                                      
         CLI   5(R2),0                                                          
         BE    VKEY10B                                                          
         GOTO1 VALFIL,PARAS,QFILT,L'QFILT                                       
*                                            VALIDATE FILTER 2                  
VKEY10B  LA    R2,PROFIL2H                                                      
         CLI   5(R2),0                                                          
         BE    VKEY10C                                                          
         GOTO1 VALFIL,PARAS,QFILT2,L'QFILT2                                     
*                                            VALIDATE FILTER 3                  
VKEY10C  LA    R2,PROFIL3H                                                      
         CLI   5(R2),0                                                          
         BE    VKEY10D                                                          
         GOTO1 VALFIL,PARAS,QFILT3,L'QFILT3                                     
*                                            VALIDATE FILTER 4                  
VKEY10D  LA    R2,PROFIL4H                                                      
         CLI   5(R2),0                                                          
         BE    VKEY10E                                                          
         GOTO1 VALFIL,PARAS,QFILT4,L'QFILT4                                     
*                                            VALIDATE FILTER 5                  
VKEY10E  LA    R2,PROFIL5H                                                      
         CLI   5(R2),0                                                          
         BE    VKEY12                                                           
         GOTO1 VALFIL,PARAS,QFILT5,L'QFILT5                                     
*                                                                               
VKEY12   LA    R2,PROFORMH                                                      
         GOTO1 ANY                                                              
         MVI   ERROR,INVALID                                                    
         MVC   QREPTYPE,WORK       GET FORMAT                                   
         CLI   QREPTYPE,C'F'       TEST FOR FILE DATA                           
         BE    VKEY14                                                           
         CLI   QREPTYPE,C'E'       TEST FOR EFFECTIVE OPTIONS                   
         BE    VKEY14                                                           
         CLI   QREPTYPE,C'C'       TEST FOR COMBINED FORMAT                     
         BNE   ERREND                                                           
         SPACE 1                                                                
VKEY14   MVI   QCLOSE,C'N'         DEFAULT IS EXCLUDE CLOSED JOBS               
         LA    R2,PROICLOH                                                      
         CLI   5(R2),0                                                          
         BE    VKEY16                                                           
         MVI   ERROR,INVALID                                                    
         MVC   QCLOSE,8(R2)                                                     
         CLI   QCLOSE,C'Y'                                                      
         BE    VKEY16                                                           
         CLI   QCLOSE,C'N'                                                      
         BNE   ERREND                                                           
*                                                                               
VKEY16   MVI   QLOCKED,C'N'        DEFAULT IS INCLUDE LOCKED JOBS               
         LA    R2,PROXLOCH                                                      
         CLI   5(R2),0                                                          
         BE    VKEY18                                                           
         MVI   ERROR,INVALID                                                    
         MVC   QLOCKED,8(R2)                                                    
         CLI   QLOCKED,C'Y'                                                     
         BE    VKEY18                                                           
         CLI   QLOCKED,C'N'                                                     
         BNE   ERREND                                                           
*                                                                               
VKEY18   MVI   QDEFOPT,C'N'        DEFAULT IS SUPPRESS DEFAULT OPTIONS          
         LA    R2,PRODEFH                                                       
         CLI   5(R2),0             TEST FOR INPUT                               
         BE    VKEY20                                                           
         MVI   ERROR,INVALID                                                    
         CLI   QREPTYPE,C'F'                                                    
         BE    ERREND                                                           
         MVC   QDEFOPT,8(R2)                                                    
         CLI   QDEFOPT,C'Y'                                                     
         BE    VKEY20                                                           
         CLI   QDEFOPT,C'N'                                                     
         BNE   ERREND                                                           
*                                                                               
VKEY20   MVI   QWCOPT,C'N'         DEFAULT IS OMIT WORKCODE OPTIONS             
         LA    R2,PROWCH                                                        
         CLI   5(R2),0                                                          
         BE    VKEY22                                                           
         MVI   ERROR,INVALID                                                    
         CLI   QREPTYPE,C'F'                                                    
         BE    ERREND                                                           
         MVC   QWCOPT,8(R2)                                                     
         CLI   QWCOPT,C'Y'                                                      
         BE    VKEY22                                                           
         CLI   QWCOPT,C'N'                                                      
         BNE   ERREND                                                           
*                                                                               
VKEY22   MVI   NOPRO,C'N'          DEFAULT IS TO PRINT PRODUCT                  
         LA    R2,PRONOPH                                                       
         CLI   5(R2),0                                                          
         BE    VKEY24                                                           
         CLI   8(R2),C'N'                                                       
         BE    VKEY24                                                           
         MVI   ERROR,INVALID                                                    
         CLI   8(R2),C'Y'                                                       
         BNE   ERREND                                                           
         CLI   QWCOPT,C'Y'         PRINTING WORKCODES ?                         
         BE    ERREND              YES, NOT GOOD                                
         MVC   NOPRO,8(R2)                                                      
*                                                                               
VKEY24   MVC   NOJOB,NOPRO          START WITH SAME AS PRODUCT                  
         LA    R2,PRONOJH                                                       
         CLI   5(R2),0                                                          
         BE    VKEY28                                                           
         MVI   ERROR,INVALID                                                    
         MVC   NOJOB,8(R2)                                                      
         CLI   8(R2),C'Y'                                                       
         BNE   VKEY26                                                           
         CLI   QWCOPT,C'Y'         PRINTING WORKCODES ?                         
         BE    ERREND              YES, NOT GOOD                                
         B     VKEY28                                                           
*                                                                               
VKEY26   CLI   8(R2),C'N'                                                       
         BNE   ERREND                                                           
         CLI   PRONOPH+8,C'Y'      IS PRODUCT SUPPRESSED ?                      
         BE    ERREND              YES, ERROR, MUST SUPPRESS JOB                
*                                                                               
VKEY28   MVI   QDRFT,C'N'          DEFAULT IS NO DRAFT JOBS                     
         LA    R2,PRONODH                                                       
         CLI   5(R2),0                                                          
         BE    VKEYX                                                            
         MVI   ERROR,INVALID                                                    
         MVC   QDRFT,8(R2)                                                      
         CLI   8(R2),C'Y'          VALID ENTIES ARE Y, N OR O                   
         BE    VKEYX                                                            
         CLI   QDRFT,C'N'                                                       
         BE    VKEYX                                                            
         CLI   QDRFT,C'O'                                                       
         BNE   ERREND                                                           
*                                                                               
VKEYX    B     XIT                                                              
         EJECT                                                                  
*              PRINT REPORT                                                     
         SPACE 3                                                                
*--- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---            
* SUB-ROUTINE TO VALIDATE THE FILTER FIELD                                      
* AT ENTRY, R2=A(FLDH), P1=A(OUTPUT), P2=N'OUTPUT POSITIONS                     
*                                                                               
VALFIL   NTR1  ,                                                                
         L     R3,0(R1)            R3=A(OUTPUT)                                 
         L     R5,4(R1)            R5=N'OUTPUT POSITIONS                        
         MVI   ERROR,INVALID                                                    
         LA    R4,8(R2)            R4=A(INPUT)                                  
         ZIC   R6,5(R2)                                                         
         LR    R1,R5                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     VALFIL2                                                          
         MVC   0(0,R3),SPACES      PRE-CLEAR OUTPUT TO SPACES                   
*                                                                               
VALFIL2  CLI   0(R4),C'*'          TEST FOR NEGATIVE FILTER                     
         BE    VALFIL4             YES                                          
         CLI   0(R4),C' '          TEST ANY FILTER IN POSITION                  
         BE    VALFIL6             YES                                          
         CLI   0(R4),C'.'          TEST FOR NO FILTER VALUE                     
         BE    VALFIL3                                                          
         LA    R0,1                                                             
         GOTO1 TSTAN,(R4)                                                       
*                                                                               
VALFIL3  MVC   0(1,R3),0(R4)       SET VALUE                                    
         B     VALFIL6                                                          
*                                                                               
VALFIL4  LA    R4,1(R4)            NEXT INPUT CHARACTER                         
         SH    R6,=H'1'                                                         
         BZ    ERREND                                                           
         LA    R0,1                                                             
         GOTO1 TSTAN,(R4)                                                       
         MVC   0(1,R3),0(R4)                                                    
         NI    0(R3),X'FF'-X'40'   TURN OFF UPPER CASE BIT                      
*                                                                               
VALFIL6  LA    R3,1(R3)                                                         
         LA    R4,1(R4)                                                         
         SH    R6,=H'1'                                                         
         BZ    VALFILX             NO MORE INPUT TO EDIT                        
         BCT   R5,VALFIL2                                                       
         B     ERREND              EXCEEDED FILTER LIMIT                        
*                                                                               
VALFILX  B      XIT                                                             
         EJECT                                                                  
* SUB-ROUTINE TO CHECK FOR ALPHANUMERIC FIELD                                   
*                                                                               
* AT ENTRY, R0=N'BYTES TO CHECK, R1=A(STRING TO CHECK)                          
*                                                                               
TSTAN    MVI   ERROR,INVALID                                                    
*                                                                               
TSTAN1   CLI   0(R1),C'0'                                                       
         BL    *+16                                                             
         CLI   0(R1),C'9'                                                       
         BH    ERREND                                                           
         B     TSTAN2                                                           
         CLI   0(R1),C'A'                                                       
         BL    ERREND                                                           
         CLI   0(R1),C'I'                                                       
         BNH   TSTAN2              CHARACTER IS BETWEEN A-I                     
         CLI   0(R1),C'J'                                                       
         BL    ERREND                                                           
         CLI   0(R1),C'R'                                                       
         BNH   TSTAN2                                                           
         CLI   0(R1),C'S'                                                       
         BL    ERREND                                                           
         CLI   0(R1),C'Z'                                                       
         BH    ERREND                                                           
*                                                                               
TSTAN2   LA    R1,1(R1)            NEXT CHARACTER IN FIELD                      
         BCT   R0,TSTAN1                                                        
*                                                                               
         BR    RE                                                               
         EJECT                                                                  
*--- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---            
PREP     NTR1                                                                   
         BAS   RE,INITREP          INITIALIZE OUR BUFFER                        
         LA    R2,HOOK                                                          
         ST    R2,HEADHOOK                                                      
         L     R2,=A(MYSPECS)                                                   
         A     R2,RELO                                                          
         ST    R2,SPECS                                                         
*                                                                               
         LH    R2,=Y(GOXBLOCK-SUBSYSD)                                          
         AR    R2,R9                                                            
         ST    R2,AGOXBLOC                                                      
*                                                                               
         LH    R2,=Y(GOBBLOCK-SUBSYSD)                                          
         AR    R2,R9                                                            
         ST    R2,AGOBBLOC                                                      
*                                                                               
         LH    R2,=Y(GOPANBLK-SUBSYSD)                                          
         AR    R2,R9                                                            
         ST    R2,APANBLOC                                                      
*                                                                               
         SPACE 1                                                                
         MVC   GOADM,DATAMGR       SET UP GOBLOCK                               
         MVC   GOSELCUL,CUL                                                     
         MVI   GOSELLEV,0          NOW GET EFFECTIVE                            
         MVC   GOABUFF,AGBBUFF                                                  
         MVC   GOLBUFF,=F'30000'                                                
         MVC   GOAFROM,AFROM                                                    
         MVC   GOACOMP,ACOMP                                                    
         MVC   GOALEDG,ALEDG                                                    
         MVC   GOACLI,ACLI                                                      
         MVC   GOAPRO,APROD                                                     
         MVC   GOAJOB,AJOB                                                      
         MVC   GOACOVL,COVAIL                                                   
         MVC   GOABINSR,BINSRCH                                                 
         MVC   GOAEXT,AGOXBLOC                                                  
         MVC   GOABEXT,AGOBBLOC                                                 
         MVC   GOAPAN,APANBLOC                                                  
*                                                                               
PREP2    MVC   KEY,SPACES          READ COMPANY AND SAVE                        
         MVC   KEY(1),CUL                                                       
         GOTO1 READ                                                             
         L     RE,ACOMP                                                         
         LA    RF,1000                                                          
         L     R0,AIO                                                           
         LR    R2,R0                                                            
         LH    R1,ACLENGTH-ACKEYD(R2) GET RECORD LENGTH                         
         MVCL  RE,R0                                                            
         SPACE 1                                                                
         MVC   KEY,SPACES          GET LEDGER RECORD                            
         MVC   KEY(3),CUL                                                       
         GOTO1 READ                                                             
         L     RE,ALEDG                                                         
         LA    RF,1000                                                          
         L     R0,AIO                                                           
         LR    R2,R0                                                            
         LH    R1,ACLENGTH-ACKEYD(R2) GET RECORD LENGTH                         
         MVCL  RE,R0               SAVE LEDGER RECORD                           
         SPACE 1                                                                
         MVI   FIRSTCLI,C'Y'       SET FIRST TIME SWITCHES                      
         MVI   FIRSTPRO,C'Y'                                                    
         LA    R3,KEY+3                                                         
         LA    RE,QCLI                                                          
         CLC   QCLI,SPACES         TEST FOR 'ALL' CLIENTS                       
         BNE   *+8                                                              
         LA    RE,=CL6'A'          YES-FORCE START AT FIRST ONE                 
         MVC   0(6,R3),0(RE)                                                    
         B     PREP4               READ FIRST RECORD                            
         SPACE 1                                                                
SKIP     MVC   KEY,SAVELACC        SKIP TO NEXT ACCOUNT                         
         MVI   KEY+15,X'FF'                                                     
         SPACE 1                                                                
PREP4    GOTO1 HIGH                                                             
         ZIC   R1,LCONBRK          CHECK FOR END OF REQUEST                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   KEY(0),KEYSAVE                                                   
         BNE   XIT                                                              
         MVC   SAVELACC,KEY                                                     
         SPACE 1                                                                
LEVTEST  ZIC   R1,LCLI             ACCOUNT - WHAT TYPE?                         
         LA    R1,KEY+3(R1)                                                     
         CLI   0(R1),X'41'                                                      
         BL    CLIHOOK                                                          
         ZIC   R0,LPRO                                                          
         AR    R1,R0                                                            
         CLI   0(R1),X'41'                                                      
         BL    PRDHOOK                                                          
         B     JOBHOOK                                                          
         SPACE 1                                                                
CLIHOOK  GOTO1 SETCLI                                                           
         GOTO1 SETNAME,DMCB,AIO,SAVECNAM                                        
         L     RE,ACLI                                                          
         LA    RF,2000                                                          
         L     R0,AIO                                                           
         LR    R2,R0                                                            
         LH    R1,ACLENGTH-ACKEYD(R2) GET RECORD LENGTH                         
         MVCL  RE,R0                                                            
         MVC   GOSELCLI,CLICODE                                                 
         MVC   GOSELPRO,SPACES                                                  
         MVC   GOSELJOB,SPACES                                                  
         MVI   RECTYPE,C'C'        NOTE CLIENT RECORD                           
         MVI   FORCEHED,C'Y'       BREAK PAGE FOR NEW CLIENT                    
*                                                                               
         OC    QOGROUP,QOGROUP     TEST FOR OFFICE GROUP FILTER                 
         BZ    CLIHOOK1            NO                                           
         GOTO1 SETOG                                                            
         MVC   KEY,SAVELACC        RESTORE CLIENT KEY                           
         CLC   QOGROUP,EFFOFG                                                   
         BNE   CLIHOOK2                                                         
         GOTO1 READ                RE-READ CLIENT                               
*                                                                               
CLIHOOK1 OC    QOFFICE,QOFFICE     TEST FOR OFFICE FILTER                       
         BZ    *+14                NO                                           
         CLC   QOFFICE,EFFOFFC                                                  
         BNE   CLIHOOK2                                                         
         BAS   RE,GETADDR                                                       
         BAS   RE,DOREP                                                         
*                                                                               
CLIHOOK2 CLI   FIRSTCLI,C'Y'       TEST FIRST TIME FOR CLIENT                   
         BNE   SKIP                NO-NEXT ACCOUNT                              
         MVI   FIRSTCLI,C'N'       TURN OFF SWITCH                              
         LA    RE,QPROD                                                         
         CLC   QPROD,SPACES        TEST PRODUCT REQUESTED                       
         BNE   *+8                 YES                                          
         LA    RE,=CL6'A'          FORCE FIRST PRODUCT                          
         ZIC   R1,LCLI                                                          
         LA    R1,KEY+3(R1)                                                     
         MVC   0(6,R1),0(RE)                                                    
         B     PREP4                                                            
         SPACE 1                                                                
PRDHOOK  CLI   NOPRO,C'Y'          ELIMINATE PRODUCT LEVEL ?                    
         BE    SKIP                YES                                          
         GOTO1 SETPROD                                                          
*                                                                               
         OC    QOGROUP,QOGROUP     TEST FOR OFFICE GROUP                        
         BZ    PRDHOOK1                                                         
         GOTO1 SETOG                                                            
         MVC   KEY,SAVELACC        RESTORE PRODUCT KEY                          
         CLC   QOGROUP,EFFOFG      APPLY OFFICE GROUP FILTER                    
         BNE   PRDHOOK2            REJECT ITEM                                  
         GOTO1 READ                RE-READ PRODUCT                              
*                                                                               
PRDHOOK1 OC    QOFFICE,QOFFICE     TEST FOR OFFICE                              
         BZ    PRODHOOK3                                                        
         CLC   QOFFICE,EFFOFFC     FILTER ON OFFICE                             
         BE    PRODHOOK3                                                        
*                                                                               
PRDHOOK2 ZIC   R1,LCLIPRO                                                       
         LA    R1,KEY+3(R1)                                                     
         MVI   0(R1),X'FF'         FORCE NEXT PRODUCT                           
         B     PREP4                                                            
         SPACE 1                                                                
PRODHOOK3 L    RE,APROD            SAVE PRODUCT                                 
         LA    RF,2000                                                          
         L     R2,AIO                                                           
         LR    R0,R2                                                            
         LH    R1,ACLENGTH-ACKEYD(R2) GET RECORD LENGTH                         
         MVCL  RE,R0                                                            
         GOTO1 SETNAME,DMCB,APROD,SAVEPNAM                                      
         MVC   GOSELPRO,PRODCODE                                                
         MVC   GOSELJOB,SPACES                                                  
         MVI   RECTYPE,C'P'        NOTE PRODUCT RECORD                          
         BAS   RE,GETADDR                                                       
         BAS   RE,DOREP                                                         
*                                                                               
         CLI   FIRSTPRO,C'Y'       TEST FOR FIRST PRODUCT                       
         BNE   SKIP                NO                                           
         MVI   FIRSTPRO,C'N'                                                    
         LA    RE,QJOB                                                          
         CLC   QJOB,SPACES         TEST FOR SPECIFIC JOB                        
         BNE   *+8                                                              
         LA    RE,=CL6'A'                                                       
         ZIC   R1,LCLIPRO                                                       
         LA    R1,KEY+3(R1)                                                     
         MVC   0(6,R1),0(RE)                                                    
         B     PREP4                                                            
         SPACE 1                                                                
JOBHOOK  GOTO1 SETJOB              SET FILTERS                                  
         CLI   NOJOB,C'Y'          ELIMINATE JOB LEVEL ?                        
         BE    SKIP                YES                                          
         CLI   NOPRO,C'Y'          NO, ELIMINATE PRODUCT LEVEL ?                
         BE    SKIP                YES, SKIP JOB THEN ALSO                      
*                                                                               
         L     R6,AIO                                                           
         TM    ACTRSTAT-ACTRECD(R6),ACTSDRFT                                    
         BO    JOBHOOK0                                                         
         CLI   QDRFT,C'O'          ONLY WANT DRAFTS                             
         BE    SKIP                SO SKIP THIS ONE                             
         B     JOBHOOK1                                                         
*                                                                               
JOBHOOK0 CLI   QDRFT,C'N'          DON'T WANT DRAFTS                            
         BE    SKIP                SO SKIP THIS ONE                             
*                                                                               
JOBHOOK1 BAS   RE,GETADDR                                                       
         MVI   CLOSED,C' '                                                      
         L     R6,ASTATEL                                                       
         USING ACSTATD,R6                                                       
         TM    ACSTSTAT,X'40'      NO, IS JOB CLOSED ?                          
         BNO   JOBHOOK2            NO                                           
         MVI   CLOSED,C'C'         YES, INDICATE IT                             
         CLI   QCLOSE,C'Y'         INCLUDE CLOSED JOB ?                         
         BNE   SKIP                NO, SKIP IT                                  
*                                                                               
JOBHOOK2 CLI   QLOCKED,C'Y'        EXCLUDE LOCKED JOBS ?                        
         BNE   JOBHOOK4            NO                                           
         TM    ACSTSTAT,X'20'      YES, IS JOB LOCKED ?                         
         BO    SKIP                YES, SKIP IT                                 
*                                                                               
JOBHOOK4 DS    0H                                                               
*                                                                               
         MVC   BYTE,QFILT          FILTER 1                                     
         GOTO1 APPFIL,EFF1                                                      
         BNE   SKIP                                                             
*                                                                               
         MVC   BYTE,QFILT2         FILTER 2                                     
         GOTO1 APPFIL,EFF2                                                      
         BNE   SKIP                                                             
*                                                                               
         MVC   BYTE,QFILT3         FILTER 3                                     
         GOTO1 APPFIL,EFF3                                                      
         BNE   SKIP                                                             
*                                                                               
         MVC   BYTE,QFILT4         FILTER 4                                     
         GOTO1 APPFIL,EFF4                                                      
         BNE   SKIP                                                             
*                                                                               
         MVC   BYTE,QFILT5         FILTER 5                                     
         GOTO1 APPFIL,EFF5                                                      
         BNE   SKIP                                                             
*                                                                               
         L     RE,AJOB             SHOW JOB SETTINGS                            
         LA    RF,2000                                                          
         L     R2,AIO                                                           
         LR    R0,R2                                                            
         LH    R1,ACLENGTH-ACKEYD(R2) GET RECORD LENGTH                         
         MVCL  RE,R0                                                            
         GOTO1 SETNAME,DMCB,AJOB,SAVEJNAM                                       
         ZIC   R1,LCLIPRO                                                       
         LA    R1,KEY+3(R1)                                                     
         MVC   GOSELJOB,0(R1)                                                   
         MVI   RECTYPE,C'J'        JOB RECORD                                   
         CLI   QREPTYPE,C'F'       TEST FILE DATA ONLY                          
         BE    JOBHOOK6                                                         
         CLI   QWCOPT,C'Y'         TEST PROCESS W/C OPTION                      
         BNE   JOBHOOK6            NO-SKIP LOOKING AT ESTIMATES/CHARGES         
*                                                                               
         BAS   RE,DOGETOPT                                                      
         BAS   RE,LOOKUP                                                        
*                                                                               
JOBHOOK6 BAS   RE,DOREP                                                         
         B     SKIP                                                             
         EJECT                                                                  
         SPACE 1                                                                
*--- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---            
** ROUTINE CHECKS FILTERS                                                       
*--- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---            
APPFIL   CLI   BYTE,0              TEST FOR NO REQUESTED FILTER                 
         BER   RE                  YES-EXIT WITH CC=EQ                          
         CLI   BYTE,C' '           TEST FOR BLANK=TAKE EVERYTHING               
         BE    APPFILY                                                          
         CLI   BYTE,C'.'           TEST FOR NO FILTER ON JOB                    
         BNE   APPFIL2                                                          
         CLI   0(R1),C' '          FILTER POSITION CANNOT BE A VALUE            
         BH    APPFILN                                                          
         B     APPFILY                                                          
*                                                                               
APPFIL2  TM    BYTE,X'40'          TEST FOR POSITIVE FILTER                     
         BZ    APPFIL4                                                          
         CLC   BYTE,0(R1)          TEST FOR MATCH ON FILTER                     
         BE    APPFILY                                                          
         B     APPFILN                                                          
*                                                                               
APPFIL4  OI    BYTE,X'40'          RESTORE UPPER CASE BIT                       
         CLC   BYTE,0(R1)          TEST FOR DIFFERENCE                          
         BNE   APPFILY                                                          
         B     APPFILN                                                          
*                                                                               
APPFILY  CR    RB,RB                                                            
         B     APPFILX                                                          
*                                                                               
APPFILN  LTR   RB,RB                                                            
*                                                                               
APPFILX  BR    RE                                                               
*--- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---            
         SPACE 2                                                                
DOREP    NTR1                                                                   
         LA    RF,FILE                                                          
         CLI   QREPTYPE,C'F'                                                    
         BE    DOREP2                                                           
         LA    RF,COMB                                                          
         CLI   QREPTYPE,C'C'                                                    
         BE    DOREP2                                                           
         LA    RF,EFF                                                           
*                                                                               
DOREP2   BASR  RE,RF                                                            
         B     XIT                                                              
         SPACE 2                                                                
DOGETOPT NTR1                                                                   
         GOTO1 GETOPT,DMCB,GOBLOCK                                              
         B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO PRINT THE FILE DATA REPORT                                     
*                                                                               
FILE     NTR1                                                                   
         MVI   RCSUBPRG,0                                                       
         GOTO1 SPOOL,DMCB,(R8)     SKIP A LINE                                  
         L     R2,APRTTAB                                                       
         USING PRTD,R2                                                          
         BAS   RE,NUMBER                                                        
         MVC   FILACC,BLOCK+2      EXTRACT ACCOUNT NUMBER                       
         BAS   RE,NAME                                                          
         LH    R0,BLOCK            GET LENGTH OF OUTPUT                         
         LTR   R0,R0                                                            
         BZ    FILE2               NO NAME DATA PRESENT                         
         LA    R3,L'FILNAME                                                     
         GOTO1 CHOPPER,DMCB,((R0),BLOCK+2),((R3),FILNAME),(C'P',2),0            
*                                                                               
FILE2    BAS   RE,RECV                                                          
         MVC   FILRECV,BLOCK+2                                                  
         BAS   RE,COST                                                          
         MVC   FILCOST,BLOCK+2                                                  
*                                                                               
FILE4    LA    R4,FILTAB                                                        
         LA    R5,FILENT                                                        
*                                                                               
FILE5    L     RF,0(R4)                                                         
         A     RF,RELO                                                          
         BASR  RE,RF               CALL ROUTINE TO DATA                         
         OC    BLOCK(2),BLOCK      TEST IF ANYTHING TO PRINT                    
         BZ    FILE6               NO                                           
*                                                                               
         LH    R0,BLOCK                                                         
         GOTO1 CHOPPER,DMCB,((R0),BLOCK+2),(L'FILOTH,FILOTH),(C'P',6),0         
         L     RF,DMCB+8           GET N'LINES                                  
         LA    RE,L'P                                                           
         MR    RE,RE                                                            
         AR    R2,RF               BUMP OUTPUT POINTER                          
*                                                                               
FILE6    LA    R4,L'FILTAB(R4)                                                  
         BCT   R5,FILE5                                                         
*                                                                               
         GOTO1 COMM,DMCB,(R2),L'FILOTH,FILOTH                                   
*                                                                               
         BAS   RE,PRTTAB           PRINT THE TABLE                              
         B     XIT                                                              
         SPACE 2                                                                
* TABLE OF FILE DATA ROUTINES                                                   
*                                                                               
FILTAB   DS    0F                                                               
         DC    A(ADDR)                                                          
         DC    A(BILLGRP)                                                       
         DC    A(UNIT)                                                          
         DC    A(SALES)                                                         
         DC    A(CLOSE)                                                         
         DC    A(OPEN)                                                          
         DC    A(FILT)                                                          
         DC    A(ANAL)                                                          
         DC    A(COSTGRP)                                                       
         DC    A(BLPR)                                                          
         DC    A(NARR)                                                          
         DC    A(XCOMM)                                                         
         DC    A(JLONG)                                                         
FILENT   EQU   (*-FILTAB)/L'FILTAB                                              
         EJECT                                                                  
* SUB-ROUTINE TO PRINT THE COMBINED FORMAT REPORT                               
*                                                                               
COMB     NTR1                                                                   
         MVI   RCSUBPRG,1                                                       
         GOTO1 SPOOL,DMCB,(R8)     SKIP A LINE                                  
         L     R2,APRTTAB                                                       
         USING PRTD,R2                                                          
         BAS   RE,NUMBER                                                        
         MVC   COMACCN(16),BLOCK+2 EXTRACT ACCOUNT NUMBER                       
         BAS   RE,NAME                                                          
         LH    R0,BLOCK            GET LENGTH OF OUTPUT                         
         LTR   R0,R0                                                            
         BZ    COMB2               NO NAME DATA PRESENT                         
         LA    R3,COMACCN+L'P      PRINT POSITION                               
         GOTO1 CHOPPER,DMCB,((R0),BLOCK+2),(L'COMACCN,(R3)),(C'P',2),0          
*                                                                               
COMB2    LA    R4,COMBTAB                                                       
         LA    R5,COMBENT                                                       
*                                                                               
COMB4    L     RF,0(R4)                                                         
         A     RF,RELO                                                          
         BASR  RE,RF               CALL ROUTINE TO DATA                         
         OC    BLOCK(2),BLOCK      TEST IF ANYTHING TO PRINT                    
         BZ    COMB6               NO                                           
*                                                                               
         LH    R0,BLOCK                                                         
         GOTO1 CHOPPER,DMCB,((R0),BLOCK+2),(L'COMOTH,COMOTH),(C'P',6),0         
         L     RF,DMCB+8           GET N'LINES                                  
         LA    RE,L'P                                                           
         MR    RE,RE                                                            
         AR    R2,RF               BUMP OUTPUT POINTER                          
*                                                                               
COMB6    LA    R4,L'COMBTAB(R4)                                                 
         BCT   R5,COMB4                                                         
*                                                                               
         GOTO1 COMM,DMCB,(R2),L'COMOTH,COMOTH                                   
*                                                                               
         CLI   RECTYPE,C'J'        TEST FOR JOB RECORD                          
         BE    COMB14                                                           
*                                                                               
         BAS   RE,DOGETOPT         GET THE OPTIONS                              
         CLI   NOPRO,C'Y'          EXCLUDE PRODUCT AND JOB ?                    
         BNE   COMB12              NO, SEE IF JOB                               
*                                                                               
COMB8    L     R2,APRTTAB          YES, GET OPTIONS                             
         ST    R2,APRTLIN                                                       
         BAS   RE,PROCOPT                                                       
*                                                                               
COMB10   BAS   RE,PRTTAB           PRINT THE TABLE                              
         B     COMBX                                                            
*                                                                               
COMB12   CLI   NOJOB,C'Y'          EXCLUDE JOB ?                                
         BNE   COMB10              NO                                           
         CLI   RECTYPE,C'P'        YES, ARE WE AT PRODUCT ?                     
         BNE   COMB10              NO                                           
         B     COMB8               YES                                          
*                                                                               
COMB14   L     R2,APRTTAB                                                       
         ST    R2,APRTLIN          INITIALIZE PRINT LINE POINTER                
         BAS   RE,JOBOPT           PROCESS JOB AND WORKCODE OPTIONS             
         BAS   RE,PRTTAB           PRINT THE TABLE                              
*                                                                               
COMBX    B     XIT                                                              
         SPACE 2                                                                
* TABLE OF FILE DATA ROUTINES                                                   
*                                                                               
COMBTAB  DS    0F                                                               
         DC    A(RECV2)                                                         
         DC    A(COST2)                                                         
         DC    A(ADDR)                                                          
         DC    A(BILLGRP)                                                       
         DC    A(UNIT)                                                          
         DC    A(SALES)                                                         
         DC    A(CLOSE)                                                         
         DC    A(OPEN)                                                          
         DC    A(FILT)                                                          
         DC    A(ANAL)                                                          
         DC    A(COSTGRP)                                                       
         DC    A(BLPR)                                                          
         DC    A(NARR)                                                          
         DC    A(XCOMM)                                                         
         DC    A(JLONG)                                                         
COMBENT  EQU   (*-COMBTAB)/L'COMBTAB                                            
         EJECT                                                                  
* SUB-ROUTINE TO PRINT THE EFFECTIVE OPTIONS FORMAT REPORT                      
*                                                                               
EFF      NTR1                                                                   
         MVI   RCSUBPRG,2                                                       
         GOTO1 SPOOL,DMCB,(R8)                                                  
         L     R2,APRTTAB                                                       
         USING PRTD,R2                                                          
         BAS   RE,NUMBER                                                        
         MVC   EFFACC,BLOCK+2                                                   
         BAS   RE,NAME                                                          
         MVC   EFFNAME,BLOCK+2                                                  
         CLI   RECTYPE,C'J'        TEST FOR JOB                                 
         BE    EFF8                YES                                          
*                                                                               
         BAS   RE,DOGETOPT         GET THE OPTIONS                              
         CLI   NOPRO,C'Y'          EXCLUDE PRODUCT AND JOB ?                    
         BNE   EFF6                NO, SEE IF JOB                               
*                                                                               
EFF02    L     R2,APRTTAB          YES, GET OPTIONS                             
         ST    R2,APRTLIN                                                       
         BAS   RE,PROCOPT                                                       
*                                                                               
EFF04    BAS   RE,PRTTAB           PRINT THE TABLE                              
         B     EFFX                                                             
*                                                                               
EFF6     CLI   NOJOB,C'Y'          EXCLUDE JOB ?                                
         BNE   EFF04               NO                                           
         CLI   RECTYPE,C'P'        YES, ARE WE AT PRODUCT ?                     
         BNE   EFF04               NO                                           
         B     EFF02               YES                                          
*                                                                               
EFF8     ST    R2,APRTLIN          INITIALIZE PRINT LINE POINTER                
         BAS   RE,JOBOPT           GET THE OPTIONS FOR JOB/WORKCODES            
*                                                                               
         BAS   RE,PRTTAB                                                        
EFFX     B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO PROCESS OPTIONS FOR THE JOB AND RELATED WORKCODE               
* OVERRIDES                                                                     
*                                                                               
JOBOPT   NTR1                                                                   
         MVI   WHICHREC,C'J'                                                    
         MVC   GOSELWC,SPACES                                                   
         BAS   RE,DOGETOPT                                                      
         BAS   RE,PROCOPT                                                       
         CLI   QWCOPT,C'Y'                                                      
         BNE   XIT                                                              
         MVI   WHICHREC,C'W'                                                    
         MVC   SAVEOPT,GOPTIONS                                                 
*                                                                               
         SR    R1,R1                                                            
         L     R3,ACOLTAB                                                       
         USING JBCOLD,R3                                                        
         L     R5,AJOBLOCK                                                      
         USING JBLOCKD,R5                                                       
         CLI   JBNEWEST,JBMCSQ                                                  
         BE    JOBOPT6                                                          
         ICM   R1,3,JBNROWS                                                     
         BZ    XIT                                                              
*                                                                               
JOBOPT2  CLI   JBCOLTYP,JBCOLTWC   IS THIS A WORKCODE ?                         
         BNE   JOBOPT4             NO                                           
         MVC   GOSELWC,JBCOLWC     YES, PASS IT TO GETOPT                       
         MVC   OPTWC,GOSELWC        AND SAVE FOR OPTIONS                        
         LA    R4,OPTWCNAM         ADDRESS AREA FOR WORKCODE NAME               
         BAS   RE,LUPWORK          AND PLUG IN NAME                             
         BAS   RE,DOGETOPT                                                      
         BAS   RE,PROCOPT                                                       
*                                                                               
JOBOPT4  AH    R3,JBLCOL           GET NEXT ENTRY                               
         BCT   R1,JOBOPT2                                                       
         B     XIT                                                              
*                                                                               
         USING MJETABD,R3                                                       
JOBOPT6  CLI   MJETTYP,MJETTEQ     AT END?                                      
         BE    XIT                 YES                                          
         CLI   MJETTYP,MJETTWQ     AT WORKCODE?                                 
         BNE   JOBOPT8             NO                                           
         MVC   GOSELWC,MJETWCD     YES, PASS IT TO GETOPT                       
         MVC   OPTWC,GOSELWC        AND SAVE FOR OPTIONS                        
         LA    R4,OPTWCNAM         ADDRESS AREA FOR WORKCODE NAME               
         BAS   RE,LOOKWCN          AND PLUG IN NAME                             
         BAS   RE,DOGETOPT                                                      
         BAS   RE,PROCOPT                                                       
*                                                                               
JOBOPT8  IC    R1,MJETLEN          GET NEXT TABLE ENTRY                         
         AR    R3,R1                                                            
         B     JOBOPT6                                                          
         DROP  R2,R3,R5                                                         
         EJECT                                                                  
* SUB-ROUTINE TO PROCESS GOBLOCK - SHOWS ALL NON-DEFAULT OPTIONS                
* VALUES.  CALLED FROM JOBOPT                                                   
*                                                                               
PROCOPT  NTR1                                                                   
         L     R3,APRTLIN                                                       
         USING PRTD,R3                                                          
         L     R5,AOPTTAB          PICK OFF FIELDS FROM                         
         USING OPTBD,R5            OPTIONS FIELD                                
         SPACE 1                                                                
PO2      CLI   OPTBOPN,X'00'       IF WE ARE ALL DONE....                       
         BE    XIT                                                              
         SPACE 1                                                                
         TM    OPTBSHOW,B'00000011'  TEST VALID FOR WC                          
         BZ    PO4                 NO                                           
         TM    OPTBSHOW,B'11111100'  TEST ALSO VALID FOR SOMETHING ELSE         
         BNZ   PO4                 YES                                          
         CLI   WHICHREC,C'W'       TEST WORKCOD CALL                            
         BNE   PO30                NO-SKIP THEM                                 
         SPACE 1                                                                
PO4      LH    R2,OPTBDISP         DISP INTO GOBLOCK                            
         SPACE 1                                                                
PO6      LA    R4,GOPTIONS         R4=A(DATA IN GOPTIONS)                       
*                                                                               
         TM    OPTBIND,OPTBEXT                                                  
         BNO   *+8                                                              
         L     R4,AGOXBLOC                                                      
*                                                                               
         TM    OPTBIND,OPTBBIL                                                  
         BNO   *+8                                                              
         L     R4,AGOBBLOC                                                      
*                                                                               
         TM    OPTBIND,OPTBPAN                                                  
         BNO   *+8                                                              
         L     R4,APANBLOC                                                      
*                                                                               
         AR    R4,R2                                                            
         ZIC   R1,OPTBMAX          (MAX LENGTH)                                 
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         OC    0(0,R4),0(R4)       ANY ACTIVITY?                                
         BNZ   *+12                                                             
         TM    OPTBIND,OPTBZERO    TEST IF ZERO IS VALID                        
         BZ    PO30                NO                                           
         CLI   WHICHREC,C'W'       IF WE'RE DOING WORK CODES                    
         BNE   PO10                                                             
         LA    RF,SAVEOPT                                                       
         AR    RF,R2                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R4),0(RF)       SEE IF VALUE DIFFERS FROM PARENT             
         BE    PO30                                                             
         SPACE 1                                                                
PO10     LA    R2,OPTVALH                                                       
         MVC   OPTVAL,SPACES                                                    
         GOTO1 VDISOPT,DMCB,(OPTBOPN,GOBLOCK),OPTVALH                           
         SPACE 1                                                                
         MVC   OPTDESC(L'OPTBSHRT),OPTBSHRT                                     
         LA    RE,OPTDESC+L'OPTBSHRT                                            
         MVI   0(RE),C'-'                                                       
         MVC   1(L'OPTBDESC,RE),OPTBDESC                                        
         ZIC   RE,OPTBOPN          DISPLACE INTO 'FROM' BUFFER                  
         SLL   RE,2                                                             
         A     RE,GOAFROM                                                       
         MVC   OPTFROM,0(RE)       AND SHOW FROM CODE                           
*                                                                               
         LA    R0,LEVELS                                                        
         LA    RF,LEVTAB                                                        
         CLC   OPTFROM(1),0(RF)    MATCH ON ONE CHARACTER GETOPT LEVEL          
         BE    *+16                                                             
         LA    RF,L'LEVTAB(RF)                                                  
         BCT   R0,*-14                                                          
         B     PO11                                                             
*                                                                               
         MVC   OPTFROM(2),1(RF)    MOVE OUT TWO CHARACTER LABEL                 
*                                                                               
PO11     CLI   OPTFROM,C'D'        TEST FOR DEFAULT VALUE                       
         BNE   *+12                NO                                           
         CLI   QDEFOPT,C'Y'        TEST TO SHOW DEFAULT OPTIONS                 
         BNE   PO30                NO                                           
         SPACE 1                                                                
PO12     CLI   WHICHREC,C'W'       IF WE ARE HANDLING WORK CODES                
         BNE   PO20                                                             
         CLI   OPTFROM+3,C'*'      LOOK FOR WORK ACTIVITY                       
         BE    PO30                                                             
         SPACE 1                                                                
PO20     CLI   QREPTYPE,C'E'                                                    
         BE    PO24                                                             
         CLI   WHICHREC,C'W'       TEST FOR WORKCODE                            
         BNE   PO22                                                             
         MVC   COMOPT(L'OPTWC),OPTWC GET WORK CODE AND NAME                     
         MVC   COMOPT+L'OPTWC+1(L'OPTWCNAM),OPTWCNAM                            
         LA    R3,L'P(R3)                                                       
*                                                                               
PO22     MVC   COMOPT,OPTDESC                                                   
         MVC   COMVAL,OPTVAL                                                    
         MVC   COMFROM,OPTFROM                                                  
         B     PO28                                                             
         SPACE 1                                                                
PO24     CLI   WHICHREC,C'W'       TEST FOR WORKCODE                            
         BNE   PO26                                                             
         MVC   EFFOPT(L'OPTWC),OPTWC GET WORK CODE AND NAME                     
         MVC   EFFOPT+L'OPTWC+1(L'OPTWCNAM),OPTWCNAM                            
         LA    R3,L'P(R3)                                                       
*                                                                               
PO26     MVC   EFFOPT,OPTDESC                                                   
         MVC   EFFVAL,OPTVAL                                                    
         MVC   EFFFROM,OPTFROM                                                  
         SPACE 1                                                                
PO28     LA    R3,L'P(R3)          NEXT PRINT LINE                              
         ST    R3,APRTLIN          SAVE UPDATED PRINT POSITION                  
         SPACE 1                                                                
PO30     LA    R5,OPTBL(R5)                                                     
         B     PO2                                                              
         DROP  R3,R5                                                            
         EJECT                                                                  
* SUB-ROUTINES TO EXTRACT FILE DATA TO A COMMON AREA                            
*                                                                               
* ON EXIT, BLOCK(2)=L'EXTRACTED DATA                                            
*          BLOCK+2 =A(EXTRACTED DATA)                                           
*                                                                               
NUMBER   ST    RE,SAVERE                                                        
         MVC   BLOCK(2),=H'14'                                                  
         MVC   BLOCK+2(16),SPACES                                               
         L     R6,AIO                                                           
         USING ACKEYD,R6                                                        
         LA    RE,BLOCK+2          RE=A(OUTPUT STRING)                          
         LA    RF,ACKEYACC+3       RF=A(ACCOUNT NUMBER)                         
*                                                                               
         ZIC   R1,LCLI                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(RF)                                                    
         LA    RE,2(R1,RE)         NEXT OUTPUT POSITION                         
         LA    RF,1(R1,RF)         NEXT KEY POSITION                            
         CLI   RECTYPE,C'C'                                                     
         BE    NUMBERX                                                          
*                                                                               
         ZIC   R1,LPRO                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(RF)                                                    
         LA    RE,2(R1,RE)                                                      
         LA    RF,1(R1,RF)                                                      
         CLI   RECTYPE,C'P'                                                     
         BE    NUMBERX                                                          
*                                                                               
         ZIC   R1,LJOB                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(RF)                                                    
         LA    RE,2(R1,RE)                                                      
         MVC   0(1,RE),CLOSED                                                   
*                                                                               
NUMBERX  L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
NAME     ST    RE,SAVERE                                                        
         XC    BLOCK(2),BLOCK      CLEAR LENGTH                                 
         MVC   BLOCK+2(36),SPACES                                               
         ICM   R6,15,ANAMEL                                                     
         BZR   RE                                                               
         USING ACNAMED,R6                                                       
         ZIC   R1,ACNMLEN                                                       
         SH    R1,=Y(ACNMNAME-ACNAMED+1)                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BLOCK+2(0),ACNMNAME                                              
         LA    R1,1(R1)                                                         
         STH   R1,BLOCK                                                         
         L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
ADDR     NTR1                                                                   
         XC    BLOCK(2),BLOCK                                                   
         ICM   R6,15,AADDREL                                                    
         BZ    ADDRX                                                            
         USING ACADDD,R6                                                        
         LA    R2,BLOCK+2                                                       
         MVC   0(16,R2),=C'BILLING ADDRESS='                                    
         LA    R3,16                                                            
         LA    R2,16(R2)                                                        
         ZIC   R1,ACADLEN                                                       
         SH    R1,=Y(ACADADD-ACADDD+1)                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),ACADADD                                                  
         LA    R3,1(R1,R3)                                                      
         GOTO1 SQUASHER,DMCB,BLOCK+2,(R3)                                       
         L     R2,DMCB+4           GET LEN RETURNED BY SQUASHER                 
         STH   R2,BLOCK                                                         
*                                                                               
ADDRX    B     XIT                                                              
         SPACE 2                                                                
RECV     XC    BLOCK(2),BLOCK                                                   
         MVC   BLOCK+2(12),SPACES                                               
         ICM   R6,15,APROFEL                                                    
         BZR   RE                                                               
         USING ACPROFD,R6                                                       
         OC    ACPRRECV,ACPRRECV   TEST FOR RECEIVABLES A/C                     
         BZR   RE                                                               
         MVC   BLOCK+2(12),ACPRRECV+3 EXTRACT ACCOUNT CODE                      
         MVC   BLOCK(2),=H'12'                                                  
         BR    RE                                                               
         SPACE 2                                                                
RECV2    XC    BLOCK(2),BLOCK                                                   
         ICM   R6,15,APROFEL                                                    
         BZR   RE                                                               
         USING ACPROFD,R6                                                       
         OC    ACPRRECV,ACPRRECV   TEST FOR RECEIVABLES A/C                     
         BZR   RE                                                               
         MVC   BLOCK+2(19),=C'RECEIVABLE ACCOUNT='                              
         MVC   BLOCK+21(12),ACPRRECV+3 EXTRACT ACCOUNT CODE                     
         MVC   BLOCK(2),=H'31'                                                  
         BR    RE                                                               
         SPACE 2                                                                
COST     XC    BLOCK(2),BLOCK                                                   
         MVC   BLOCK+2(12),SPACES                                               
         ICM   R6,15,APROFEL                                                    
         BZR   RE                                                               
         USING ACPROFD,R6                                                       
         OC    ACPRCOST,ACPRCOST   TEST FOR COSTING A/C                         
         BZR   RE                                                               
         MVC   BLOCK+2(12),ACPRCOST+3 EXTRACT ACCOUNT CODE                      
         MVC   BLOCK(2),=H'12'                                                  
         BR    RE                                                               
         SPACE 2                                                                
COST2    XC    BLOCK(2),BLOCK                                                   
         ICM   R6,15,APROFEL                                                    
         BZR   RE                                                               
         USING ACPROFD,R6                                                       
         OC    ACPRCOST,ACPRCOST   TEST FOR COSTING A/C                         
         BZR   RE                                                               
         MVC   BLOCK+2(16),=C'COSTING ACCOUNT='                                 
         MVC   BLOCK+18(12),ACPRCOST+3 EXTRACT ACCOUNT CODE                     
         MVC   BLOCK(2),=H'28'                                                  
         BR    RE                                                               
         SPACE 2                                                                
BILLGRP  XC    BLOCK(2),BLOCK                                                   
         ICM   R6,15,APROFEL                                                    
         BZR   RE                                                               
         USING ACPROFD,R6                                                       
         CLC   ACPRGRUP,SPACES     TEST FOR BILLING GROUP                       
         BNHR  RE                  NO                                           
         MVC   BLOCK+2(14),=C'BILLING GROUP='                                   
         MVC   BLOCK+16(3),ACPRGRUP                                             
         MVC   BLOCK(2),=H'17'                                                  
         BR    RE                                                               
         SPACE 2                                                                
UNIT     XC    BLOCK(2),BLOCK                                                   
         ICM   R6,15,APROFEL                                                    
         BZR   RE                                                               
         USING ACPROFD,R6                                                       
         CLC   ACPROFFC,SPACES                                                  
         BNHR  RE                                                               
         MVC   BLOCK+2(7),=C'OFFICE='                                           
         MVC   BLOCK+9(2),ACPROFFC                                              
         MVC   BLOCK(2),=H'9'                                                   
         BR    RE                                                               
         SPACE 2                                                                
BLPR     XC    BLOCK(2),BLOCK                                                   
         ICM   R6,15,APROFEL                                                    
         BZR   RE                                                               
         USING ACPROFD,R6                                                       
         CLC   ACPRBLPR,SPACES                                                  
         BNHR  RE                                                               
         ST    RE,SAVERE                                                        
         MVC   BLOCK+2(15),=C'PRINT ON BILLS='                                  
         MVC   BLOCK+17(50),ACPRBLPR                                            
         OC    BLOCK+17(50),SPACES                                              
         GOTO1 SQUASHER,DMCB,BLOCK+2,65                                         
         L     R0,DMCB+4                                                        
         STH   R0,BLOCK                                                         
         L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
NARR     ST    RE,SAVERE                                                        
         XC    BLOCK(2),BLOCK                                                   
         ICM   R6,15,APROFEL                                                    
         BZR   RE                                                               
         USING ACPROFD,R6                                                       
         CLI   ACPRLEN,ACPRNARR-ACPROFD                                         
         BER   RE                  NO NARRATIVE PRESENT                         
         MVC   BLOCK+2(14),=C'HEAD COMMENTS='                                   
         ZIC   R1,ACPRLEN                                                       
         SH    R1,=Y(ACPRNARR-ACPROFD+1)                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BLOCK+16(0),ACPRNARR                                             
         LA    R6,18(R1)                                                        
         GOTO1 SQUASHER,DMCB,BLOCK+2,(R6)                                       
         L     R6,DMCB+4                                                        
         STH   R6,BLOCK                                                         
         L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
XCOMM    ST    RE,SAVERE                                                        
         XC    BLOCK(2),BLOCK                                                   
         ICM   R6,15,AXCOMM                                                     
         BZR   RE                                                               
         USING FFTELD,R6                                                        
         MVC   BLOCK+2(15),=C'EXTRA COMMENTS='                                  
         ZIC   R1,FFTDLEN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BLOCK+17(0),FFTDATA                                              
         LA    R6,18(R1)                                                        
         GOTO1 SQUASHER,DMCB,BLOCK+2,(R6)                                       
         L     R6,DMCB+4                                                        
         STH   R6,BLOCK                                                         
         L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
JLONG    ST    RE,SAVERE                                                        
         XC    BLOCK(2),BLOCK                                                   
         ICM   R6,15,AJLONG                                                     
         BZR   RE                                                               
         USING JLDELD,R6                                                        
         MVC   BLOCK+2(16),=C'JOB DESCRIPTION='                                 
         ZIC   R1,JLDLN                                                         
         SH    R1,=Y(JLDDESC-JLDELD+1)                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BLOCK+18(0),JLDDESC                                              
         LA    R6,18(R1)                                                        
         GOTO1 SQUASHER,DMCB,BLOCK+2,(R6)                                       
         L     R6,DMCB+4                                                        
         STH   R6,BLOCK                                                         
         L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
CLOSE    XC    BLOCK(2),BLOCK                                                   
         ICM   R6,15,AJOBEL                                                     
         BZR   RE                                                               
         USING ACJOBD,R6                                                        
         ST    RE,SAVERE                                                        
         MVC   BLOCK+2(11),=C'CLOSE DATE='                                      
         GOTO1 DATCON,DMCB,(1,ACJBCLOS),(8,BLOCK+13)                            
         CLI   BLOCK+13,C' '       FOR UK WHEN DAY < 10                         
         BNE   *+8                                                              
         OI    BLOCK+13,X'F0'                                                   
         MVC   BLOCK(2),=H'19'                                                  
         L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
OPEN     XC    BLOCK(2),BLOCK                                                   
         ICM   R6,15,AJOBEL                                                     
         BZ    OPEN3                                                            
         USING ACJOBD,R6                                                        
         ST    RE,SAVERE                                                        
         MVC   BLOCK+2(10),=C'OPEN DATE='                                       
         OC    ACJBOPND,ACJBOPND                                                
         BZ    OPEN2                                                            
         GOTO1 DATCON,DMCB,(1,ACJBOPND),(8,BLOCK+12)                            
         B     OPEN4                                                            
*                                                                               
OPEN2    GOTO1 DATCON,DMCB,(1,ACJBSTRT),(8,BLOCK+12)                            
         B     OPEN4                                                            
*                                                                               
OPEN3    ICM   R6,15,ASTATEL                                                    
         BZR   RE                                                               
         USING RSTEL,R6                                                         
         OC    RSTTDATE,RSTTDATE                                                
         BZR   RE                                                               
         ST    RE,SAVERE                                                        
         MVC   BLOCK+2(10),=C'OPEN DATE='                                       
         GOTO1 DATCON,DMCB,(1,RSTTDATE),(8,BLOCK+12)                            
*                                                                               
OPEN4    CLI   BLOCK+12,C' '       FOR UK WHEN DAY < 10                         
         BNE   *+8                                                              
         OI    BLOCK+12,X'F0'                                                   
         MVC   BLOCK(2),=H'18'                                                  
         L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
ANAL     XC    BLOCK(2),BLOCK                                                   
         L     R6,ASTATEL                                                       
         USING ACSTATD,R6                                                       
         CLI   ACSTANAL,C' '                                                    
         BER   RE                                                               
         MVC   BLOCK+2(14),=C'ANALYSIS CODE='                                   
         MVC   BLOCK+16(1),ACSTANAL                                             
         MVC   BLOCK(2),=H'15'                                                  
         BR    RE                                                               
         SPACE 2                                                                
*                                                                               
FILT     XC    BLOCK(2),BLOCK                                                   
         LA    RF,BLOCK+2                                                       
         L     R6,ASTATEL                                                       
         USING RSTELD,R6                                                        
*                                                                               
         CLI   RSTFILT1,C' '       TEST FOR ANY FILT1 VALUE                     
         BE    *+20                NO                                           
         MVC   0(6,RF),=C'FILT1='                                               
         MVC   6(1,RF),RSTFILT1                                                 
         LA    RF,7(RF)                                                         
*                                                                               
         CLI   RSTFILT2,C' '       TEST FOR ANY FILT2 VALUE                     
         BE    *+20                NO                                           
         MVC   0(7,RF),=C' FILT2='                                              
         MVC   7(1,RF),RSTFILT2                                                 
         LA    RF,8(RF)                                                         
*                                                                               
         CLI   RSTFILT3,C' '       TEST FOR ANY FILT3 VALUE                     
         BE    *+20                NO                                           
         MVC   0(7,RF),=C' FILT3='                                              
         MVC   7(1,RF),RSTFILT3                                                 
         LA    RF,8(RF)                                                         
*                                                                               
         CLI   RSTFILT4,C' '       TEST FOR ANY FILT4 VALUE                     
         BE    *+20                NO                                           
         MVC   0(7,RF),=C' FILT4='                                              
         MVC   7(1,RF),RSTFILT4                                                 
         LA    RF,8(RF)                                                         
*                                                                               
         CLI   RSTFILT5,C' '       TEST FOR ANY FILT5 VALUE                     
         BE    *+20                NO                                           
         MVC   0(7,RF),=C' FILT5='                                              
         MVC   7(1,RF),RSTFILT5                                                 
         LA    RF,8(RF)                                                         
*                                                                               
         LA    R0,BLOCK+2          LENGTH OF DATA IN FIRST 2 BYTES              
         SR    RF,R0                                                            
         STCM  RF,3,BLOCK                                                       
         BR    RE                                                               
         DROP  R6                                                               
         SPACE 2                                                                
*                                                                               
COSTGRP  XC    BLOCK(2),BLOCK                                                   
         L     R6,ASTATEL                                                       
         USING ACSTATD,R6                                                       
         CLI   ACSTCOST,C' '                                                    
         BNHR  RE                                                               
         MVC   BLOCK+2(14),=C'COSTING GROUP='                                   
         MVC   BLOCK+16(1),ACSTCOST                                             
         MVC   BLOCK(2),=H'15'                                                  
         BR    RE                                                               
         SPACE 2                                                                
SALES    XC    BLOCK(2),BLOCK                                                   
         ICM   R6,15,ASALEL                                                     
         BZR   RE                                                               
         USING ACSAND,R6                                                        
         MVC   BLOCK+2(14),=C'SALES ACCOUNT='                                   
         MVC   BLOCK+16(12),ACSACODE+3                                          
         MVC   BLOCK(2),=H'26'                                                  
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO PROCESS COMMENT ELEMENTS                                       
*                                                                               
* AT ENTRY, P1=A(PRINT LINE), P2=(L'OUTPUT AREA), P3=A(OUTPUT AREA)             
* ON EXIT, R2=A(PRINT LINE)                                                     
*                                                                               
COMM     NTR1                                                                   
         LM    R2,R4,0(R1)         GET PARMS                                    
         L     R6,AIO                                                           
         AH    R6,DATADISP                                                      
*                                                                               
COMM2    CLI   0(R6),0             TEST FOR EOR                                 
         BE    COMMX                                                            
         CLI   0(R6),X'3E'                                                      
         BE    COMM4                                                            
*                                                                               
COMM3    ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     COMM2                                                            
*                                                                               
         USING ACOMMD,R6                                                        
COMM4    ZIC   R1,ACOMLEN                                                       
         SH    R1,=Y(ACOMMENT-ACOMMD+1)                                         
         LA    RE,16                                                            
         MVC   BLOCK+2(16),=C'STD BILL && EST='                                 
         TM    ACOMTYPE,X'C0'                                                   
         BO    COMM6                                                            
         LA    RE,9                                                             
         MVC   BLOCK+2(9),=C'STD BILL='                                         
         TM    ACOMTYPE,X'80'                                                   
         BO    COMM6                                                            
         LA    RE,13                                                            
         MVC   BLOCK+2(13),=C'STD ESTIMATE='                                    
         TM    ACOMTYPE,X'40'                                                   
         BO    COMM6                                                            
         LA    RE,14                                                            
         MVC   BLOCK+2(14),=C'FOOT COMMENTS='                                   
*                                                                               
COMM6    LA    RF,BLOCK+2(RE)      POINT TO POSITION FOR COMMENT                
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),ACOMMENT                                                 
         LA    R5,1(RE,R1)         COMPUTE LENGTH OF STRING                     
*                                                                               
COMM8    GOTO1 CHOPPER,DMCB,((R5),BLOCK+2),((R3),(R4)),(C'P',6),0               
         L     RF,DMCB+8           GET N'LINES                                  
         LA    RE,L'P                                                           
         MR    RE,RE                                                            
         AR    R2,RF               BUMP OUTPUT POINTER                          
         AR    R4,RF               BUMP OUTPUT POINTER                          
         B     COMM3                                                            
*                                                                               
COMMX    XIT1  REGS=(R2)                                                        
         EJECT                                                                  
*              HEAD HOOK                                                        
         SPACE 3                                                                
HOOK     NTR1                                                                   
         MVC   H4+10(L'GOSELCLI),GOSELCLI                                       
         MVC   H4+17(20),SAVECNAM                                               
         L     R4,ABOX                                                          
         USING BOXD,R4                                                          
         LTR   R4,R4                                                            
         BZ    HOOKX                                                            
         SPACE 1                                                                
HOOK1    MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
         SPACE 1                                                                
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+6,C'T'                                                   
         LA    R2,BOXROWS+8                                                     
         CLI   RCSUBPRG,1                                                       
         BNE   *+8                                                              
         LA    R2,BOXROWS+9                                                     
         MVI   0(R2),C'M'                                                       
         MVI   BOXROWS+58,C'B'                                                  
         SPACE 1                                                                
         MVC   BOXCOLS,SPACES                                                   
         LA    R2,BOXCOLS                                                       
         CLI   RCSUBPRG,0          TEST FILE DATA                               
         BNE   HOOK2                                                            
         MVI   FILLBOX-PRTD(R2),C'L'                                            
         MVI   FILBOX1-PRTD(R2),C'C'                                            
         MVI   FILBOX2-PRTD(R2),C'C'                                            
         MVI   FILBOX3-PRTD(R2),C'C'                                            
         MVI   FILBOX4-PRTD(R2),C'C'                                            
         MVI   FILRBOX-PRTD(R2),C'R'                                            
         B     HOOKX                                                            
         SPACE 1                                                                
HOOK2    CLI   RCSUBPRG,1          TEST COMBINED REPORT                         
         BNE   HOOK4                                                            
         MVI   COMLBOX-PRTD(R2),C'L'                                            
         MVI   COMBOX1-PRTD(R2),C'C'                                            
         MVI   COMBOX2-PRTD(R2),C'C'                                            
         MVI   COMBOX3-PRTD(R2),C'C'                                            
         MVI   COMBOX4-PRTD(R2),C'C'                                            
         MVI   COMRBOX-PRTD(R2),C'R'                                            
         B     HOOKX                                                            
         SPACE 1                                                                
HOOK4    MVI   EFFLBOX-PRTD(R2),C'L'  EFFECTIVE OPTIONS REPORT                  
         MVI   EFFBOX1-PRTD(R2),C'C'                                            
         MVI   EFFBOX2-PRTD(R2),C'C'                                            
         MVI   EFFBOX3-PRTD(R2),C'C'                                            
         MVI   EFFBOX4-PRTD(R2),C'C'                                            
         MVI   EFFRBOX-PRTD(R2),C'R'                                            
         SPACE 1                                                                
HOOKX    B     XIT                                                              
         EJECT                                                                  
INITIAL  NTR1                                                                   
*                                                                               
         LA    R1,EXTTAB                                                        
         LA    R0,EXTTABL                                                       
*                                                                               
INIT020  LM    RE,RF,0(R1)                                                      
         LA    RE,LOCAL(RE)                                                     
         LA    RF,LOCAL(RF)                                                     
         ST    RE,0(RF)                                                         
         LA    R1,L'EXTTAB(R1)                                                  
         BCT   R0,INIT020                                                       
*                                                                               
         L     RE,=V(DUMMY)                                                     
         A     RE,RELO                                                          
         ST    RE,DMCB                                                          
         MVC   DMCB+4(3),SYSPHASE                                               
         MVI   DMCB+7,X'50'                                                     
         GOTO1 CALLOV,DMCB                                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,7,1(R1)                                                       
*                                                                               
         LM    R0,R1,0(RF)                                                      
         AR    R0,RF                                                            
         STM   R0,R1,ACOLTAB                                                    
*                                                                               
         LM    R0,R1,8(RF)                                                      
         AR    R0,RF                                                            
         STM   R0,R1,AOPVTAB                                                    
*                                                                               
         LA    RF,GOBLOCK                                                       
         ST    RF,AGOBLOCK                                                      
*                                                                               
         GOTO1 VJOBCOL,DMCB,FLDH,ACOLIST,ACOMFACS                               
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
INITREP  NTR1                                                                   
         L     R0,LENBUFF          ACQUIRE AN ADDITIONAL BUFFER                 
         GETMAIN R,LV=(0)                                                       
         ST    R1,ABUFF            SAVE A(BUFFER)                               
         LR    R2,R1               R2=BUFFER POINTER                            
         LR    RE,R2                                                            
         L     RF,LENBUFF          AND CLEAR IT                                 
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         SPACE 1                                                                
INITREP1 L     RE,TWAMASTC         RE=A(MASTER CONTROL BLOCK)                   
         USING MASTD,RE                                                         
         STCM  R2,15,MCUSRDMP      PRINT THE BUFFER IN A DUMP                   
         LR    RF,R2                                                            
         A     RF,LENBUFF                                                       
         STCM  RF,15,MCUSRDMP+4    END OF DUMP AREA                             
         DROP  RE                                                               
         SPACE 1                                                                
INITREP2 ST    R2,ACOMP                                                         
         LA    R2,2000(R2)                                                      
         ST    R2,ALEDG                                                         
         LA    R2,2000(R2)                                                      
         ST    R2,ACLI                                                          
         LA    R2,2000(R2)                                                      
         ST    R2,APROD                                                         
         LA    R2,2000(R2)                                                      
         ST    R2,AJOB                                                          
         LA    R2,2000(R2)                                                      
         ST    R2,AFROM                                                         
         LA    R2,GOFROML(R2)                                                   
         ST    R2,AWORKTAB                                                      
         A     R2,=F'20000'                                                     
         ST    R2,AGBBUFF                                                       
         A     R2,=F'30000'                                                     
         ST    R2,APRTTAB                                                       
         BAS   RE,INITPRT                                                       
         SPACE 1                                                                
         L     R2,AWORKTAB         BUILD A LIST OF WORK CODES                   
         LA    R0,1000                                                          
         MVC   KEY,SPACES                                                       
         MVI   KEY,X'0A'                                                        
         MVC   KEY+1(3),CUL                                                     
         GOTO1 HIGH                                                             
         B     INITREP6                                                         
         SPACE 1                                                                
INITREP4 GOTO1 SEQ                                                              
         SPACE 1                                                                
INITREP6 CLC   KEY(4),KEYSAVE                                                   
         BNE   XIT                                                              
         MVC   0(2,R2),KEY+4       FOUND A WC - SAVE CODE                       
         MVI   ELCODE,ACANELQ                                                   
         BAS   RE,GETELIO                                                       
         BNE   INITREP8                                                         
         USING ACANALD,R6                                                       
         MVC   2(15,R2),ACANDESC                                                
         SPACE 1                                                                
INITREP8 LA    R2,20(R2)                                                        
         BCT   R0,INITREP4                                                      
         B     XIT                                                              
         SPACE 2                                                                
* SUB-ROUTINE TO WRAP UP REPORT                                                 
*                                                                               
WRAP     NTR1                                                                   
         L     R1,ABUFF                                                         
         L     R0,LENBUFF                                                       
         FREEMAIN R,A=(1),LV=(0)                                                
         L     RE,TWAMASTC                                                      
         USING MASTD,RE                                                         
         XC    MCUSRDMP(8),MCUSRDMP CLEAR OUT EXTRA DUMP AREA                   
         B     XIT                                                              
         DROP  RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO INITIALIZE PRINT BUFFER                                        
*                                                                               
INITPRT  ST    RE,SAVERE                                                        
         LA    R0,MAXPRT                                                        
         L     RE,APRTTAB                                                       
         MVC   0(L'P,RE),SPACES                                                 
         LA    RE,L'P(RE)                                                       
         BCT   R0,*-10                                                          
         L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
PRTTAB   NTR1                                                                   
         L     R2,APRTTAB                                                       
         LA    R0,MAXPRT                                                        
         MVI   ALLOWLIN,2          ALLOW AT LEAST TWO LINES ON PAGE             
*                                                                               
PRTTAB2  CLC   0(L'P,R2),SPACES    TEST FOR BLANK LINE                          
         BE    PRTTABX             YES-EVERYTHING HAS BEEN PRINTED              
         MVC   P,0(R2)                                                          
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R2,L'P(R2)                                                       
         BCT   R0,PRTTAB2                                                       
*                                                                               
PRTTABX  BAS   RE,INITPRT          RE-INITIALIZE PRINT TABLE                    
         B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO SET ELEMENT ADDRESSES FOR RECORD                               
*                                                                               
GETADDR  NTR1                                                                   
         L     R6,AIO                                                           
         AH    R6,DATADISP                                                      
         SR    R0,R0               CLEAR WORK REGISTER                          
         XC    ADELEMS,ADELEMS     CLEAR ALL ELEMENT ADDRESSES                  
         SPACE 1                                                                
GETADDR2 CLI   0(R6),0             TEST FOR EOR                                 
         BE    GETADDRX                                                         
*                                                                               
         CLI   0(R6),ACNMELQ                                                    
         BNE   *+12                                                             
         ST    R6,ANAMEL                                                        
         B     GETADDR4                                                         
*                                                                               
         CLI   0(R6),ACADELQ                                                    
         BNE   *+12                                                             
         ST    R6,AADDREL                                                       
         B     GETADDR4                                                         
*                                                                               
         CLI   0(R6),ACPRELQ                                                    
         BNE   *+12                                                             
         ST    R6,APROFEL                                                       
         B     GETADDR4                                                         
*                                                                               
         CLI   0(R6),ACJBELQ                                                    
         BNE   *+12                                                             
         ST    R6,AJOBEL                                                        
         B     GETADDR4                                                         
*                                                                               
         CLI   0(R6),ACSTELQ                                                    
         BNE   *+12                                                             
         ST    R6,ASTATEL                                                       
         B     GETADDR4                                                         
*                                                                               
         CLI   0(R6),ACXPELQ                                                    
         BNE   *+12                                                             
         ST    R6,AXPROFEL                                                      
         B     GETADDR4                                                         
*                                                                               
         CLI   0(R6),X'3D'                                                      
         BNE   *+12                                                             
         ST    R6,ASALEL                                                        
         B     GETADDR4                                                         
*                                                                               
         CLI   0(R6),JLDELQ                                                     
         BNE   *+12                                                             
         ST    R6,AJLONG                                                        
         B     GETADDR4                                                         
*                                                                               
         CLI   0(R6),FFTELQ                                                     
         BNE   GETADDR4                                                         
         CLI   2(R6),FFTTPCOM                                                   
         BNE   GETADDR4                                                         
         ST    R6,AXCOMM                                                        
*                                                                               
GETADDR4 IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     GETADDR2                                                         
*                                                                               
GETADDRX B     XIT                                                              
         EJECT                                                                  
*   LOOKUP WORKCODE NAME                                                        
*                                                                               
LUPWORK  NTR1                                                                   
         L     R2,AWORKTAB         R2 = A(WORK CODE TABLE)                      
         USING JBCOLD,R3           R3 = A(COLTAB)                               
         MVC   0(15,R4),SPACES     R4 = A(OUTPUT)                               
         LA    R0,1000                                                          
*                                                                               
LUPW2    CLI   0(R2),0             END OF TABLE ?                               
         BE    XIT                 YES                                          
         CLC   0(2,R2),JBCOLWC                                                  
         BE    LUPW4                                                            
         LA    R2,20(R2)                                                        
         BCT   R0,LUPW2                                                         
         B     XIT                                                              
*                                                                               
LUPW4    MVC   0(15,R4),2(R2)                                                   
         B     XIT                                                              
*                                                                               
LOOKWCN  NTR1                                                                   
         L     R2,AWORKTAB         R2 = A(WORK CODE TABLE)                      
         USING MJETABD,R3          R3 = A(COLTAB)                               
         MVC   0(15,R4),SPACES     R4 = A(OUTPUT)                               
         LA    R0,1000                                                          
*                                                                               
LOOKW2   CLI   0(R2),0             END OF TABLE ?                               
         BE    XIT                 YES                                          
         CLC   0(2,R2),MJETWCD                                                  
         BE    LOOKW4                                                           
         LA    R2,20(R2)                                                        
         BCT   R0,LOOKW2                                                        
         B     XIT                                                              
*                                                                               
LOOKW4   MVC   0(15,R4),2(R2)                                                   
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
LOOKUP   NTR1                                                                   
         L     R5,AJOBLOCK                                                      
         USING JBLOCKD,R5                                                       
         MVC   JBAJOB,AJOB                                                      
         MVC   JBACOLS,ACOLIST                                                  
         MVC   JBACOM,ACOMFACS                                                  
         MVC   JBAGOBLK,AGOBLOCK                                                
         MVC   JBAIO,AIO2                                                       
         MVC   JBAKEY,AJOB                                                      
         MVC   JBGETOPT,GETOPT                                                  
*                                                                               
         MVC   JBACOLTB,ACOLTAB                                                 
         MVC   JBLCOLTB,LCOLTAB                                                 
         MVC   JBAOPVTB,AOPVTAB                                                 
         MVC   JBLOPVTB,LOPVTAB                                                 
*                                                                               
         LA    RE,FLDH                                                          
         ST    RE,JBORICLI                                                      
*                                                                               
         MVI   JBSELFUN,JBGETDE    GET BO DETAILS                               
*                                                                               
         GOTO1 JOBBER,DMCB,AJOBLOCK                                             
         CLI   JBERROR,X'00'                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         XIT1                                                                   
*                                                                               
FLDH     DC    AL1(8+L'FLD),4X'00',AL1(L'FLD),AL2(0)                            
FLD      DC    C'CE,ACT'                                                        
         DROP  R5                                                               
         EJECT                                                                  
GETELIO  L     R6,AIO                                                           
         GETEL (R6),DATADISP,ELCODE                                             
         SPACE 1                                                                
ERREND   GOTO1 VERRCUR                                                          
         SPACE 1                                                                
XIT      XIT1                                                                   
         SPACE 1                                                                
RELO     DS    A                                                                
         SPACE 1                                                                
LENBUFF  DC    F'100000'           LENGTH OF ACQUIRED BUFFER                    
         SPACE 1                                                                
* LEVEL ABBREVIATION TABLE                                                      
*                                                                               
*   BYTE 0    = 1 CHARACTER GETOPT CODE                                         
*   BYTES 1-2 = 2 CHARACTER OPTION MAINT CODE                                   
*                                                                               
LEVTAB   DS    0CL3                                                             
         DC    C'D',C'DF'                                                       
         DC    C'A',C'AG'                                                       
         DC    C'G',C'OG'                                                       
         DC    C'O',C'OF'                                                       
         DC    C'C',C'CL'                                                       
         DC    C'P',C'PR'                                                       
         DC    C'J',C'JB'                                                       
LEVELS   EQU   (*-LEVTAB)/L'LEVTAB                                              
         SPACE 3                                                                
EXTTAB   DS    0D                                                               
         DC    AL4(JOBLOCKA-LOCAL),AL4(AJOBLOCK-LOCAL)                          
         DC    AL4(COLIST-LOCAL),AL4(ACOLIST-LOCAL)                             
EXTTABL  EQU   (*-EXTTAB)/L'EXTTAB                                              
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              SPECS FOR HEADINGS ETC                                           
         SPACE 3                                                                
MYSPECS  DS    0F                                                               
         SPROG 0,1,2                                                            
         SSPEC H1,2,CREATED        SPECS FOR REGULAR PRINTING                   
         SSPEC H2,2,REQUESTOR                                                   
         SSPEC H1,45,C'PRODUCTION LEDGER REPORT'                                
         SSPEC H2,45,C'------------------------'                                
         SSPEC H1,97,AGYNAME                                                    
         SSPEC H2,97,AGYADD                                                     
         SSPEC H4,2,C'CLIENT'                                                   
         SSPEC H4,97,REPORT                                                     
         SSPEC H4,110,PAGE                                                      
*                                                                               
         SPROG 0                                                                
         SSPEC H4,49,C'FILE DATA FORMAT'                                        
         SSPEC H8,2,C'ACCOUNT'                                                  
         SSPEC H8,20,C'ACCOUNT NAME'                                            
         SSPEC H8,46,C'RECV ACCOUNT'                                            
         SSPEC H8,60,C'COST ACCOUNT'                                            
         SSPEC H8,74,C'FILE DATA'                                               
*                                                                               
         SPROG 1                                                                
         SSPEC H4,44,C'COMBINED FILE DATA/OPTIONS'                              
         SSPEC H8,2,C'ACCOUNT/'                                                 
         SSPEC H9,2,C'ACCOUNT NAME'                                             
         SSPEC H8,24,C'FILE DATA'                                               
         SSPEC H8,70,C'OPTION DESCRIPTION'                                      
         SSPEC H8,103,C'OPTION VALUE'                                           
         SSPEC H8,128,C'FROM'                                                   
*                                                                               
         SPROG 2                                                                
         SSPEC H4,46,C'EFFECTIVE JOB OPTIONS'                                   
         SSPEC H8,2,C'ACCOUNT'                                                  
         SSPEC H8,20,C'ACCOUNT NAME'                                            
         SSPEC H8,58,C'OPTION DESCRIPTION'                                      
         SSPEC H8,91,C'OPTION VALUE'                                            
         SSPEC H8,117,C'FROM'                                                   
*                                                                               
         DC    X'00'                                                            
         EJECT                                                                  
*              OTHER DSECTS ARE HIDDEN IN HERE                                  
         SPACE 3                                                                
*DDMASTD                                                                        
*ACPROWORKD                                                                     
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*ACGENBOTH                                                                      
*ACGENFILE                                                                      
*DDBIGBOX                                                                       
*ACJOBBERD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE ACPROWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE ACJOBBERD                                                      
         PRINT ON                                                               
         EJECT                                                                  
JBLOCKD  DSECT                                                                  
       ++INCLUDE ACJOBBLOCK                                                     
         EJECT                                                                  
T60BFFD  DSECT                                                                  
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE ACPROD3D                                                       
         SPACE 2                                                                
* DSECT TO COVER LOCAL WORKING STORAGE                                          
*                                                                               
SUBSYSD  DSECT                                                                  
         ORG   DRGEN                                                            
NOJOB    DS    C                                                                
NOPRO    DS    C                                                                
LOCAL    DS    0C                                                               
QOGROUP  DS    CL1                                                              
QOFFICE  DS    CL2                                                              
QCLI     DS    CL(L'CLICODE)                                                    
QPROD    DS    CL(L'PRODCODE)                                                   
QJOB     DS    CL(L'JOBNUM)                                                     
QREPTYPE DS    C                                                                
QCLOSE   DS    C                                                                
QLOCKED  DS    C                                                                
QDEFOPT  DS    C                                                                
QWCOPT   DS    C                                                                
QDRFT    DS    C                                                                
*       - - - -                                                                 
QFILT    DS    C                                                                
QFILT2   DS    C                                                                
QFILT3   DS    C                                                                
QFILT4   DS    C                                                                
QFILT5   DS    C                                                                
QFILTS   EQU   *-QFILT                                                          
*       - - - -                                                                 
CLOSED   DS    C                                                                
*                                                                               
SAVEOPT  DS    XL(L'GOPTIONS)                                                   
SAVELACC DS    CL42                                                             
SAVECNAM DS    CL36                                                             
SAVEPNAM DS    CL36                                                             
SAVEJNAM DS    CL36                                                             
ABUFF    DS    A                   A(ACQUIRED BUFFER)                           
ACOMP    DS    A                                                                
ALEDG    DS    A                                                                
ACLI     DS    A                                                                
APROD    DS    A                                                                
AJOB     DS    A                                                                
AWORKTAB DS    A                                                                
AGBBUFF  DS    A                                                                
AFROM    DS    A                                                                
APRTTAB  DS    A                   A(PRINT TABLE)                               
SAVERE   DS    A                                                                
         SPACE 1                                                                
ADELEMS  DS    0XL40                                                            
ANAMEL   DS    A                   A(NAME ELEMENT)                              
AADDREL  DS    A                   A(ADDRESS ELEMENT)                           
APROFEL  DS    A                   A(PROFILE ELEMENT)                           
AJOBEL   DS    A                   A(JOB ELEMENT)                               
ASTATEL  DS    A                   A(STATUS ELEMENT)                            
AXPROFEL DS    A                   A(EXTRA PROFILE ELEMENT)                     
ASALEL   DS    A                   A(SALES ELEMENT)                             
AXCOMM   DS    A                   A(EXTRA COMMENT ELEMENT                      
AJLONG   DS    A                   A(JOB LONG DESCRIPTION ELEMENT)              
         DS    A                                                                
         SPACE 1                                                                
APRTLIN  DS    A                   A(CURRENT PRINT LINE)                        
ANYOPT   DS    CL1                                                              
WHICHREC DS    CL1                                                              
RECTYPE  DS    C                   C=CLIENT,P=PRODUCT,J=JOB                     
LCONBRK  DS    XL1                                                              
FIRSTCLI DS    C                                                                
FIRSTPRO DS    C                                                                
         SPACE 1                                                                
OPTWC    DS    CL2                                                              
OPTWCNAM DS    CL15                                                             
OPTDESC  DS    CL31                                                             
OPTVALH  DS    CL8                                                              
OPTVAL   DS    CL24                                                             
OPTFROM  DS    CL4                                                              
*                                                                               
ACOLTAB  DS    A                                                                
LCOLTAB  DS    F                                                                
AOPVTAB  DS    A                                                                
LOPVTAB  DS    F                                                                
*                                                                               
AGOBLOCK DS    A                                                                
AJOBLOCK DS    A                                                                
ACOLIST  DS    A                                                                
AGOXBLOC DS    A                                                                
AGOBBLOC DS    A                                                                
APANBLOC DS    A                                                                
*                                                                               
         EJECT                                                                  
         DS    0D                                                               
       ++INCLUDE ACGOBLOCK                                                      
         EJECT                                                                  
COLIST   DS    CL200                                                            
JOBLOCKA DS    (JBLOCKL)X                                                       
       ++INCLUDE ACGOXBLOCK                                                     
       ++INCLUDE ACGOBBLOCK                                                     
       ++INCLUDE ACPANBLOCK                                                     
LOCALLN  EQU   *-LOCAL                                                          
         EJECT                                                                  
* DSECT TO COVER PRINT LINE                                                     
*                                                                               
PRTD     DSECT                                                                  
PRTLIN   EQU   *                                                                
         ORG   PRTLIN              FILE DATA FORMAT                             
FILLBOX  DS    C                                                                
FILACC   DS    CL16                                                             
         DS    C                                                                
FILBOX1  DS    C                                                                
FILNAME  DS    CL24                                                             
         DS    C                                                                
FILBOX2  DS    C                                                                
FILRECV  DS    CL12                                                             
         DS    C                                                                
FILBOX3  DS    C                                                                
FILCOST  DS    CL12                                                             
         DS    C                                                                
FILBOX4  DS    C                                                                
FILOTH   DS    CL60                                                             
FILRBOX  DS    C                                                                
         SPACE 1                                                                
         ORG   PRTLIN              COMBINED FILE DATA AND OPTIONS               
COMLBOX  DS    C                                                                
COMACCN  DS    CL20                                                             
         DS    C                                                                
COMBOX1  DS    C                                                                
COMOTH   DS    CL44                                                             
         DS    C                                                                
COMBOX2  DS    C                                                                
COMOPT   DS    CL31                                                             
         DS    C                                                                
COMBOX3  DS    C                                                                
COMVAL   DS    CL24                                                             
COMBOX4  DS    C                                                                
COMFROM  DS    CL4                                                              
COMRBOX  DS    C                                                                
         SPACE 1                                                                
         ORG   PRTLIN              JOB EFFECTIVE OPTIONS                        
EFFLBOX  DS    C                                                                
EFFACC   DS    CL16                                                             
         DS    C                                                                
EFFBOX1  DS    C                                                                
EFFNAME  DS    CL36                                                             
         DS    C                                                                
EFFBOX2  DS    C                                                                
EFFOPT   DS    CL31                                                             
         DS    C                                                                
EFFBOX3  DS    C                                                                
EFFVAL   DS    CL24                                                             
         DS    C                                                                
EFFBOX4  DS    C                                                                
EFFFROM  DS    CL4                                                              
EFFRBOX  DS    C                                                                
         SPACE 2                                                                
* EQUATES                                                                       
*                                                                               
MAXPRT   EQU   10000/L'P                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'044ACPRO23   04/10/15'                                      
         END                                                                    
