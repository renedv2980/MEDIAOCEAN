*          DATA SET TAREP21    AT LEVEL 063 AS OF 08/13/14                      
*PHASE T70321C,*                                                                
         TITLE 'T70321 - RELEASE LETTERS'                                       
T70321   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 MYEND-MYSTART,T70321,R7                                          
         LR    RA,RC                                                            
         USING MYD,RA                                                           
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          SYSTEM SPECIFIC WORK                         
         L     R6,ATWA                                                          
         USING CONHEADH-64,R6                                                   
         GOTO1 INITIAL,DMCB,0                                                   
         L     R2,TWAMASTC                                                      
         USING MASTD,R2                                                         
         GOTO1 DATCON,DMCB,(4,MCDATE),(8,ETODAY)                                
         SPACE 1                                                                
         CLI   MODE,VALREC                                                      
         BNE   MODE2                                                            
         BAS   RE,VREC                                                          
         B     XIT                                                              
         SPACE 1                                                                
MODE2    CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
         BAS   RE,VREC                                                          
         BAS   RE,PREP                                                          
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE RECORD                                                  
         SPACE 1                                                                
VREC     NTR1                                                                   
         SPACE 1                                                                
         MVI   TYPE,LETTER                                                      
         CLC   CONREC(2),=C'RS'                                                 
         BNE   *+8                                                              
         MVI   TYPE,SUMMARY                                                     
         GOTO1 HEXIN,DMCB,SPLCOMM,THISCOM,8                                     
         MVC   THISLET,SPLCOMM+8                                                
         MVC   THISEFF,SPACES                                                   
         CLC   SPLEFF(6),=C'000000'                                             
         BE    VAL2                                                             
         GOTO1 DATCON,DMCB,(0,SPLEFF),(8,THISEFF)                               
         SPACE 1                                                                
VAL2     XC    THISSSN,THISSSN                                                  
         XC    THISCAT,THISCAT                                                  
         XC    THISSEQ,THISSEQ                                                  
         CLC   SPLPERF(9),=C'000000000'                                         
         BE    XIT                                                              
         MVC   THISSSN,SPLPERF                                                  
         CLC   SPLPERF+6(3),SPACES            SSN OR PID?                       
         BH    VAL10                                                            
         XC    THISSSN,THISSSN                                                  
         GOTO1 SSNUNPK,DMCB,SPLPERF,THISSSN   CONVERT PID TO SSN                
         BE    *+6                                                              
         DC    H'00'                                                            
VAL10    GOTO1 HEXIN,DMCB,SPLPERF+9,THISSEQ,4                                   
         MVC   THISCAT,SPLPERF+13                                               
         B     XIT                                                              
         SPACE 1                                                                
INVERR   MVI   ERROR,INVALID                                                    
         GOTO1 ERRXIT                                                           
         EJECT                                                                  
*              PRINT REPORT                                                     
         SPACE                                                                  
PREP     NTR1                                                                   
         LA    R1,HOOK             INITIALIZE FOR REPORT                        
         ST    R1,HEADHOOK                                                      
         LA    R1,SUMSPECS                                                      
         ST    R1,SPECS                                                         
         MVC   TITLE,=CL32'RELEASE LETTER SUMMARY'                              
         CLI   TYPE,SUMMARY                                                     
         BE    PREP2                                                            
         LA    R1,LETSPECS                                                      
         ST    R1,SPECS                                                         
         MVC   TITLE,SPACES                                                     
         SPACE 3                                                                
*              GET COMMERCIAL RELATED FIELDS                                    
         SPACE                                                                  
PREP2    XC    KEY,KEY             READ COMMERCIAL PASSIVE                      
         LA    R4,KEY                                                           
         USING TLCOPD,R4                                                        
         MVI   TLCOPCD,TLCOCCDQ                                                 
         MVC   TLCOCCOM,THISCOM                                                 
         GOTO1 HIGH                                                             
         CLC   TLCOPKEY,KEYSAVE                                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         USING TLCOD,R4                                                         
         MVC   THISAGY,TLCOAGY     AGENCY FROM ACTIVE RECORD                    
         MVC   THISCLI,TLCOCLI     CLIENT FROM ACTIVE RECORD                    
         MVC   THISPRD,TLCOPRD     POSSIBLE PRODUCT CODE                        
         SPACE                                                                  
         XC    COMSTAF,COMSTAF                                                  
         XC    COMDATE,COMDATE                                                  
         XC    COMTIME,COMTIME                                                  
         BAS   RE,GETACTV          GET ACTIVITY EL., RETURNED IN R6             
         BNE   PREP5                                                            
         USING TAACD,R6                                                         
         MVC   COMDATE,TAACCDTE    SAVE DATE                                    
         MVC   COMTIME,TAACCTIM         TIME                                    
         MVC   COMSTAF,TAACSTAF         STAFF                                   
         SPACE                                                                  
PREP5    GOTO1 GETNAME,DMCB,COMTITLE  DIG OUT TITLE                             
         MVI   FREECODE,TAFNTPRD                                                
         MVC   PRDNAME,SPACES                                                   
         GOTO1 GETFREE,DMCB,PRDNAME   POSSIBLE PROD NAME                        
         L     R6,AIO                                                           
         MVI   ELCODE,TACOELQ                                                   
         BAS   RE,GETEL                                                         
         USING TACOD,R6                                                         
         MVC   THISCID,TACOCID     COMMERCIAL ID                                
         MVC   THISATT,TACOATT     ATTENTION CODE                               
         EJECT                                                                  
*              GET AGENCY RELATED FIELDS                                        
         SPACE 2                                                                
         XC    KEY,KEY             READ AGENCY RECORD                           
         LA    R4,KEY                                                           
         USING TLAYD,R4                                                         
         MVI   TLAYCD,TLAYCDQ                                                   
         MVC   TLAYAGY,THISAGY                                                  
         GOTO1 HIGH                                                             
         CLC   TLAYKEY,KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
         GOTO1 GETNAME,DMCB,AGYNAME     FOR NAME                                
         GOTO1 GETADD,DMCB,AGYADD       AND ADDRESS                             
         MVI   FREECODE,TAFNTATT                                                
         MVC   ATTNAME,SPACES                                                   
         GOTO1 GETFREE,DMCB,ATTNAME     AND ATTENTION                           
         L     R6,AIO                                                           
         MVI   ELCODE,TAAYELQ                                                   
         BAS   RE,GETEL                                                         
         USING TAAYD,R6                                                         
         MVC   THISOFF,TAAYTPOF                                                 
         SPACE 5                                                                
*              GET ATTENTION NAME                                               
         SPACE 2                                                                
         CLI   THISATT,C'A'                                                     
         BL    ATTX                                                             
         XC    KEY,KEY             READ ATTENTION RECORD                        
         LA    R4,KEY                                                           
         USING TLATD,R4                                                         
         MVI   TLATCD,TLATCDQ                                                   
         MVC   TLATAGY,THISAGY                                                  
         MVC   TLATATT,THISATT                                                  
         GOTO1 HIGH                                                             
         CLC   TLATKEY,KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
         GOTO1 GETNAME,DMCB,AGYNAME     FOR NAME                                
         GOTO1 GETADD,DMCB,AGYADD       AND ADDRESS                             
         MVI   FREECODE,TAFNTATT                                                
         GOTO1 GETFREE,DMCB,ATTNAME     AND ATTENTION                           
ATTX     DS    0H                                                               
         EJECT                                                                  
*              CLIENT AND PRODUCT RECORDS                                       
         SPACE 1                                                                
         XC    KEY,KEY             READ CLIENT RECORD                           
         LA    R4,KEY                                                           
         USING TLCLD,R4                                                         
         MVI   TLCLCD,TLCLCDQ                                                   
         MVC   TLCLAGY,THISAGY                                                  
         MVC   TLCLCLI,THISCLI                                                  
         GOTO1 HIGH                                                             
         CLC   TLCLKEY,KEYSAVE                                                  
         BE    CLI2                                                             
         MVC   TLCLKEY,KEYSAVE                                                  
         XC    TLCLAGY,TLCLAGY                                                  
         GOTO1 HIGH                                                             
         CLC   TLCLKEY,KEYSAVE                                                  
         BE    CLI2                                                             
         DC    H'0'                                                             
         SPACE 1                                                                
CLI2     GOTO1 GETREC                                                           
         GOTO1 GETNAME,DMCB,CLINAME     FOR NAME                                
         MVI   FREECODE,TAFNTATT                                                
         GOTO1 GETFREE,DMCB,ATTNAME     CLIENT CAN HAVE ATTENTION               
         CLI   THISPRD,0           IF A PRODUCT CODE                            
         BE    PRDX                                                             
         XC    KEY,KEY             READ PRODUCT RECORD                          
         LA    R4,KEY                                                           
         USING TLPRD,R4                                                         
         MVI   TLPRCD,TLPRCDQ                                                   
         MVC   TLPRAGY,THISAGY                                                  
         MVC   TLPRCLI,THISCLI                                                  
         MVC   TLPRPRD,THISPRD                                                  
         GOTO1 HIGH                                                             
         CLC   TLPRKEY,KEYSAVE                                                  
         BE    PRD2                                                             
         MVC   TLPRKEY,KEYSAVE                                                  
         XC    TLPRAGY,TLPRAGY                                                  
         XC    TLPRCLI,TLPRCLI                                                  
         GOTO1 HIGH                TRY FOR GLOBAL PRODUCT                       
         CLC   TLPRKEY,KEYSAVE                                                  
         BE    PRD2                                                             
         DC    H'0'                                                             
PRD2     GOTO1 GETREC                                                           
         GOTO1 GETNAME,DMCB,PRDNAME     FOR NAME                                
         SPACE 1                                                                
PRDX     DS    0H                                                               
         EJECT                                                                  
         SPACE 1                                                                
         MVC   TIACOMFC,ACOMFACS   SET UP FOR TASYSIO                           
         LA    R1,IOHOOK                                                        
         ST    R1,TIHOOK                                                        
         MVI   TIREAD,TLCACDQ      SET UP TO READ CAST                          
         MVC   TIFAGY,THISAGY      FOR AGENCY/COMMERCIAL                        
         MVC   TIFCOM,THISCOM                                                   
         MVC   TIFSSN,THISSSN      FILTER SSN OPTIONAL                          
         MVC   TIFCAT,THISCAT             AND CAT                               
         MVI   CAFLAG,C'N'         INITIALIZE NO CAST FOR REQUEST               
         STM   R0,RF,MYREGS                                                     
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
         CLI   TYPE,SUMMARY        IF RELEASE SUMMARY                           
         BNE   XIT                                                              
         OC    TIFSSN,TIFSSN       AND FOR ONE PERFORMER                        
         BZ    MAIN20                                                           
         CLI   CAFLAG,C'Y'         AND NO PERFORMER FOUND                       
         BE    MAIN20                                                           
         MVC   W4NAME,=CL36'**NOT ON FILE/NO RELEASE NOTICE**'                  
         MVC   REQSTAF,SPACES      CLEAR BECAUSE SET OFF CAST RECORD            
         BAS   RE,SETSUMM                                                       
*                                                                               
MAIN20   BAS   RE,SPLAT                                                         
         L     R1,DRONE            USE DRONE TO SAVE PAGE,LINE                  
         MVC   1(2,R1),PAGE                                                     
         MVC   3(1,R1),LINE                                                     
         B     XIT                                                              
         EJECT                                                                  
*              SYSIO HOOK ROUTINE                                               
*                                                                               
IOHOOK   NTR1                                                                   
         CLI   TIMODE,PROCREC      IF THERE IS SOMETHING INTERESTING            
         BNE   XIT                                                              
         L     R6,TIAREC                                                        
         CLI   0(R6),TLCACDQ                                                    
         BNE   XIT                                                              
         BAS   RE,PROCCAST         GO AND HANDLE                                
         MVC   KEY,TIKEY           RESTABLISH SEQUENCE                          
         GOTO1 HIGH                                                             
         B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE TO GET THE ACTIVITY ELEMENT IN AIO                       
*              RETURNS CC NOT EQ IF NOT FOUND,                                  
*              ELSE RETURNS CC EQ AND RETURNS R6=A(ELEMENT)                     
*                                                                               
GETACTV  NTR1                                                                   
         L     R6,AIO                                                           
         MVI   ELCODE,TAACELQ                                                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
GETAC5   BAS   RE,NEXTEL                                                        
         BNE   NO                                                               
         SPACE                                                                  
         USING TAACD,R6                                                         
         CLI   TAACSCR,X'81'       FILTER ON RELEASE REPORT SCREEN              
         BNE   GETAC5                                                           
         XR    RC,RC               SET CC YES                                   
         LTR   RC,RC                                                            
         XIT1  REGS=(R6)           RETURN A(ELEMENT) IN R6                      
         EJECT                                                                  
*              HANDLE THE CAST RECORD                                           
         SPACE 1                                                                
PROCCAST NTR1                                                                   
         L     R4,TIAREC                                                        
         USING TLCAD,R4                                                         
         OC    THISSEQ,THISSEQ     MAY FILTER ON SEQ #                          
         BZ    CAST2                                                            
         CLC   THISSEQ,TLCASEQ                                                  
         BNE   XIT                                                              
         SPACE 1                                                                
CAST2    MVC   THISSSN,TLCASSN     NOW HAVE SS#                                 
         L     R6,TIAREC                                                        
         CLI   THISLET,C'A'        DOWNGRADES NOT ALLOWED FOR EXTRAS            
         BNE   *+12                                                             
         TM    TLCASORT,X'20'                                                   
         BO    XIT                                                              
         SPACE 1                                                                
         MVI   ELCODE,TACAELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING TACAD,R6                                                         
         CLC   TACAUN,=C'AFM'      REJECT AFM                                   
         BE    XIT                                                              
******   CLC   TACAUN,=C'ACT'             AND ACTRA                             
******   BE    XIT                        (NOW OK 11/1/91)                      
         MVC   THISNCDE,TACANCDE   MAY HAVE AGENT CODE                          
         MVC   THISCORP,TACACORP                                                
         SPACE 1                                                                
         CLI   TYPE,SUMMARY        IF SUMMARY, DO NOT UPDATE RECORD             
         BE    CAST3                                                            
         SPACE 1                                                                
         L     R6,TIAREC           MARK CAST RECORD WITH TODAY'S DATE           
         MVI   ELCODE,TARLELQ      GET CAST RELEASE ELEMENT                     
         BAS   RE,GETEL                                                         
         BNE   CAST3                                                            
         USING TARLD,R6                                                         
         MVC   AIO,AIO2            REGET CAST REC IN AIO2 FOR PUTREC            
         XC    KEY,KEY                                                          
         MVC   KEY(L'TIKEY),TIKEY  RESET KEY FOR PUTREC                         
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
         MVC   AIO,TIAREC          CAST RECORD                                  
         GOTO1 DATCON,DMCB,(5,0),(1,TARLDATE)                                   
         GOTO1 PUTREC                                                           
         MVC   AIO,AIO1            RESET AIO                                    
         SPACE 1                                                                
         USING TAACD,R6                                                         
CAST3    MVC   REQSTAF,COMSTAF     INIT STAFF                                   
         MVC   AIO,TIAREC          SET AIO TO CAST REC                          
         BAS   RE,GETACTV          GET ACTIVITY EL., RETURNED IN R6             
         MVC   AIO,AIO1            RESET AIO                                    
         BNE   CAST5                                                            
         CLC   COMDATE,TAACCDTE    SKIP IF DATE BEFORE COMM'L REC               
         BH    CAST5                                                            
         CLC   COMTIME,TAACCTIM    OR IF TIME BEFORE COMM'L REC                 
         BH    CAST5                                                            
         MVC   REQSTAF,TAACSTAF    ELSE SAVE STAFF                              
         SPACE 1                                                                
CAST5    XC    KEY,KEY             READ W4 RECORD                               
         LA    R4,KEY                                                           
         USING TLW4D,R4                                                         
         MVI   TLW4CD,TLW4CDQ                                                   
         MVC   TLW4SSN,THISSSN                                                  
         GOTO1 HIGH                                                             
         CLC   TLW4KEY,KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
         GOTO1 GETREC                                                           
         GOTO1 GETW4,DMCB,W4NAME                                                
         BAS   RE,GETW4ADD         GET W4 ADDRESS                               
         MVC   CORPNAME,SPACES                                                  
         CLI   THISCORP,C'1'                                                    
         BL    CORPX                                                            
         L     R6,AIO                                                           
         MVI   ELCODE,TATIELQ                                                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
         SPACE 1                                                                
CORP2    BAS   RE,NEXTEL                                                        
         BNE   CORPX                                                            
         USING TATID,R6                                                         
         CLC   THISCORP,TATICRPN                                                
         BNE   CORP2                                                            
         MVC   CORPSSN,TATIID                                                   
         XC    KEY,KEY             READ CORP RECORD                             
         LA    R4,KEY                                                           
         USING TLW4D,R4                                                         
         MVI   TLW4CD,TLW4CDQ                                                   
         MVC   TLW4SSN,CORPSSN                                                  
         GOTO1 HIGH                                                             
         CLC   TLW4KEY,KEYSAVE                                                  
         BNE   CORPX                                                            
         SPACE 1                                                                
         GOTO1 GETREC                                                           
         GOTO1 GETW4,DMCB,CORPNAME                                              
         BAS   RE,GETW4ADD                                                      
         SPACE                                                                  
CORPX    MVC   AGNTNAME,SPACES                                                  
         OC    THISNCDE,THISNCDE   MAY NEED AGENT                               
         BZ    AGENTX                                                           
         XC    KEY,KEY             READ AGENT RECORD                            
         LA    R4,KEY                                                           
         USING TLANPD,R4                                                        
         MVI   TLANPCD,TLANCCDQ                                                 
         GOTO1 TRNSAGT,DMCB,(X'40',THISNCDE),TLANCAGT                           
         GOTO1 HIGH                                                             
         CLC   TLANPKEY,KEYSAVE                                                 
         BNE   AGENTX                                                           
         SPACE 1                                                                
         GOTO1 GETREC                                                           
         GOTO1 GETNAME,DMCB,AGNTNAME                                            
         BAS   RE,GETW4ADD                                                      
         SPACE 1                                                                
AGENTX   DS    0H                                                               
         EJECT                                                                  
*              FORMAT SUMMARY LINE                                              
         SPACE 1                                                                
         CLI   TYPE,SUMMARY                                                     
         BNE   CASTLET                                                          
         L     R1,DRONE            USE DRONE TO SAVE PAGE,LINE                  
         CLI   0(R1),X'90'                                                      
         BE    FSUMM2                                                           
         MVC   PAGE,1(R1)                                                       
         MVC   LINE,3(R1)                                                       
         MVI   FORCEHED,C'N'                                                    
         SPACE 1                                                                
FSUMM2   MVI   0(R1),0                                                          
         BAS   RE,SETSUMM          SET ONE SUMMARY LINE                         
         BAS   RE,SPLAT                                                         
         MVI   CAFLAG,C'Y'         SET PRINTED CAST FOR REQUEST                 
         L     R1,DRONE            USE DRONE TO SAVE PAGE,LINE                  
         MVC   1(2,R1),PAGE                                                     
         MVC   3(1,R1),LINE                                                     
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO SET ONE SUMMARY LINE                                  
         SPACE                                                                  
SETSUMM  NTR1                                                                   
         LA    R2,P                R2=A(PRINT LINE)                             
         USING PRINTD,R2                                                        
         MVC   PSTAFF,REQSTAF                                                   
         MVC   POFF+2(1),THISOFF                                                
         MVC   PAGY,THISAGY                                                     
         MVC   PCLI,THISCLI                                                     
         MVC   PPRD(6),THISPRD                                                  
         MVC   PCID,THISCID                                                     
         MVC   PLETTER+2(1),THISLET                                             
         MVC   PEFF,THISEFF                                                     
         MVC   PSSN,THISSSN                                                     
         XC    PSSN,PSSN                                                        
         GOTO1 SSNPACK,DMCB,THISSSN,PSSN    CONVERT SSN TO PID                  
         OC    PSSN,SPACES                                                      
SSUMM10  MVC   PNAME,W4NAME                                                     
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*              FORMAT LETTER                                                    
         SPACE 1                                                                
CASTLET  MVI   FORCEHED,C'Y'       FORCE SKIP TO CHANNEL 1 FOR EACH             
         MVC   P+7(30),AGYNAME                                                  
         MVC   P+61(21),=C'TALENT RELEASE NOTICE'                               
         BAS   RE,SPLAT                                                         
         MVC   P+7(30),AGYADD                                                   
         BAS   RE,SPLAT                                                         
         MVC   P+7(30),AGYADD+30                                                
         BAS   RE,SPLAT                                                         
         MVC   P+7(30),AGYADD+60                                                
         BAS   RE,SPLAT                                                         
         MVC   P+7(30),AGYADD+90                                                
         BAS   RE,SPLAT                                                         
         BAS   RE,SPLAT                                                         
         BAS   RE,SPLAT                                                         
         BAS   RE,SPLAT                                                         
         BAS   RE,SPLAT                                                         
         SPACE 1                                                                
         MVC   P+7(7),=C'AGENCY:'                                               
         MVC   P+15(29),AGYNAME                                                 
******   GOTO1 DATCON,DMCB,(5,0),(8,P+45)                                       
         MVC   P+45(8),ETODAY                                                   
         BAS   RE,SPLAT                                                         
         MVC   P+7(14),=C'COMMERCIAL ID:'                                       
         MVC   P+22(12),THISCID                                                 
         MVC   P+45(6),=C'TITLE:'                                               
         MVC   P+52(30),COMTITLE                                                
         BAS   RE,SPLAT                                                         
         MVC   P+7(11),=C'ADVERTISER:'                                          
         MVC   P+19(25),CLINAME                                                 
         MVC   P+45(8),=C'PRODUCT:'                                             
         MVC   P+54(30),PRDNAME                                                 
         BAS   RE,SPLAT                                                         
         BAS   RE,SPLAT                                                         
         BAS   RE,DOTEXT                                                        
         CLI   THISLET,C'D'        IF RELEASE LETTER D                          
         BNE   *+14                                                             
         CLC   THISEFF,SPACES      AND PRINTING EFFECTIVE DATE                  
         BNE   *+8                 DON'T SKIP A LINE                            
         BAS   RE,SPLAT                                                         
         MVC   P+54(10),=C'SINCERELY,'                                          
         BAS   RE,SPLAT                                                         
         MVC   P+54(29),AGYNAME                                                 
         BAS   RE,SPLAT                                                         
         MVC   P+43(10),=C'ISSUED BY:'                                          
         MVC   P+54(36),ATTNAME                                                 
         BAS   RE,SPLAT                                                         
         BAS   RE,SPLAT                                                         
         BAS   RE,SPLAT                                                         
         BAS   RE,SPLAT                                                         
         BAS   RE,SPLAT                                                         
         MVC   P+7(8),REQSTAF      REQUESTOR STAFF ID                           
         CLC   CORPNAME,SPACES                                                  
         BNE   CASTLET2                                                         
         MVC   P+47(33),W4NAME                                                  
         BAS   RE,SPLAT                                                         
         B     CASTLET4                                                         
         SPACE 1                                                                
CASTLET2 MVC   P+47(36),CORPNAME   CORP                                         
         BAS   RE,SPLAT                                                         
         MVC   P+47(3),=C'FSO'                                                  
         MVC   P+51(33),W4NAME                                                  
         BAS   RE,SPLAT                                                         
         SPACE 1                                                                
CASTLET4 CLC   AGNTNAME,SPACES     AGENT                                        
         BE    CASTLET6                                                         
         MVC   P+47(3),=C'C/O'                                                  
         MVC   P+51(36),AGNTNAME                                                
         BAS   RE,SPLAT                                                         
         SPACE 1                   NOW FOR THE ADDRESS - HOPE IT FITS!          
CASTLET6 CLI   NEWADD,C'Y'         TEST FOR NEW STYLE ADDRESS                   
         BNE   CASTLET7                                                         
         MVC   P+47(39),W4ADD                                                   
         BAS   RE,SPLAT                                                         
         MVC   P+47(39),W4ADD+39                                                
         BAS   RE,SPLAT                                                         
         MVC   P+47(39),W4ADD+78                                                
         BAS   RE,SPLAT                                                         
         MVC   P+47(39),W4ADD+117                                               
         BAS   RE,SPLAT                                                         
         B     XIT                                                              
         SPACE                                                                  
CASTLET7 MVC   P+47(30),W4ADD      OLD STYLE ADDRESS                            
         BAS   RE,SPLAT                                                         
         MVC   P+47(30),W4ADD+30                                                
         BAS   RE,SPLAT                                                         
         MVC   P+47(30),W4ADD+60                                                
         BAS   RE,SPLAT                                                         
         MVC   P+47(30),W4ADD+90                                                
         BAS   RE,SPLAT                                                         
         B     XIT                                                              
         EJECT                                                                  
*              FORMAT THE CENTER OF THE LETTER                                  
         SPACE 1                                                                
DOTEXT   NTR1                                                                   
         ZIC   R1,THISLET          (A-D)                                        
         SLL   R1,28                                                            
         SRL   R1,28               (1-4)                                        
         LTR   R1,R1                                                            
         BCTR  R1,0                (0-3)                                        
         CH    R1,=H'3'                                                         
         BH    XIT                                                              
         SLL   R1,3                (0 8 16 24)                                  
         LA    R1,TEXTADDS(R1)                                                  
         LM    R3,R4,0(R1)         A(START/END TEXT)                            
         CLC   THISEFF,SPACES      OPTIONAL EFFECTIVE DATE                      
         BE    DOTEXT2                                                          
                                                                                
         CLI   THISLET,C'A'        A                                            
         BNE   *+10                                                             
         MVC   STARTAEF(8),THISEFF                                              
                                                                                
         CLI   THISLET,C'B'        B                                            
         BNE   *+10                                                             
         MVC   STARTBEF(8),THISEFF                                              
                                                                                
         CLI   THISLET,C'C'        NOT ALLOWED FOR A AND B                      
         BL    DOTEXT2                                                          
         SH    R3,=AL2(L'LTEFFDAT) BACK UP TEXT START                           
         MVC   0(L'LTEFFDAT,R3),LTEFFDAT                                        
         MVC   16(8,R3),THISEFF                                                 
         SPACE 1                                                                
DOTEXT2  SR    R4,R3               R4=L'TEXT                                    
         MVC   DMCB+12(4),=C'LEN=' OPTIONAL P4                                  
         LA    R2,7                NUMBER OF LINES TO CHOP                      
         CLI   THISLET,C'D'        IF RELEASE LETTER D                          
         BNE   DOTEXT3                                                          
         CLC   THISEFF,SPACES      WITH EFFECTIVE DATE                          
         BE    *+8                                                              
         LA    R2,1(R2)            ONE MORE LINE NEEDED                         
DOTEXT3  GOTO1 CHOPPER,DMCB,(R3),(75,LETBLOCK),(R2),,(R4)                       
         LA    R3,LETBLOCK                                                      
         SPACE 1                                                                
DOTEXT4  MVC   P+7(75),0(R3)       NOW PRINT THE TEXT                           
         BAS   RE,SPLAT                                                         
         LA    R3,75(R3)                                                        
         BCT   R2,DOTEXT4                                                       
         B     XIT                                                              
         SPACE 1                                                                
TEXTADDS DS    0F                                                               
         DC    A(STARTA)                                                        
         DC    A(ENDA)                                                          
         DC    A(STARTB)                                                        
         DC    A(ENDB)                                                          
         DC    A(STARTC)                                                        
         DC    A(ENDC)                                                          
         DC    A(STARTD)                                                        
         DC    A(ENDD)                                                          
         EJECT                                                                  
*              ODD ROUTINES                                                     
         SPACE 1                                                                
GETW4    NTR1                                                                   
         L     R2,0(R1)            GET W4 NAME                                  
         L     R6,AIO                                                           
         MVI   ELCODE,TAW4ELQ                                                   
         BAS   RE,GETEL                                                         
         MVC   0(36,R2),SPACES                                                  
         BNE   XIT                                                              
         USING TAW4D,R6                                                         
         MVC   0(32,R2),TAW4CRPN                                                
         CLI   TAW4TYPE,TAW4TYCO                                                
         BE    XIT                                                              
         CLI   TAW4TYPE,TAW4TYTR                                                
         BE    XIT                                                              
         MVC   0(49,R2),SPACES                                                  
         CLI   TAW4LEN,TAW4LN2Q    IS THERE A MIDDLE NAME?                      
         BE    GETW410                                                          
         MVC   0(16,R2),TAW4NAM1                                                
         MVC   17(16,R2),TAW4NAM2                                               
         GOTO1 SQUASHER,DMCB,(R2),33                                            
         B     XIT                                                              
         SPACE 1                                                                
GETW410  MVC   0(16,R2),TAW4NAM1                                                
         MVC   17(16,R2),TAW4MIDN                                               
         MVC   34(16,R2),TAW4NAM2                                               
         GOTO1 SQUASHER,DMCB,(R2),50                                            
         B     XIT                                                              
         SPACE 1                                                                
GETNAME  NTR1                                                                   
         L     R2,0(R1)            GET NAME OUT                                 
         L     R6,AIO                                                           
         MVI   ELCODE,TANAELQ                                                   
         BAS   RE,GETEL                                                         
         MVC   0(36,R2),SPACES                                                  
         BNE   XIT                                                              
         USING TANAD,R6                                                         
         ZIC   R1,TANALEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     XIT                                                              
         MVC   0(0,R2),TANANAME                                                 
         SPACE 1                                                                
GETADD   NTR1                                                                   
         L     R2,0(R1)            GET ADDRESS                                  
         L     R6,AIO                                                           
         MVI   ELCODE,TAADELQ                                                   
         BAS   RE,GETEL                                                         
         MVC   0(120,R2),SPACES                                                 
         BNE   XIT                                                              
         USING TAADD,R6                                                         
         ZIC   R1,TAADLEN                                                       
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     XIT                                                              
         MVC   0(0,R2),TAADADD                                                  
         EJECT                                                                  
GETFREE  NTR1                                                                   
         L     R2,0(R1)            GET NAME OUT                                 
         L     R6,AIO                                                           
         MVI   ELCODE,TAFNELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         SPACE 1                                                                
         USING TAFND,R6                                                         
GETFREE2 CLC   FREECODE,TAFNTYPE                                                
         BE    GETFREE4                                                         
         BAS   RE,NEXTEL                                                        
         BNE   XIT                                                              
         B     GETFREE2                                                         
         SPACE 1                                                                
GETFREE4 MVC   0(36,R2),SPACES                                                  
         ZIC   R1,TAFNLEN                                                       
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     XIT                                                              
         MVC   0(0,R2),TAFNNAME                                                 
         SPACE 1                                                                
GETW4ADD NTR1                      GET ADDRESS FROM W4 RECORD                   
         MVI   W4ADD,C' '                                                       
         MVC   W4ADD+1(L'W4ADD-1),W4ADD                                         
         MVI   NEWADD,C'N'         INIT FLAG                                    
         L     R6,AIO                                                           
         MVI   ELCODE,TAA2ELQ      SEE IF THERE'S NEW STYLE ADDRESS             
         BAS   RE,GETEL                                                         
         BNE   GETW4A10                                                         
         MVI   NEWADD,C'Y'         SET FLAG                                     
         SPACE                                                                  
         USING TAA2D,R6                                                         
         LA    R2,W4ADD            R2=WHERE TO SAVE IT IN W4ADD                 
         LA    R4,TAA2ADD1                                                      
         LHI   R0,3                ASSUME 3 ADDRESS LINES                       
         CLI   TAA2LEN,TAA2LNQ     FOR OLD STYLE ADDRESSES                      
         BL    GETW4A2                                                          
         CLC   TAA2CTRY,=C'US'     AND NEW STYLE US ADDRESSES                   
         BE    GETW4A2                                                          
         LHI   R0,2                ELSE USE 2                                   
GETW4A2  CLC   0(30,R4),SPACES     TEST FOR ANY DATA LEFT                       
         BNH   GETW4A5                                                          
         MVC   0(30,R2),0(R4)                                                   
         LA    R2,39(R2)           BUMP IN W4ADD (LEAVE ROOM FOR 39)            
         LA    R4,30(R4)           BUMP TO NEXT ADDRESS LINE                    
         BCT   R0,GETW4A2                                                       
         SPACE                                                                  
GETW4A5  XC    WORK(39),WORK                                                    
         MVC   WORK(25),TAA2CITY      CITY                                      
         MVC   WORK+26(2),TAA2ST      STATE                                     
         MVC   WORK+29(10),TAA2ZIP    ZIP                                       
         GOTO1 SQUASHER,DMCB,WORK,39  SQUASH IT                                 
         MVC   0(39,R2),WORK                                                    
         SPACE                                                                  
         CLI   TAA2LEN,TAA2LNQ        FOR OLD STYLE ADDRESSES                   
         BL    XIT                                                              
         CLC   TAA2CTRY,=C'US'        AND NEW STYLE US ADDRESSES                
         BE    XIT                                                              
         GOTO1 VALCTRY,DMCB,(X'80',TAA2CTRY)                                    
         BNE   XIT                                                              
         USING CTRYTABD,R1                                                      
         L     R1,TGACTRY                                                       
         ZIC   RE,CTRYDSP                                                       
         LTR   RE,RE                                                            
         BNZ   *+8                                                              
         IC    RE,CTRYLEN                                                       
         SHI   RE,CTRYDESC-CTRYTABD+1                                           
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   39(0,R2),CTRYDESC                                                
         EX    RE,*+8                                                           
         B     *+10                                                             
         OC    39(0,R2),SPACES                                                  
         B     XIT                                                              
         DROP  R1                                                               
         SPACE                                                                  
GETW4A10 GOTO1 GETADD,DMCB,W4ADD   OLD STYLE ADDRESS                            
         B     XIT                                                              
         EJECT                                                                  
YES      SR    R1,R1                                                            
         B     *+8                                                              
         SPACE 1                                                                
NO       LA    R1,1                                                             
         LTR   R1,R1                                                            
         B     XIT                                                              
         SPACE 1                                                                
SPLAT    NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              HEADLINES ETC                                                    
         SPACE 1                                                                
HOOK     NTR1                                                                   
         CLI   TYPE,SUMMARY                                                     
         BNE   XIT                                                              
         MVC   H1+52(24),TITLE                                                  
         GOTO1 CENTER,DMCB,H1+52,24                                             
         GOTO1 UNDERLIN,DMCB,(24,H1+52),(X'BF',H2+52)                           
         MVC   H7,MYH7                                                          
         L     R6,ABOX                                                          
         USING BOXD,R6                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVC   BOXCOLS,MYCOLS                                                   
         MVC   BOXROWS,MYROWS                                                   
         MVI   BOXOFF,0                                                         
         XIT1                                                                   
         SPACE 1                                                                
SUMSPECS SSPEC H1,1,RUN                                                         
         SSPEC H1,99,AGYNAME                                                    
         SSPEC H2,99,AGYADD                                                     
         SSPEC H3,99,REPORT                                                     
         SSPEC H3,112,PAGE                                                      
         DC    H'0'                                                             
         SPACE 1                                                                
LETSPECS DS    0H                                                               
         DC    H'0'                                                             
         SPACE 1                                                                
LETTER   EQU   C'L'                                                             
SUMMARY  EQU   C'S'                                                             
         SPACE 1                                                                
MYROWS   DC    C'     T M'                                                      
         DC    CL57' '                                                          
         DC    CL80'B'                                                          
         SPACE 1                                                                
MYCOLS   DC    CL5' '                                                           
         DC    CL1'L'                                                           
         DC    CL8' '                                                           
         DC    CL1'C'                                                           
         DC    CL6' '                                                           
         DC    CL1'C'                                                           
         DC    CL6' '                                                           
         DC    CL1'C'                                                           
         DC    CL6' '                                                           
         DC    CL1'C'                                                           
         DC    CL7' '                                                           
         DC    CL1'C'                                                           
         DC    CL12' '                                                          
         DC    CL1'C'                                                           
         DC    CL6' '                                                           
         DC    CL1'C'                                                           
         DC    CL8' '                                                           
         DC    CL1'C'                                                           
         DC    CL9' '                                                           
         DC    CL1'C'                                                           
         DC    CL33' '                                                          
         DC    CL1'R'                                                           
         DC    CL15' '                                                          
         SPACE 1                                                                
MYH7     DC    CL5' '                                                           
         DC    CL1' '                                                           
         DC    CL8'REQUESTR'                                                    
         DC    CL1' '                                                           
         DC    CL6'OFFICE'                                                      
         DC    CL1' '                                                           
         DC    CL6'AGENCY'                                                      
         DC    CL1' '                                                           
         DC    CL6'CLIENT'                                                      
         DC    CL1' '                                                           
         DC    CL7'PRODUCT'                                                     
         DC    CL1' '                                                           
         DC    CL12' COMMERCIAL '                                               
         DC    CL1' '                                                           
         DC    CL6'LETTER'                                                      
         DC    CL1' '                                                           
         DC    CL8' EFFECT '                                                    
         DC    CL1' '                                                           
         DC    CL9' S.S. #  '                                                   
         DC    CL1' '                                                           
         DC    CL33'PERFORMER NAME'                                             
         DC    CL1' '                                                           
         DC    CL15' '                                                          
         SPACE 1                                                                
LTEFFDAT DC    C'EFFECTIVE AS OF MMMDD/YY, OR AT THE END OF CURRENT USEX        
                CYCLE, '                                                        
         EJECT                                                                  
*              LTORG ETC                                                        
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              TEXT MESSAGES                                                    
         SPACE 1                                                                
         DS    0D                                                               
         DS    CL32                                                             
STARTA   EQU   *                                                                
         DC    C'AN EXAMINATION OF THE ABOVE COMMERCIAL IN ITS '                
         DC    C'FINAL FORM REVEALS THAT YOU HAVE BEEN DOWNGRADED, '            
         DC    C'EFFECTIVE AS OF '                                              
STARTAEF DC    C'MMMDD/YY. '                                                    
         DC    C'THEREFORE, IN ACCORDANCE WITH THE PROVISION IN THE '           
         DC    C'APPLICABLE UNION CONTRACT, A CHECK WILL BE FORWARDED '         
         DC    C'TO YOU FROM '                                                  
         DC    C'TALENT PARTNERS '                                              
         DC    C'IN THE AMOUNT OF AN ADDITIONAL BASE PAY OR SESSION '           
         DC    C'FEE, WHICHEVER IS APPLICABLE. '                                
         DC    C'FIRST USE OF COMMERCIAL IS NOTED ABOVE.'                       
ENDA     EQU   *                                                                
         SPACE 1                                                                
         DS    0D                                                               
         DS    CL32                                                             
STARTB   EQU   *                                                                
         DC    C'AN EXAMINATION OF THE ABOVE COMMERCIAL IN ITS '                
         DC    C'FINAL FORM REVEALS THAT YOU HAVE BEEN '                        
         DC    C'COMPLETELY EDITED FROM THE COMMERCIAL, '                       
         DC    C'EFFECTIVE AS OF '                                              
STARTBEF DC    C'MMMDD/YY. '                                                    
         DC    C'THEREFORE, IN ACCORDANCE WITH THE PROVISION IN THE '           
         DC    C'APPLICABLE UNION CONTRACT, '                                   
         DC    C'WE ARE HEREBY NOTIFYING YOU THAT WE ARE RELEASING '            
         DC    C'YOU FROM EMPLOYMENT AGREEMENT NOTED ABOVE. '                   
ENDB     EQU   *                                                                
         SPACE 1                                                                
         DS    0D                                                               
         DS    CL(L'LTEFFDAT)                                                   
STARTC   EQU   *                                                                
         DC    C'THE ABOVE COMMERCIAL IN WHICH YOUR SERVICES ARE '              
         DC    C'UTILIZED IS BEING WITHDRAWN FROM USE. '                        
         DC    C'PLEASE BE ADVISED THAT THIS WITHDRAWAL NOTICE '                
         DC    C'COVERS ONLY THIS COMMERCIAL. '                                 
         DC    C'IT DOES NOT RELEASE YOU FROM CONTRACTUAL '                     
         DC    C'OBLIGATIONS WITH REGARD TO OTHER COMMERCIALS '                 
         DC    C'ADVERTISING THE SAME OR OTHER PRODUCTS OR SERVICES '           
         DC    C'OF THIS ADVERTISER, ITS SUBSIDIARIES '                         
         DC    C'OR AFFILIATES.'                                                
ENDC     EQU   *                                                                
         SPACE 1                                                                
         DS    0D                                                               
         DS    CL(L'LTEFFDAT)                                                   
STARTD   EQU   *                                                                
         DC    C'THE ABOVE COMMERCIAL IN WHICH YOUR SERVICES ARE '              
         DC    C'UTILIZED IS BEING WITHDRAWN FROM USE '                         
         DC    C'IN THE U.S., ITS COMMONWEALTHS, TERRITORIES, '                 
         DC    C'POSSESSIONS, CANADA AND MEXICO, '                              
         DC    C'BUT MAY STILL BE USED IN FOREIGN AREAS. '                      
         DC    C'PLEASE BE ADVISED THAT THIS WITHDRAWAL NOTICE '                
         DC    C'COVERS ONLY THIS COMMERCIAL. '                                 
         DC    C'IT DOES NOT RELEASE YOU FROM CONTRACTUAL '                     
         DC    C'OBLIGATIONS WITH REGARD TO OTHER COMMERCIALS '                 
         DC    C'ADVERTISING THE SAME OR OTHER PRODUCTS OR SERVICES '           
         DC    C'OF THIS ADVERTISER, ITS SUBSIDIARIES '                         
         DC    C'OR AFFILIATES.'                                                
ENDD     EQU   *                                                                
         SPACE 1                                                                
         EJECT                                                                  
*              MY WORKING STORAGE                                               
         SPACE 1                                                                
MYD      DSECT                                                                  
MYSTART  DS    0D                                                               
MYREGS   DS    16F                                                              
ETODAY   DS    CL8                                                              
THISOFF  DS    CL1                                                              
THISCOM  DS    XL4                                                              
THISSSN  DS    CL9                                                              
THISCAT  DS    CL3                                                              
THISSEQ  DS    XL2                                                              
THISEFF  DS    CL8                                                              
THISLET  DS    CL1                                                              
THISAGY  DS    CL6                                                              
THISATT  DS    CL2                                                              
THISCID  DS    CL12                                                             
THISCLI  DS    CL6                                                              
THISPRD  DS    CL6                                                              
THISNCDE DS    XL2                                                              
THISCORP DS    CL1                                                              
COMTITLE DS    CL36                                                             
CLINAME  DS    CL36                                                             
PRDNAME  DS    CL36                                                             
AGYNAME  DS    CL36                                                             
AGYADD   DS    CL120                                                            
W4NAME   DS    CL36                                                             
CORPNAME DS    CL36                                                             
CORPSSN  DS    CL9                                                              
AGNTNAME DS    CL36                                                             
W4ADD    DS    CL156                                                            
ATTNAME  DS    CL36                                                             
FREECODE DS    CL1                                                              
REQSTAF  DS    CL8                 REQUESTOR STAFF ID                           
COMSTAF  DS    CL8                 REQUESTOR STAFF ID FROM COMM'L REC           
COMDATE  DS    XL3                 REQUESTED DATE                               
COMTIME  DS    XL3                 REQUESTED TIME                               
TYPE     DS    CL1                                                              
NEWADD   DS    CL1                 Y=NEW STYLE ADDRESS FROM W4 RECORD           
CAFLAG   DS    CL1                 Y=CAST FOUND FOR REQUEST                     
LETBLOCK DS    675C                75 X 8 LINES PLUS A BIT!                     
         SPACE 1                                                                
MYEND    DS    0D                                                               
         SPACE 3                                                                
*                                  PRINT LINE DSECT FOR SUMMARY                 
         SPACE 1                                                                
PRINTD   DSECT                                                                  
         DS    CL5                                                              
         DS    CL1                                                              
PSTAFF   DS    CL8                                                              
         DS    CL1                                                              
POFF     DS    CL6                                                              
         DS    CL1                                                              
PAGY     DS    CL6                                                              
         DS    CL1                                                              
PCLI     DS    CL6                                                              
         DS    CL1                                                              
PPRD     DS    CL7                                                              
         DS    CL1                                                              
PCID     DS    CL12                                                             
         DS    CL1                                                              
PLETTER  DS    CL6                                                              
         DS    CL1                                                              
PEFF     DS    CL8                                                              
         DS    CL1                                                              
PSSN     DS    CL9                                                              
         DS    CL1                                                              
PNAME    DS    CL33                                                             
         DS    CL1                                                              
         DS    CL15                                                             
         EJECT                                                                  
*              OTHER DSECTS ARE HIDDEN IN HERE                                  
         SPACE 3                                                                
*TAREPWORKD                                                                     
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*DDBIGBOX                                                                       
*TAGENFILE                                                                      
*TASYSCTRYD                                                                     
*FAFACTS                                                                        
*FATIOB                                                                         
*TAREPFFD                                                                       
         PRINT OFF                                                              
       ++INCLUDE TAREPWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSCTRYD                                                     
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE DDREPMASTD                                                     
       ++INCLUDE FATIOB                                                         
       ++INCLUDE TAREPFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE TAREPE1D                                                       
SAVELINE DS    XL1                                                              
SAVEPAGE DS    XL2                                                              
SAVESW   DS    XL1                                                              
         SPACE 1                                                                
       ++INCLUDE DDGENTWA                                                       
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'063TAREP21   08/13/14'                                      
         END                                                                    
