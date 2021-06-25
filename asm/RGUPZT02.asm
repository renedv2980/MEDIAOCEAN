*          DATA SET RGUPZT02   AT LEVEL 157 AS OF 10/10/00                      
*PHASE ACZT02A,*                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE HELEN                                                                  
*INCLUDE HELLO                                                                  
         TITLE 'TIMESHEET LINE NUM PROBLEM FIX FOR TEMPO'                       
**********************************************************************          
*   QOPT1=Y: PRINT ALL TIMES FOR BAD PERSONS ONLY                    *          
*   QOPT2=Y: QOPT1 MUST BE Y, PRINT BAD TIMES FOR BAD PERSONS ONLY   *          
**********************************************************************          
ACZT02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACZT**                                                       
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACZTD,RC                                                         
         L     R8,VBIGPRNT                                                      
         USING BIGPRNTD,R8                                                      
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                      INITIALIZATIONS FOR THE RUN.           
         CLI   MODE,COMPFRST                                                    
         BE    CPYF                      INITIALIZATIONS FOR THE RUN.           
         CLI   MODE,REQFRST              REQUEST FIRST                          
         BE    REQF                                                             
         CLI   MODE,LEDGFRST                                                    
         BE    LDGF                                                             
         CLI   MODE,LEVAFRST                                                    
         BE    LEVAF                                                            
         CLI   MODE,LEVBFRST                                                    
         BE    LEVBF                                                            
         CLI   MODE,LEVCFRST                                                    
         BE    LEVCF                                                            
         CLI   MODE,PROCACC                                                     
         BE    PACC                                                             
         CLI   MODE,PROCTIME                                                    
         BE    PTIME                                                            
         CLI   MODE,ACCLAST              ACCOUNT LAST                           
         BE    ACCL                                                             
         CLI   MODE,COMPLAST                                                    
         BE    CPYL                      LAST FOR COMPANY                       
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                      LAST FOR RUN                           
*                                                                               
EXIT     XMOD1 1                                                                
         EJECT                                                                  
**********************************************************************          
* RUN FIRST                                                          *          
**********************************************************************          
         SPACE 1                                                                
RUNF     DS    0H                                                               
         MVC   VTYPES(VTYPLNQ),ADCONS        RELOCATE ADDRESSES                 
*                                                                               
         USING ACCRECD,RE                                                       
         LA    RE,IO                                                            
         MVC   DISP2,=Y(ACCRFST-ACCKEY)      SET DISP2                          
         DROP  RE                                                               
*                                                                               
         XC    CPYWRK,CPYWRK       CLEAR COMPANY TABLE                          
         MVI   CPYWRK,X'FF'        MARK EOF                                     
*                                                                               
         MVI   FCSUPOFC,C'Y'                                                    
         MVI   FCGETOPT,C'N'                                                    
*                                                                               
         L     R2,ADMASTC                                                       
         USING MASTD,R2                                                         
         L     R4,MCBXAREA                                                      
         USING BOXD,R4                                                          
         MVC   BOXWIDTH,=F'198'         SET WIDTH FOR REPORT                    
*                                                                               
         ZAP   PKCPYTOT,=P'0'                                                   
RUNFX    B     EXIT                                                             
         DROP  R2,R4                                                            
         EJECT                                                                  
**********************************************************************          
* REQUEST FIRST                                                      *          
**********************************************************************          
         SPACE 1                                                                
REQF     DS    0H                                                               
         MVI   RCSUBPRG,1                                                       
         MVI   FLAG,0              INITIALIZE FLAG                              
*                                                                               
         XC    START,START                                                      
         CLC   QSTART,SPACES       ANY START DATE                               
         BE    REQF10                                                           
         MVC   WORK(L'QSTART),QSTART                                            
         CLC   WORK+4(2),SPACES                                                 
         BNE   *+10                                                             
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(1,START)                                   
*                                                                               
REQF10   MVC   END,=X'FFFFFF'                                                   
         CLC   QEND,SPACES         ANY END DATE                                 
         BE    REQFX                                                            
         MVC   WORK(L'QEND),QEND                                                
         CLC   WORK+4(2),SPACES                                                 
         BNE   REQF20                                                           
         MVC   WORK+4(2),=C'31'                                                 
         GOTO1 DATCON,DMCB,(X'30',WORK),(0,WORK),(1,0) LAST DAY OF MO           
REQF20   GOTO1 DATCON,DMCB,(0,WORK),(1,END)                                     
*                                                                               
REQFX    B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* COMPANY FIRST                                                      *          
**********************************************************************          
         SPACE 1                                                                
CPYF     DS    0H                                                               
         NI    FLAG2,X'FF'-FLGCPY                                               
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* LEDGER FIRST                                                       *          
**********************************************************************          
         SPACE 1                                                                
LDGF     DS    0H                                                               
         USING CPYELD,R1                                                        
         L     R1,ADCMPEL          GET X'10' ELEMENT COMPANY ELEM               
*                                                                               
         TM    CPYSTAT4,CPYSOFF2   IS NEW 2 CHAR OFFICE IN USE                  
         BNO   *+8                                                              
         OI    FLAG,FLGNOFF        NEW 2 CHAR OFFICE IN USE                     
*                                                                               
         MVC   SVCLOGO,CPYLOGO     GET LOGIN ID / CONNECT ID                    
         DROP  R1                                                               
*                                                                               
         BAS   RE,GETLEVS          GET LEVELS                                   
*                                                                               
LDGFX    B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* LEVEL A FIRST                                                      *          
**********************************************************************          
         SPACE 1                                                                
LEVAF    DS    0H                                                               
         L     R2,ADLVANAM                                                      
         LA    R3,LEVANME                                                       
         BAS   RE,GETNME                                                        
*                                                                               
LEVAFX   B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* LEVEL B FIRST                                                      *          
**********************************************************************          
         SPACE 1                                                                
LEVBF    DS    0H                                                               
         L     R2,ADLVBNAM                                                      
         LA    R3,LEVBNME                                                       
         BAS   RE,GETNME                                                        
*                                                                               
LEVBFX   B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* LEVEL C FIRST                                                      *          
**********************************************************************          
         SPACE 1                                                                
LEVCF    DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         L     R2,ADLVCNAM                                                      
         LA    R3,LEVCNME                                                       
         BAS   RE,GETNME                                                        
*                                                                               
LEVCFX   B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* PROCESS ACCOUNT                                                    *          
**********************************************************************          
         SPACE 1                                                                
PACC     DS    0H                                                               
         USING BIND,R2                                                          
         L     R2,ATIMETAB         TRANSACTION TABLE                            
         XC    BININ,BININ         CLEAR BIN TABLE                              
         DROP  R2                                                               
*                                                                               
         LA    R0,PKFLDLNQ         NUMBER OF PACKED FIELDS                      
         LA    R1,PKFLDS           R1=A(PACKED FIELDS)                          
         ZAP   0(L'PKFLDS,R1),=P'0'     CLEAR FIELDS                            
         LA    R1,L'PKFLDS(R1)                                                  
         BCT   R0,*-10                                                          
*                                                                               
         BAS   RE,SETCDE           SET LEVEL CODES                              
*                                                                               
         L     R2,ADACCNAM                                                      
         LA    R3,LEVDNME                                                       
         BAS   RE,GETNME                                                        
*                                                                               
         NI    FLAG,X'FF'-(FLGPER+FLGNOFF+FLGBDPER) CLEAR FLAG                  
         MVC   SVACCT,SPACES       CLEAR ACCOUNT                                
         XC    SVPEDT,SVPEDT       CLEAR PERIOD END DATE                        
         MVC   LASTKEY,SPACES      LAST KEY FOR EVERY NEW KEY                   
*                                                                               
PACCX    B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* PROCESS TIME                                                       *          
**********************************************************************          
         SPACE 1                                                                
         USING TIMELD,R4                                                        
PTIME    DS    0H                                                               
         NI    FLAG,X'FF'-(FLGTEMPO+HASTMPLN)                                   
         L     R4,ADTRANS                                                       
*                                                                               
         CLI   TIMEL,TIMELQ        MAKE SURE IT'S A TIME ELEMENT                
         BNE   PTIMEX                                                           
*                                                                               
         USING ACMD,R7                                                          
         L     R7,AMONACC                                                       
*                                                                               
         L     R7,ACMALTN          GET ADDRESS OF TIME RECORD                   
         USING TIMRECD,R7                                                       
*                                                                               
         CLC   TIMKPEDT,START                                                   
         BL    PTIMEX              IGNORE OLD ONES                              
         CLC   TIMKPEDT,END                                                     
         BH    PTIMEX              IGNORE AFTER END DATE OF REQUEST             
*                                                                               
         CLI   TIMETYP,TIMEINP                                                  
         BE    *+6                                                              
         DC    H'0'                JUST CHECKING                                
*                                                                               
         TM    TIMSTAT,TIMTEMPO    IS IT TEMPO TIME                             
         BNO   *+8                                                              
         OI    FLAG,FLGTEMPO       FLAG THAT IT IS TEMPO                        
*                                                                               
T        USING TIMELD,R6                                                        
         LR    R6,R4                                                            
*                                                                               
PTIME10  SR    R1,R1                                                            
         IC    R1,1(R6)             GET LENGTH OF THIS ELEMENT                  
         AR    R6,R1                                                            
         CLI   0(R6),0                                                          
         BE    PTIME20                                                          
         CLI   0(R6),TIMELQ                                                     
         BNE   PTIME10                                                          
         CLI   T.TIMETYP,TIMEINP  IS IT INPUT TYPE                              
         BE    PTIME20                                                          
         CLI   T.TIMETYP,TIMEXTRA IS IT TEMPO LINE                              
         BNE   PTIME10                                                          
         OI    FLAG,HASTMPLN       TEMPO TIME HAS TEMPO LINE                    
*                                                                               
PTIME20  DS    0H                                                               
         USING TIMED,R5                                                         
         LA    R5,TIMEWRK                                                       
         MVC   TIMEWRK,SPACES                                                   
         MVI   TIMEFLAG,0            FLAG FOR EACH BIN ENTRY                    
*                                                                               
         MVC   TIMEPER,LEVDCDE       PERSON CODE                                
         MVC   TIMENME,LEVDNME       PERSON NAME                                
         MVC   TIMELINE,TIMLINE#     TIME SHEET LINE NUMBER                     
         MVC   TIMHOURS,TIMHRS       HOURS                                      
         TM    FLAG,HASTMPLN                                                    
         BNO   *+10                                                             
         MVC   TIMTMPLN,T.TIMXTLN#   TEMPO LINE NUMBER                          
         MVC   TIMEPEDT,TIMKPEDT     PERIOD END DATE                            
         MVC   TIMESTA,TIMSTAT       SAVE STATUS BYTES                          
*                                                                               
         TM    FLAG,FLGTEMPO       IS IT TEMPO                                  
         BNO   *+16                                                             
         TM    FLAG,HASTMPLN       IF TEMPO MUST HAVE TEMPO LINE #              
         BO    PTIME50                                                          
         OI    TIMEFLAG,TMPNOLIN                                                
*                                                                               
         TM    FLAG,HASTMPLN       NOT TEMPO, IS TEMPO LINE THERE               
         BNO   PTIME50             NO TEMPO, NO TMPO LIN # GOOD                 
*                                                                               
         MVC   MSG,=CL15'TIME REC IN'                                           
         SR    R3,R3                                                            
         ICM   R3,3,TIMRLEN                                                     
         GOTO1 ADUMP,DMCB,(RC),(R7),(R3)                                        
*                                                                               
         MVI   T.TIMEL,DELELQ              INIT ELEMENT TO BE DELETED           
         GOTO1 VHELLO,DMCB,(C'D',ACCOUNT),('DELELQ',(R7)),0,0                   
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   MSG,=CL15'TIME REC OUT'                                          
         SR    R3,R3                                                            
         ICM   R3,3,TIMRLEN                                                     
         GOTO1 ADUMP,DMCB,(RC),(R7),(R3)                                        
*                                                                               
         OI    FLAG,FLGBDPER       THIS PERSON IS NOT GOOD                      
         OI    TIMEFLAG,TMSTMPLN   NOT A TEMPO BUT TEMPOLINE#                   
         OI    FLAG2,FLGCPY        THIS COMPANY IS BAD                          
*                                                                               
         CLI   QOPT3,C'Y'                                                       
         BNE   PTIME25                                                          
         CLI   RCWRITE,C'Y'                                                     
         BNE   *+8                                                              
         MVI   MODE,WRITRANS                                                    
*                                                                               
PTIME25  LA    R1,CPYWRK                                                        
         LA    R0,(L'CPYWRK/2)-1                                                
PTIME30  CLI   0(R1),X'FF'                                                      
         BE    PTIME40                                                          
         CLC   ALPHAID,0(R1)                                                    
         BE    PTIME50                                                          
         LA    R1,2(R1)                                                         
         BCT   R0,PTIME30                                                       
*                                                                               
PTIME40  MVC   0(2,R1),ALPHAID     MOVE IN COMPANY TO TABLE                     
         MVI   2(R1),X'FF'         MARK EOF                                     
*                                                                               
PTIME50  DS    0H                                                               
*                                                                               
         GOTO1 ABINADD,DMCB,(RC),TIMEWRK,ATIMETAB   ADD TABLE ENTRY             
*                                                                               
PTIMEX   B     EXIT                                                             
         DROP  R4,R5,T                                                          
         EJECT                                                                  
**********************************************************************          
* ACCOUNT LAST                                                       *          
**********************************************************************          
         SPACE 1                                                                
         USING PLINED,R5                                                        
ACCL     DS    0H                                                               
         LA    R5,XP                                                            
*                                                                               
         XC    LSTTMPTS,LSTTMPTS   CLEAR CHECK TEMPO T/S FIELD                  
         XC    LSTTS,LSTTS         CLEAR CHECK T/S FIELD                        
*                                                                               
         CLI   QOPT1,C'Y'          SHOW ONLY SCREWED UP T/S IF Y                
         BNE   ACCL10                                                           
         TM    FLAG,FLGBDPER       IS THIS PERSON BAD                           
         BNO   ACCLX                                                            
*                                                                               
         USING BIND,R1                                                          
ACCL10   L     R1,ATIMETAB         R1=A(TRANSACTION TABLE)                      
         ICM   R3,15,BININ                                                      
         BZ    ACCLX                                                            
         USING TIMED,R2                                                         
         LA    R2,BINTAB                                                        
         DROP  R1                                                               
*                                                                               
ACCL20   DS    0H                                                               
         CLI   QOPT2,C'Y'          SHOW ONLY SCREWED UP TIME LINE IF Y          
         BNE   ACCL30                                                           
         TM    TIMEFLAG,TMSTMPLN   WANT TO SEE ONLY BAD LINES                   
         BNO   ACCL80                                                           
*                                                                               
ACCL30   TM    FLAG,FLGPER         WAS CODE AND NAME PRINTED?                   
         BO    ACCL40                                                           
         OI    FLAG,FLGPER                                                      
         MVC   PPERCDE,TIMEPER       PERSON CODE                                
         MVC   PPERNME,TIMENME       PERSON NAME                                
ACCL40   AP    PKLINES,=P'1'       INCREMENT RECORD COUNTER                     
*                                                                               
         CLC   LSTTS,TIMEPEDT      ARE WE DOING A NEW T/S?                      
         BE    ACCL50                                                           
         AP    PKTS,=P'1'                                                       
         MVC   LSTTS,TIMEPEDT                                                   
         GOTO1 DATCON,DMCB,(1,TIMEPEDT),(8,PEDATE)                              
*                                                                               
ACCL50   EDIT  TIMELINE,PTMSLIN    PRINT TMS LINE NO.                           
         MVC   PTEMPO,SPACES                                                    
*                                                                               
         TM    TIMESTA,TIMTEMPO    IS IT A TEMPO TIMESHEET?                     
         BNO   ACCL60                                                           
         MVC   PTEMPO,=CL7'*TEMPO*'                                             
         AP    PKTMPLNS,=P'1'      INCREMENT TEMPO LINES COUNTER                
         CLC   LSTTMPTS,TIMEPEDT   SAME TIMESHEET AS BEFORE?                    
         BE    ACCL60                                                           
         AP    PKTMPTS,=P'1'       INCREMENT TEMPO T/S COUNTER                  
         MVC   LSTTMPTS,TIMEPEDT   UPDATE CHECK T/S                             
*                                                                               
ACCL60   CLC   TIMTMPLN,SPACES                                                  
         BE    ACCL70                                                           
         AP    PKTMPNUM,=P'1'                                                   
         EDIT  TIMTMPLN,PTMPLIN     TEMPO LINE NUMBER                           
*                                                                               
ACCL70   EDIT  (P3,TIMHOURS),PHOURS,2,MINUS=YES                                 
         MVC   PERROR,SPACES                                                    
         TM    TIMEFLAG,TMPNOLIN                                                
         BNO   *+16                                                             
         MVC   PERROR,=CL20'TEMPO BUT NO LINE#'                                 
         AP    PKTMPNLN,=P'1'      TEMPO WITHOUT TEMPO LINE NUMBERS             
         BAS   RE,PRINTIT                                                       
ACCL80   LA    R2,TIMELNQ(R2)                                                   
         BCT   R3,ACCL20                                                        
         BAS   RE,PRINTIT                                                       
*                                                                               
         NI    FLAG,X'FF'-(FLGPER+FLGNOFF)  RESET FLAG                          
         CP    PKLINES,=P'0'                                                    
         BE    ACCLX                                                            
*                                                                               
         MVC   PPERNME(30),=CL30'TOTAL NUMBER OF TIMESHEET : '                  
         EDIT  PKTS,PEDATE,ZERO=NOBLANK                                         
         BAS   RE,PRINTIT                                                       
*                                                                               
         MVC   PPERNME+1(30),=CL30'TOTAL NUMBER OF TMS T/S : '                  
         ZAP   PKFLDS,PKTS                                                      
         SP    PKFLDS,PKTMPTS      SUBTRACT OUT TEMPO                           
         EDIT  PKFLDS,PEDATE,ZERO=NOBLANK                                       
         BAS   RE,PRINTIT                                                       
*                                                                               
         MVC   PPERNME+1(30),=CL30'TOTAL NUMBER OF TEMPO T/S : '                
         EDIT  PKTMPTS,PEDATE,ZERO=NOBLANK                                      
         BAS   RE,PRINTIT                                                       
*                                                                               
         MVC   PPERNME(30),=CL30'TOTAL NUMBER OF TIME LINES : '                 
         EDIT  PKLINES,PEDATE,ZERO=NOBLANK                                      
         BAS   RE,PRINTIT                                                       
*                                                                               
         MVC   PPERNME+1(30),=CL30'TOTAL NUMBER OF TMS LINES : '                
         ZAP   PKFLDS,PKLINES                                                   
         SP    PKFLDS,PKTMPLNS     SUBTRACT OUT TEMPO                           
         EDIT  PKFLDS,PEDATE,ZERO=NOBLANK                                       
         BAS   RE,PRINTIT                                                       
*                                                                               
         MVC   PPERNME+1(30),=CL30'TOTAL NUMBER OF TEMPO LINES : '              
         EDIT  PKTMPLNS,PEDATE,ZERO=NOBLANK                                     
         BAS   RE,PRINTIT                                                       
*                                                                               
         MVC   PPERNME(30),=CL30'TOTAL LINES W/ TEMPO LINE # :'                 
*        ZAP   PKFLDS,PKTMPNUM                                                  
*        SP    PKFLDS,PKTMPNLN                                                  
         EDIT  PKTMPNUM,PEDATE,ZERO=NOBLANK                                     
         BAS   RE,PRINTIT                                                       
*                                                                               
         MVC   PPERNME(30),=CL30'TOTAL TMS LINE W/ TEMPO LINE# : '              
         ZAP   PKFLDS,PKTMPLNS                                                  
         SP    PKFLDS,PKTMPNUM                                                  
         ZAP   PKFLD1,PKTMPNLN                                                  
         SP    PKFLD1,PKFLDS                                                    
         EDIT  PKFLD1,PEDATE,ZERO=NOBLANK                                       
         BAS   RE,PRINTIT                                                       
*                                                                               
         MVC   PPERNME+1(38),=CL38'TOTAL TEMPO LINES W/O TEMPO LINE#S:'         
         EDIT  PKTMPNLN,PEDATE,ZERO=NOBLANK                                     
         BAS   RE,PRINTIT                                                       
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
ACCLX    B     EXIT                                                             
         DROP  R2,R5                                                            
         EJECT                                                                  
**********************************************************************          
* COMPANY LAST                                                       *          
**********************************************************************          
         SPACE 1                                                                
CPYL     DS    0H                                                               
         TM    FLAG2,FLGCPY                                                     
         BNO   CPYLX                                                            
         AP    PKCPYTOT,=P'1'                                                   
CPYLX    B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* RUN LAST                                                           *          
**********************************************************************          
         SPACE 1                                                                
RUNL     DS    0H                                                               
         MVI   RCSUBPRG,2                                                       
         CP    PKCPYTOT,=P'0'                                                   
         BE    RUNX                                                             
         MVC   XP(36),=CL36'TOTAL NUMBER OF BAD COMPANIES: '                    
         EDIT  PKCPYTOT,(8,XP+38)                                               
         GOTO1 ACREPORT                                                         
*                                                                               
         LA    R1,CPYWRK                                                        
RUNL10   CLI   0(R1),X'FF'         EOF?                                         
         BE    RUNX                                                             
         MVC   XP+3(2),0(R1)                                                    
         GOTO1 ACREPORT                                                         
         LA    R1,2(R1)                                                         
         B     RUNL10                                                           
*                                                                               
RUNX     B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* PRINT ROUTINE                                                      *          
**********************************************************************          
         SPACE 1                                                                
PRINTIT  NTR1                                                                   
         BAS   RE,HEADUP                                                        
         GOTO1 ACREPORT                                                         
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* GET HEIRARCHY LEVEL STRUCTURE                                      *          
**********************************************************************          
         SPACE 1                                                                
         USING ACLELD,R5                                                        
GETLEVS  NTR1                                                                   
         L     R5,ADLDGHIR         GET HEIRARCHY LEVELS                         
         MVC   LEVELS(LEVLNQ),SPACES                                            
         SR    R1,R1                                                            
         IC    R1,ACLLN            R1 = LENGTH OF ELEMENT                       
         SH    R1,=Y(ACLLN1Q+1)    SUBTRACT OVERHEAD                            
         EX    R1,*+4                                                           
         MVC   LEVELS(0),ACLVALS   MVC LEVEL LENGTHS/NAMES INTO STORAGE         
*                                                                               
* CONVERT 1,3,5,12 -> 1,2,2,7                                                   
*                                                                               
         LA    R0,LEVELQ           R0 = MAXIMUM NUMBER OF LEVELS                
         STC   R0,LEVNUM           ASSUME 4 LEVEL STRUCTURE                     
         LA    R1,LEVELS           R1 = FIRST LEVEL LENGTH                      
         LA    R2,LEVLNQS          R2 = FIRST LEVEL INDIVIDUAL LENGTH           
         SR    R3,R3               R3 = PREV LEVEL LENGTH (INITIALLY 0)         
         SR    R4,R4                                                            
GLEV10   ICM   R4,1,0(R1)          CURRENT LEVEL LENGTH                         
         BZ    GLEV20              NO MORE LEVELS - ADJUST LEVNUM               
         SR    R4,R3               SUBTRACT CURRENT FROM PREVIOUS               
         STC   R4,0(R2)            CURRENT INDIVIDUAL LENGTH                    
         IC    R3,0(R1)            UPDATE R3                                    
         LA    R1,L'ACLVALS(R1)                                                 
         LA    R2,1(R2)                                                         
         BCT   R0,GLEV10                                                        
         B     GLEVX                                                            
*                                                                               
GLEV20   LA    R1,LEVELQ           R1 = MAXIMUM NUMBER OF LEVELS                
         SR    R1,R0               R0 = NUM OF LEVELS OF MAX NOT USED           
         STC   R1,LEVNUM           LEVNUM = ACTUAL NUMBER OF LEVELS             
*                                                                               
GLEVX    B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
**********************************************************************          
* SET LEVEL CODES                                                    *          
**********************************************************************          
         SPACE 1                                                                
         USING ACTRECD,R5                                                       
SETCDE   NTR1                                                                   
         L     R5,ADACC            A(ACCOUNT RECORD)                            
         MVC   LEVSCDE(LEVSLNQ),SPACES     CLEAR LEVEL CODES AREA               
         SR    R0,R0                                                            
         IC    R0,LEVNUM           NUMBER OF LEVELS/LOOPS                       
         LA    R1,ACTKACT          FULL ACCOUNT CODE                            
         LA    R2,LEVSCDE          FIRST LEVEL CODE                             
         LA    R3,LEVLNQS          FIRST LEVEL LENGTHS                          
*                                                                               
SETCDE10 SR    R4,R4                                                            
         IC    R4,0(R3)            CURRENT INDIVIDUAL LEV LENGTH                
         BCTR  R4,0                                                             
         EX    R4,*+4                                                           
         MVC   0(0,R2),0(R1)                                                    
         LA    R1,1(R4,R1)         BUMP TO NEXT LEVEL IN ACCOUNT CODE           
         LA    R2,L'LEVSCDE(R2)    BUMP TO NEXT LEVEL CODE AREA                 
         LA    R3,1(R3)            BUMP TO NEXT INDIVIUAL LEV LENGTH            
         BCT   R0,SETCDE10                                                      
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
**********************************************************************          
* GET NAME ROUTINE                                                   *          
*     R2 = NAME ELEMENT (SOURCE)                                     *          
*     R3 = NAME FIELD   (DESTINATION)                                *          
**********************************************************************          
         SPACE 1                                                                
         USING NAMELD,R2                                                        
GETNME   NTR1                                                                   
         MVC   0(L'LEVNMES,R3),SPACES                                           
         IC    R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q+1)                                                 
         BM    GETNX                                                            
         EX    R1,*+4                                                           
         MVC   0(0,R3),NAMEREC                                                  
*                                                                               
GETNX    B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
**********************************************************************          
* SETUP HEADLINES                                                    *          
**********************************************************************          
         SPACE 1                                                                
HEADUP   NTR1                                                                   
         MVC   XHEAD3+1(10),=CL10'LOGIN ID :'                                   
         MVC   XHEAD3+12(L'SVCLOGO),SVCLOGO  COMPANY'S MAIN ID                  
*                                                                               
         USING HEADD,R3                                                         
         LA    R3,XHEAD5                                                        
         MVC   HEADDESC,LEVADSC      LEVEL DESCRIPTION                          
         MVC   HEADCODE,LEVACDE      LEVEL CODE                                 
         MVC   HEADNAME,LEVANME      LEVEL NAME                                 
*                                                                               
         LA    R3,XHEAD6                                                        
         MVC   HEADDESC,LEVBDSC      LEVEL DESCRIPTION                          
         MVC   HEADCODE,LEVBCDE      LEVEL CODE                                 
         MVC   HEADNAME,LEVBNME      LEVEL NAME                                 
*                                                                               
         LA    R3,XHEAD7                                                        
         MVC   HEADDESC,LEVCDSC      LEVEL DESCRIPTION                          
         MVC   HEADCODE,LEVCCDE      LEVEL CODE                                 
         MVC   HEADNAME,LEVCNME      LEVEL NAME                                 
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* CONSTANTS                                                          *          
**********************************************************************          
         SPACE 1                                                                
ATIMETAB DC    A(TIMETAB)          TRANSACTION TABLE                            
ABINADD  DC    A(BINADD)           ROUTINE TO ADD TO BINSEARCH TABLE            
ADUMP    DC    A(DUMP)             ROUTINE TO DITTO RECORDS                     
*                                                                               
ACCOUNT  DC    CL8'ACCOUNT'                                                     
         SPACE 2                                                                
**********************************************************************          
* RELOCATACBLES                                                      *          
**********************************************************************          
         SPACE 1                                                                
ADCONS   DS    0F                                                               
         DC    V(PRNTBL)           PRINT DATA                                   
         DC    V(HELLO)            HELLO CALL                                   
         SPACE 2                                                                
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* GETEL                                                              *          
**********************************************************************          
         SPACE 1                                                                
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
***********************************************************************         
* ADD ITEM TO BINSRCH TABLE AND ACCUMULATE TOTALS                     *         
*  P1    A(ITEM TO BE ADDED)                                          *         
*  P2    A(TABLE)                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING BIND,R5                                                          
BINADD   DS    0D                                                               
         NMOD1 0,**BINA**                                                       
         L     RC,0(R1)                                                         
*                                                                               
         L     R5,8(R1)                                                         
         MVC   DMCB+8(16),BININ    NUMBER LENGTH,KEY,MAX                        
         LA    R6,BINTAB           A(TABLE)                                     
         L     R3,4(R1)            A(ITEM)                                      
         GOTO1 BINSRCH,DMCB,(X'01',(R3)),(R6)                                   
         OC    DMCB(4),DMCB                                                     
         BNZ   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
         MVC   BININ,DMCB+8        UPDATE COUNT                                 
*                                                                               
         CLI   DMCB,1              RECORD WAS ADDED                             
         BE    BINXIT                                                           
*                                                                               
         L     R4,DMCB             A(RECORD FOUND)                              
         LA    R6,TIMKLNQ          BUMP PAST KEY                                
         AR    R4,R6                  IN TABLE ENTRY                            
         AR    R3,R6                  IN NEW ENTRY                              
         OC    0(1,R4),0(R3)       TURN ON STATUS                               
         B     BINXIT                                                           
*                                                                               
BINXIT   XIT1                                                                   
         DROP  R5                                                               
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DUMP RECORDS                                                        *         
***********************************************************************         
         SPACE 1                                                                
DUMP     NMOD1 0,**DMP**                                                        
         L     RC,0(R1)                                                         
         CLI   QOPT7,C'Y'                                                       
         BNE   DUMPX                                                            
         LA    R0,L'MSG                                                         
         LA    R2,MSG                                                           
         L     R3,4(R1)                                                         
         L     R4,8(R1)                                                         
*                                                                               
         LA    R5,=C'2D'                                                        
         GOTO1 PRNTBL,DMCB,((R0),(R2)),(R3),C'DUMP',(R4),(R5),         X        
               (C'P',PRINT)                                                     
*                                                                               
DUMPX    XIT1                                                                   
*        MVC   MSG,=CL10'TRNS  REC'                                             
*        GOTO1 ADUMP,DMCB,(RC),(R2),(R6)                                        
*                                                                               
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* DATAMGR INTERFACE                                                  *          
**********************************************************************          
         SPACE 1                                                                
DMSEQDR  NMOD1 0,SEQ               READ SEQUENTIAL                              
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,(X'88',DMRSEQ),=C'ACCDIR ',SVKEY,IO,0               
         B     DMX                                                              
*                                                                               
DMHIGHDR NMOD1 0,HIGH              READ HIGH                                    
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,(X'88',DMRDHI),=C'ACCDIR ',SVKEY,IO,0               
         B     DMX                                                              
*                                                                               
DMREADDR NMOD1 0,READ              READ                                         
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,(X'88',DMREAD),=C'ACCDIR ',SVKEY,IO,0               
         B     DMX                                                              
*                                                                               
DMGETREC NMOD1 0,GREC              GET RECORD                                   
         L     RC,0(R1)            RESET RC                                     
         USING ACCRECD,R3                                                       
         LA    R3,IO                                                            
         GOTO1 DATAMGR,DMCB,DMGET,=C'ACCOUNT ',ACCKDA,IO,DMWORK                 
         B     DMX                                                              
*                                                                               
DMPUTREC NMOD1 0,PREC              PUT RECORD                                   
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,=CL8'PUTREC',=C'ACCOUNT',ACCKDA,IO,DMWORK           
*                                                                               
DMX      XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* TABLES                                                             *          
**********************************************************************          
        SPACE 1                                                                 
*                                                                               
* BINTABLE 1 - TRANSACTION TABLE                                                
*                                                                               
         DC    C'*TIMEBIN**'                                                    
TIMETAB  DS    0D                  BINTABLE CONSTANTS FOR TABLE 5               
         DC    F'0'                    NUMBER IN TABLE                          
         DC    AL4(TIMELNQ)            LENGTH OF ENTRY                          
         DC    AL1(0)                  DISP. TO KEY                             
         DC    AL3(TIMKLNQ)            KEY LENGTH                               
         DC    AL4(TIMEMAX)            MAX IN TABLE                             
         DS    (TIMEMAX*TIMELNQ)XL1    TABLE                                    
*                                                                               
TIMEMAX  EQU   20000                                                            
         EJECT                                                                  
**********************************************************************          
* WORKING STORAGE                                                    *          
**********************************************************************          
        SPACE 1                                                                 
ACZTD    DSECT                                                                  
VTYPES   DS    0A                                                               
PRNTBL   DS    V                   PRINT DATA                                   
VHELLO   DS    V                   HELLO                                        
VTYPLNQ  EQU   *-VTYPES                                                         
*                                                                               
DISP2    DS    H                   DISPLACEMENT TO KEY                          
ELCODE   DS    CL1                 ELEMENT CODE                                 
DELELQ   EQU   X'FF'               DELETE ELEMENT FF                            
*                                                                               
PKFLDS   DS    PL4                                                              
PKFLD1   DS    PL4                                                              
PKTS     DS    PL4                 TOTAL TIMESHEETS                             
PKTMPTS  DS    PL4                 TOTAL TEMPO TIMESHEETS                       
PKLINES  DS    PL4                 TOTAL LINES                                  
PKTMPLNS DS    PL4                 TOTAL TEMPO LINES                            
PKTMPNUM DS    PL4                 TEMPO LINES WITH TEMPO LINE #                
PKTMPNLN DS    PL4                 TEMPO LINES WITHOUT TEMPO LINE #             
PKFLDLNQ EQU   (*-PKFLDS)/L'PKFLDS                                              
PKCPYTOT DS    PL4                 TOTAL NO. OF COMPANIES                       
*                                                                               
MSG      DS    CL15                DUMP MESSAGE                                 
*                                                                               
FLAG     DS    XL1                                                              
FLGPER   EQU   X'80'               PERSON WAS DONE ALREADY                      
FLGNOFF  EQU   X'40'               NEW OFFICE IN USE                            
FLGTEMPO EQU   X'20'                                                            
HASTMPLN EQU   X'10'                                                            
FLGBDPER EQU   X'08'  PERSON HAS TIMESHEET WITH TEMPO LIN BUT NO TEMPO          
FLAG2    DS    XL1                                                              
FLGCPY   EQU   X'80'                                                            
*                                                                               
SVKEY    DS    CL49                                                             
SVACCT   DS    CL12                                                             
SVPEDT   DS    CL3                                                              
SVCLOGO  DS    CL7                 COMPANY'S LOGIN ID (LOGO)                    
*                                                                               
LASTKEY  DS    CL(TIMKEND)                                                      
LSTTS    DS    XL3                 LAST T/S DATE                                
LSTTMPTS DS    XL3                 LAST TEMPO T/S DATE                          
*                                                                               
START    DS    XL3                                                              
END      DS    XL3                                                              
*                                                                               
LEVELS   DS    0XL16                                                            
LEVA     DS    XL1                 LEVEL A LENGTH                               
LEVADSC  DS    CL15                LEVEL A NAME                                 
LEVB     DS    XL1                 LEVEL B LENGTH (A+B)                         
LEVBDSC  DS    CL15                LEVEL B NAME                                 
LEVC     DS    XL1                 LEVEL C LENGTH (A+B+C)                       
LEVCDSC  DS    CL15                LEVEL C NAME                                 
LEVD     DS    XL1                 LEVEL D LENGTH (A+B+C+D)                     
LEVDDSC  DS    CL15                LEVEL D NAME                                 
LEVLNQ   EQU   *-LEVELS                                                         
*                                                                               
LEVLNQS  DS    0XL1                                                             
LEVLNQA  DS    XL1                 LEVEL A INDIVIDUAL LENGTH                    
LEVLNQB  DS    XL1                 LEVEL B INDIVIDUAL LENGTH                    
LEVLNQC  DS    XL1                 LEVEL C INDIVIDUAL LENGTH                    
LEVLNQD  DS    XL1                 LEVEL D INDIVIDUAL LENGTH                    
*                                                                               
LEVNUM   DS    XL1                 NUMBER OF LEVELS IN HEIRARCHY                
LEVELQ   EQU   4                   MAXIMUM NUMBER OF LEVELS                     
*                                                                               
LEVSCDE  DS    0CL12               LEVEL CODES                                  
LEVACDE  DS    CL12                LEVEL A CODE                                 
LEVBCDE  DS    CL12                LEVEL B CODE                                 
LEVCCDE  DS    CL12                LEVEL C CODE                                 
LEVDCDE  DS    CL12                LEVEL D CODE                                 
LEVSLNQ  EQU   *-LEVSCDE                                                        
*                                                                               
LEVNMES  DS    0CL36               LEVEL NAMES                                  
LEVANME  DS    CL36                LEVEL A NAME                                 
LEVBNME  DS    CL36                LEVEL B NAME                                 
LEVCNME  DS    CL36                LEVEL C NAME                                 
LEVDNME  DS    CL36                LEVEL D NAME                                 
*                                                                               
TIMEWRK  DS    CL(TIMELNQ)                                                      
*                                                                               
CPYWRK   DS    CL200               ONE BYTE FOR EOF                             
*                                                                               
IO       DS    0CL2042                                                          
IOKEY    DS    CL42                KEY                                          
IODATA   DS    CL2000              DATA                                         
IOLNQ    EQU   *-IO                LENGTH                                       
         EJECT                                                                  
***********************************************************************         
* DSECT FOR PRINT LINE                                                *         
***********************************************************************         
         SPACE 1                                                                
PLINED   DSECT                                                                  
PRTLINE  DS    0C                                                               
         DS    CL1                                                              
PPERCDE  DS    CL6                 PERSON CODE                                  
         DS    CL2                                                              
PPERNME  DS    CL36                PERSON NAME                                  
         DS    CL4                                                              
PEDATE   DS    CL8                 TRANSACTION DATE                             
         DS    CL12                                                             
PTMSLIN  DS    CL6                 TMS LINE NUMBER                              
         DS    CL5                                                              
PTEMPO   DS    CL7                 TEMPO EYECATCHER - *TEMPO*                   
         DS    CL5                                                              
PTMPLIN  DS    CL2                 TEMPO LINE NUMBER                            
         DS    CL5                                                              
PHOURS   DS    CL8                 TOTAL HOURS                                  
         DS    CL2                                                              
PERROR   DS    CL20                                                             
         DS    CL2                                                              
PLINLNQ  EQU   *-PLINED                                                         
*                                                                               
         EJECT                                                                  
***********************************************************************         
* DSECT FOR HEADLINES                                                 *         
***********************************************************************         
         SPACE 1                                                                
HEADD    DSECT                                                                  
         DS    CL1                                                              
HEADDESC DS    CL15                                                             
         DS    CL1                                                              
HEADCODE DS    CL6                 1R OFFICE                                    
         DS    CL1                                                              
HEADNAME DS    CL36                DECSRIPTION                                  
HEADLN   EQU   *-HEADD                                                          
         EJECT                                                                  
***********************************************************************         
* DSECT TIME TABLE WORK AREA                                          *         
***********************************************************************         
         SPACE 1                                                                
TIMED    DSECT                                                                  
TIMEPER  DS    CL10                PERSON CODE                                  
TIMEPEDT DS    XL3                 PERIOD END DATE                              
TIMELINE DS    XL2                                                              
TIMKLNQ  EQU   *-TIMED                                                          
TIMHOURS DS    PL3                                                              
TIMTMPLN DS    XL2                                                              
TIMENME  DS    CL36                PERSON NAME                                  
TIMESTA  DS    XL1                 STATUS BYTE                                  
TIMEFLAG DS    XL1                 FLAG FOR EACH TIME LINE                      
TMPNOLIN EQU   X'80'               IT IS TEMPO BUT NO TMPO LINE                 
TMSTMPLN EQU   X'40'               TMS SHEET WITH TEMPO LINE                    
TIMELNQ  EQU   *-TIMED                                                          
         EJECT                                                                  
***********************************************************************         
* DSECT FOR BINSRCH PARAMETERS                                        *         
***********************************************************************         
         SPACE 1                                                                
BIND     DSECT                                                                  
BININ    DS    F                   NUMBER IN TABLE                              
BINLEN   DS    F                   RECORD LENGTH                                
BINDISP  DS    CL1                 DISPLACEMENT IN RECORD                       
BINKEY   DS    CL3                 KEY LENGTH                                   
BINMAX   DS    F                   MAXIMUM NUMBER IN TABLE                      
BINKLN   EQU   *-BIND                                                           
BINTAB   DS    0C                  THE TABLE                                    
         EJECT                                                                  
***********************************************************************         
*               ++INCLUDES                                            *         
***********************************************************************         
*                                                                               
* ACBIGPRINTD                                                                   
* ACREPWORKD                                                                    
* ACGENFILE                                                                     
* ACGENMODES                                                                    
* DDLOGOD                                                                       
* ACMASTD                                                                       
* DDMASTD                                                                       
* DDBIGBOX                                                                      
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACBIGPRNTD                                                     
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE DDLOGOD                                                        
       ++INCLUDE ACMASTD                                                        
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDDLCB                                                         
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'157RGUPZT02  10/10/00'                                      
         END                                                                    
