*          DATA SET ACREPIM02  AT LEVEL 012 AS OF 08/17/00                      
*PHASE ACIM02A                                                                  
*INCLUDE DLFLD                                                                  
*INCLUDE PRNTBL                                                                 
***********************************************************************         
*             QOPT1:'Y'= DOWNLOAD                                     *         
***********************************************************************         
         TITLE 'ELECTRONIC VENDOR ESTIMATE INFORMATION'                         
ACIM02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACIM**,R7,R9    BASE REGISTERS 11,9                          
         L     RA,0(R1)                                                         
         USING ACWORKD,RA          RA = A(GLOBAL W/S)                           
         LA    RC,SPACEND                                                       
         USING ACIMD,RC            RC = A(SAVE W/S)                             
         L     R8,VBIGPRNT                                                      
         USING BIGPRNTD,R8                                                      
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         CLI   MODE,PROCACC                                                     
         BE    PACC                                                             
         CLI   MODE,REQLAST                                                     
         BE    REQL                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* RUN FIRST                                                           *         
***********************************************************************         
         SPACE 1                                                                
RUNF     DS    0H                                                               
         USING ACCRECD,RE                                                       
         L     RE,AIO1                                                          
         MVC   DISP2,=Y(ACCRFST-ACCKEY)      SET DISP2                          
         DROP  RE                                                               
*                                                                               
         MVC   VTYPES(VTYPLNQ),ADCONS        RELOCATE ADDRESSES                 
*                                                                               
         L     R2,ABOXRC           SET UP BOX ROUTINE                           
         ST    RC,0(R2)                                                         
         L     R2,ABXHOOK                                                       
         ST    R2,HEADHOOK                                                      
         GOTO1 DATCON,DMCB,(5,0),(5,TODAY)                                      
*                                                                               
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* REQUEST FIRST                                                       *         
***********************************************************************         
         SPACE 1                                                                
REQF     DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         L     RE,AIO1             RE=A(IO1 AREA)                               
         LA    RF,IO1LNQ           RF=(LENGTH OF IO1 AREA)                      
         SR    R1,R1                                                            
         MVCL  RE,R0               CLEAR IO1 AREA                               
*                                                                               
         L     RF,ADWNBUF             RF=A(DOWNLOAD BUFFER)                     
         XC    0(L'DWNBUF,RF),0(RF)   CLEAR DOWNLOAD BUFFER                     
         XC    DWNSTAT,DWNSTAT        CLEAR DOWNSTAT BYTE                       
*                                                                               
         CLI   QOPT1,C'Y'          NO BOXES FOR DOWNLOADING                     
         BNE   REQF10                                                           
         MVI   RCSUBPRG,9          SET PRINT RTE AS INVALID                     
         GOTO1 ADWNL,DMCB,(RC),DWNINIT      INITIALIZE DOWNLOAD RTE             
*                                                                               
REQF10   DS    0H                                                               
         XC    STRT3,STRT3                                                      
         CLC   QSTART,SPACES                                                    
         BE    REQF20                                                           
         GOTO1 DATCON,DMCB,(0,QSTART),(1,STRT3)                                 
*                                                                               
REQF20   MVC   END3,=XL3'FFFFFF'                                                
         CLC   QEND,SPACES                                                      
         BE    REQF30                                                           
         GOTO1 DATCON,DMCB,(0,QEND),(1,END3)                                    
*                                                                               
REQF30   MVC   SAVCUL(1),RCCOMPFL                                               
         MVC   SAVCUL+1(2),=CL2'SJ'                                             
         ZAP   REQNET,=P'0'                                                     
         ZAP   REQCOM,=P'0'                                                     
         MVI   FCRDTRNS,C'N'                                                    
*                                                                               
         MVC   USERTAB(2),=C'JI'                                                
         CLI   SAVCUL,X'F5'        IS THIS BBDO?                                
         BNE   REQFX               NO                                           
         MVC   USERTAB(2),=C'PB'   YES, USE PB INSTEAD                          
*                                                                               
REQFX    B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* PROCESS ACCOUNT                                                    *          
**********************************************************************          
         SPACE 1                                                                
PACC     DS    0H                                                               
         XC    ESTSTAT,ESTSTAT                                                  
*                                                                               
*        CLI   QOPT2,C'C'          CLOSED JOBS ONLY?                            
*        BNE   *+12                NO                                           
*        BAS   RE,CLSD             IS THIS JOB CLOSED                           
*        BNE   EXIT                NO, REJECT                                   
*                                                                               
         ZAP   JOBNET,=P'0'                                                     
         ZAP   JOBCOM,=P'0'                                                     
*                                                                               
         L     R4,ADACCNAM                                                      
         LA    R5,JOBNAME                                                       
         BAS   RE,GETNAME                                                       
*                                                                               
* SAVE DATES FORM JOB ELEMENT                                                   
*                                                                               
         USING JOBELD,R4                                                        
         MVI   ELCODE,JOBELQ       X'26' - PRODUCTION JOB ELEMENT               
         L     R4,ADACC                                                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   CLOSDTE,JOBCDATE       JOB CLOSED AND OPEN DATES                 
         MVC   OPENDTE,JOBODATE       JOB CLOSED AND OPEN DATES                 
         DROP  R4                                                               
*                                                                               
         L     R4,ADACC                                                         
         USING EVERECD,R2                                                       
         LA    R2,SVKEY                                                         
         XC    SVKEY,SVKEY                                                      
         MVI   EVEKTYP,EVEKTYPQ                                                 
         MVI   EVEKSUB,EVEKSUBQ                                                 
         MVC   EVEKCPY(3),SAVCUL                                                
         MVC   EVEKCLI,SPACES                                                   
         MVC   EVEKCLI(3),3(R4)    CLIENT                                       
         MVC   EVEKPRO,SPACES                                                   
         MVC   EVEKPRO(3),6(R4)    PRODUCT                                      
         MVC   EVEKJOB(6),9(R4)    JOB                                          
         MVI   EVEKTYPE,EVEKTREV   LOOKING FOR REVISIONS ONLY                   
         MVI   EVEKVER,1           AND THE FIRST ONE AT THAT                    
*                                                                               
         L     R2,AIO1                                                          
         L     R3,AIO2                                                          
         GOTO1 =A(DMREADDR),DMCB,(RC)       READ                                
         B     PACC15                                                           
*                                                                               
PACC10   GOTO1 =A(DMSEQDR),DMCB,(RC)                                            
PACC15   CLC   0(EVEKVER-EVEKEY,R2),SVKEY STILL READING SAME JOB                
         BNE   PACC30                                                           
*                                                                               
* FILTER ESTIMATES ON DATE APPROVED                                             
*                                                                               
         USING EPRELD,R4                                                        
         MVI   ELCODE,EPRELQ       GET DATE ESTIMATE PREPARED                   
         LR    R4,R2               PREP FOR GETEL                               
         BAS   RE,GETEL                                                         
         BNE   PACC10                                                           
         MVC   SVPRDTE,EPRDATE     SAVED AREA FOR PREPARER DATE                 
*                                                                               
* IF CLOSED ONLY REQUEST - DON'T CHECK APPROVED                                 
*                                                                               
*        CLI   QOPT2,C'C'          ONLY CLOSED JOBS REQUESTED                   
*        BE    PACC20              YES - DON'T WORRY ABOUT APPROVED EST         
         MVI   ELCODE,EAPELQ       DON'T INCLUDE APPROVED ESTIMATES             
         LR    R4,R2               PREP FOR GETEL                               
         BAS   RE,GETEL                                                         
         BNE   PACC20                                                           
         USING EAPELD,R4                                                        
         CLC   EAPDATE,=XL3'00'                                                 
         BE    PACC20                                                           
         NI    ESTSTAT,X'FF'-GOTANEST                                           
         B     PACC10                                                           
*                                                                               
PACC20   BAS   RE,CHKZERO          SEE IF ITS ALL ZEROS                         
         BE    PACC10              YES                                          
*                                                                               
         OI    ESTSTAT,GOTANEST                                                 
         XR    R2,R3               SWAP R2 AND R3, DATAMGR USES R2              
         XR    R3,R2                                                            
         XR    R2,R3                                                            
         B     PACC10                                                           
*                                                                               
PACC30   ST    R2,TEXTBUFF         SAVE R2 TO READ TEXT DATA                    
*                                                                               
         LR    R2,R3               SAVED ESTIMATE POINTER TO R2                 
         TM    ESTSTAT,GOTANEST                                                 
         BNO   PACCX               THERE WERE NO SAVED REVISIONS                
*                                                                               
         USING ESTRECD,R3                                                       
         LA    R3,ESTWRK           R3=A(ESTIMATE WORK AREA)                     
*                                                                               
         MVC   ESHNUM,TEXNUM       ESTIMATE NUMBER                              
         MVC   ESHJBNM,JOBNAME                                                  
         GOTO1 DATCON,DMCB,(1,SVPRDTE),(20,WORK)                                
         MVC   ESHCREDT(4),WORK+4  MMDD                                         
         MVC   ESHCREDT+4(4),WORK  YYYY                                         
         GOTO1 DATCON,DMCB,(1,CLOSDTE),(20,WORK)                                
         MVC   ESHCLDT(4),WORK+4   MMDD                                         
         MVC   ESHCLDT+4(4),WORK   YYYY                                         
         GOTO1 DATCON,DMCB,(1,OPENDTE),(20,WORK)                                
         MVC   ESHOPDT(4),WORK+4   MMDD                                         
         MVC   ESHOPDT+4(4),WORK   YYYY                                         
*                                                                               
         BAS   RE,LDUSR            LOAD USER ROUTINE                            
         BAS   RE,GTWC             GET WORKCODE DATA                            
*                                                                               
PACCX    B     EXIT                                                             
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
**********************************************************************          
* REQUEST LAST                                                       *          
**********************************************************************          
         SPACE 1                                                                
REQL     DS    0H                                                               
         CLI   QOPT1,C'Y'          WAS DOWNLOAD OPTION SELECTED?                
         BNE   REQLX                                                            
         GOTO1 ADWNRTE,DMCB,(RC)   IF SO - POINT TO DOWNLOAD                    
REQLX    B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* RUN LAST                                                           *          
**********************************************************************          
         SPACE 1                                                                
RUNL     DS    0H                                                               
*                                                                               
RUNLX    B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* UTILITY ROUTINES                                                   *          
*         GETNAME - EDITIT - EDITRATE                                *          
**********************************************************************          
         SPACE 1                                                                
         USING NAMELD,R4                                                        
GETNAME  NTR1                                                                   
         MVC   0(36,R5),SPACES                                                  
         CLI   NAMEL,NAMELQ        MAKE SURE I WAS PASSED THE NAME EL           
         BNE   EXIT                                                             
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q+1)                                                 
         EX    R1,*+4                                                           
         MVC   0(0,R5),NAMEREC                                                  
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* ADD ITEM TO BINSRCH TABLE AND ACCUMULATE TOTALS                     *         
*  P1    A(ITEM TO BE ADDED)                                          *         
*  P2    A(TABLE)                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING BIND,R5                                                          
BINADD   NTR1                                                                   
         L     R5,4(R1)                                                         
         MVC   DMCB+8(16),BININ    NUMBER LENGTH,KEY,MAX                        
         LA    R6,BINTAB           A(TABLE)                                     
         L     R3,0(R1)            A(ITEM)                                      
         GOTO1 BINSRCH,DMCB,(X'01',(R3)),(R6)                                   
         OC    DMCB(4),DMCB                                                     
         BNZ   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
         MVC   BININ,DMCB+8        UPDATE COUNT                                 
         CLI   DMCB,1              RECORD WAS ADDED                             
         BE    BINXIT                                                           
         L     R4,DMCB             A(RECORD FOUND)                              
         SR    R0,R0                                                            
         ICM   R0,1,BINNUM         NUMBER OF BUCKETS                            
         BZ    BINXIT                NO BUCKETS - EXIT                          
         DC    H'0'                NO BUCKETS SO DIE IF GETS HERE               
*        SR    R6,R6                                                            
*        IC    R6,BINFST           DISPLACEMENT TO FIRST BUCKET                 
*        AR    R4,R6               BUMP TO FIRST BUCKET IN TABLE                
*        AR    R3,R6               BUMP TO FIRST BUCKET IN NEW ITEM             
*INA10   AP    0(L'NCPBKT,R4),0(L'NCPBKT,R3)   ADD TO BUCKET                    
*        LA    R3,L'NCPBKT(R3)     BUMP TO NEXT ENTRY IN NEW ITEM               
*        LA    R4,L'NCPBKT(R4)     BUMP TO NEXT ENTRY IN TABLE                  
*        BCT   R0,BINA10                                                        
*                                                                               
BINXIT   B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
**********************************************************************          
* MAKE SURE THERE ARE NON ZERO AMOUNTS ON THE ESTIMATE               *          
*      R2 IS A(ESTIMATE RECORD)                                      *          
*      RETURNS NEQ IF NON-ZERO                                       *          
**********************************************************************          
         SPACE 1                                                                
CHKZERO  NTR1                                                                   
         LR    R4,R2                                                            
         USING EDAELD,R4                                                        
         MVI   ELCODE,EDAELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   CHKZYES                                                          
*                                                                               
CHKZ10   CP    EDACOMM,=P'0'       ASSURE NON ZERO ESTIMATE                     
         BNE   CHKZNO              RETURN W/ NEQ CC                             
         CLI   EDALN,EDALNQ1       IS THERE A NON-COMMISSIONABLE AMOUNT         
         BE    CHKZ20              NO                                           
         CP    EDANCOM,=P'0'       RETURN NEQ                                   
         BNE   CHKZNO                                                           
*                                                                               
CHKZ20   BAS   RE,NEXTEL                                                        
         BE    CHKZ10                                                           
*                                                                               
CHKZYES  CR    RB,RB               RETURN EQ CC (EST IS ALL ZEROS)              
CHKZNO   B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* LOAD UFDATA WITH DATA FROM THE USER FIELDS ON THIS JOB             *          
**********************************************************************          
         SPACE 1                                                                
         USING UFSELD,R4                                                        
LDUSR    NTR1                                                                   
         BAS   RE,CLRUFDAT                                                      
         L     R4,ADACC                                                         
         MVI   ELCODE,UFSELQ                                                    
         BAS   RE,GETEL                                                         
LU10     BNE   EXIT                                                             
         USING LUD,R3                                                           
         LA    R3,USERTAB                                                       
         LA    R0,LUNUM                                                         
*                                                                               
LU20     CLC   UFSCODE,LUCODE      IS THIS CODE IN THE TABLE                    
         BE    LU50                YES, SAVE IT IN UFDATA                       
         LA    R3,LUDLN(R3)                                                     
         BCT   R0,LU20                                                          
*                                                                               
         B     LU60                THIS USER FIELD NOT NEEDED ON TAPE           
*                                                                               
LU50     ZIC   R2,UFSLN                                                         
         SH    R2,=H'33'                                                        
         BM    LU60                NO DATA ON THIS USER FIELD                   
*                                                                               
         LA    R1,UFDATA           POINT R1 TO FIELD FOR THIS UF                
         SR    R0,R0                                                            
         ICM   R0,B'0011',LUJBFLD  OFFSET INTO UFDATA OF THIS FIELD             
         AR    R1,R0                                                            
*                                                                               
         ZIC   R6,LUJBLEN          LENGTH OF THIS TAPE FIELD                    
         BCTR  R6,0                                                             
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),SPACES                                                   
*                                                                               
         CR    R2,R6               Q, ACTUAL LENGTH GREATER THAN MAX.           
         BNH   *+6                                                              
         LR    R2,R6               Y, ALLOW ONLY THE MAX. LENGTH                
         EX    R2,LU80                                                          
LU60     BAS   RE,NEXTEL                                                        
         B     LU10                                                             
*                                                                               
LU80     MVC   0(0,R1),UFSDATA                                                  
         DROP  R3,R4                                                            
         EJECT                                                                  
**********************************************************************          
* GET WORK CODE DATA, CREATE/REPORT 30 RECORDS                       *          
* R2 IS A(ESTIMATE RECORD)                                           *          
**********************************************************************          
         SPACE 1                                                                
GTWC     NTR1                                                                   
         LR    R4,R2                                                            
         USING EDAELD,R4                                                        
         MVI   ELCODE,EDAELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   GTWCX                                                            
*                                                                               
GTWC10   CP    EDACOMM,=P'0'       ASSURE NON ZERO ESTIMATE                     
         BNE   GTWC40                                                           
*                                                                               
         CLI   EDALN,EDALNQ1       IS THERE A NON-COMMISSIONABLE AMOUNT         
         BE    GTWC20              NO                                           
         CP    EDANCOM,=P'0'                                                    
         BNE   GTWC40                                                           
*                                                                               
GTWC20   BAS   RE,NEXTEL                                                        
         BE    GTWC10                                                           
         B     GTWCX               NO NON ZERO WORKCODES                        
*                                                                               
GTWC40   DS    0H                                                               
         MVC   WC,EDAWORK                                                       
         ZAP   WCCOMM,EDACOMM                                                   
         ZAP   WCNCOM,=P'0'                                                     
         CLI   EDALN,EDALNQ1       IS THERE A NON-COMMISSIONABLE AMOUNT         
         BE    *+10                NO                                           
         ZAP   WCNCOM,EDANCOM                                                   
         ZAP   WCNET,WCCOMM        NET IS COMMISSIONABLE PLUS                   
         AP    WCNET,WCNCOM        NON-COMMISSIONABLE                           
*                                                                               
         BAS   RE,CALCCOM          GET WORKCODE RATE/COMMISSION AMOUNT          
*                                  RETURN COMMISSION AMOUNT IN WCCOMM           
         AP    JOBNET,WCNET                                                     
         AP    REQNET,WCNET                                                     
         AP    JOBCOM,WCCOMM                                                    
         AP    REQCOM,WCCOMM                                                    
*                                                                               
GTWC50   MVI   ELCODE,EDAELQ                                                    
         BAS   RE,NEXTEL                                                        
         BNE   GTWCX               DONE W/ ESTIMATE                             
*                                                                               
         CP    EDACOMM,=P'0'       ASSURE NON ZERO ESTIMATE                     
         BNE   GTWC40                                                           
*                                                                               
         CLI   EDALN,EDALNQ1       IS THERE A NON-COMMISSIONABLE AMOUNT         
         BE    GTWC50              NO                                           
         CP    EDANCOM,=P'0'                                                    
         BNE   GTWC40                                                           
         B     GTWC50                                                           
*                                                                               
GTWCX    B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
**********************************************************************          
* CALL GETOPT, CALCULATE GROSS FIGURES FOR THIS ESTIMATE             *          
*      RETURNS COMMISSION AMOUNT IN WCCOMM                           *          
**********************************************************************          
         SPACE 1                                                                
CALCCOM  NTR1                                                                   
         USING ACTRECD,R4                                                       
         L     R4,ADACC                                                         
         USING GOBLOCKD,R2                                                      
         L     R2,ADGOBLOC                                                      
         MVC   GOADM,DATAMGR                                                    
         MVC   GOSELWC,WC                                                       
         GOTO1 GETOPT,DMCB,GOBLOCKD                                             
         MVC   GOSELWC,WC                                                       
         XC    GOSELWC,GOSELWC       RESET GETOPT                               
         ZAP   WCRATE,GOAGYCOM                                                  
         ZAP   PL16,GOAGYCOM                                                    
         MP    PL16,WCCOMM                                                      
         SRP   PL16,64-6,5                                                      
         ZAP   WCCOMM,PL16                                                      
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* ROUTINE TO CHECK IF JOB IS CLOSED                                  *          
**********************************************************************          
         SPACE 1                                                                
CLSD     NTR1                                                                   
         USING RSTELD,R4                                                        
         L     R4,ADACC                                                         
         MVI   ELCODE,RSTELQ                                                    
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    RSTSTAT,RSTSACIC    IS THIS CLOSED                               
         BO    CLSDYES             EXIT WITH CC OF EQUAL                        
         CR    RB,R4                                                            
         B     *+6                 EXIT W/O CC OF EQUAL                         
CLSDYES  CR    R4,R4                                                            
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* GETEL                                                               *         
***********************************************************************         
         SPACE 1                                                                
         GETEL R5,DATADISP,ELCODE                                               
         EJECT                                                                  
***********************************************************************         
* GETEL#2                                                             *         
***********************************************************************         
         SPACE 1                                                                
         GETELN R5,DISP2,ELCODE,2                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CONSTANTS                                                           *         
***********************************************************************         
         SPACE 1                                                                
ABOXRC   DC    A(BOXRC)                                                         
ABXHOOK  DC    A(BXHOOK)                                                        
ADUMP    DC    A(DUMP)             PRINTABLE ROUTINE                            
AIO1     DC    A(IO1)                                                           
AIO2     DC    A(IO2)                                                           
*                                                                               
TEXNUM   DC    CL25'1234567890123456789012345'                                  
VENCON   DC    CL60'MMODICA@BBDO.COM'                                           
VENNUM   DC    CL15'123456789012345'                                            
FEDID    DC    CL10'1234567890'                                                 
TEXCON   DC    CL30'ALEXA VASILIA SHEA'                                         
CONMAIL  DC    CL75'ANDREANNA.SHEA@SHEATOWNUSA.COM'                             
         EJECT                                                                  
***********************************************************************         
* RELOCATABLES                                                        *         
***********************************************************************         
         SPACE 1                                                                
ADCONS   DS    0F                                                               
         DC    A(DWNL)             DOWNLOAD ROUTINE                             
         DC    A(DWNBUF)           DOWNLOAD BUFFER                              
         DC    A(DWNRTE)           DOWNLOAD TOTALS ROUTINE                      
         DC    A(GRPTAB)           GROUP INFO BINTABLE                          
         DC    A(HD1TAB)           FIRST HEADLINE DWNLD FIELDS TABLE            
         DC    A(HD2TAB)           SECOND HEADLINE DWNLD FIELDS TABLE           
*                                                                               
         DC    V(PRNTBL)           PRINT DATA                                   
         DC    V(DLFLD)            DOWNLOAD MODULE                              
         EJECT                                                                  
**********************************************************************          
* DOWNLOAD ROUTINE                                                   *          
**********************************************************************          
         SPACE 1                                                                
DWNRTE   DS    0D                                                               
         NMOD1 0,**DWNR**                                                       
         L     RC,0(R1)                                                         
         MVI   RCSUBPRG,9                                                       
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         USING BOXD,R3                                                          
         L     R3,ADBOX                                                         
         MVI   BOXOFF,C'Y'         NO BOXES IF DOWNLOADING                      
         DROP  R3                                                               
*                                                                               
*        TM    DWNSTAT,DWNHDLN     WERE THE HEADLINES DOWNLOADED?               
*        BO    *+8                                                              
*        BAS   RE,DWNHEAD          DOWNLOAD HEADLINES                           
*                                                                               
*        USING BIND,R1                                                          
*        L     R1,AGRPTAB          R2=A(CLIENT TABLE)                           
*        ICM   R0,15,BININ                                                      
*        BZ    DWNXIT                                                           
*        USING GRPD,R2                                                          
*        LA    R2,BINTAB                                                        
*        DROP  R1                                                               
*                                                                               
DWNR10   DS    0H                                                               
*                                                                               
DWNR20   MVI   PRTSIZE,0                  SET COLUMN LEN=0-NO PADDING           
*        MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
*        MVC   DWNFLD(L'ALPHAID),ALPHAID  MOVE AGENCY ID TO DWN FLD             
*        LA    R1,L'ALPHAID               ALPHAID LENGTH                        
*        STC   R1,PRTSIZE                                                       
*        GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
*        MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
*        MVC   DWNFLD(L'GRPCLT),GRPCLT    MOVE CLIENT CODE TO DWN FLD           
*        LA    R1,L'GRPCLT                PAD CLIENT COLUMN                     
*        STC   R1,PRTSIZE                                                       
*        GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*        MVI   PRTSIZE,0                  RESET LENGTH=0-NO PADDING             
*                                                                               
*        MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
*        MVC   DWNFLD(L'GRPDIV),GRPDIV    MOVE DIV CODE TO DWN FLD              
*        LA    R1,L'GRPDIV                GROUP DIVISION LENGTH                 
*        STC   R1,PRTSIZE                                                       
*        GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
*        MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
*        MVC   DWNFLD,GRPDIVNM            MOVE DIV NAME TO DWN FLD              
*        LA    R1,L'GRPDIVNM              PAD DIVISION NAME COLUMN              
*        STC   R1,PRTSIZE                                                       
*        GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*        MVI   PRTSIZE,0                  RESET LENGTH=0-NO PADDING             
*                                                                               
*        MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
*        MVC   DWNFLD(L'GRPCAT),GRPCAT    MOVE CAT CODE TO DWN FLD              
*        LA    R1,L'GRPCAT                GROUP CATEGORY LENGTH                 
*        STC   R1,PRTSIZE                                                       
*        GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
*        MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
*        MVC   DWNFLD,GRPCATNM            MOVE CAT NAME TO DWN FLD              
*        LA    R1,L'GRPCATNM              PAD CATEGORY NAME COLUMN              
*        STC   R1,PRTSIZE                                                       
*        GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*        MVI   PRTSIZE,0                  RESET LENGTH=0-NO PADDING             
*                                                                               
*        MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
*        MVC   DWNFLD(L'TODAY),TODAY      MOVE RUN DATE TO DWN FLD              
*        LA    R1,L'TODAY                 RUN DATE FIELD LENGTH                 
*        STC   R1,PRTSIZE                                                       
*        GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
*        MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
*        GOTO1 ADWNL,DMCB,(RC),DWNEOL     DOWNLOAD EOL MARKER                   
*                                                                               
*WNR30   LA    R2,GRPLNQ(R2)                                                    
*        BCT   R0,DWNR10                                                        
*                                                                               
*        MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
*        GOTO1 ADWNL,DMCB,(RC),DWNEOR     DOWNLOAD EOR MARKER                   
*                                                                               
*WNXIT   XMOD1                                                                  
*        DROP  R2                                                               
         EJECT                                                                  
**********************************************************************          
* DOWNLOAD HEADLINES (ONCE PER REPORT)                               *          
**********************************************************************          
         SPACE 1                                                                
DWNHEAD  NTR1                                                                   
*        OI    DWNSTAT,DWNHDLN     SET SWITCH TO SHOW HDLNS WERE DWNLD          
*                                                                               
*        LA    R0,HD1LNQ                 NUMBER OF HEADINGS IN LINE 1           
*        L     R2,AHD1TAB                FIRST HEADLINE TABLE                   
*WNH10   MVC   DWNFLD,SPACES             SPACE OUT DOWNLOAD FIELD               
*        MVC   DWNFLD(L'HD1TAB),0(R2)    FIRST HEADLINE FIELDS                  
*        GOTO1 ADWNL,DMCB,(RC),DWNTEXT   DOWNLOAD TEXT                          
*        LA    R2,L'HD1TAB(R2)                                                  
*        BCT   R0,DWNH10                                                        
*                                                                               
*        MVC   DWNFLD,SPACES             SPACE OUT DOWNLOAD FIELD               
*        GOTO1 ADWNL,DMCB,(RC),DWNEOL    DOWNLOAD EOL MARKER                    
*                                                                               
*        LA    R0,HD2LNQ                 NUMBER OF HEADINGS IN LINE 2           
*        L     R2,AHD2TAB                SECOND HEADLINE TABLE                  
*WNH20   MVC   DWNFLD,SPACES             SPACE OUT DOWNLOAD FIELD               
*        MVC   DWNFLD(L'HD2TAB),0(R2)    FIRST HEADLINE FIELDS                  
*                                                                               
*        GOTO1 ADWNL,DMCB,(RC),DWNTEXT   DOWNLOAD TEXT                          
*        LA    R2,L'HD2TAB(R2)                                                  
*        BCT   R0,DWNH20                                                        
*                                                                               
*        MVC   DWNFLD,SPACES             SPACE OUT DOWNLOAD FIELD               
*        GOTO1 ADWNL,DMCB,(RC),DWNEOL    DOWNLOAD EOL MARKER                    
*                                                                               
*WNHX    B     DWNXIT                                                           
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* DOWNLOAD MODULE                                                    *          
*          PARM1 - RC                                                *          
*          PARM2 - ACTION                                            *          
**********************************************************************          
         SPACE 1                                                                
DWNL     DS    0D                                                               
         NMOD1 0,**DOWN**                                                       
         L     RC,0(R1)                                                         
         L     RF,4(R1)                                                         
         STC   RF,DWNMODE          SAVE CURRENT MODE                            
         USING DLCBD,R5                                                         
         L     R5,ADWNBUF                                                       
*                                                                               
         CLI   DWNMODE,DWNINIT     INITIALIZE                                   
         BE    DWNL10                                                           
         CLI   DWNMODE,DWNTEXT     DOWN-LOAD TEXT                               
         BE    DWNL20                                                           
         CLI   DWNMODE,DWNNUM      DOWN-LOAD NUMBER                             
         BE    DWNL30                                                           
         CLI   DWNMODE,DWNPACK     DOWN-LOAD NUMBER (PACKED)                    
         BE    DWNL40                                                           
         MVI   DLCBACT,DLCBEOL                                                  
         CLI   DWNMODE,DWNEOL      END OF LINE                                  
         BE    DWNL50                                                           
         MVI   DLCBACT,DLCBEOR                                                  
         CLI   DWNMODE,DWNEOR      END OF REPORT                                
         BE    DWNL50                                                           
         DC    H'0'                                                             
*                                                                               
* INITIALIZATION                                                                
*                                                                               
DWNL10   TM    DWNSTAT,DWNINTZ     HAS IT ALREADY BEEN INITIALIZED?             
         BO    DWNLX               YES - EXIT                                   
         MVI   DLCBACT,DLCBINIT    DOWN LOAD ACTION IS START                    
         LA    RE,XP               PRINT LINE                                   
         ST    RE,DLCBAPL                                                       
         LA    RE,DWNHOOK          POINT TO HOOK FOR APPLICATION                
         ST    RE,DLCBAPR                                                       
         MVC   DLCXMAXL,=Y(L'XP)                                                
         MVI   DLCXDELC,C' '       DELIMITER                                    
         MVI   DLCXEOTC,C'"'       TEXT DELIMITER                               
         MVI   DLCXEOTA,C''''      ALTERNATE TEXT DELIMITER                     
         MVI   DLCXEOLC,X'5E'      SEMI-COLON, END-OF-LINE                      
         MVI   DLCXEORC,C':'       END-OF-REPORT                                
         GOTO1 DLFLD,(R5)                                                       
         MVI   FORCEHED,C'Y'       EXCEPT FIRST TIME IN                         
         GOTO1 ACREPORT                                                         
         MVC   DLCBFLD,SPACES      MUST CLEAR FIRST TIME IN                     
*                                  TURN OFF DOWN-LOAD ROW FLDS AS C' '          
         OI    DWNSTAT,DWNINTZ     TURN ON INITIALIZED BYTE                     
         B     DWNLX               EXIT                                         
*                                                                               
* DOWNLOAD A RECORD - TEXT                                                      
*                                                                               
DWNL20   MVC   DLCBLEN,PRTSIZE     LEN OF FIELD-SET TO 0 (NO PADDING)           
         MVI   DLCBACT,DLCBPUT     ACTION IS PUT                                
         MVI   DLCBTYP,DLCBTXT     TYPE   IS TEXT                               
         MVC   DLCBFLD,SPACES                                                   
         MVC   DLCBFLD(L'DWNFLD),DWNFLD                                         
         B     DWNL50              DOWN-LOAD FIELD                              
*                                                                               
* DOWNLOAD A RECORD - NUMBER                                                    
*                                                                               
DWNL30   MVI   DLCBLEN,0           LEN OF FIELD-SET TO 0 (NO PADDING)           
         OI    DLCBFLG1,DLCBFTRM   DOWN-LOAD WITH TRAILING MINUS                
         MVI   DLCBACT,DLCBPUT     ACTION IS PUT                                
         MVI   DLCBTYP,DLCBNUM     TYPE   IS NUMBER                             
         MVI   DLCBLEN,16          YES, USE MAXIMUM LENGTH OF NUMERICS          
         MVC   DLCBFLD,SPACES                                                   
         MVC   DLCBFLD(L'DWNFLD),DWNFLD                                         
         B     DWNL50              DOWN-LOAD FIELD                              
*                                                                               
* DOWNLOAD A RECORD - NUMBER (PACKED)                                           
*                                                                               
DWNL40   MVI   DLCBTYP,DLCBPACF    PACKED DATA                                  
         OI    DLCBFLG1,DLCBFTRM   DOWN-LOAD WITH TRAILING MINUS                
         MVI   DLCBACT,DLCBPUT     ACTION IS PUT                                
         MVI   DLCBLEN,L'PKFLDS    YES, USE MAXIMUM LENGTH OF NUMERICS          
         XC    DLCBFLD,DLCBFLD     CLEAN DWNLOAD FIELD TO 0'S                   
         MVC   DLCBFLD(L'DWNFLD),DWNFLD                                         
         NC    DLCBFLD,DLCBFLD     YES, MAKE SURE NUMERIC FLD NOT ZEROS         
         BNZ   DWNL50              NOT  ZERO, DOWN-LOAD FIELD                   
         MVI   DLCBLEN,1           ZERO, SET LENGTH 1 TO DOWN-LOAD A 0          
*                                                                               
* END OF LINE/END OF RECORD                                                     
*                                                                               
DWNL50   GOTO1 DLFLD,(R5)          DOWN-LOAD FIELD                              
*                                                                               
DWNLX    XMOD1                                                                  
         DROP  R5                                                               
         EJECT                                                                  
**********************************************************************          
* DOWNLOAD HOOK                                                      *          
**********************************************************************          
         SPACE 1                                                                
DWNHOOK  MVI   FORCEHED,C'N'       NEVER HEAD UP                                
         MVI   SPACING,1                                                        
         MVI   LINE,1                                                           
         L     RF,ACREPORT                                                      
         BR    RF                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* DATAMGR INTERFACE                                                  *          
**********************************************************************          
         SPACE 1                                                                
DMSEQDR  NMOD1 0,SEQ               READ SEQUENTIAL                              
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,DMRSEQ,=C'ACCDIR ',SVKEY,(R2),0                     
         B     DMX                                                              
*                                                                               
DMHIGHDR NMOD1 0,HIGH              READ HIGH                                    
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,DMRDHI,=C'ACCDIR ',SVKEY,(R2),0                     
         B     DMX                                                              
*                                                                               
DMREADDR NMOD1 0,READ              READ                                         
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,DMREAD,=C'ACCDIR ',SVKEY,(R2),0                     
         B     DMX                                                              
*                                                                               
DMGETREC NMOD1 0,GREC              GET RECORD                                   
         L     RC,0(R1)            RESET RC                                     
         USING ACCRECD,R3                                                       
         LA    R3,IO                                                            
         GOTO1 DATAMGR,DMCB,DMGET,=C'ACCMST ',ACCKDA,(R2),DMWORK                
DMX      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BOX HOOK                                                            *         
***********************************************************************         
         SPACE 1                                                                
         USING BIGPRNTD,R8                                                      
         USING BOXD,R4                                                          
BXHOOK   DS    0D                                                               
         NMOD1 0,*BHOOK                                                         
         L     RC,BOXRC            RESTORE REG C                                
         L     R8,VBIGPRNT                                                      
         L     R4,ADBOX                                                         
*                                                                               
         CLI   RCSUBPRG,1          REGULAR P/O                                  
         BNE   BXXIT                                                            
*                                                                               
         MVC   BOXCOLS(165),XSPACES                                             
         MVC   BOXROWS,XSPACES                                                  
         MVI   BOXROWS+4,C'T'                                                   
         MVI   BOXROWS+8,C'M'                                                   
         MVI   BOXCOLS+1,C'L'                                                   
*                                                                               
         MVI   BOXCOLS+(PCLT-PRTLNE-1),C'C'                                     
         MVI   BOXCOLS+(PDIV-PRTLNE-1),C'C'                                     
         MVI   BOXCOLS+(PDIVNM-PRTLNE-1),C'C'                                   
         MVI   BOXCOLS+(PCAT-PRTLNE-1),C'C'                                     
         MVI   BOXCOLS+(PCATNM-PRTLNE-1),C'C'                                   
         MVI   BOXCOLS+(PDATE-PRTLNE-1),C'C'                                    
         MVI   BOXCOLS+PRLNQ,C'R'                                               
*                                                                               
BXXIT    MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXBLANK,C'N'                                                    
*                                                                               
         XMOD1 1                                                                
*                                                                               
BOXRC    DC    A(0)                                                             
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
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
*        MVC   MSG,=CL10'GROUP REC'                                             
*        GOTO1 ADUMP,DMCB,(RC),(R2),NCPLNQ                                      
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BUFFERS                                                             *         
***********************************************************************         
         SPACE 1                                                                
USERTAB  DS    0C                                                               
         DC    C'JI',AL2(UFJI-UFDATA),AL1(L'UFJI)                               
         DC    C'CI',AL2(UFCI-UFDATA),AL1(L'UFCI)                               
         DC    C'CE',AL2(UFCE-UFDATA),AL1(L'UFCE)                               
         DC    C'TY',AL2(UFTY-UFDATA),AL1(L'UFTY)                               
         DC    C'AC',AL2(UFAC-UFDATA),AL1(L'UFAC)                               
         DC    C'AN',AL2(UFAN-UFDATA),AL1(L'UFAN)                               
         DC    C'CC',AL2(UFCC-UFDATA),AL1(L'UFCC)                               
         DC    C'PA',AL2(UFPA-UFDATA),AL1(L'UFPA)                               
         DC    C'CN',AL2(UFCN-UFDATA),AL1(L'UFCN)                               
         DC    C'PI',AL2(UFPI-UFDATA),AL1(L'UFPI)                               
LUNUM    EQU   (*-USERTAB)/LUDLN                                                
*                                                                               
         DS    0D                  DOWNLOAD BUFFER                              
DWNBUF   DS    CL250                                                            
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE                                                     *         
***********************************************************************         
         SPACE 1                                                                
ACIMD    DSECT                                                                  
ADBOX    DS    A                   ADDRESS OF BOX ROUTINE                       
*                                                                               
VTYPES   DS    0A                                                               
ADWNL    DS    A                   DOWNLOAD ROUTINE                             
ADWNBUF  DS    A                   DOWNLOAD BUFFER                              
ADWNRTE  DS    A                   DOWNLOAD TOTALS ROUTINE                      
*                                                                               
PRNTBL   DS    V                   PRINT DATA                                   
DLFLD    DS    V                   DOWNLOAD MODULE                              
VTYPLNQ  EQU   *-VTYPES                                                         
*                                                                               
TEXTBUFF DS    A                   A(WHERE TO READ TEXT RECORDS)                
SAVERE   DS    A                                                                
*                                                                               
DISP2    DS    H                   DISPLACEMENT TO ELEMENTS                     
TODAY    DS    CL8                 TODAY'S DATE MMMDD/YY                        
*                                                                               
REQNET   DS    PL8                                                              
REQCOM   DS    PL8                                                              
JOBNET   DS    PL8                                                              
JOBCOM   DS    PL8                                                              
*                                                                               
WC       DS    CL2                                                              
WCNET    DS    PL6                                                              
WCCOMM   DS    PL6                                                              
WCNCOM   DS    PL6                                                              
WCRATE   DS    PL6                                                              
WCTXCNT  DS    PL6                 MAX 10 "35" RECORDS ON TAPE                  
PL16     DS    PL16                                                             
*                                                                               
CLOSDTE  DS    PL3                 JOB CLOSE DATE                               
OPENDTE  DS    PL3                 JOB OPEN  DATE                               
*                                                                               
JOBNAME  DS    CL36                                                             
ESTSTAT  DS    XL1                 ESTIMATE STATUS BYTE                         
GOTANEST EQU   X'80'                                                            
STRT3    DS    CL3                 PACKED REQUEST START DATE                    
END3     DS    CL3                 & END DATE                                   
SAVCUL   DS    CL3                 COMP, UNIT, LEDG I'M READING                 
SVKEY    DS    CL42                BUILD ESTIMATE KEY                           
SVTXTKEY DS    CL(L'TXTKEY)        JOB LEVEL TEXT KEY                           
SVPRDTE  DS    PL3                 PREPARED DATE                                
*                                                                               
DWNFLD   DS    CL36                SAVED AREA FOR FIELD TO BE DWNLOADED         
PRTSIZE  DS    CL1                 DOWNLOAD FLD PRINT SIZE FOR PADDING          
*                                                                               
DWNSTAT  DS    XL1                 DOWNLOAD STATUS                              
DWNINTZ  EQU   X'80'               DOWNLOAD INITIALIZED                         
DWNHDLN  EQU   X'40'               DOWNLOAD HEADLINES                           
*                                                                               
DWNMODE  DS    XL1                 DOWNLOAD MODE                                
DWNINIT  EQU   1                      DOWN-LOAD INITIALIZATION                  
DWNEOL   EQU   2                      MARK END OF LINE                          
DWNEOR   EQU   3                      MARK END OF REPORT                        
DWNTEXT  EQU   4                      DOWN-LOAD TEXT                            
DWNNUM   EQU   5                      DOWN-LOAD NUMBER                          
DWNPACK  EQU   6                      DOWN-LOAD NUMBER (PACKED)                 
*                                                                               
ELCODE   DS    XL1                 ELEMENT CODE FOR GETEL                       
*                                                                               
FLAG     DS    XL1                 SPECIFIC TO EACH REQUEST                     
*                                                                               
MSG      DS    CL10                                                             
*                                                                               
DKEY     DS    CL(L'ACCKEY)        DIRECTORY KEY                                
DA       DS    F                   DISK ADDRESS                                 
*                                                                               
EOF      EQU   X'FF'               END OF FILE MARKER                           
*                                                                               
UFDATA   DS    0C                  USER FIELD DATA FROM JOB REC                 
UFCI     DS    CL4                                                              
UFCE     DS    CL8                                                              
UFTY     DS    CL1                                                              
UFAC     DS    CL40                                                             
UFAN     DS    CL10                                                             
UFCC     DS    CL8                                                              
UFPA     DS    CL2                                                              
UFJI     DS    CL2                                                              
UFCN     DS    CL3                                                              
UFPI     DS    CL2                                                              
UFDATALN EQU   *-UFDATA                                                         
*                                                                               
ESTWRK   DS    CL(ESHLNQ)          LENGTH OF ESTIMATE WORK                      
TEXWRK   DS    CL1100                                                           
*                                                                               
IO1      DS    0CL2042                                                          
IO1KEY   DS    CL42                KEY                                          
IO1DATA  DS    CL2000              DATA                                         
IO1LNQ   EQU   *-IO1               LENGTH                                       
*                                                                               
IO2      DS    0CL2042                                                          
IO2KEY   DS    CL42                KEY                                          
IO2DATA  DS    CL2000              DATA                                         
IO2LNQ   EQU   *-IO2               LENGTH                                       
         EJECT                                                                  
**********************************************************************          
* DSECT TO COVER USER FIELD TABLE                                    *          
**********************************************************************          
         SPACE 1                                                                
LUD      DSECT                                                                  
LUCODE   DS    CL2                                                              
LUJBFLD  DS    AL2                                                              
LUJBLEN  DS    AL1                                                              
LUDLN    EQU   *-LUD                                                            
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
BINNUM   DS    CL1                 NUMBER OF BUCKETS                            
BINFST   DS    CL1                 DISPLACEMENT TO FIRST BUCKET                 
BINTAB   DS    0C                  THE TABLE                                    
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER DATA LINE                                            *         
***********************************************************************         
         SPACE 1                                                                
ESTRECD  DSECT                                                                  
ESHD     DS    0C                  HEADER RECORD                                
ESHNUM   DS    CL25                ESTIMATE NUMBER                              
ESHJBNM  DS    CL250               ESTIMATE JOB NAME                            
ESHCREDT DS    CL8                 CREATED IN SUPPLIERS SYS(PREPARERS)          
ESHOPDT  DS    CL8                 OPEN DATE                                    
ESHCLDT  DS    CL8                 CLOSED DATE                                  
ESHVENCN DS    CL60                VENDOR CONTACT                               
ESHVENNM DS    CL15                VENDOR NAME                                  
ESHPRON  DS    CL25                PROJECT NUMBER                               
ESHTASK  DS    CL3                 PROJECT TASK NUMBER                          
ESHPTYPE DS    CL20                TEXACO PRODUCT TYPE                          
ESHETYPE DS    CL20                TEXACO ESTIMATE TYPE                         
ESHCOMM  DS    CL500               COMMENTS                                     
ESHREV   DS    CL3                 REVISION NUMBER                              
ESHBUDYR DS    CL4                 BUDGET YEAR                                  
ESHTOT   DS    CL15                ESTIMATE TOTAL                               
ESHFEDID DS    CL10                FEDERAL ID                                   
ESHTEXCN DS    CL30                TEXACON CONTACT NAME                         
ESHCONT  DS    CL75                CONTACT NAME                                 
ESHLNQ   EQU   *-ESHD              LEGTH OF HEADER RECORD                       
*                                                                               
         ORG   ESHD                                                             
ESDET    DS    0C                  DETAIL INFO                                  
ESDNUM   DS    CL25                ESTIMATE NUMBER                              
ESDREV   DS    CL3                 REVISION NUMBER                              
ESDFEDID DS    CL10                FEDERAL ID                                   
ESDDESC  DS    CL500               DESCRIPTION                                  
ESDAMNT  DS    CL15                ITEM AMOUNT                                  
ESDID    DS    CL4                 ITEM IDENTIFIER                              
ESDLNQ   EQU   *-ESDET             LEGTH OF DETAIL RECORD                       
         EJECT                                                                  
**********************************************************************          
* OTHER DSECTS                                                       *          
**********************************************************************          
         SPACE 1                                                                
GOBLOCKD DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE ACGOBLOCK                                                      
         PRINT ON                                                               
         EJECT                                                                  
***********************************************************************         
*              ++INCLUDES                                             *         
***********************************************************************         
         SPACE 1                                                                
* ACGENMODES                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
*                                                                               
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
*                                                                               
* ACREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
*                                                                               
* ACMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
*                                                                               
* ACJOBBERD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACJOBBERD                                                      
         PRINT ON                                                               
*                                                                               
* ACBIGPRNTD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACBIGPRNTD                                                     
         PRINT ON                                                               
*                                                                               
JBLOCKD  DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE ACJOBBLOCK                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012ACREPIM02 08/17/00'                                      
         END                                                                    
