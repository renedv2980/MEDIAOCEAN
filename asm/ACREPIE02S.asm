*          DATA SET ACREPIE02S AT LEVEL 020 AS OF 08/17/00                      
*PHASE ACIE02A                                                                  
         TITLE 'ACIE - TEXACO ESTIMATE INTRERFACE TAPE'                         
**********************************************************************          
* HISTORY                                                            *          
* 01/11/94 - INCLUDE ESTIMATES BY PREPARER DATE (NOT APPROVED DATE)  *          
* 10/20/94 - ADDED OPTION 2 = C, INCLUDE CLOSED JOBS ONLY            *          
* 11/21/95 - IF CLOSED ONLY REQUEST - SKIP APPROVED CHECK AS PER LRES*          
**********************************************************************          
         EJECT                                                                  
ACIE02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**IE02**,R9                                                    
         USING ACWORKD,RA                                                       
         L     RA,0(R1)                                                         
         USING ACIE02D,RC                                                       
         LA    RC,SPACEND                                                       
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
**********************************************************************          
* RUN FIRST                                                          *          
**********************************************************************          
         SPACE 1                                                                
RUNF     DS    0H                                                               
         L     R2,=A(HOOKRC)                                                    
         ST    RC,0(R2)                                                         
         L     R2,=A(HOOK)                                                      
         ST    R2,HEADHOOK                                                      
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* REQUEST FIRST                                                      *          
**********************************************************************          
         SPACE 1                                                                
REQF     DS    0H                                                               
         MVI   RCSUBPRG,0                                                       
         CLI   QOPT1,C'Y'                                                       
         BNE   REQF10                                                           
*                                  ALWAYS OPEN INCASE WRITING TO DASD           
         CLC   OUTCNT,=H'0'        IS THIS THE FIRST REQUEST                    
         BNE   REQF10              NO                                           
         LH    R2,OUTCNT                                                        
         LA    R2,1(R2)                                                         
         STH   R2,OUTCNT                                                        
         MVC   DSPARM+13(2),ALPHAID FILL IN TAPE DATASET NAME                   
         GOTO1 DYNALLOC,DMCB,(0,DDPARM),((R2),DSPARM)                           
         OPEN  (OUTP,(OUTPUT))                                                  
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         ZAP   TAPECNT,=P'0'                                                    
*                                                                               
REQF10   XC    STRT3,STRT3                                                      
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
         BE    *+12                                                             
         CLI   SAVCUL,X'E3'        IS THIS JWT?                                 
         BNE   REQX                NO                                           
         MVC   USERTAB(2),=C'PB'   YES, USE PB INSTEAD                          
*                                                                               
REQX     B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* PROCESS ACCOUNT                                                    *          
**********************************************************************          
         SPACE 1                                                                
PACC     DS    0H                                                               
         XC    ESTSTAT,ESTSTAT                                                  
*                                                                               
         CLI   QOPT2,C'C'          CLOSED JOBS ONLY?                            
         BNE   *+12                NO                                           
         BAS   RE,CLSD             IS THIS JOB CLOSED                           
         BNE   EXIT                NO, REJECT                                   
*                                                                               
         ZAP   JOBNET,=P'0'                                                     
         ZAP   JOBCOM,=P'0'                                                     
*                                                                               
*        BAS   RE,SAVESEQ                                                       
         BAS   RE,GETACC                                                        
*                                                                               
         L     R4,ADACC                                                         
         USING EVERECD,R2                                                       
         LA    R2,MYKEY                                                         
         XC    MYKEY,MYKEY                                                      
         MVI   EVEKTYP,EVEKTYPQ                                                 
         MVI   EVEKSUB,EVEKSUBQ                                                 
         MVC   EVEKCPY(3),SAVCUL                                                
         MVC   EVEKCLI,SPACES                                                   
         MVC   EVEKCLI(3),3(R4)    CLIENT                                       
         MVC   EVEKPRO,SPACES                                                   
         MVC   EVEKPRO(3),6(R4)   PRODUCT                                       
         MVC   EVEKJOB(6),9(R4)    JOB                                          
         MVI   EVEKTYPE,EVEKTREV   LOOKING FOR REVISIONS ONLY                   
         MVI   EVEKVER,1           AND THE FIRST ONE AT THAT                    
*                                                                               
         LA    R2,IOAREA1                                                       
         LA    R3,IOAREA2                                                       
         BAS   RE,READDM                                                        
         B     *+8                                                              
*                                                                               
PACC10   BAS   RE,SEQDM                                                         
         CLC   0(EVEKVER-EVEKEY,R2),MYKEY STILL READING SAME JOB                
         BNE   PACC30                                                           
*                                                                               
* AS PER SPEEDDNY 6/30/93, FILTER ESTS ON DATE APPROVED                         
*                                                                               
         MVI   ELCODE,EPRELQ       GET DATE ESTIMATE PREPARED                   
         LR    R4,R2               PREP FOR GETEL                               
         BAS   RE,GETEL                                                         
         BNE   PACC10                                                           
         USING EPRELD,R4                                                        
         CLC   EPRDATE,STRT3      PREPARED WITHIN DATE RANGE                    
         BL    PACC10                                                           
         CLC   EPRDATE,END3                                                     
         BH    PACC10              NO                                           
*                                                                               
* AS PER LRESDDNY 11/95, IF CLOSED ONLY REQUEST - DON'T CHECK APPROVED          
*                                                                               
         CLI   QOPT2,C'C'          ONLY CLOSED JOBS REQUESTED                   
         BE    PACC20              YES - DON'T WORRY ABOUT APPROVED EST         
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
         BAS   RE,LDUSR            LOAD USER ROUTINE                            
         BAS   RE,BLD10            BUILD 10 RECORD                              
         BAS   RE,GTWC             GET WORKCODE DATA                            
         BAS   RE,BUILD80                                                       
*                                                                               
*ACCX    BAS   RE,RESTSEQ          RESTORE KEY FOR MONACC                       
PACCX    B     EXIT                                                             
         DROP  R2,R4                                                            
         EJECT                                                                  
**********************************************************************          
* REQUEST LAST - GET A RECORD FROM SORT, PUT TO TAPE AND REPORT      *          
**********************************************************************          
         SPACE 1                                                                
         USING PRTD,R6                                                          
REQL     DS    0H                                                               
         LA    R6,P                                                             
         MVI   FORCEHED,C'Y'                                                    
         MVC   PNET,=CL13'    NET'                                              
         MVC   PCOM,=CL13' COMMISSION'                                          
         MVC   PSECOND+1(14),=C'REQUEST TOTALS'                                 
         LA    R6,PSECOND                                                       
         MVC   PNET,=CL13'-------------'                                        
         MVC   PCOM,=CL13'-------------'                                        
*                                                                               
         LA    R6,PTHIRD                                                        
         LA    R5,PNET                                                          
         ZAP   DOUBLE,REQNET                                                    
         BAS   RE,EDITIT                                                        
*                                                                               
         LA    R5,PCOM                                                          
         ZAP   DOUBLE,REQCOM                                                    
         BAS   RE,EDITIT                                                        
*                                                                               
         GOTO1 ACREPORT                                                         
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* RUN LAST                                                           *          
**********************************************************************          
         SPACE 1                                                                
RUNL     DS    0H                                                               
         CLC   OUTCNT,=H'0'        WAS A TAPE OPENED                            
         BE    RUNLX               NO                                           
         MVC   P+1(23),=CL23'NUMBER OF TAPE RECORDS:'                           
         EDIT  (P4,TAPECNT),(13,P+25),COMMAS=YES,ZERO=NOBLANK,ALIGN=LEFX        
               T                                                                
         MVI   SPACING,2                                                        
         GOTO1 ACREPORT                                                         
         ZAP   TAPECNT,=P'0'                                                    
         CLOSE (OUTP)                                                           
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
RUNLX    B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
**********************************************************************          
* PROCESS AN ACCOUNT RECORD                                          *          
* GET USER FIELD INFO                                                *          
**********************************************************************          
         SPACE 1                                                                
GETACC   NTR1                                                                   
         L     R4,ADLVANAM                                                      
         LA    R5,CLINAME                                                       
         BAS   RE,GETNAME                                                       
         L     R4,ADLVBNAM                                                      
         LA    R5,PRONAME                                                       
         BAS   RE,GETNAME                                                       
         L     R4,ADACCNAM                                                      
         LA    R5,JOBNAME                                                       
         BAS   RE,GETNAME                                                       
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* BUILD A 10 RECORD FRON THE ESTIMATE AT 0(R2)                       *          
**********************************************************************          
         SPACE 1                                                                
BLD10    NTR1                                                                   
         BAS   RE,CLRTPREC                                                      
         USING IED,R5                                                           
         LA    R5,TPREC                                                         
         MVC   IETEX01,=C'*****TEX01843'                                        
         BAS   RE,PUTTAPE                                                       
*                                                                               
         BAS   RE,CLRTPREC                                                      
*                                                                               
         LA    R1,VENTAB           VENDOR NUMBER TABLE                          
         LA    R0,VENTBLNQ         NUMBER OF ENTRIES IN TABLE                   
BLD1010  CLC   ORIGINUM,0(R1)      MATCH ON ORIGIN NUMBER                       
         BNE   BLD1020                                                          
         MVC   IE10VEN,2(R1)       MOVE IN VENDOR NUMBER                        
         B     BLD1030                                                          
*                                                                               
BLD1020  LA    R1,L'VENTAB(R1)                                                  
         BCT   R0,BLD1010                                                       
         MVC   IE10VEN,=CL13'4109856650002'   NO MATCH - USE DEFAULT            
*                                                                               
         USING EVERECD,R4                                                       
BLD1030  LR    R4,R2                                                            
         MVC   IE10,=C'10'                                                      
*                                                                               
         SR    R1,R1                                                            
         IC    R1,EVEKVER                                                       
         EDIT  (R1),(2,IE10REV),FILL=0                                          
*                                                                               
         MVC   IE10CLI,EVEKCLI                                                  
         LA    R1,IE10CLI                                                       
         BAS   RE,ZEROFILL                                                      
         MVC   IE10PRO,EVEKPRO                                                  
         LA    R1,IE10PRO                                                       
         BAS   RE,ZEROFILL                                                      
         MVC   IE10PRD,EVEKPRO+1   2ND AND THIRD  POS THINK                     
         CLI   IE10PRD+1,C' '                                                   
         BNE   *+8                                                              
         MVI   IE10PRD+1,C'0'                                                   
         MVC   IE10JOB,EVEKJOB                                                  
*                                                                               
         MVC   IE10DESC(L'JOBNAME),JOBNAME                                      
*                                                                               
         BAS   RE,CLSD                                                          
         BNE   *+8                                                              
         MVI   IE10CLFL,C'C'                                                    
*                                                                               
         USING EPRELD,R4                                                        
         MVI   ELCODE,EPRELQ       GET DATE ESTIMATE PREPARED                   
         LR    R4,R2               PREP FOR GETEL                               
         BAS   RE,GETEL                                                         
         BNE   BLD1040                                                          
         GOTO1 DATCON,DMCB,(1,EPRDATE),(20,WORK)                                
         MVC   IE10PRDA(4),WORK+4         SET MMDD                              
         MVC   IE10PRDA+4(4),WORK         CENTURY AND YEAR                      
         DROP  R4                                                               
*                                                                               
BLD1040  MVC   IE10CI,UFCI         FILL IN USER FIELD DATA                      
         MVC   IE10PI,UFPI                                                      
         MVC   IE10JI,UFJI                                                      
         MVC   IE10CN,UFCN                                                      
         MVC   IE10TY,UFTY                                                      
         MVC   IE10PA,UFPA                                                      
         MVC   IE10AC,UFAC                                                      
         MVC   IE10AN,UFAN                                                      
         MVC   IE10CC,UFCC                                                      
         MVC   IE10CE,UFCE                                                      
*                                                                               
         BAS   RE,REPT10           REPORT 10 RECORD DATA                        
*                                                                               
         BAS   RE,TXKEY            BUILD KEY TO READ TEXT                       
         BAS   RE,JOBTXT           WRITE JOB TEXT TO IE10TEXT                   
*                                                                               
         BAS   RE,PUTTAPE                                                       
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* REPORT ON 10 RECORD DATA                                           *          
**********************************************************************          
         SPACE 1                                                                
REPT10   NTR1                                                                   
         MVI   FORCEHED,C'Y'                                                    
         MVC   P,ACHEAD1                                                        
         MVC   PSECOND,ACHEAD2                                                  
         GOTO1 ACREPORT                                                         
*                                                                               
         USING PRTD,R4                                                          
         LA    R4,P                                                             
         USING IED,R5                                                           
         LA    R5,TPREC                                                         
         MVC   PACCT,IE10ACCT                                                   
         MVC   PREV,IE10REV                                                     
         MVC   WORK+2(4),IE10PRDA  EXTRACT MMDD                                 
         MVC   WORK(2),IE10PRDA+6  AND YY                                       
         GOTO1 DATCON,DMCB,(0,WORK),(5,PDATE)                                   
         BAS   RE,PRTUFS                                                        
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
**********************************************************************          
* WRITE/REPORT JOB TEXT RECORDS                                      *          
**********************************************************************          
         SPACE 1                                                                
JOBTXT   NTR1                                                                   
         MVC   P+1(8),=C'JOB TEXT'                                              
         MVC   PSECOND+1(8),=C'--------'                                        
         GOTO1 ACREPORT                                                         
*                                                                               
         USING TXTRECD,R2                                                       
         MVC   MYKEY,MYTXTKEY                                                   
         LA    R2,MYKEY                                                         
         XC    TXTKWRK,TXTKWRK                                                  
         L     R2,TEXTBUFF                                                      
         BAS   RE,READDM                                                        
*                                                                               
         CLC   MYKEY,0(R2)         DID I GET ANYTHING                           
         BNE   JBTXTX                                                           
*                                                                               
         LR    R4,R2                                                            
         MVI   ELCODE,TFDELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   JBTXTX                                                           
*                                                                               
         USING IED,R5                                                           
         LA    R5,TPREC                                                         
         LA    R6,IE10TEXT+L'IE10TEXT CREATE POINTER TO END OF FIELD            
         LA    R5,IE10TEXT            POINTER TO CURRENT TEXT POSITION          
         DROP  R5                                                               
*                                                                               
         USING TFDELD,R4                                                        
JBTXT10  CLI   TFDTYPE,TFDTREG REGULAR TEXT                                     
         BNE   JBTXT30                                                          
*                                                                               
         ZIC   R1,TFDLN                                                         
         SH    R1,=Y(TFDTEXT-TFDEL)                                             
         BNP   JBTXT30             NOTHING IN THIS ELEMENT                      
*                                                                               
         LA    R7,0(R1,R5)         SEE IF THIS WILL FIT                         
         CR    R7,R6               WILL NEW END BE PAST MAX                     
         BL    JBTXT20             NO                                           
         SR    R7,R6               CALCULATE OVERFLOW AMOUNT                    
         SR    R1,R7               SUBTRACT FROM LENGTH TO MOVE                 
         BZ    JBTXT40             THATS ALL - END                              
         SR    R7,R7               FLAG NO MORE ROOM                            
*                                                                               
JBTXT20  BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),TFDTEXT                                                  
*                                                                               
         LA    R5,2(R1,R5)         BUMP TO NEXT OUTPUT POSITION                 
*                                                                               
         LTR   R7,R7               ANY MORE ROOM LEFT IN IE10TEXT               
         BZ    *+12                NO - END                                     
JBTXT30  BAS   RE,NEXTEL                                                        
         BE    JBTXT10                                                          
JBTXT40  BAS   RE,PRJTXT           PRINT IE10TEXT                               
*                                                                               
JBTXTX   BAS   RE,PRWCTXT                                                       
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* PRINT JOB TEXT ON REPORT                                           *          
**********************************************************************          
         SPACE 1                                                                
PRJTXT   NTR1                                                                   
         USING IED,R5                                                           
         USING PRTD,R4                                                          
         LA    R5,TPREC                                                         
         LA    R5,IE10TEXT                                                      
         LA    R4,P                PRINT JOB TEXT IN 3 BLOCKS OF 100            
         MVC   PTEXT,0(R5)                                                      
         LA    R4,PSECOND                                                       
         MVC   PTEXT,L'PTEXT(R5)                                                
         LA    R4,PTHIRD                                                        
         MVC   PTEXT,2*L'PTEXT(R5)                                              
         GOTO1 ACREPORT                                                         
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
**********************************************************************          
* GET WORK CODE DATA, CREATE/REPORT 30 RECORDS                       *          
* R2 IS A(ESTIMATE RECORD)                                           *          
**********************************************************************          
         SPACE 1                                                                
GTWC     NTR1                                                                   
         LA    R5,TPREC                                                         
         LR    R4,R2                                                            
         USING EDAELD,R4                                                        
         MVI   ELCODE,EDAELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   GTWC60                                                           
*                                                                               
GTWC10   CP    EDACOMM,=P'0'       ASSURE NON ZERO ESTIMATE                     
         BNE   GTWC30                                                           
*                                                                               
         CLI   EDALN,EDALNQ1       IS THERE A NON-COMMISSIONABLE AMOUNT         
         BE    GTWC20              NO                                           
         CP    EDANCOM,=P'0'                                                    
         BNE   GTWC30                                                           
*                                                                               
GTWC20   BAS   RE,NEXTEL                                                        
         BE    GTWC10                                                           
         B     GTWC60              NO NON ZERO WORKCODES                        
*                                                                               
GTWC30   GOTO1 ACREPORT            SKIP A LINE                                  
         MVC   P+1(13),=C'ESTIMATE DATA'                                        
         USING PRTD,R6                                                          
         LA    R6,P                                                             
         MVC   PWC(4),=C'CODE'                                                  
         MVC   PNET,=CL13'    NET'                                              
         MVC   PCOM,=CL13' COMMISSION'                                          
         MVC   PPCT,=CL13'    RATE'                                             
         LA    R6,PSECOND                                                       
         MVC   PWC(4),=C'----'                                                  
         MVC   PNET,=CL13'-------------'                                        
         MVC   PCOM,=CL13'-------------'                                        
         MVC   PPCT,=CL13'-------------'                                        
         MVC   PSECOND+1(13),=C'-------------'                                  
         GOTO1 ACREPORT                                                         
         DROP  R6                                                               
*                                                                               
GTWC40   BAS   RE,CLRTPREC                                                      
*                                                                               
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
         BAS   RE,PRWC                                                          
*                                                                               
         AP    JOBNET,WCNET                                                     
         AP    REQNET,WCNET                                                     
         AP    JOBCOM,WCCOMM                                                    
         AP    REQCOM,WCCOMM                                                    
*                                                                               
         MVC   IE30,=C'30'                                                      
         MVC   IE30WC,EDAWORK                                                   
*                                                                               
         UNPK  IE30NET,WCNET                                                    
         OI    IE30NET+L'IE30NET-1,X'F0'                                        
*                                                                               
         UNPK  IE30COM,WCCOMM                                                   
         OI    IE30COM+L'IE30COM-1,X'F0'                                        
*                                                                               
         BAS   RE,WCTXT            GET WORKCODE TEXT                            
         BAS   RE,PUTTAPE                                                       
*                                                                               
GTWC50   MVI   ELCODE,EDAELQ                                                    
         BAS   RE,NEXTEL                                                        
         BNE   GTWC60              DONE W/ ESTIMATE                             
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
GTWC60   CP    JOBNET,=P'0'        ANYTHING ON JOB?                             
         BNE   GTWC70                                                           
         CP    JOBCOM,=P'0'                                                     
         BE    GTWCX                                                            
*                                                                               
         USING PRTD,R6                                                          
GTWC70   LA    R6,P                                                             
         MVC   PNET,=CL13'-------------'                                        
         MVC   PCOM,=CL13'-------------'                                        
         MVC   PPCT,=CL13'-------------'                                        
         MVC   PSECOND+1(10),=C'JOB TOTALS'                                     
         LA    R6,PSECOND                                                       
         LA    R5,PNET                                                          
         ZAP   DOUBLE,JOBNET                                                    
         BAS   RE,EDITIT                                                        
*                                                                               
         LA    R5,PCOM                                                          
         ZAP   DOUBLE,JOBCOM                                                    
         BAS   RE,EDITIT                                                        
         GOTO1 ACREPORT                                                         
*                                                                               
GTWCX    B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
**********************************************************************          
* BUILD 80                                                           *          
**********************************************************************          
         SPACE 1                                                                
         USING IED,R5                                                           
BUILD80  NTR1                                                                   
         LA    R5,TPREC                                                         
         BAS   RE,CLRTPREC                                                      
         MVC   IE80,=C'80'                                                      
         UNPK  IE80NET,JOBNET                                                   
         OI    IE30NET+L'IE30NET-1,X'F0'                                        
*                                                                               
         UNPK  IE80COM,JOBCOM                                                   
         OI    IE80COM+L'IE80COM-1,X'F0'                                        
*                                                                               
         ZAP   DUB,JOBNET                                                       
         AP    DUB,JOBCOM                                                       
         UNPK  IE80GRS,DUB                                                      
         OI    IE80GRS+L'IE80GRS-1,X'F0'                                        
         BAS   RE,PUTTAPE                                                       
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* MAKE SURE THERE ARE NON ZERO AMOUNTS ON THE ESTIMATE               *          
* R2 IS A(ESTIMATE RECORD)                                           *          
* RETURNS NEQ IF NON-ZERO                                            *          
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
* PRINT WORKCODE DATA ON REPORT                                      *          
**********************************************************************          
         SPACE 1                                                                
PRWC     NTR1                                                                   
         USING PRTD,R4                                                          
         LA    R4,P                                                             
         MVC   PWC,WC                                                           
         LA    R5,PNET                                                          
         ZAP   DOUBLE,WCNET                                                     
         BAS   RE,EDITIT                                                        
*                                                                               
         LA    R5,PCOM                                                          
         ZAP   DOUBLE,WCCOMM                                                    
         BAS   RE,EDITIT                                                        
*                                                                               
         LA    R5,PPCT                                                          
         ZAP   DOUBLE,WCRATE                                                    
         BAS   RE,EDITRATE                                                      
*                                                                               
         GOTO1 ACREPORT                                                         
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
**********************************************************************          
* PRINT WORKCODE TEXT ON REPORT                                      *          
**********************************************************************          
         SPACE 1                                                                
PRWCTXT  NTR1                                                                   
         USING IED,R5                                                           
         USING PRTD,R4                                                          
         LA    R5,TPREC                                                         
         LA    R4,P                                                             
         MVC   PWCTEXT,IE30TEXT                                                 
         GOTO1 ACREPORT                                                         
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
**********************************************************************          
* CALL GETOPT, CALCULATE GROSS FIGURES FOR THIS ESTIMATE             *          
* RETURNS COMMISSION AMOUNT IN WCCOMM                                *          
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
* BUILD A TEXT KEY                                                   *          
**********************************************************************          
         SPACE 1                                                                
TXKEY    NTR1                                                                   
         USING TXTRECD,R2                                                       
         LA    R2,MYTXTKEY                                                      
         XC    TXTKEY,TXTKEY                                                    
         MVI   TXTKTYP,TXTKTYPQ                                                 
         MVI   TXTKSUB,TXTKSUBQ                                                 
         MVI   TXTKSUB,TXTKSUBQ                                                 
         USING ACTRECD,R4                                                       
         L     R4,ADACC                                                         
         MVC   TXTKCPY,ACTKCPY                                                  
         MVC   TXTKUNT,ACTKUNT                                                  
         MVC   TXTKLDG,ACTKLDG                                                  
         MVC   TXTKCLI,SPACES                                                   
         MVC   TXTKCLI(3),ACTKACT                                               
         MVC   TXTKPRO,SPACES                                                   
         MVC   TXTKPRO(3),ACTKACT+3                                             
         MVC   TXTKJOB,ACTKACT+6                                                
         MVC   TXTKJOB,ACTKACT+6                                                
         MVI   TXTKFORM,TXTKFEST   GET ESTIMATE FOOTER RECORDS                  
         MVC   TXTKWHER,=C'F '                                                  
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* BUILD WORKCODE TEXT RECORDS                                        *          
**********************************************************************          
         SPACE 1                                                                
         USING TXTRECD,R2                                                       
WCTXT    NTR1                                                                   
         LA    R2,MYKEY                                                         
         MVC   MYKEY,MYTXTKEY                                                   
         MVC   TXTKWRK,WC                                                       
         L     R2,TEXTBUFF                                                      
         BAS   RE,READDM                                                        
*                                                                               
         CLC   MYKEY,0(R2)         DID I GET ANYTHING                           
         BNE   WCTXX                                                            
*                                                                               
         LR    R4,R2                                                            
         USING TFDELD,R4                                                        
         MVI   ELCODE,TFDELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   WCTXX                                                            
*                                                                               
         USING IED,R5                                                           
         LA    R5,TPREC                                                         
         LA    R6,IE30TEXT+L'IE30TEXT CREATE POINTER TO END OF FIELD            
         LA    R5,IE30TEXT                                                      
         DROP  R5                                                               
*                                                                               
WCTXT10  CLI   TFDTYPE,TFDTREG     REGULAR TEXT?                                
         BNE   WCTXT20             NO, TRY NEXT                                 
*                                                                               
         ZIC   R1,TFDLN                                                         
         SH    R1,=Y(TFDTEXT-TFDEL)                                             
         BNP   WCTXT20             NOTHING IN THIS ELEMENT                      
*                                                                               
         LA    R7,0(R1,R5)         SEE IF THIS WILL FIT                         
         CR    R7,R6               WILL NEW END BE PAST MAX                     
         BH    WCTXX               YES                                          
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),TFDTEXT WRITE  AS MUCH TEXT AS YOU CAN                   
         LA    R5,1(R1,R5)                                                      
*                                                                               
WCTXT20  MVI   ELCODE,TFDELQ                                                    
         BAS   RE,NEXTEL                                                        
         BE    WCTXT10                                                          
*                                                                               
WCTXX    BAS   RE,PRWCTXT                                                       
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* SAVESEQ - SAVE THE READ SEQUENCE                                   *          
*                                                                    *          
* RESTSEQ - RESTORE THE READ SEQUENCE                                *          
*           NOTE R2 IS ASSUMED TO BE POINTING TO AN UNUSED BUFFER    *          
**********************************************************************          
         SPACE 1                                                                
**********************************************************************          
*AVESEQ  NTR1                                                        *          
*        L     RE,ADACCFIL         A(ACC DIR)                        *          
*        L     RE,ISPDKEY-ISDTF(RE)                                  *          
*        MVC   DCBKEY,0(RE)                                          *          
*        B     EXIT                                                  *          
*                                                                    *          
*ESTSEQ  NTR1                                                        *          
*        MVC   MYKEY,DCBKEY                                          *          
*        BAS   RE,READDM                                             *          
*        B     EXIT                                                  *          
**********************************************************************          
         EJECT                                                                  
**********************************************************************          
* PUT RECORDS TO TAPE                                                *          
**********************************************************************          
         SPACE 1                                                                
PUTTAPE  NTR1                                                                   
         CLI   QOPT1,C'Y'                                                       
         BNE   EXIT                                                             
         PUT   OUTP,TPREC                                                       
         AP    TAPECNT,=P'1'                                                    
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* DATA MANAGER INTERFACE                                             *          
**********************************************************************          
         SPACE 1                                                                
READDM   MVC   COMMAND,=CL8'DMREAD'                                             
         B     CALLDM                                                           
SEQDM    MVC   COMMAND,=CL8'DMRSEQ'                                             
         B     CALLDM                                                           
CALLDM   NTR1                                                                   
         GOTO1 DATAMGR,DMCB,COMMAND,=C'ACCOUNT',MYKEY,(R2)                      
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* MOVE SPACES TO TPREC                                               *          
**********************************************************************          
         SPACE 1                                                                
CLRTPREC NTR1                                                                   
         LA    R2,TPREC                                                         
         LM    R3,R5,=A(L'TPREC,0,C' ')                                         
         SLL   R5,24               SHIFT THE C' ' TO TOP BYTE                   
         MVCL  R2,R4                                                            
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* MOVE SPACES TO UFDATA                                              *          
**********************************************************************          
         SPACE 1                                                                
RESUFDAT NTR1                                                                   
         LA    R2,UFDATA                                                        
         LM    R3,R5,=A(UFDATALN,0,C' ')                                        
         SLL   R5,24               SHIFT THE C' ' TO TOP BYTE                   
         MVCL  R2,R4                                                            
*                                                                               
         USING LUD,R1                                                           
         LA    R1,USERTAB          RESET USER FIELD MAX LENS                    
         LA    R0,LUNUM                                                         
         MVI   LUJBLEN,L'UFDATA                                                 
         LA    R1,LUDLN(R1)                                                     
         BCT   R0,*-8                                                           
         B     EXIT                                                             
         DROP  R1                                                               
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
*                                                                               
EDITIT   NTR1                                                                   
         EDIT  (P8,DOUBLE),(13,(R5)),2,MINUS=YES                                
         B     EXIT                                                             
*                                                                               
EDITRATE NTR1                                                                   
         EDIT  (P8,DOUBLE),(13,(R5)),4                                          
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* LOOP THRU USERTAB, PRINTING USER FIELDS                            *          
**********************************************************************          
         SPACE 1                                                                
PRTUFS   NTR1                                                                   
         USING PRTD,R4                                                          
         LA    R4,P                                                             
         LA    R4,PUFS             OFFSET INTO P TO PRINT USER FIELDS           
*                                                                               
         USING LUD,R3                                                           
         LA    R3,USERTAB                                                       
         LA    R0,LUNUM                                                         
         XR    R1,R1                                                            
*                                                                               
PRUF10   LA    R5,UFDATA                                                        
         MVC   0(2,R4),LUCODE                                                   
         MVI   2(R4),C'='                                                       
         SR    R1,R1                                                            
         ICM   R1,3,LUJBFLD        GET DISPLACMENT TO FIELD                     
         AR    R5,R1               POINT R5 TO CORRECT USERFIELD                
         SR    R1,R1                                                            
         IC    R1,LUJBLEN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R5),SPACES      IS THIS USER FIELD DEFINED                   
         BH    PRUF20              YES, PRINTEM                                 
*                                                                               
         MVC   3(11,R4),=C'*UNDEFINED*'                                         
         B     PRUF30                                                           
*                                                                               
PRUF20   EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   3(0,R4),0(R5)                                                    
*                                                                               
PRUF30   GOTO1 ACREPORT                                                         
         LA    R1,L'UFDATA(R1)                                                  
         LA    R3,LUDLN(R3)        AND NEXT TABLE ENTRY                         
         BCT   R0,PRUF10                                                        
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
**********************************************************************          
* LOAD UFDATA WITH DATA FROM THE USER FIELDS ON THIS JOB             *          
**********************************************************************          
         SPACE 1                                                                
         USING UFSELD,R4                                                        
LDUSR    NTR1                                                                   
         BAS   RE,RESUFDAT         CLEAR AND RESET USER TABLE                   
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
LU50     SR    R2,R2                                                            
         IC    R2,UFSLN                                                         
         SH    R2,=Y(UFSLN1Q+1)    SUBTRACT OVERHEAD+1 FOR EX                   
         BM    LU60                NO DATA ON THIS USER FIELD                   
*                                                                               
         LA    R1,UFDATA           POINT R1 TO FIELD FOR THIS UF                
         SR    R0,R0                                                            
         ICM   R0,B'0011',LUJBFLD  OFFSET INTO UFDATA OF THIS FIELD             
         AR    R1,R0                                                            
*                                                                               
         SR    R6,R6                                                            
         IC    R6,LUJBLEN          LENGTH OF THIS TAPE FIELD                    
         BCTR  R6,0                                                             
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),SPACES                                                   
*                                                                               
         CR    R2,R6               Q, ACTUAL LENGTH GREATER THAN MAX.           
         BNH   *+6                                                              
         LR    R2,R6               Y, ALLOW ONLY THE MAX. LENGTH                
         LR    R6,R2                                                            
         AHI   R6,1                ADD BACK THE 1 BYTE FOR THE EX               
         STC   R6,LUJBLEN          RESET ACTUAL LENGTH INTO FIELD               
         EX    R2,*+4                                                           
         MVC   0(0,R1),UFSDATA                                                  
LU60     BAS   RE,NEXTEL                                                        
         B     LU10                                                             
         DROP  R3,R4                                                            
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
**********************************************************************          
* ZERO FILL THE SECOND AND THIRD CHARACTERS OF THE FIELD             *          
* AT 0(R1)                                                           *          
**********************************************************************          
         SPACE 1                                                                
ZEROFILL NTR1                                                                   
         CLI   1(R1),C' '                                                       
         BNE   *+8                                                              
         MVI   1(R1),C'0'                                                       
         CLI   2(R1),C' '                                                       
         BNE   *+8                                                              
         MVI   2(R1),C'0'                                                       
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* TABLES                                                             *          
**********************************************************************          
         SPACE 1                                                                
* USER FIELD TABLE - TABLE DEFINES WHERE IN TPREC TO PUT A USER FIELD           
*                    COVERED BY LUD                                             
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
* VENDOR NUMBER TABLE - VENDOR NUMBER MATCHED BY ORIGIN NUMBER                  
VENTAB   DS    0CL15                                                            
         DC    HL2'0034',C'1304731200001'  BDNY  - BBDO                         
         DC    HL2'0294',C'1329938710001'  BSNY  - BATES USA                    
         DC    HL2'0460',C'0000500087994'  JWNYA - J. WALTER THOMPSON           
         DC    HL2'1565',C'1329938710001'  BSSP  - BATES USA                    
         DC    HL2'2532',C'1331980750001'  DFCN  - SAATCHI AND SAATCHI          
         DC    HL2'2533',C'1331980750001'  DWCN  - SAATCHI AND SAATCHI          
         DC    HL2'3742',C'1329938710001'  BSHO  - BATES USA                    
         DC    HL2'4175',C'1329938710001'  BSZNY - ZENITH MEDIA SERVICE         
         DC    HL2'6703',C'0000500000460'  BDHO  - BBDO WORLDWIDE               
         DC    HL2'8488',C'0000500087994'  JWHO  - J. WALTER THOMPSON           
VENTBLNQ EQU   (*-VENTAB)/L'VENTAB                                              
         EJECT                                                                  
**********************************************************************          
* CONSTANTS                                                          *          
**********************************************************************          
         SPACE 1                                                                
DDPARM   DC    CL8'OUTP'                                                        
DSPARM   DC    CL20'ACCTAPE.AC0IEXX1' THE XX WILL BECOME THE ALPHAID            
OUTCNT   DC    H'0'                   RELATIVE GENERATION FOR DYNALLOC          
OUTP     DCB   DDNAME=OUTP,DSORG=PS,RECFM=FB,LRECL=IERECLN,MACRF=PM,   X        
               BLKSIZE=IERECLN*10                                               
*                                                                               
ACHEAD1  DC    CL132' ACCOUNT           REV   DATE   USER FIELDS       X        
                          '                                                     
ACHEAD2  DC    CL132' ----------------  --- -------- ------------------X        
               -----------'                                                     
         EJECT                                                                  
**********************************************************************          
* GETEL                                                              *          
**********************************************************************          
         SPACE 1                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* HEAD HOOK                                                          *          
**********************************************************************          
         SPACE 1                                                                
HOOK     NMOD1 0,**HDHK**                                                       
         L     RC,HOOKRC                                                        
         CLI   QOPT2,C'C'                                                       
         BNE   HOOKX                                                            
         MVC   HEAD3+100(16),=C'CLOSED JOBS ONLY'                               
HOOKX    XIT1                                                                   
HOOKRC   DS    A                                                                
         EJECT                                                                  
**********************************************************************          
* STORAGE DSECT                                                      *          
**********************************************************************          
         SPACE 1                                                                
ACIE02D  DSECT                                                                  
TPVOLS   DS    D                                                                
TEXTBUFF DS    A                   A(WHERE TO READ TEXT RECORDS)                
SAVERE   DS    A                                                                
TAPECNT  DS    PL4                                                              
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
TPDSN    DS    CL20                                                             
CLINAME  DS    CL36                                                             
PRONAME  DS    CL36                                                             
JOBNAME  DS    CL36                                                             
ELCODE   DS    CL1                                                              
ESTSTAT  DS    CL1                                                              
GOTANEST EQU   1                                                                
COMMAND  DS    CL8                                                              
STRT3    DS    CL3                 PACKED REQEST START DATE                     
END3     DS    CL3                 & END DATE                                   
SAVCUL   DS    CL3                 COMP, UNIT, LEDG I'M READING                 
MYKEY    DS    CL42                BUILD ESTIMATE KEY                           
MYTXTKEY DS    CL(L'TXTKEY)        JOB LEVEL TEXT KEY                           
*CBKEY   DS    CL42                SAVE KEY                                     
TPREC    DS    CL(IERECLN)                                                      
*                                                                               
UFDATA   DS    0CL40               USER FIELD DATA FROM JOB REC                 
UFCI     DS    CL40                                                             
UFCE     DS    CL40                                                             
UFTY     DS    CL40                                                             
UFAC     DS    CL40                                                             
UFAN     DS    CL40                                                             
UFCC     DS    CL40                                                             
UFPA     DS    CL40                                                             
UFJI     DS    CL40                                                             
UFCN     DS    CL40                                                             
UFPI     DS    CL40                                                             
UFDATALN EQU   *-UFDATA                                                         
*                                                                               
COMRATE  DS    PL16                                                             
IOAREA1  DS    CL2000                                                           
IOAREA2  DS    CL2000              SAVE A RECORD AREA                           
ACIEDLN  EQU   *-ACIE02D           MUST BE LAST                                 
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
**********************************************************************          
* TAPE RECORD DSECT                                                  *          
**********************************************************************          
         SPACE 1                                                                
IED      DSECT                                                                  
IEREC    DS    0CL(IERECLN)                                                     
IETEX01  DS    CL13                *****843TEX01 ALWAYS PRECEEDS REC 10         
         ORG   IEREC                                                            
IE10     DS    CL2                                                              
IE10VEN  DS    CL13                CODE FOR CENY OR CEHO                        
IE10ACCT DS    0CL12               JOB NUMBER                                   
IE10CLI  DS    CL3                                                              
IE10PRO  DS    CL3                                                              
IE10JOB  DS    CL6                                                              
IE10REV  DS    CL2                 REVISION NUMBER 1-N                          
IE10CI   DS    CL4                 BUD YEAR                                     
IE10DESC DS    CL79                JOB NAME??                                   
IE10CE   DS    CL8                 INITIATOR LOGON ID                           
IE10TY   DS    CL1                 MEDIA BUY FLAG                               
IE10PRDA DS    CL8                 APPROVAL DATE MMDDYYYY                       
IE10AC   DS    CL40                AGENCY CONTACT NAME                          
IE10AN   DS    CL10                AGENCY CONTACT PHONE #                       
         DS    CL4                 PHONE NUMBER EXTENSION                       
IE10CC   DS    CL8                 FIELD CONTACT LOGIN ID                       
IE10PRD  DS    CL2                 PROD CODE+1(2)                               
IE10PA   DS    CL2                 ADV TYPE                                     
IE10JI   DS    CL2                 SVC CHARGE CDE                               
IE10CN   DS    CL3                 VEHICLE NUMBER                               
IE10PI   DS    CL2                 PAYOR CODE                                   
IE10TEXT DS    CL300               JOB LEVEL FOOTER COMMENT                     
IE10CLFL DS    CL1                 C IF JOB CLOSED                              
IERECLN  EQU   *-IE10                                                           
*                                                                               
* 20 RECORD TYPE RELOACED BY CL300                                              
*                                                                               
         ORG   IEREC                                                            
IE30     DS    CL2                                                              
IE30NET  DS    CL11                                                             
IE30COM  DS    CL11                                                             
IE30WC   DS    CL2                                                              
IE30TEXT DS    CL300                                                            
         ORG   IEREC                                                            
IE80     DS    CL2                                                              
IE80NET  DS    CL11                                                             
IE80GRS  DS    CL11                                                             
IE80COM  DS    CL11                                                             
         EJECT                                                                  
**********************************************************************          
* PRINT LINE DSECT                                                   *          
**********************************************************************          
         SPACE 1                                                                
PRTD     DSECT                                                                  
PREC     DS    0CL132                                                           
         DS    CL1                                                              
PACCT    DS    CL12                                                             
         DS    CL1                                                              
PTYPE    DS    CL2                 O OR C                                       
         DS    CL3                                                              
PREV     DS    CL2                 REVISION NUMBER                              
         DS    CL2                                                              
PDATE    DS    CL8                                                              
         DS    CL1                                                              
PUFS     DS    CL40                                                             
         DS    CL1                                                              
         ORG   PREC                                                             
         DS    CL1                                                              
PTEXT    DS    CL100                                                            
*                                                                               
         ORG   PREC                                                             
         DS    CL15                                                             
PWC      DS    CL2                                                              
         DS    CL3                                                              
PNET     DS    CL13                                                             
         DS    CL1                                                              
PCOM     DS    CL13                                                             
         DS    CL1                                                              
PPCT     DS    CL13                                                             
         DS    CL1                                                              
         ORG   PNET                                                             
PWCTEXT  DS    CL80                                                             
         EJECT                                                                  
**********************************************************************          
* OTHER DSECTS                                                       *          
**********************************************************************          
         SPACE 1                                                                
GOBLOCKD DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE ACGOBLOCK                                                      
         PRINT ON                                                               
*                                                                               
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
* DMDTFIS                                                                       
*********PRINT OFF                                                              
*********INCLUDE DMDTFIS                                                        
*********PRINT ON                                                               
*                                                                               
JBLOCKD  DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE ACJOBBLOCK                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'020ACREPIE02S08/17/00'                                      
         END                                                                    
