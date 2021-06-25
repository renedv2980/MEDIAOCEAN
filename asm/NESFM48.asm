*          DATA SET NESFM48    AT LEVEL 018 AS OF 04/01/08                      
*PHASE T31C48A,*                                                                
***********************************************************************         
*                                                                               
*  TITLE: T31C48 - COMMERCIAL SEPERATION REPORT                                 
*                                                                               
***********************************************************************         
         TITLE 'T31C48 COMMERCIAL SEPERATION'                                   
T31C48   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,CMSEP,R7                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,PRINTREP       PRINT RECORDS                                
         BE    PR                                                               
EXIT     XIT1                                                                   
MAXPRD   EQU   252                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE KEY                                                                  
***********************************************************************         
VK       DS    0H                                                               
         XC    CLT,CLT                                                          
         XC    CLTA,CLTA                                                        
         XC    PRD,PRD                                                          
         XC    PRDA,PRDA                                                        
         XC    PRDHEAD,PRDHEAD                                                  
         XC    NETA,NETA                                                        
         XC    STARTC,STARTC                                                    
         XC    ENDC,ENDC                                                        
         XC    PERIOD,PERIOD                                                    
         XC    TIMEB,TIMEB                                                      
*                                                                               
         LA    R2,CMSMEDH          MEDIA                                        
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
*                                                                               
         GOTO1 VALIMED             AGY/MED IN BAGYMD                            
*                                                                               
         LA    R2,CMSCLTH          CLIENT                                       
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
*                                                                               
         MVC   AIO,AIO1                                                         
         GOTO1 VALICLT             2 BYTE CLT IN BCLT, READ CLT REC             
         MVC   CLT,BCLT                                                         
**** NOT SURE WE NEED THIS, CLTNM ALREADY HAS  IT                               
*        MVC   CLTNAME,CLTNM       CLIENT NAME                                  
**** NOT SURE WE NEED THIS, CLTNM ALREADY HAS  IT                               
         MVC   CLTA,QCLT                                                        
*                                                                               
         LA    R2,CMSPRDH          PRODUCT                                      
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
*                                                                               
         MVC   PRDA,=C'ALL'                                                     
         MVC   PRDHEAD,=C'ALL'                                                  
         MVC   PRDNAME(3),=C'ALL'                                               
         OC    PRDNAME,SPACES                                                   
*                                                                               
         OI    MYFLAG,ALLPRD                                                    
         CLC   8(3,R2),=C'ALL'                                                  
         BE    VK40                                                             
         CLC   8(3,R2),=C'POL'                                                  
         BE    VK40                                                             
         NI    MYFLAG,X'FF'-ALLPRD                                              
*                                                                               
         MVC   PRDA,8(R2)                                                       
         MVC   PRDHEAD,8(R2)                                                    
         OC    PRDA,SPACES         SPACE PADDED                                 
         OC    PRDHEAD,SPACES      SPACE PADDED                                 
*                                                                               
         LA    R6,KEY                                                           
         USING PRDHDR,R6                                                        
         XC    KEY,KEY                                                          
         MVC   PLSTTYPE(2),=XL2'0DF1'                                           
         MVC   PLSTAM,BAGYMD                                                    
         MVC   PLSTCLT,BCLT                                                     
         MVC   PLSTPRD,PRDA                                                     
         CLC   PRDA,=CL3'POL'                                                   
         BNE   *+8                                                              
         MVI   PLSTXFF,X'FF'                                                    
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR ',KEY,KEY,MYDMWRK              
         CLC   KEY(9),KEYSAVE                                                   
         BNE   INVLPRD                                                          
         MVC   PRD,PLSTBPRD                                                     
*                                                                               
         MVC   AIO,AIO1                                                         
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFIL ',KEY+14,AIO,MYDMWRK           
         L     R6,AIO                                                           
         MVC   PRDNAME,PNAME       PRODUCT NAME                                 
         DROP  R6                                                               
*                                                                               
*  REMOVE OLD PRODUCT VALIDATION ROUTINE                                        
*                                                                               
******      LA    R5,MAXPRD           MAX # OF PRODUCTS                         
*                                                                               
***         LA    R6,SVCLIST                                                    
*                                                                               
***VK20     DS    0H                                                            
***         OC    0(4,R6),0(R6)                                                 
***         BZ    INVLPRD                                                       
*                                                                               
***         CLC   0(3,R6),PRDA        PRODUCT MATCH?                            
***         BE    VK30                                                          
*                                                                               
***         LA    R6,4(R6)                                                      
***         BCT   R5,VK20                                                       
***         B     INVLPRD                                                       
*                                                                               
***VK30     DS    0H                                                            
***         MVC   PRD,3(R6)           SAVE PRODUCT EQUATE                       
*                                                                               
***         XC    KEY,KEY             GET PRODUCT RECORD                        
***         LA    R6,KEY                                                        
***         USING PRDHDRD,R6                                                    
*                                                                               
***         MVC   PKEYAM,BAGYMD                                                 
***         MVC   PKEYCLT,CLT                                                   
***         MVC   PKEYPRD,PRDA                                                  
*                                                                               
***         MVC   KEYSAVE,KEY                                                   
***         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR ',KEY,KEY,MYDMWRK           
***         CLC   KEY(13),KEYSAVE                                               
***         BNE   INVLPRD                                                       
*                                                                               
*                                                                               
VK40     DS    0H                                                               
         LA    R2,CMSNETH          NETWORK                                      
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
*                                                                               
         GOTO1 VALINTWK                                                         
*                                                                               
         MVC   NETA,8(R2)                                                       
         OC    NETA,SPACES                                                      
*                                                                               
         LA    R2,CMSPERH          PERIOD                                       
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
*                                                                               
         LA    RF,CMSPER           SETUP DMCB FOR PERVAL CALL                   
         ST    RF,DMCB                                                          
         MVC   DMCB(1),CMSPERH+5   INPUT LENGTH                                 
*                                                                               
         XC    MYBLOCK,MYBLOCK                                                  
         LA    RF,MYBLOCK                                                       
         ST    RF,DMCB+4                                                        
*                                                                               
         GOTO1 PERVAL,DMCB                                                      
         CLI   DMCB+4,0                                                         
         BNE   INVLPER                                                          
*                                                                               
         LA    RF,MYBLOCK                                                       
         USING PERVALD,RF                                                       
*                                                                               
         MVC   PERIOD,PVALCPER     DISPLAYABLE PERIOD                           
         MVC   STARTC,PVALCSTA     START OF PERIOD                              
         MVC   ENDC,PVALCEND       END OF PERIOD                                
         DROP  RF                                                               
*                                                                               
         MVI   TIMEB,X'14'         DEFAULT TO 20 MINUTES                        
         MVC   TIMEA,=C'20 '                                                    
*                                                                               
         LA    R2,CMSTIMEH                                                      
         CLI   5(R2),0                                                          
         BE    VKX                                                              
         TM    4(R2),X'08'         MUST BE NUMERIC                              
         BZ    INVLTIME                                                         
*                                                                               
         ZIC   R1,CMSTIMEH+5                                                    
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,CMSTIME(0)                                                   
         CVB   R0,DUB                                                           
*                                                                               
         LTR   R0,R0               TEST FOR ZERO                                
         BZ    INVLTIME                                                         
*                                                                               
         STC   R0,TIMEB                                                         
*                                                                               
         MVC   TIMEA,8(R2)                                                      
         OC    TIMEA,=C'   '                                                    
*                                                                               
VKX      DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* PRINT RECORDS                                                                 
***********************************************************************         
PR       DS    0H                                                               
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD,0                                   
*                                                                               
         XC    PREVUNIT,PREVUNIT                                                
*                                                                               
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         LA    R1,HDRTN                                                         
         ST    R1,HEADHOOK                                                      
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING NURECD,R6                                                        
*                                                                               
         MVI   NUKTYPE,X'04'                                                    
         MVC   NUKAM,BAGYMD                                                     
         MVC   NUKCLT,CLT                                                       
*                                                                               
         GOTO1 HIGH                                                             
         B     PR10                                                             
*                                                                               
PRSEQ    GOTO1 SEQ                                                              
*                                                                               
PR10     DS    0H                                                               
         LA    R6,KEY                                                           
         CLC   KEY(4),KEYSAVE                                                   
         BNE   PR100                                                            
*                                                                               
         CLC   NUKDATE,STARTC                                                   
         BL    PRSEQ                                                            
         CLC   NUKDATE,ENDC                                                     
         BH    PRSEQ                                                            
*                                                                               
         CLC   NUKNET,NETA         SAME NETWORK?                                
         BNE   PRSEQ                                                            
*                                                                               
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
*                                                                               
*!!!     MVC   SVDATE,NUKDATE      INITIALIZE TO AIR DATE                       
         XC    SVDATE,SVDATE                                                    
         MVC   SVDATEA,NUKDATE     AIR DATE                                     
         MVI   BYTE,0                                                           
*                                                                               
         L     R6,AIO                                                           
*                                                                               
         TM    NUUNITST,X'40'      PREEMPTED UNIT?                              
         BO    PRSEQ               YES - SKIP IT                                
*                                                                               
         TM    NUUNITST,X'02'      MISSED UNIT?                                 
         BO    PRSEQ               YES - SKIP IT                                
*                                                                               
         TM    NUPACKST,X'20'      LOCKED?                                      
         BO    PRSEQ                                                            
*                                                                               
         OC    NUAFFTIM,NUAFFTIM   IF NO AFFID TIME, THEN ERROR                 
         BZ    PR17                                                             
         DROP  R6                                                               
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BNE   PR17                                                             
         USING NUSDRD,R6                                                        
*                                                                               
         OC    NUSDAFDT,NUSDAFDT                                                
         BZ    PR17                                                             
*                                                                               
         CLC   NUSDAFDT,STARTC                                                  
         BL    PR17                                                             
         CLC   NUSDAFDT,ENDC                                                    
         BH    PR17                                                             
         MVC   SVDATE,NUSDAFDT     NON SFLG=X'FF' UNIT, USE AFFID DATE          
         B     PR20                                                             
         DROP  R6                                                               
*                                                                               
PR17     MVI   BYTE,X'FF'                                                       
         XC    SVDATE,SVDATE                                                    
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BNE   PR20                                                             
         USING NUSDRD,R6                                                        
*                                                                               
         OC    NUSDAFDT,NUSDAFDT                                                
         BZ    PR20                                                             
*                                                                               
         CLC   NUSDAFDT,STARTC                                                  
         BL    PR20                                                             
         CLC   NUSDAFDT,ENDC                                                    
         BH    PR20                                                             
         MVC   SVDATE,NUSDAFDT     USE AFFID DATE IN FF UNIT AS WELL            
*                                                                               
PR20     DS    0H                  IF ALL PROD GET CURRENT VALUES               
         L     R5,AIO                                                           
         USING NURECD,R5                                                        
         TM    MYFLAG,ALLPRD                                                    
         BZ    PR25                                                             
         XC    PRDA,PRDA                                                        
         MVC   PRD,NUPRD                                                        
         L     R6,AIO                                                           
         MVI   ELCODE,X'19'                                                     
         BAS   RE,GETEL                                                         
         DROP  R5                                                               
         BNE   PR25                                                             
         USING NUPDED,R6                                                        
         MVC   PRDA,NUPDEPR                                                     
         DROP  R5                                                               
*                                                                               
PR25     DS    0H                                                               
         TM    MYFLAG,ALLPRD                                                    
         BO    *+12                                                             
         BAS   RE,CHKPRD           CHECK PRODUCT FILTER                         
         BNE   PRSEQ                                                            
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'18'                                                     
         BAS   RE,GETEL                                                         
         BNE   PR30                                                             
         USING NUDTAD,R6                                                        
*                                                                               
         MVC   INVA,NUDTINVN      INVOICE #                                     
         DROP  R6                                                               
*                                                                               
PR30     DS    0H                                                               
         MVC   SVUNTKEY,KEY                                                     
*                                                                               
         L     R6,AIO                                                           
         USING NURECD,R6                                                        
*                                                                               
         LA    R5,P                                                             
         USING SORTRECD,R5                                                      
         XC    P(SRECLNQ),P                                                     
*                                                                               
         MVC   SFLG,BYTE           X'FF' - ERROR                                
         MVC   SAM,NUKAM           AGY/MD                                       
         MVC   SCLT,NUKCLT         CLIENT                                       
         MVC   SNET,NUKNET         NETWORK                                      
         MVC   SPRD,PRD            PRODUCT EQUATE                               
         MVC   SPRDA,PRDA          PRODUCT ALPHA                                
         MVC   SDATE,SVDATE        AFFID DATE                                   
         MVC   STIME,NUAFFTIM      START TIME                                   
         MVC   SPGM,NUKPROG        PROGRAM                                      
         MVC   SCOST,NUACTUAL      COST                                         
         MVC   SLEN,NULEN          LENGTH                                       
         MVC   SSUB,NUKSUB         SUB-LINE                                     
         MVC   SINVOICE,INVA       INVOICE #                                    
         MVC   SEST,NUKEST         ESTIMATE                                     
         MVC   SPACK,NUPACK        PACKAGE                                      
         MVC   SDATEA,SVDATEA      AIR DATE                                     
         DROP  R5                                                               
*                                                                               
         GOTO1 SORTER,DMCB,=C'PUT',P                                            
         B     PRSEQ                                                            
*                                                                               
PR100    DS    0H                                                               
         GOTO1 SORTER,DMCB,=C'GET'                                              
         OC    4(4,R1),4(R1)                                                    
         BZ    PRX                                                              
*                                                                               
         L     R6,4(R1)            A(SORTED RECORD)                             
         ST    R6,ASORTREC                                                      
         USING SORTRECD,R6                                                      
*                                                                               
         CLI   SFLG,X'FF'                                                       
         BE    PR150                                                            
*                                                                               
         OC    PREVUNIT,PREVUNIT   FIRST TIME THROUGH?                          
         BNZ   PR110                                                            
         MVC   PREVUNIT,0(R6)                                                   
         OI    MYFLAG,NEWCLT       SAVE CLIENT                                  
         B     PR100                                                            
*                                                                               
PR110    DS    0H                                                               
         CLC   1(13,R6),PREVUNIT+1 SAME AGY/CLT/NET/PRD/DATE?                   
         BE    PR115                                                            
*                                                                               
         CLC   SCLT,PREVUNIT+2     SAME CLIENT                                  
         BE    *+8                                                              
         OI    MYFLAG,NEWCLT                                                    
         B     PR140                                                            
*                                                                               
PR115    DS    0H                                                               
         SR    R4,R4                                                            
         SR    R5,R5                                                            
         MVC   HALF,STIME          CURRENT UNIT TIME                            
         LH    R5,HALF             GET ONLY THE HOUR PORTION                    
         D     R4,=F'100'                                                       
*                                                                               
         MHI   R5,60               HOURS X MINUTES = HOUR IN MINUTES            
         AR    R4,R5               HOUR IN MINUTES + LEFTOVER MINUTES           
         ST    R4,STARTH           START TIME IN MINUTES                        
*                                                                               
         SR    R4,R4                                                            
         SR    R5,R5                                                            
         MVC   HALF,PREVUNIT+14    PREVIOUS UNIT TIME                           
         LH    R5,HALF             GET ONLY THE HOUR PORTION                    
         D     R4,=F'100'                                                       
*                                                                               
         MHI   R5,60               HOURS X MINUTES = HOUR IN MINUTES            
         AR    R4,R5               HOUR IN MINUTES + LEFTOVER MINUTES           
*                                                                               
         ZIC   RF,TIMEB            SEPERATION TIME                              
         AR    R4,RF                                                            
         STH   R4,SVTIME           START TIME + SEPERATION TIME                 
*                                                                               
         CLC   STARTH+2(2),SVTIME  CURRENT UNIT WITHIN SEP TIME?                
         BH    PR140                                                            
*&&DO                                                                           
         MVC   SVTIME,PREVUNIT+14  START TIME OF PREV UNIT                      
         LH    R3,SVTIME                                                        
         ZIC   RF,TIMEB            SEPERATION TIME                              
         AR    R3,RF                                                            
         STH   R3,SVTIME           START TIME + SEPERATION TIME                 
*                                                                               
         CLC   STIME(2),SVTIME     CURRENT UNIT WITHIN SEP TIME?                
         BH    PR140                                                            
*&&                                                                             
PR120    DS    0H                                                               
         LA    R6,PREVUNIT         PRINT PREV UNIT                              
         XC    SEPTIME,SEPTIME                                                  
         BAS   RE,PRLINE                                                        
*                                                                               
         L     R6,ASORTREC         PRINT CURRENT UNIT                           
*                                                                               
         SR    R4,R4                                                            
         SR    R5,R5                                                            
         MVC   HALF,STIME          CURRENT UNIT TIME                            
         LH    R5,HALF             GET ONLY THE HOUR PORTION                    
         D     R4,=F'100'                                                       
*                                                                               
         MHI   R5,60               HOURS X MINUTES = HOUR IN MINUTES            
         AR    R4,R5               HOUR IN MINUTES + LEFTOVER MINUTES           
         ST    R4,STARTH           START TIME IN MINUTES                        
*                                                                               
         SR    R4,R4                                                            
         SR    R5,R5                                                            
         MVC   HALF,PREVUNIT+14    PREVIOUS UNIT TIME                           
         LH    R5,HALF             GET ONLY THE HOUR PORTION                    
         D     R4,=F'100'                                                       
*                                                                               
         MHI   R5,60               HOURS X MINUTES = HOUR IN MINUTES            
         AR    R4,R5               HOUR IN MINUTES + LEFTOVER MINUTES           
         ST    R4,ENDH             END TIME IN MINUTES                          
*                                                                               
         L     R4,STARTH           FIND TIME SEPERATION BETWEEN UNITS           
         L     R5,ENDH             IN MINUTES                                   
         SR    R4,R5                                                            
         STH   R4,SEPTIME                                                       
*                                                                               
*&&DO                                                                           
         MVC   HALF,STIME          CURRENT UNIT TIME                            
         LH    R3,HALF                                                          
         MVC   HALF,PREVUNIT+14    PREV UNIT                                    
         LH    RF,HALF             FIND TIME SEPERATION BETWEEN UNITS           
         SR    R3,RF                                                            
         STH   R3,SEPTIME                                                       
*&&                                                                             
         BAS   RE,PRLINE                                                        
*                                                                               
PR130    DS    0H                                                               
         XC    PREVUNIT,PREVUNIT   SET CURRENT UNIT AS PREV                     
         MVC   PREVUNIT,0(R6)                                                   
*                                                                               
         XC    P,P                                                              
         OC    P,SPACES                                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     PR100                                                            
*                                                                               
PR140    DS    0H                                                               
         XC    PREVUNIT,PREVUNIT   SET CURRENT UNIT AS PREV                     
         MVC   PREVUNIT,0(R6)                                                   
         B     PR100                                                            
*                                                                               
PR150    DS    0H                  PRINT ERROR RECORDS                          
         XC    P,P                                                              
         OC    P,SPACES                                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   P(35),=C'**********     ERROR     **********'                    
         GOTO1 SPOOL,DMCB,(R8)                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         XC    SEPTIME,SEPTIME                                                  
         MVI   BYTE,X'FF'                                                       
         BAS   RE,PRLINE                                                        
*                                                                               
PR155    DS    0H                                                               
         GOTO1 SORTER,DMCB,=C'GET'                                              
         OC    4(4,R1),4(R1)                                                    
         BZ    PRX                                                              
*                                                                               
         BAS   RE,PRLINE                                                        
         B     PR155                                                            
*                                                                               
PRX      DS    0H                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,16,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=63'                                    
*                                                                               
***********************************************************************         
* PRINT INFORMATION                                                             
***********************************************************************         
PRLINE   NTR1                                                                   
         LA    R5,P                                                             
         XC    P(PLENQ),P                                                       
         USING PLINED,R5                                                        
         USING SORTRECD,R6                                                      
*                                                                               
         GOTO1 CLUNPK,DMCB,SCLT,PCLT                CLIENT                      
         MVC   PNET,SNET                            NETWORK                     
*                                                                               
         XC    FULL,FULL                                                        
******   MVC   FULL+3(1),SPRD                                                   
******   BAS   RE,GETPRDA                           GET ALPHA PRODUCT           
******   MVC   PPRD,FULL                            PRODUCT                     
         MVC   PPRD,SPRDA                           PRODUCT                     
*                                                                               
         GOTO1 DATCON,DMCB,(2,SDATEA),(5,PDATEA)    AIR DATE                    
*                                                                               
         OC    SDATE,SDATE                          ANY AFF DATE?               
         BZ    PRL15                                                            
*                                                                               
         GOTO1 DATCON,DMCB,(2,SDATE),(5,PDATE)      AFF DATE                    
*                                                                               
PRL15    OC    STIME,STIME                                                      
         BZ    PRL20                                                            
*                                                                               
         XC    FULL,FULL                                                        
         MVC   FULL(2),STIME                                                    
         GOTO1 UNTIME,DMCB,FULL,PTIME               START TIME                  
*                                                                               
         OC    SEPTIME,SEPTIME     PRINTING CURRENT UNIT?                       
         BZ    PRL20                                                            
         EDIT  SEPTIME,PSEP,ZERO=NOBLANK            SEP TIME                    
*                                                                               
PRL20    DS    0H                                                               
         MVC   PPGM,SPGM                            PROGRAM CODE                
*                                                                               
         XC    HALF,HALF                                                        
         MVC   HALF,SDATE                           START TIME                  
         MVC   PROG,PPGM                            PROGRAM CODE                
*                                                                               
         BAS   RE,GETPGMN                                                       
         MVC   PPGMNAME,WORK                        PROGRAM NAME                
*                                                                               
         EDIT  SCOST,PCOST,2,ZERO=NOBLANK,FLOAT=$,COMMAS=YES                    
         EDIT  SLEN,PLNG,ZERO=NOBLANK               UNIT LENGTH                 
         EDIT  SSUB,PSUB,ZERO=NOBLANK               SUB-LINE                    
         EDIT  SPACK,PPACK,ZERO=NOBLANK             PACKAGE                     
         EDIT  SEST,PEST,ZERO=NOBLANK               ESTIMATE                    
*                                                                               
         MVC   PINVOICE,SINVOICE                    INVOICE #                   
*                                                                               
         OC    P,SPACES                                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
         DROP  R5,R6                                                            
*                                                                               
PRLINEX  DS    0H                                                               
         B     EXIT                                                             
***********************************************************************         
* GET PRODUCT ALPHA                                                             
***********************************************************************         
GETPRDA  NTR1                                                                   
         MVC   AIO,AIO1                                                         
         TM    MYFLAG,NEWCLT                                                    
         BZ    GPRDA10                                                          
         GOTO1 GETCLT,DMCB,CLTA    GET CLIENT RECORD AND SVCLIST                
         NI    MYFLAG,X'FF'-NEWCLT                                              
*                                                                               
GPRDA10  DS    0H                                                               
         LA    R6,SVCLIST                                                       
         LA    R5,MAXPRD           MAX # OF PRODUCTS                            
*                                                                               
GPRDA20  DS    0H                                                               
         OC    0(4,R6),0(R6)                                                    
         BZ    GETPRDAX                                                         
*                                                                               
         CLC   3(1,R6),FULL+3                                                   
         BE    GPRDA30                                                          
*                                                                               
         LA    R6,4(R6)                                                         
         BCT   R5,GPRDA20                                                       
         B     GETPRDAX                                                         
*                                                                               
GPRDA30  DS    0H                                                               
         MVC   FULL(3),0(R6)       SAVE ALPHA PRODUCT                           
*                                                                               
GETPRDAX DS    0H                                                               
         B     EXIT                                                             
***********************************************************************         
* GET PROGRAM NAME                                                              
***********************************************************************         
GETPGMN  NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING NPGRECD,R6                                                       
*                                                                               
         MVC   NPGKTYP,=X'0D20'                                                 
         MVC   NPGKAM,BAGYMD                                                    
         MVC   NPGKNET,BMKTSTA     NETWORK MARKET #                             
         MVC   NPGKPROG,PROG       PROGRAM CODE                                 
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR ',KEY,KEY,MYDMWRK              
         B     GPGMN20                                                          
*                                                                               
GPGMNSEQ DS    0H                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'SPTDIR ',KEY,KEY,MYDMWRK              
*                                                                               
GPGMN20  DS    0H                                                               
         CLC   KEY(11),KEYSAVE                                                  
         BNE   GETPGMNX                                                         
*                                                                               
         CLC   HALF,NPGKEND        UNIT DATE > PROGRAM END DATE ?               
         BH    GPGMNSEQ                                                         
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFIL ',KEY+14,AIO,MYDMWRK           
         DROP  R6                                                               
*                                                                               
         GOTO1 HELLO,DMCB,(C'G',=C'SPTFIL '),(X'92',AIO),0                      
         CLI   12(R1),0            TEST IF FOUND                                
         BNE   GETPGMNX                                                         
         L     R6,12(R1)                                                        
         USING NPGEL92,R6                                                       
*                                                                               
         MVC   WORK(16),NPGNAME    PROGRAM NAME                                 
         DROP  R6                                                               
*                                                                               
GETPGMNX DS    0H                                                               
         B     EXIT                                                             
***********************************************************************         
* CHECK PRODUCT FILTER                                                          
***********************************************************************         
CHKPRD   NTR1                                                                   
         L     R6,AIO                                                           
         USING NURECD,R6                                                        
*  PROCESS ALPHA PRODUCT CHECK                                                  
         MVI   ELCODE,X'19'                                                     
         BAS   RE,GETEL                                                         
         BNE   CP100                                                            
         USING NUPDED,R6                                                        
*                                                                               
         SR    RE,RE                                                            
         ZIC   RF,NUPDELEN                                                      
         SH    RF,=H'3'                                                         
         D     RE,=F'7'                                                         
         LA    R6,NUPDEPR          PRODUCT CODE                                 
*                                                                               
CP20     DS    0H                                                               
         CLC   0(3,R6),PRDA        SAME PRODUCT?                                
         BE    CPYES                                                            
         LA    R6,7(R6)            BUMP TO NEXT PRODUCT                         
         BCT   RF,CP20                                                          
         B     CPNO                                                             
         DROP  R6                                                               
*  PROCESS 01 ELEMENT CHECK                                                     
CP100    L     R6,AIO                                                           
         USING NURECD,R6                                                        
         CLI   NUPRD,0                                                          
         BE    CP150                                                            
*                                                                               
         CLC   NUPRD,PRD           SAME PRODUCT?                                
         BE    CPYES                                                            
         CLC   NUPRD2,PRD                                                       
         BE    CPYES                                                            
         B     CPNO                                                             
         DROP  R6                                                               
*  PRCESS 14 ELEMENT CHECK                                                      
CP150    DS    0H                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'14'                                                     
         BAS   RE,GETEL                                                         
         BNE   CPNO                                                             
         USING NUPRDD,R6                                                        
*                                                                               
         SR    RE,RE                                                            
         ZIC   RF,NUPRDLEN                                                      
         SH    RF,=H'3'                                                         
         D     RE,=F'6'                                                         
         LA    R6,NUPRDPR          PRODUCT CODE                                 
*                                                                               
CP200    DS    0H                                                               
         CLC   0(1,R6),PRD         SAME PRODUCT?                                
         BE    CPYES                                                            
         LA    R6,6(R6)            BUMP TO NEXT PRODUCT                         
         BCT   RF,CP200                                                         
         B     CPNO                                                             
         DROP  R6                                                               
*                                                                               
CPYES    SR    R1,R1                                                            
         B     *+8                                                              
CPNO     LA    R1,1                                                             
         LTR   R1,R1                                                            
*                                                                               
CHKPRDX  DS    0H                                                               
         B     EXIT                                                             
***********************************************************************         
* HEADER AND HEAD HOOK ROUTINES                                                 
***********************************************************************         
HEADING  DS    0H                                                               
         SSPEC H1,3,REQUESTOR                                                   
*!!!     SSPEC H3,3,C'MEDIA    -'                                               
         SSPEC H1,41,C'COMMERCIAL SEPARATION EXCEPTION REPORT'                  
         SSPEC H2,41,C'--------------------------------------'                  
         SSPEC H1,93,AGYNAME                                                    
         SSPEC H2,93,AGYADD                                                     
         SSPEC H3,93,REPORT                                                     
         SSPEC H4,3,C'CLIENT   -'                                               
         SSPEC H4,93,RUN                                                        
         SSPEC H5,3,C'PRODUCT  -'                                               
         SSPEC H5,103,PAGE                                                      
         SSPEC H6,3,C'NETWORK  -'                                               
         SSPEC H7,3,C'PERIOD   -'                                               
         SSPEC H8,3,C'SEP TIME -'                                               
         DC    X'00'                                                            
*                                                                               
HDRTN    NTR1                                                                   
*!!!     MVI   H3+19,C'N'          MEDIA                                        
         MVC   H4+19(3),CLTA       CLIENT                                       
         MVC   H4+25(20),CLTNM     CLIENT NAME                                  
         MVC   H5+19(3),PRDHEAD    PRODUCT                                      
         CLC   PRDA,=C'ALL'                                                     
         BE    *+10                                                             
         MVC   H5+25(20),PRDNAME   PRODUCT NAME                                 
         MVC   H6+19(4),NETA       NETWORK                                      
         MVC   H7+19(17),PERIOD    PERIOD                                       
         MVC   H8+19(3),TIMEA      SEPERATION TIME                              
*                                                                               
         LA    RF,H9                                                            
         USING PLINED,RF                                                        
*                                                                               
         MVC   PCLT,=C'CLT'                                                     
         MVC   PNET,=C'NET '                                                    
         MVC   PPRD,=C'PRD'                                                     
         MVC   PDATE,=C'AFF DATE'                                               
         MVC   PDATEA,=C'AIR DATE'                                              
         MVC   PTIME,=C'TIME '                                                  
         MVC   PSEP(3),=C'SEP'                                                  
         MVC   PPGM,=C'PROG  '                                                  
         MVC   PPGMNAME,=C'PROGRAM NAME    '                                    
         MVC   PCOST,=C'      COST'                                             
         MVC   PLNG,=C'LNG'                                                     
         MVC   PSUBH(6),=C'LINE #'                                              
         MVC   PINVOICE,=C'INVOICE ID'                                          
         MVC   PPACK,=C'PKG'                                                    
         MVC   PEST,=C'EST'                                                     
*                                                                               
         LA    RF,H10                                                           
         MVI   0(RF),C'-'                                                       
         MVC   1(PLENQ-1,RF),0(RF)                                              
*                                                                               
HDRTNX   B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ERROR MESSAGES                                                                
***********************************************************************         
INVLFLD  MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
MISSFLD  MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
INVLCLI  MVI   ERROR,INVCLI                                                     
         B     TRAPERR                                                          
*                                                                               
INVLACT  MVI   ERROR,INVACT                                                     
         LA    R2,CONACTH                                                       
         B     TRAPERR                                                          
*                                                                               
INVLAGY  DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(38),=C'ERROR RECORD NOT VALID FOR THIS AGENCY'           
         B     MYERR                                                            
*                                                                               
INVLPRD  DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(39),=C'ERROR PRODUCT DOES NOT EXIST FOR CLIENT'          
         B     MYERR                                                            
*                                                                               
INVLPER  DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(19),=C'ERROR INVALID DATES'                              
         B     MYERR                                                            
*                                                                               
INVLTIME DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(18),=C'ERROR INVALID TIME'                               
         B     MYERR                                                            
*                                                                               
MYERR    GOTO1 ERREX2                                                           
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
       ++INCLUDE NESFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NESFMAFD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE NESFMWORKD                                                     
         ORG   SYSSPARE                                                         
*                           *******  T31C48 WORK AREA  *******                  
WORKAREA DS    0CL1                                                             
MYDMWRK  DS    12D                                                              
*                                                                               
PREVUNIT DS    XL60                BASE SORT RECORD TO COMPARE WITH             
ASORTREC DS    A                   A('GET' SORT REC)                            
*                                                                               
SVSPTKEY DS    XL13                                                             
SVUNTKEY DS    XL20                                                             
SVAREC   DS    A                   A(RECORD)                                    
*                                                                               
SVDATE   DS    XL2                 AFFID DATE                                   
SVDATEA  DS    XL2                 AIR DATE                                     
SVTIME   DS    H                   START TIME (MILITARY)                        
*                                                                               
SEPTIME  DS    H                   SEP TIME BETWEEN UNITS                       
*                                                                               
ATABLE   DS    A                   A(UPPER STORAGE TABLE)                       
*                                                                               
CLT      DS    XL2                 CLIENT                                       
CLTA     DS    CL3                 CLIENT ALPHA                                 
CLTNAME  DS    CL20                CLIENT NAME                                  
*                                                                               
PRD      DS    XL1                 PRODUCT                                      
PRDA     DS    CL3                 PRODUCT ALPHA                                
PRDNAME  DS    CL20                PRODUCT NAME                                 
PRDHEAD  DS    CL3                                                              
*                                                                               
NETA     DS    CL4                 NETWORK ALPHA                                
*                                                                               
STARTC   DS    XL2                 START DATE (COMPRESSED)                      
ENDC     DS    XL2                 END DATE (COMPRESSED)                        
PERIOD   DS    CL17                DISPLAYABLE PERIOD (AFTER PERVAL)            
*                                                                               
TIMEB    DS    XL1                 SEPERATION TIME (BINARY)                     
TIMEA    DS    CL3                 SEPERATION TIME ALPHA                        
*                                                                               
PROG     DS    CL6                 PROGRAM CODE                                 
*                                                                               
INVA     DS    CL10                INVOICEE                                     
*                                                                               
MYFLAG   DS    XL1                                                              
ALLPRD   EQU   X'01'               ALL PRODUCTS                                 
NEWCLT   EQU   X'02'               CLIENT BREAK                                 
*                                                                               
MYBLOCK  DS    XL100                                                            
*                                                                               
STARTH   DS    F                                                                
ENDH     DS    F                                                                
*                                                                               
WORKEND  EQU   *                                                                
         EJECT                                                                  
*                                                                               
SORTRECD DSECT                                                                  
SFLG     DS    XL1                 X'FF'- ERROR                                 
SAM      DS    XL1                 AGY/MEDIA                                    
SCLT     DS    XL2                 CLIENT                                       
SNET     DS    XL4                 NETWORK                                      
SPRD     DS    XL1                 PRODUCT EQUATE                               
SPRDA    DS    XL3                 PRODUCT ALPHA                                
SDATE    DS    XL2                 AFF DATE                                     
STIME    DS    XL2                 START/END TIME                               
SPGM     DS    CL6                 PROGRAM CODE                                 
SPGMNAME DS    CL16                PROGRAM NAME                                 
SCOST    DS    XL4                 COST                                         
SLEN     DS    XL1                 UNIT LENGTH                                  
SSUB     DS    XL1                 SUB-LINE                                     
SINVOICE DS    CL10                INVOICE #                                    
SPACK    DS    XL1                 PACKAGE                                      
SEST     DS    XL1                 ESTIMATE                                     
SDATEA   DS    XL2                 AIR DATE                                     
SRECLNQ  EQU   *-SAM                                                            
*                                                                               
PLINED   DSECT                                                                  
PCLT     DS    CL3                 CLIENT                                       
         DS    CL2                                                              
PNET     DS    CL4                 NETWORK                                      
         DS    CL2                                                              
PPRD     DS    CL3                 PRODUCT                                      
         DS    CL2                                                              
PDATE    DS    CL8                 AFF DATE                                     
         DS    CL2                                                              
PDATEA   DS    CL8                 AIR DATE                                     
         DS    CL2                                                              
PTIME    DS    CL5                 TIME                                         
         DS    CL2                                                              
PSEP     DS    CL2                 SEPERATION (IN MINUTES)                      
         DS    CL3                                                              
PPGM     DS    CL6                 PROGRAM CODE                                 
         DS    CL2                                                              
PPGMNAME DS    CL16                PROGRAM NAME                                 
         DS    CL2                                                              
PCOST    DS    CL10                ACTUAL COST                                  
         DS    CL2                                                              
PLNG     DS    CL3                 UNIT LENGTH                                  
         DS    CL2                                                              
PINVOICE DS    CL10                INVOICE #                                    
         DS    CL2                                                              
PSUBH    DS    CL3                 SUB-LINE                                     
PSUB     DS    CL3                                                              
         DS    CL2                                                              
PPACK    DS    CL3                 PACKAGE                                      
         DS    CL2                                                              
PEST     DS    CL3                 ESTIMATE                                     
PLENQ    EQU   *-PCLT                                                           
*                                                                               
CLTRECD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENPROG                                                      
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE NEGENUNIT                                                      
*                                                                               
         PRINT GEN                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'018NESFM48   04/01/08'                                      
         END                                                                    
