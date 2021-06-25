*          DATA SET ACBAT0E    AT LEVEL 012 AS OF 03/23/11                      
*PHASE T61B0EA                                                                  
         PRINT NOGEN                                                            
         TITLE 'BILLING TRANSFER'                                               
T61B0E   CSECT                                                                  
         NMOD1 PROGDX-PROGD,**T61B0E,R7,CLEAR=YES                               
         USING TWAD,RA                                                          
         L     R9,4(R1)                                                         
         USING GWS,R9                                                           
         USING PROGD,RC                                                         
VALDOC   LA    R2,BITDOCH                                                       
         BAS   RE,ANY                                                           
*                                                                               
VALDAT   LA    R2,BITDATH                                                       
         MVC   FVMSGNO,=AL2(AE$INDAT)                                           
         CLI   BITDATH+5,0                                                      
         BNE   *+12                                                             
         BAS   RE,GETODAY                                                       
         B     VALD02                                                           
         GOTO1 DATVAL,DMCB,(0,8(R2)),WORK                                       
         OC    DMCB(4),DMCB                                                     
         BZ    ERROR                                                            
*                                                                               
VALD02   GOTO1 DATCON,DMCB,(0,WORK),(1,DATESAVE)                                
         GOTO1 DATECHK,DMCB,DATESAVE                                            
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
*                                                                               
VALAMT   LA    R2,BITAMTH                                                       
         MVC   FVMSGNO,=AL2(AE$INAMT)                                           
         ZIC   R3,BITAMTH+5                                                     
         GOTO1 AMTVAL,DMCB,BITAMT,(R3)                                          
         CLI   0(R1),X'FF'                                                      
         BE    ERROR                                                            
         L     R4,DMCB+4                                                        
         LA    R4,0(R4)                                                         
         ZAP   SAVECASH,0(8,R4)                                                 
         ZAP   TRANSAMT,SAVECASH                                                
         CLI   INPUT,15            BILLING TRANSFER                             
         BNE   VALACT                                                           
         ZAP   DUB,SAVECASH                                                     
         MP    DUB,=P'-1'                                                       
         ZAP   SAVECASH,DUB                                                     
         EJECT                                                                  
***********************************************************************         
*              VALIDATE & SAVE ACCOUNTS                                         
***********************************************************************         
*                                                                               
VALACT   MVC   FINOFC,SPACES                                                    
         MVC   ANLOFC,SPACES                                                    
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         SR    R3,R3                                                            
         MVI   OFFSW,C'N'                                                       
         TM    COMPSTAT,X'20'                                                   
         BZ    VALOFF                                                           
         MVI   OFFSW,C'Y'                                                       
*                                                                               
VALOFF   LA    R2,BITUNFH                                                       
         CLI   5(R2),0                                                          
         BE    VALO02                                                           
         OC    BITUNF,SPACES                                                    
         GOTO1 AVALOFFC,DMCB,(X'80',BITUNF)                                     
         CLI   ERRNUM,OK                                                        
         BNE   ERROR                                                            
         MVC   FINOFC,BITUNF                                                    
         MVC   ANLOFC,FINOFC                                                    
*                                                                               
VALO02   OI    BITUNFH+6,X'80'                                                  
         OI    BITUNAH+6,X'80'                                                  
*                                                                               
VALCLI   LA    R2,BITCLIH                                                       
         BAS   RE,ANY                                                           
         MVC   FVMSGNO,=AL2(AE$FLDTL)  CLIENT TOO LONG?                         
         CLC   5(1,R2),CLILNGTH                                                 
         BH    ERROR                                                            
         LA    R4,COMPEL                                                        
         USING ACCOMPD,R4                                                       
         MVC   KEY+1(2),ACMPJOB    U/L FOR CLI/PROD/JOBS                        
         TM    BITCLIH+4,X'20'                                                  
         BO    VALC02                                                           
         XC    PRODPROF,PRODPROF                                                
         NI    BITPROH+4,X'DF'                                                  
         NI    BITJOBH+4,X'DF'                                                  
         MVC   BITCLIN,SPACES                                                   
         OI    BITCLINH+6,X'80'                                                 
         MVC   BITPRON,SPACES                                                   
         OI    BITPRONH+6,X'80'                                                 
         MVC   BITJOBN,SPACES                                                   
         OI    BITJOBNH+6,X'80'                                                 
*                                                                               
VALC02   IC    R3,BITCLIH+5                                                     
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),BITCLI                                                  
         LA    R5,KEY+3                                                         
         IC    R3,CLILNGTH         LEVA LENGTH                                  
         AR    R5,R3               READY FOR PRODUCT                            
         TM    BITCLIH+4,X'20'                                                  
         BO    VALPRO                                                           
         OI    BITCLIH+4,X'20'                                                  
         XC    CLIPROF,CLIPROF                                                  
         XC    PRODPROF,PRODPROF                                                
         XC    JOBPROF,JOBPROF                                                  
         LA    R6,CLIPROF                                                       
         BAS   RE,GETACC                                                        
         MVC   FVMSGNO,=AL2(AE$INACP)                                           
         TM    ACCTSTAT,X'10'      LOCKED CLIENT                                
         BO    BADACC                                                           
         MVC   BITCLIN,ACCTNAME                                                 
         OI    BITCLINH+6,X'80'                                                 
*                                                                               
VALPRO   LA    R2,BITPROH                                                       
         BAS   RE,ANY                                                           
         TM    BITPROH+4,X'20'                                                  
         BO    VALP02                                                           
         NI    BITJOBH+4,X'DF'                                                  
         XC    PRODPROF,PRODPROF                                                
         XC    JOBPROF,JOBPROF                                                  
         MVC   BITPRON,SPACES                                                   
         OI    BITPRONH+6,X'80'                                                 
         MVC   BITJOBN,SPACES                                                   
         OI    BITJOBNH+6,X'80'                                                 
*                                                                               
VALP02   IC    R3,BITPROH+5                                                     
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),BITPRO                                                   
         LA    R5,KEY+3                                                         
         IC    R3,PRDLNGTH         LEVB LENGTH                                  
         AR    R5,R3               READY FOR JOB                                
         TM    BITPROH+4,X'20'                                                  
         BO    VALJOB                                                           
         OI    BITPROH+4,X'20'                                                  
         LA    R6,PRODPROF                                                      
         BAS   RE,GETACC                                                        
         MVC   BITPRON,ACCTNAME                                                 
         OI    BITPRONH+6,X'80'                                                 
*                                                                               
VALJOB   LA    R2,BITJOBH                                                       
         BAS   RE,ANY                                                           
         TM    BITJOBH+4,X'20'                                                  
         BO    VALJ02                                                           
         MVC   BITJOBN,SPACES                                                   
         OI    BITJOBNH+6,X'80'                                                 
         XC    JOBPROF,JOBPROF                                                  
*                                                                               
VALJ02   IC    R3,BITJOBH+5                                                     
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),BITJOB                                                   
         SR    R6,R6                                                            
         LA    R6,JOBPROF                                                       
         BAS   RE,GETACC                                                        
         MVC   FVMSGNO,=AL2(AE$INACP)                                           
         TM    ACCTSTAT,X'80'                                                   
         BZ    BADACC                                                           
         MVC   FVMSGNO,=AL2(AE$ACTCL) ACCOUNT IS CLOSED                         
         TM    ACCTSTAT,X'20'                                                   
         BO    BADACC                                                           
         MVC   FVMSGNO,=AL2(AE$ACTLK) ACCOUNT IS LOCKED                         
         TM    ACCTSTAT,X'10'                                                   
         BO    BADACC                                                           
         MVC   BITJOBN,ACCTNAME                                                 
         TM    BITJOBH+4,X'20'                                                  
         BO    VALJ04                                                           
         OI    BITJOBNH+6,X'80'                                                 
         OI    BITJOBH+4,X'20'                                                  
*                                                                               
VALJ04   MVC   JOBNUM,KEY                                                       
         MVC   JOBNAME,BITJOBN     NAME FROM SCREEN                             
*                                                                               
GETPRF   BAS   RE,PROFMERG                                                      
         LA    R5,PROFILE                                                       
         USING ACPROFD,R5                                                       
         MVC   CSJOBNUM,ACPRCOST   COSTING SUSPENSE/CLIENT ACCOUNT              
         CLC   ANLOFC,SPACES                                                    
         BH    *+10                                                             
         MVC   ANLOFC,ACPROFFC                                                  
         CLC   FINOFC,SPACES                                                    
         BH    *+10                                                             
         MVC   FINOFC,ACPROFFC                                                  
*                                                                               
GETOFF   XC    ANOELM,ANOELM       CLEAR ANALYSIS OFFICE ELEM                   
         LA    R2,BITUNAH          WAS ANALYSIS OFFICE INPUT                    
         CLI   5(R2),0             IF NOT DEFAULT TO FINANCIAL                  
         BE    GETOFF2             OFFICE IF IT WAS INPUT                       
         OC    BITUNA,SPACES       ELSE VALIDATE ANALYSIS INPUT                 
         GOTO1 AVALOFFC,DMCB,(X'80',BITUNA)                                     
         CLI   ERRNUM,OK                                                        
         BNE   ERROR                                                            
         MVC   ANLOFC,BITUNA                                                    
*                                                                               
GETOFF2  CLC   ANLOFC,FINOFC       IF THE SAME DO NOT NEED TO BUILD             
         BE    VALCOST             ANALYSIS OFFICE ELEMENT                      
         LA    R1,ANOELM           BUILD ANALYSED OFFICE ELEMENT                
         USING ANOELD,R1                                                        
         MVI   ANOEL,ANOELQ                                                     
         MVI   ANOLN,ANOLNQ                                                     
         MVI   ANOTYPE,ANOTPER     PERSON OFFICE                                
         MVC   ANOOFFC,ANLOFC                                                   
*                                                                               
VALCOST  MVC   KEY(15),CSJOBNUM                                                 
         TM    COMPSTAT,X'10'      ON COSTING                                   
         BZ    VALEXP                                                           
         BAS   RE,GETACC                                                        
         MVC   FVMSGNO,=AL2(AE$INACP)                                           
         TM    ACCTSTAT,X'80'                                                   
         BZ    BADACC                                                           
         TM    ACCTSTAT,X'10'      LOCKED                                       
         BO    BADACC                                                           
         MVC   CSJOBNAM,ACCTNAME                                                
         DROP  R5                                                               
*                                                                               
VALEXP   MVC   DUSTNUM,SPACES      WRITE-OFF ACCOUNT (OPTIONAL)                 
         MVC   KEY+1(14),SPACES                                                 
         MVC   KEY+1(2),=C'SE'     HARD FOR EXPENSES                            
         SR    R6,R6               NO MORE PROFILES                             
         LA    R2,BITDUSH                                                       
         CLI   5(R2),0                                                          
         BNE   VALE02                                                           
         TM    COMPSTA3,X'40'                                                   
         BZ    VALE04                                                           
         BAS   RE,ANY                                                           
*                                                                               
VALE02   IC    R3,BITDUSH+5                                                     
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),BITDUS                                                  
         BAS   RE,GETACC                                                        
         MVC   FVMSGNO,=AL2(AE$INACP)  INVALID ACCOUNT FOR POSTING              
         TM    ACCTSTAT,X'80'      POSTABLE                                     
         BZ    BADACC                                                           
         TM    ACCTSTAT,X'10'      LOCKED                                       
         BO    BADACC                                                           
         MVC   DUSTNUM,ACCTNUM                                                  
         MVC   DUSTNAME,ACCTNAME                                                
         TM    BITDUSH+4,X'20'                                                  
         BO    VALE04                                                           
         OI    BITDUSH+4,X'20'                                                  
         MVC   BITDUSN,ACCTNAME                                                 
         OI    BITDUSNH+6,X'80'                                                 
*                                                                               
VALE04   LA    R2,BITEXPH          EXPENSE ACCOUNT                              
         BAS   RE,ANY                                                           
         MVC   KEY+3(12),SPACES                                                 
         IC    R3,BITEXPH+5                                                     
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),BITEXP                                                  
         MVC   FVMSGNO,=AL2(AE$INLDG)  INVALID LEDGER                           
         CLI   KEY+3,C'*'                                                       
         BNE   VALE06                                                           
         MVC   WORK(14),KEY+4                                                   
         MVC   KEY+1(L'KEY-1),SPACES                                            
         MVC   KEY+1(14),WORK                                                   
         CLC   KEY+1(2),=C'SB'                                                  
         BE    VALE06                                                           
         CLC   KEY+1(2),=C'SL'                                                  
         BNE   ERROR                                                            
*                                                                               
VALE06   BAS   RE,GETACC                                                        
         TM    ACCTSTAT,X'80'                                                   
         BZ    BADACC                                                           
         TM    ACCTSTAT,X'10'      LOCKED                                       
         BO    BADACC                                                           
         MVC   NOMEXNUM,ACCTNUM                                                 
         MVC   NOMEXNAM,ACCTNAME                                                
         MVC   ANALBYTE,ACCOST                                                  
         MVI   DEPSW,C'N'                                                       
         TM    ACCTSTAT,X'08'      EXPENSE ACCT MAY HAVE DEPT FLAG              
         BZ    *+8                                                              
         MVI   DEPSW,C'Y'                                                       
         MVI   COSTSW,C'N'                                                      
         TM    COMPSTAT,X'10'                                                   
         BZ    GET13                                                            
         CLI   ACCOST,C' '                                                      
         BE    GET13                                                            
         MVI   COSTSW,C'Y'                                                      
*                                                                               
GET13    LA    RF,BCCPYEL                                                       
         TM    BCCPYST5-BCCPYEL(RF),CPYSNCST                                    
         BNO   GETNME                                                           
         MVI   COSTSW,C'N'                                                      
         OI    COSTNSW,COSTNEW                                                  
         LA    R1,CATBLK                                                        
         USING CATD,R1                                                          
         MVC   CATDMGR,DATAMGR     BUILD CONTROL BLOCK                          
         MVC   CATSEAC,NOMEXNUM    DEBIT ACCOUNT                                
         MVC   CATOFF,ANLOFC       OFFICE                                       
         MVC   CATDPT,BITDEP       DEPARTMENT                                   
         OC    CATDPT,SPACES                                                    
         GOTO1 VCATCALL,CATD                                                    
         LA    R2,BITEXPH                                                       
         MVC   KEY,SPACES                                                       
         MVC   KEY(15),CATACC3                                                  
         CLI   CATERR,0                                                         
         BE    GET1302                                                          
         MVC   FVMSGNO,=AL2(AE$IANAL)                                           
         B     ERROR                                                            
*                                                                               
GET1302  CLI   CATPST,C'N'         NO COST POSTING                              
         BE    GETNME                                                           
         MVC   COSTANAL,CATCDE                                                  
         MVC   CN13NUM,CATACC3     SAVE 13 ACCOUNT                              
         MVI   COSTSW,C'Y'                                                      
*                                                                               
GETNME   MVC   KEY(1),COMPANY      RESTORE COMPANY CODE                         
         MVI   STFSW,C'N'                                                       
         TM    ACCTSTAT,X'40'                                                   
         BZ    *+8                                                              
         MVI   STFSW,C'Y'                                                       
         TM    BITEXPH+4,X'20'                                                  
         BO    VALANL                                                           
         OI    BITEXPH+4,X'20'                                                  
         MVC   BITEXPN,ACCTNAME                                                 
         OI    BITEXPNH+6,X'80'                                                 
         OI    BITEXPH+4,X'20'                                                  
*                                                                               
VALANL   CLI   DEPSW,C'N'                                                       
         BE    VALA02                                                           
         MVC   KEY(15),NOMEXNUM                                                 
         MVC   KEY+1(2),=C'28'     ANALYSIS,DEPT SUSPENSE                       
         BAS   RE,GETACC                                                        
         TM    ACCTSTAT,X'80'                                                   
         BZ    BADACC                                                           
         TM    ACCTSTAT,X'10'      LOCKED                                       
         BO    BADACC                                                           
         MVC   DSEXPNUM,ACCTNUM                                                 
         MVC   DSEXPNAM,ACCTNAME                                                
*                                                                               
VALA02   LA    R2,BITDEPH                                                       
         CLI   DEPSW,C'N'                                                       
         BE    VALA04                                                           
         BAS   RE,ANY                                                           
         MVC   FVMSGNO,=AL2(AE$IANAL)                                           
         CLC   BITDEPH+5(1),DPTLNGTH                                            
         BH    ERROR                                                            
*                                                                               
VALA04   CLI   COSTSW,C'N'                                                      
         BE    VALA10                                                           
         MVC   KEY+3(12),SPACES                                                 
         MVC   KEY+5(1),ANALBYTE                                                
         LA    R1,KEY+3                                                         
         CLI   OFFSW,C'N'                                                       
         BE    VALA06                                                           
*                                                                               
         CLI   OFCLNGTH,0                                                       
         BE    VALA06                                                           
         ZIC   RF,OFCLNGTH               VARIABLE OFFICE LENGTH                 
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),ANLOFC           MOVE IN OFFICE                         
         LA    R1,1(RF,R1)               BUMP PAST OFFICE IN KEY                
*                                                                               
VALA06   SR    R3,R3                                                            
         IC    R3,DPTLNGTH               VARIABLE DEPARTMENT LENGTH             
         LA    RF,=C'9999'                                                      
         CLI   DEPSW,C'Y'                                                       
         BNE   VALA08                                                           
         LA    RF,BITDEP                                                        
         IC    R3,BITDEPH+5                                                     
*                                                                               
VALA08   BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                      MOVE IN DEPARTMENT                     
         MVC   0(0,R1),0(RF)                                                    
         LA    R1,1(R3,R1)                                                      
         MVC   0(1,R1),ANALBYTE                                                 
         MVC   KEY+1(2),=C'1P'                                                  
*                                                                               
         LA    RF,BCCPYEL                                                       
         TM    BCCPYST5-BCCPYEL(RF),CPYSNCST TEST NEW COSTING                   
         BNO   *+10                                                             
         MVC   KEY+3(12),=C'999999999999' SAYS VANESSA                          
*                                                                               
         BAS   RE,GETACC                                                        
         TM    ACCTSTAT,X'80'                                                   
         BZ    BADACC                                                           
         TM    ACCTSTAT,X'10'      LOCKED                                       
         BO    BADACC                                                           
         MVC   CNEXPNUM,ACCTNUM                                                 
         MVC   CNEXPNAM,ACCTNAME                                                
*                                                                               
         TM    COSTNSW,COSTNEW                                                  
         BO    VALA10                                                           
*                                                                               
         MVC   KEY+2(13),SPACES                                                 
         MVI   KEY+2,C'3'                                                       
         MVC   KEY+3(1),ANALBYTE                                                
         BAS   RE,GETACC                                                        
         MVC   CN13NUM,ACCTNUM                                                  
         MVC   CN13NAME,ACCTNAME                                                
*                                                                               
VALA10   MVC   KEY+1(14),SPACES                                                 
         MVC   KEY+1(2),=C'2D'                                                  
         LA    R1,KEY+3                                                         
         CLI   OFFSW,C'N'                                                       
         BE    VALA12                                                           
         CLI   OFCLNGTH,0                                                       
         BE    VALA12                                                           
         ZIC   RF,OFCLNGTH               VARIABLE OFFICE LENGTH                 
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),ANLOFC           MOVE IN OFFICE                         
         LA    R1,1(RF,R1)               BUMP PAST OFFICE IN KEY                
*                                                                               
VALA12   SR    RF,RF                                                            
         IC    RF,BITDEPH+5              VARIABLE DEPARTMENT LENGTH             
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                      MOVE IN DEPARTMENT                     
         MVC   0(0,R1),BITDEP                                                   
*                                                                               
         MVC   DEPTNUM,KEY                                                      
         CLI   DEPSW,C'N'          WHEN BUILDING 2D WE EXPECT DEPTNUM           
         BE    VALWRK              TO HAVE PROPER STRUCTURE                     
         BAS   RE,GETACC           EVEN IF IT DOESN'T EXIST                     
         TM    ACCTSTAT,X'80'                                                   
         BZ    BADACC                                                           
         TM    ACCTSTAT,X'10'                                                   
         BO    BADACC                                                           
         MVC   DEPTNUM,ACCTNUM                                                  
         MVC   DEPTNAME,ACCTNAME                                                
         TM    BITDEPH+4,X'20'                                                  
         BO    VALWRK                                                           
         OI    BITDEPH+4,X'20'                                                  
         MVC   BITDEPN,ACCTNAME                                                 
         OI    BITDEPNH+6,X'80'                                                 
*                                                                               
VALWRK   LA    R2,BITWRKH                                                       
         MVC   FVMSGNO,=AL2(AE$INWRK)                                           
         BAS   RE,ANY                                                           
         MVC   KEY,SPACES                                                       
         MVI   KEY,X'0A'           VALIDATE WORK CODE                           
         MVC   KEY+1(1),COMPANY                                                 
         MVC   KEY+2(2),ACMPJOB                                                 
         ZIC   RF,5(R2)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   KEY+4(0),BITWRK                                                  
         BAS   RE,HIGH                                                          
         CLC   KEY(15),KEYSAVE                                                  
         BNE   ERROR                                                            
         LA    RE,IOAREA                                                        
         MVC   BITWRK,KEY+4                                                     
                                                                                
VALW02   CLI   0(RE),0                                                          
         BE    JOBSET                                                           
         CLI   0(RE),X'12'                                                      
         BE    VALW04                                                           
         ZIC   RF,1(RE)                                                         
         AR    RE,RF                                                            
         B     VALW02                                                           
*                                                                               
         USING ACANALD,RE                                                       
VALW04   MVC   BITWRKN,ACANDESC                                                 
         OI    BITWRKNH+6,X'80'                                                 
*                                                                               
JOBSET   LA    RE,JOBNUM                                                        
         GOTO1 ASETJOB,DMCB,(X'80',(RE)),8(R2)                                  
         TM    ACOPSTAT,ACOXJOB    IS THIS AN X-JOB?                            
         BZ    CHKWRK              NO                                           
         LA    R2,BITJOBH          YES, STOP IT                                 
         MVC   FVMSGNO,=AL2(AE$BXJOB)                                           
         B     ERROR                                                            
*                                                                               
CHKWRK   L     R1,AGOBLOCK                                                      
         CLI   INPUT,14                                                         
         BE    CHKOPT                                                           
         MP    DUB,=P'-1'           MAKE IT POSITIVE                            
*                                                                               
         USING GOBLOCKD,R1                                                      
         MVC   FVMSGNO,=AL2(AE$INWRK)                                           
         LA    R1,GOUWLIST                                                      
         BAS   RE,CHKW02                                                        
         B     CHKOPT                                                           
*                                                                               
CHKW02   LA    R0,6                                                             
CHKW04   CLC   8(2,R2),0(R1)                                                    
         BE    ERROR                                                            
         LA    R1,2(R1)                                                         
         BCT   R0,CHKW04                                                        
         BR    RE                                                               
*                                                                               
CHKOPT   CLI   INPUT,15            ARE WE DOING TYPE 15 ?                       
         BE    CHKO02              YES, OK DO IT ALL                            
         CLI   JOBFORM,C'N'        NO, TYPE 14, IS THIS NEW FORMAT ?            
         BNE   VAL2P               NO, SKIP IT ALL                              
         CLI   GOBATCH,C'A'        YES, BUT MUST BE OPTION A                    
         BNE   VAL2P                                                            
         DROP  R1                                                               
*                                                                               
CHKO02   GOTO1 AOPTVAL             CHECK OPTION, DO LOOKUP                      
         BNE   ERROR                                                            
         CLI   CHECK,C' '          CHECKING AMOUNT ?                            
         BE    VAL2P               NO                                           
         MVC   FVMSGNO,=AL2(AE$AEEWC) CHECK FOR DOLLARS                         
         ZAP   WRKCAMT,DUB                                                      
         MVC   WRKC,8(R2)                                                       
         LA    R8,WRKC                                                          
         GOTO1 AWRKVAL,DMCB,(R8)  VALIDATE WORKCODE AMOUNT                      
         BH    ERROR                                                            
*                                                                               
VAL2P    MVC   KEY(1),COMPANY                                                   
         LA    R2,BITSTFH                                                       
         CLI   STFSW,C'N'                                                       
         BE    VALCON                                                           
         BAS   RE,ANY                                                           
         MVC   KEY+3(12),SPACES                                                 
         MVC   KEY+1(2),=C'2P'                                                  
         IC    R3,BITSTFH+5                                                     
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),BITSTF                                                  
         MVC   PERSNUM,KEY         SAVE KEY                                     
         MVC   KEY+3(12),SPACES                                                 
         BAS   RE,READ             READ LEDGER                                  
         LA    RF,IOAREA                                                        
         SR    RE,RE                                                            
         SR    R4,R4                                                            
*                                                                               
VAL2P02  CLI   0(RF),0                                                          
         BE    VAL2P12                                                          
         CLI   0(RF),X'16'                                                      
         BE    VAL2P04                                                          
         IC    RE,1(RF)                                                         
         AR    RF,RE                                                            
         B     VAL2P02                                                          
*                                                                               
         USING ACHEIRD,RF                                                       
VAL2P04  CLI   ACHRLEVA,12         1 LEVEL LEDGER                               
         BE    VAL2P12                                                          
         MVC   KEY(3),DEPTNUM                                                   
         MVI   KEY+2,C'P'                                                       
*                                                                               
         LA    R2,BITDEPH                                                       
         BAS   RE,ANY                                                           
         SR    R3,R3                                                            
         IC    R3,BITDEPH+5                                                     
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),BITDEP                                                  
         IC    R4,ACHRLEVA                                                      
         CLI   ACHRLEVB,12         12=2 LEVEL LEDGER                            
         BNE   VAL2P06                                                          
         CLI   OFFSW,C'Y'                                                       
         BE    VAL2P10                                                          
         CLI   ACHRLEVA,2                                                       
         BNE   VAL2P10                                                          
*                                                                               
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),BITDEP      NO OFFICE BUT DEPT IS TWO LONG              
         B     VAL2P10                                                          
*                                                                               
VAL2P06  LA    R1,KEY+3                                                         
         CLI   OFCLNGTH,0                                                       
         BE    VAL2P08                                                          
         ZIC   R3,OFCLNGTH                                                      
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),ANLOFC                                                   
         LA    R1,1(R3,R1)                                                      
*                                                                               
VAL2P08  SR    R3,R3                                                            
         IC    R3,BITDEPH+5                                                     
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),BITDEP                                                   
         IC    R4,ACHRLEVB                                                      
*                                                                               
VAL2P10  LA    RF,KEY+3                                                         
         AR    RF,R4                                                            
         IC    R4,BITSTFH+5                                                     
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     VAL2P14                                                          
         MVC   0(0,RF),BITSTF                                                   
*                                                                               
VAL2P12  MVC   KEY(15),PERSNUM     RESET                                        
*                                                                               
VAL2P14  BAS   RE,GETACC                                                        
         MVC   FVMSGNO,=AL2(AE$INACP)                                           
         TM    ACCTSTAT,X'80'                                                   
         BZ    BADACC                                                           
         TM    ACCTSTAT,X'10'                                                   
         BO    BADACC                                                           
         MVC   PERSNUM,ACCTNUM                                                  
         MVC   PERSNAME,ACCTNAME                                                
         TM    BITSTFH+4,X'20'                                                  
         BO    VAL29                                                            
         OI    BITSTFH+4,X'20'                                                  
         MVC   BITSTFN,ACCTNAME                                                 
         OI    BITSTFNH+6,X'80'                                                 
*                                                                               
VAL29    CLI   STFSW,C'Y'          STAFF ANALYSIS?                              
         BNE   VALCON                                                           
         MVC   KEY+1(2),=C'29'     NOW CLIENTS IN 29                            
         MVC   KEY+3(12),CSJOBNUM+3                                             
         LA    R2,BITCLIH                                                       
         BAS   RE,GETACC                                                        
         TM    ACCTSTAT,X'80'                                                   
         BZ    BADACC                                                           
         TM    ACCTSTAT,X'10'                                                   
         BO    BADACC                                                           
         MVC   PSCLINUM,ACCTNUM                                                 
         MVC   PSCLINAM,ACCTNAME                                                
*                                                                               
VALCON   MVC   JOBCAC,SPACES                                                    
         LA    R2,BITJCAH                                                       
         CLI   5(R2),0                                                          
         BE    BLDPOST                                                          
         CLI   5(R2),2             HAS TO BE LONGER THAN 2 CHARS                
         BH    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INACC)                                           
         B     ERROR                                                            
*                                                                               
         MVC   KEY+1(14),SPACES                                                 
         ZIC   RF,5(R2)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   KEY+1(0),BITJCA                                                  
*                                                                               
         MVC   FVMSGNO,=AL2(AE$INLDG)  INVALID LEDGER                           
         CLC   KEY+1(2),=C'SK'                                                  
         BE    ERROR                                                            
*                                                                               
         BAS   RE,GETACC                                                        
         MVC   JOBCAC,ACCTNUM                                                   
         MVC   JOBCACN,ACCTNAME                                                 
         TM    BITJCAH+4,X'20'                                                  
         BO    BLDPOST                                                          
         OI    BITJCAH+4,X'20'                                                  
         MVC   BITJCAN,ACCTNAME                                                 
         OI    BITJCANH+6,X'80'                                                 
         EJECT                                                                  
*              BUILD 64 ELEMENT                                                 
*                                                                               
BLDPOST  XC    FVMSGNO,FVMSGNO                                                  
         MVI   ERRNUM,OK                                                        
         LA    R8,IOAREA+2                                                      
         USING DLDESCD,R8                                                       
         MVI   DLDSEL,X'64'                                                     
         MVC   DLDSREF,SPACES                                                   
         IC    R3,BITDOCH+5                                                     
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   DLDSREF(0),BITDOC                                                
         MVC   DLDSDATE,DATESAVE                                                
         MVI   DLDSSBRF,0                                                       
         MVI   DLDSSTAT,0                                                       
         LA    R2,BITNARH                                                       
         LA    R3,DLDSNARR                                                      
         XC    DLDSNARR,DLDSNARR                                                
         BAS   RE,NARRSCAN                                                      
         SR    R3,R3                                                            
*                                                                               
         LA    R4,DLDSNARR                                                      
         SR    R4,R8                                                            
         AR    R4,R6               L'NARRATIVE                                  
         STH   R4,FULL                                                          
         MVC   DLDSLEN,FULL+1      L'1ST ELEMENT                                
         EJECT                                                                  
*              BUILD ACCOUNTING ENTRIES                                         
         USING DLPOSTD,R8          DEBIT WRITE-OFF ACCOUNT                      
         AR    R8,R4                                                            
         BAS   RE,ADDANL                                                        
         MVI   DLPSEL,X'69'                                                     
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSDBAC,DUSTNUM                                                 
         MVC   DLPSDBNM,DUSTNAME                                                
         MVC   DLPSCRAC,JOBNUM                                                  
         MVC   DLPSCRNM,JOBNAME                                                 
         CLC   DUSTNUM,SPACES                                                   
         BNE   BLDP02                                                           
         MVC   DLPSDBAC,NOMEXNUM   REPLACE NON-EXISTANT WRITE-OFF               
         MVC   DLPSDBNM,NOMEXNAM   WITH EXPENSE ACCOUNT                         
*                                                                               
BLDP02   MVI   DLPSTYPE,0                                                       
         LA    R2,BITWRKH                                                       
         MVC   FVMSGNO,=AL2(AE$INWRK)                                           
         CLC   BITWRK,=C'99'                                                    
         BE    ERROR                                                            
         MVC   DLPSANAL,FINOFC                                                  
         ZAP   DLPSAMNT,SAVECASH                                                
*                                                                               
         IC    R3,DLPSLEN                                                       
         AR    R8,R3                                                            
         BAS   RE,BLD4C                                                         
         BAS   RE,ADDANL                                                        
         USING DLPOSTD,R8                                                       
         MVI   DLPSEL,X'69'                                                     
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSANAL,BITWRK                                                  
         OC    DLPSANAL,SPACES                                                  
         ZAP   DLPSAMNT,SAVECASH                                                
         MVI   DLPSTYPE,0                                                       
         BAS   RE,REVAMNT                                                       
         ZAP   TEMPDAMT,DLPSAMNT                                                
         MVC   DLPSDBAC,JOBNUM                                                  
         MVC   DLPSDBNM,JOBNAME                                                 
         MVC   DLPSCRAC,DUSTNUM                                                 
         MVC   DLPSCRNM,DUSTNAME                                                
         CLI   BITCOMM,C'Y'        Y=NON-COMMISSIONABLE                         
         BNE   *+8                                                              
         MVI   DLPSTYPE,X'40'                                                   
         CLC   JOBCAC,SPACES                                                    
         BE    BLDP04                                                           
         MVC   DLPSCRAC,JOBCAC                                                  
         MVC   DLPSCRNM,JOBCACN                                                 
         CLC   DUSTNUM,SPACES                                                   
         BNE   BLDP06                                                           
         B     BLDP08                                                           
*                                                                               
BLDP04   CLC   DUSTNUM,SPACES                                                   
         BNE   BLDP06                                                           
         IC    R3,DLPSLEN                                                       
         BCTR  R3,0                                                             
         MVC   DLPSCRAC,NOMEXNUM                                                
         MVC   DLPSCRNM,NOMEXNAM                                                
         B     BLDP08                                                           
*                                                                               
BLDP06   LR    R4,R8                                                            
         IC    R3,DLPSLEN                                                       
         AR    R8,R3                                                            
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R8),0(R4)       3RD ELEMENT                                  
         BAS   RE,ADDANL                                                        
         MVI   DLPSTYPE,0                                                       
         MVI   DLPSEL,X'68'                                                     
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSDBAC,NOMEXNUM                                                
         MVC   DLPSDBNM,NOMEXNAM                                                
         MVC   DLPSCRAC,DUSTNUM                                                 
         MVC   DLPSCRNM,DUSTNAME                                                
         ZAP   DLPSAMNT,TEMPDAMT                                                
         BAS   RE,REVAMNT          BACK TO POSITIVE AMOUNT                      
         ZAP   TEMPDAMT,DLPSAMNT                                                
         MVC   DLPSANAL,FINOFC                                                  
*                                                                               
BLDP08   CLI   DEPSW,C'N'          DEPARTMENTAL                                 
         BE    BLDP10                                                           
         LR    R4,R8                                                            
         IC    R3,DLPSLEN                                                       
         AR    R8,R3                                                            
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R8),0(R4)                                                    
         BAS   RE,ADDANL                                                        
         MVI   DLPSEL,X'68'                                                     
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSDBAC,DEPTNUM                                                 
         MVC   DLPSDBNM,DEPTNAME                                                
         MVC   DLPSANAL,FINOFC                                                  
         MVC   DLPSCRAC,DSEXPNUM                                                
         MVC   DLPSCRNM,DSEXPNAM                                                
         MVI   DLPSTYPE,0                                                       
         OI    DLPSTYPE,X'80'      SUBSIDIARY                                   
         ZAP   DLPSAMNT,TEMPDAMT                                                
         CLC   DUSTNUM,SPACES                                                   
         BNE   *+8                                                              
         BAS   RE,REVAMNT                                                       
         ZAP   TEMPDAMT,DLPSAMNT                                                
*                                                                               
BLDP10   CLI   COSTSW,C'N'         COSTING POSTINGS                             
         BE    BLDP12                                                           
         LR    R4,R8                                                            
         IC    R3,DLPSLEN                                                       
         AR    R8,R3                                                            
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R8),0(R4)                                                    
         BAS   RE,ADDANL                                                        
         MVI   DLPSEL,X'69'        DEBIT DEPT/PERS                              
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSANAL,FINOFC                                                  
         MVI   DLPSTYPE,0                                                       
         OI    DLPSTYPE,X'80'      SUBSIDIARY                                   
         MVC   DLPSDBAC,CNEXPNUM                                                
         MVC   DLPSDBNM,CNEXPNAM                                                
         MVC   DLPSCRAC,CSJOBNUM                                                
         MVC   DLPSCRNM,CSJOBNAM                                                
         ZAP   DLPSAMNT,SAVECASH                                                
         LR    R4,R8                                                            
         IC    R3,DLPSLEN                                                       
         AR    R8,R3                                                            
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R8),0(R4)                                                    
*                                                                               
         MVC   TEMPSAV(DLPSLNQ),0(R4)                                           
         BAS   RE,ADDANL                                                        
         MVC   0(DLPSLNQ,R8),TEMPSAV                                            
*                                                                               
         MVI   DLPSEL,X'6A'        CREDIT CLIENT                                
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSDBAC,CN13NUM                                                 
         MVC   DLPSDBNM,CN13NAME                                                
*                                                                               
BLDP12   CLI   STFSW,C'N'          STAFF POSTINGS                               
         BE    BLDP16                                                           
         LR    R4,R8                                                            
         IC    R3,DLPSLEN                                                       
         AR    R8,R3                                                            
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R8),0(R4)                                                    
         BAS   RE,ADDANL                                                        
         MVI   DLPSEL,X'6A'        CREDIT CLIENT IN 29                          
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSANAL,FINOFC                                                  
         MVI   DLPSTYPE,0                                                       
         OI    DLPSTYPE,X'80'      SUBSIDIARY                                   
         MVC   DLPSDBNM,PERSNAME                                                
         MVC   DLPSCRAC,PSCLINUM                                                
         MVC   DLPSCRNM,PSCLINAM                                                
         ZAP   DLPSAMNT,SAVECASH                                                
         ZAP   TEMPDAMT,SAVECASH                                                
         MVI   DLPSDBAC,C'*'       CONTRA IS *EXPENSE-STAFF                     
         SR    R1,R1                                                            
         IC    R1,BITEXPH+5                                                     
         BCTR  R1,R0                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DLPSDBAC+1(0),BITEXP                                             
         CLI   TENO,X'F0'          DO WE WANT TO TAKE 'N' BYTES                 
         BL    BLDP14              FROM R.H.END OF T/E ACCOUNT                  
         PACK  DUB,TENO                                                         
         CVB   R5,DUB                                                           
         MVC   DLPSDBAC+1(14),SPACES                                            
         LA    R2,BITEXPH                                                       
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         LA    RF,1(R1)            INPUT MUST BE AT LEAST 'N' LONG              
         SR    RF,R5                                                            
         BM    ERROR                                                            
         LA    RE,BITEXP(RF)                                                    
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   DLPSDBAC+1(0),0(RE)                                              
         LR    R1,R5                                                            
*                                                                               
BLDP14   STC   R1,BYTE                                                          
         LA    RF,DLPSDBAC+2(R1)                                                
         MVI   0(RF),C'-'                                                       
         LA    R1,DLPSDBAC+14                                                   
         SR    R1,RF                                                            
         BCTR  R1,R0                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   1(0,RF),PERSNUM+3                                                
*                                                                               
         LR    R4,R8                                                            
         IC    R3,DLPSLEN                                                       
         AR    R8,R3                                                            
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R8),0(R4)                                                    
*                                                                               
         MVC   TEMPSAV(DLPSLNQ),0(R4)                                           
         BAS   RE,ADDANL                                                        
         MVC   0(DLPSLNQ,R8),TEMPSAV                                            
*                                                                               
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSCRAC,PERSNUM    MINUS CREDIT STAFF                           
         MVC   DLPSCRNM,PERSNAME                                                
         ZAP   DLPSAMNT,TEMPDAMT                                                
         BAS   RE,REVAMNT                                                       
         ZAP   TEMPDAMT,DLPSAMNT                                                
         MVC   DLPSDBNM,PSCLINAM   CONTRA IS *EXPENSE-CLIENT                    
         IC    R1,BYTE                                                          
         LA    R1,1(R1)                                                         
         LA    RF,DLPSDBAC+1(R1)                                                
         LA    R1,DLPSDBAC+14                                                   
         SR    R1,RF                                                            
         BCTR  R1,R0                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   1(0,RF),PSCLINUM+3                                               
*                                                                               
BLDP16   IC    R3,DLPSLEN          DLPSLEN AGAIN                                
         AR    R8,R3                                                            
         MVI   0(R8),0             END OF RECORD                                
         LA    RF,IOAREA-1                                                      
         SR    R8,RF                                                            
         STH   R8,HALF                                                          
         MVC   IOAREA(2),HALF                                                   
         BAS   RE,PUTDAY                                                        
         SPACE 3                                                                
         XC    WORK,WORK                                                        
         IC    R3,BITDOCH+5                                                     
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),BITDOC      REF                                          
         L     R3,DMCB+8                                                        
         MVC   WORK+10(4),0(R3)    DISK ADDRESS                                 
         BAS   RE,ADTWA1                                                        
         LA    R2,BITDOCH                                                       
         XC    FVMSGNO,FVMSGNO                                                  
         MVI   ERRNUM,X'FF'                                                     
         B     EXIT                                                             
*                                                                               
REVAMNT  ZAP   DUB,DLPSAMNT                                                     
         MP    DUB,=P'-1'                                                       
         ZAP   DLPSAMNT,DUB                                                     
         BR    RE                                                               
         EJECT                                                                  
*------------------------------------------------------------------*            
*              BUILD DESCRIPTIVE ELEMENT ON EXPENSE POSTING                     
*------------------------------------------------------------------*            
BLD4C    NTR1                                                                   
         USING TRSDESCD,R8                                                      
         MVI   TRSDEL,X'4C'                                                     
         MVI   TRSDLEN,17                                                       
         MVC   TRSDACCS(15),NOMEXNUM                                            
         CLI   BITDEPH+5,3         USING 3 BYTE DEPT                            
         BNE   *+10                                                             
         MVC   TRSDACCS(15),DEPTNUM FOR O&M USE DEPT NUMBER                     
         ZIC   R5,TRSDLEN                                                       
         AR    R8,R5                                                            
         XIT1  REGS=(R8)                                                        
         EJECT                                                                  
*------------------------------------------------------------------*            
*        ADD ANALYZED OFFICE ELEMENT TO IO AREA                                 
*------------------------------------------------------------------*            
ADDANL   CLI   ANOELM,0            TEST OFFICE ELEMENT                          
         BER   RE                                                               
         ST    R1,FULL                                                          
         LA    R1,ANOELM                                                        
         USING ANOELD,R1                                                        
         SR    RF,RF                                                            
         IC    RF,ANOLN                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R8),ANOEL                                                    
         LA    R8,1(RF,R8)         R8 TO NEXT AREA                              
         L     R1,FULL                                                          
         BR    RE                                                               
         EJECT                                                                  
       ++INCLUDE ACBATCODE                                                      
         EJECT                                                                  
       ++INCLUDE ACBATDSECT                                                     
         EJECT                                                                  
       ++INCLUDE ACBATF1D                                                       
         EJECT                                                                  
PROGD    DSECT                                                                  
ANALBYTE DS    CL1                                                              
OFFSW    DS    CL1                                                              
COSTSW   DS    CL1                                                              
COSTNSW  DS    CL1                                                              
COSTACC  EQU   X'80'                                                            
COSTNEW  EQU   X'40'                                                            
STFSW    DS    CL1                                                              
DEPSW    DS    CL1                                                              
FINOFC   DS    CL2                                                              
ANLOFC   DS    CL2                                                              
FULLCASH DS    F                                                                
SAVECASH DS    PL6                                                              
TEMPDAMT DS    PL6                                                              
DATESAVE DS    CL3                                                              
BYTE     DS    CL1                                                              
JOBNUM   DS    CL15                  SJ CLI/PRD/JOB FROM SCREEN                 
JOBNAME  DS    CL36                  SJ NAME                                    
CSJOBNUM DS    CL15                  1C COST ACCOUNT FROM PROFILE               
CSJOBNAM DS    CL36                  1C ACCOUNT NAME                            
DUSTNUM  DS    CL15                  SE WRITE OFF ACCT (OPTIONAL)               
DUSTNAME DS    CL36                  SE NAME                                    
NOMEXNUM DS    CL15                  SE EXPENSE ACCOUNT                         
NOMEXNAM DS    CL36                  SE NAME                                    
DSEXPNUM DS    CL15                  28 ACCOUNT                                 
DSEXPNAM DS    CL36                  28 NAME                                    
CNEXPNUM DS    CL15                  1P ACCOUNT                                 
CNEXPNAM DS    CL36                  1P ACCOUNT NAME                            
DEPTNUM  DS    CL15                  2D DEPARTMENT ANALYSIS                     
DEPTNAME DS    CL36                  2D NAME                                    
PERSNUM  DS    CL15                  2P STAFF/CLIENT                            
PERSNAME DS    CL36                  2P NAME                                    
PSCLINUM DS    CL15                  29 CLIENT                                  
PSCLINAM DS    CL36                  29 NAME                                    
JOBCAC   DS    CL15                  ?? JOB CONTRA FROM SCREEN                  
JOBCACN  DS    CL36                  ?? INCLUDES UNIT/LEDG                      
CN13NUM  DS    CL15                  13 ACCOUNT                                 
CN13NAME DS    CL36                  13 NAME                                    
COSTANAL DS    CL5                                                              
ANOELM   DS    XL(ANOLNQ)                                                       
CATBLK   DS    XL(CATLNQ)                                                       
TEMPSAV  DS    CL(DLPSLNQ)                                                      
**   OVERLAY ASSUMES KEY IMMEDIATELY PRECEDES IOAREA                            
KEY      DS    CL49                                                             
IOAREA   DS    2000C                                                            
THISLINE DS    X                                                                
WRKC     DS    CL2                                                              
WRKCAMT  DS    PL6                                                              
PROGDX   DS    0C                                                               
         EJECT                                                                  
*ACGENBOTH                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
*ACGENDAY                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACGENDAY                                                       
         PRINT ON                                                               
*DDFLDIND                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDFLDIND                                                       
         PRINT ON                                                               
*ACCATCALLD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACCATCALLD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012ACBAT0E   03/23/11'                                      
         END                                                                    
