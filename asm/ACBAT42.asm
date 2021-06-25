*          DATA SET ACBAT42    AT LEVEL 006 AS OF 05/31/01                      
*PHASE T61B42A                                                                  
         TITLE 'JOB GROUP FOR TYPE 34'                                          
T61B42   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 LWSX-LWSD,T61B42,R8,CLEAR=YES                                    
         USING LWSD,RC                                                          
*                                                                               
         L     R9,4(R1)                                                         
         USING GWS,R9              R9=GLOBAL W/S                                
         L     RA,ATWA0                                                         
         USING TWAD,RA             RA=TWA                                       
         L     R3,0(R1)            A(JOB GROUP BLOCK)                           
         ST    R3,AJOBGRPB                                                      
         USING JGROUPD,R3                                                       
*                                                                               
MAIN     CLI   JGMODE,JGMINIT                                                   
         BE    INIT10                                                           
         CLI   JGMODE,JGM1PASS                                                  
         BE    INIT10                                                           
         CLI   JGMODE,JGM1SPVL                                                  
         BE    INIT10                                                           
         CLI   JGMODE,JGM1SPCL                                                  
         BE    INIT10                                                           
*                                                                               
MAIN5    BAS   RE,RECALC           RECALCULATE ADDRESS FOR HIT OF ENTER         
*                                                                               
         CLI   PFKEY,15            PF15, NO MORE INPUT - RETURN                 
         BE    EDIT0010              BUT VALIDATE AGAIN                         
         CLI   PFKEY,9             SCROLLING KEYS                               
         BE    LISTUP                                                           
         CLI   PFKEY,10                                                         
         BE    LISTDOWN                                                         
         CLI   PFKEY,0                                                          
         BE    EDIT0010            PF=0 'ENTER' - OK TO EDIT                    
BADPFK   LA    R2,CONACTH                                                       
         MVI   ERRNUM,251          ERROR INVALID PF KEY                         
         B     EXIT                                                             
*                                                                               
PEXIT    B     EXIT                                                             
*                                                                               
LISTUP   SR    R1,R1                                                            
         IC    R1,CURRLINE                                                      
         AHI   R1,-9                                                            
         BZ    *+8                                                              
         BNM   *+8                                                              
         LA    R1,1                RESET TO TOP OF LIST                         
         STC   R1,CURRLINE                                                      
         BAS   RE,SHOWJGRP                                                      
         B     EDIT0010                                                         
*                                                                               
         USING JGBLOCK,R5                                                       
LISTDOWN SR    R1,R1                                                            
         IC    R1,CURRLINE                                                      
         LA    R2,9                9 LINES                                      
         BAS   RE,POSNODE          POSITION NODE                                
         L     R5,CURRJGRP                                                      
LSTDWN03 CLC   JGBFTPTR,=X'FFFF'   DON'T SCROLL PAST EOL                        
         BE    LSTDWN05                                                         
         SR    RF,RF                                                            
         ICM   RF,3,JGBFTPTR                                                    
         A     RF,FRSTNODE                                                      
         LR    R5,RF                                                            
         LA    R1,1(R1)                                                         
         BCT   R2,LSTDWN03                                                      
LSTDWN05 STC   R1,CURRLINE         SAVE NEW LINE NUMBER                         
         BAS   RE,SHOWJGRP                                                      
         B     EDIT0010                                                         
         DROP  R3,R5                                                            
         EJECT                                                                  
*              SAVE THE CURRENT SCREEN TWA0 IN TWA3                             
*                                                                               
         USING JGROUPD,R3                                                       
INIT10   DS    0H                                                               
         GOTO1 ANTRSES,0                                                        
         MVC   PGMODE,JGMODE                                                    
*                                                                               
*              GET JOB DETAIL SCREEN                                            
*                                                                               
         XC    DMCB(20),DMCB                                                    
         MVC   DMCB+4(4),=X'D9061BBE'                                           
         GOTO1 CALLOV,DMCB,(0,CONTABH)                                          
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                    CANT READ SCREEN                         
*                                                                               
         MVC   MAINLINE,JGLINE                                                  
         BAS   RE,RECALC                                                        
         BAS   RE,SHOWXTRA                                                      
         CLC   PGMODE,JGM1SPCL         SKIP LOOKING FOR ERRORS                  
         BE    *+8                                                              
         BAS   RE,PRSLST                                                        
*                                                                               
         LA    RE,CONTABH              TRANSMIT SCREEN                          
         SR    R1,R1                                                            
INIT20   OI    6(RE),X'80'                                                      
         OI    7(RE),X'80'                                                      
         IC    R1,0(RE)                                                         
         AR    RE,R1                                                            
         CLI   0(RE),0                                                          
         BNE   INIT20                                                           
         XC    1(2,RE),1(RE)                                                    
*                                                                               
         LA    R2,JBGWCH          NEXT INPUT                                    
INIT72   MVI   ERRNUM,SPECIAL                                                   
         MVI   MSG,C' '                                                         
         MVC   MSG+1(L'MSG-1),MSG                                               
         MVC   MSG(25),=CL25'INPUT REQUIRED FIELDS'                             
         CLI   PGMODE,JGM1PASS                                                  
         BE    END10                                                            
         CLI   PGMODE,JGM1SPVL                                                  
         BE    END10                                                            
         CLI   PGMODE,JGM1SPCL                                                  
         BE    END10                                                            
         L     R3,AJOBGRPB        SCREEN LOADED- OK TO EDIT(NEXT TIME)          
         MVI   JGMODE,JGMEDIT                                                   
         MVI   MODE,3              INSURE WE COME BACK                          
*                                                                               
         BAS   RE,SHOWJGRP                                                      
         B     EDIT0010                                                         
         DROP  R3                                                               
         EJECT                                                                  
RECALC   NTR1                                                                   
         LR    R5,RA                                                            
         AHI   R5,X'2A00'                                                       
         LA    R5,6(R5)                                                         
         ST    R5,FRSTLIST             FIRST LIST OUT OF 10                     
         LR    RF,R5                                                            
         LA    R5,40(R5)               BUMP PAST 10 POSSIBLE LISTS              
         ST    R5,FRSTNODE                                                      
*                                                                               
         SR    R1,R1                                                            
         IC    R1,MAINLINE             GET ADDRESS OF THE POINTER               
         BCTR  R1,0                    TO CURRENT LIST                          
         LTR   R1,R1                                                            
         BZ    RECALC20                                                         
RECALC10 LA    RF,4(RF)                                                         
         BCT   R1,RECALC10                                                      
RECALC20 ST    RF,CURRLST              SAVE ADDRESS OF CURRENT LIST             
         XC    STRTLST,STRTLST         POINT TO START NODE OF LIST              
         OC    0(4,RF),0(RF)           EMPTY LIST                               
         BZ    RECALC30                                                         
         SR    R1,R1                                                            
         ICM   R1,3,0(RF)              GET DISP OF START NODE                   
         A     R1,FRSTNODE                                                      
         ST    R1,STRTLST                                                       
RECALC30 B     EXIT                                                             
         EJECT                                                                  
*                                                                               
         USING JGROUPD,R3                                                       
SHOWXTRA NTR1                                                                   
         MVC   MAINLINE,JGLINE                                                  
         MVC   JBGFJOB,JGFJOB                                                   
         OC    JBGFJOB,SPACES                                                   
         CLC   JBGFJOB,SPACES      DO WE HAVE A FROM JOB?                       
         BNH   SHXTR050                                                         
         LA    R1,L'JBGFJOB                                                     
         LA    RF,JBGFJOB+L'JBGFJOB-1                                           
SHXTR020 CLI   0(RF),C' '                                                       
         BH    *+10                                                             
         BCTR  RF,0                                                             
         BCT   R1,SHXTR020                                                      
*                                                                               
         STC   R1,JBGFJOBH+5                                                    
         LR    RE,RF                                                            
SHXTR023 CLI   0(RE),C'/'          FIND LAST DELIMITER                          
         BE    SHXTR025                                                         
         BCTR  RE,0                                                             
         B     SHXTR023                                                         
*                                                                               
SHXTR025 XC    FRMJOB,FRMJOB                                                    
         SR    RF,RE                                                            
         LA    RE,1(RE)                                                         
         EX    RF,*+4                                                           
         MVC   FRMJOB,0(RE)                                                     
*                                                                               
         LA    R2,JBGFJOBH                                                      
         BAS   RE,JOBVAL                                                        
         CLI   ERRNUM,X'FF'                                                     
         BNE   EXIT                                                             
         MVI   FNDX,0                                                           
         MVC   JBGFNAM,ACCTNAME    MOVE IN NAME                                 
*                                                                               
SHXTR050 MVC   JBGWC,JGWC                                                       
         MVC   JBGCRAC(L'JGSUPPLR),JGSUPPLR                                     
         MVC   JBGREF,JGREFN                                                    
         MVC   JBGDATE,JGDATE                                                   
         MVC   JBGSTAT,=C'   '                                                  
         CLI   JGSTATUS,C'N'                                                    
         BNE   *+10                                                             
         MVC   JBGSTAT,=C'N/C'                                                  
         MVC   JBGTAMT,JGTOTAMT                                                 
*                                                                               
         LA    R0,L'JBGTAMT                                                     
         LA    RF,JBGTAMT+L'JBGTAMT-1                                           
SHXTR055 CLI   0(RF),C' '                                                       
         BH    SHXTR057                                                         
         BCTR  RF,0                                                             
         BCT   R0,SHXTR055                                                      
         ZAP   BASEAMT,=P'0'                                                    
         B     SHXTR058                                                         
*                                                                               
SHXTR057 GOTO1 CASHVAL,DMCB,(X'82',JBGTAMT),(R0)                                
         ZAP   BASEAMT,DMCB+4(8)                                                
*                                                                               
SHXTR058 MVC   JBGTJOB,JGTJOB                                                   
         OC    JBGTJOB,SPACES                                                   
         CLC   JBGTJOB,SPACES      DO WE HAVE A FROM JOB?                       
         BNH   SHXTR080                                                         
         LA    R1,L'JBGTJOB                                                     
         LA    RF,JBGTJOB+L'JBGTJOB-1                                           
SHXTR060 CLI   0(RF),C' '                                                       
         BH    *+10                                                             
         BCTR  RF,0                                                             
         BCT   R1,SHXTR060                                                      
*                                                                               
         STC   R1,JBGTJOBH+5                                                    
         LA    R2,JBGTJOBH                                                      
         TM    JGSTAT,JGNEW        NEW JOB GROUP INPUT?                         
         BO    SHXTR065                                                         
         OC    STRTLST,STRTLST                                                  
         BNZ   SHXTR080                                                         
SHXTR065 OI    JBGTJOBH+4,X'80'    NEW INPUT THIS TIME                          
         BAS   RE,JGRPVAL                                                       
         NI    JBGTJOBH+4,X'7F'    VALIDATED GROUP                              
*                                                                               
SHXTR080 B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
*              RESTORE SAVED SCREEN                                             
*                                                                               
         USING JGROUPD,RF                                                       
END10    DS    0H                                                               
*                                                                               
         L     RF,AJOBGRPB                                                      
         CLI   PGMODE,JGM1SPVL     SPECIAL DOESN'T NEED AMOUNTS                 
         BE    END33                                                            
         CLI   PGMODE,JGM1SPCL     SPECIAL DOESN'T NEED AMOUNTS                 
         BE    END33                                                            
         CLC   TOTLAMT,BASEAMT     DID USER EXCEED 100%?                        
         BNH   END30                                                            
         MVC   FVMSGNO,=AL2(AE$OALLC)                                           
         LA    R2,JBGAMNTH                                                      
         B     ERROR                                                            
*                                                                               
END30    DS    0H                                                               
         MVI   JGMODE,JGMEXIT      TIME TO LEAVE                                
END33    XC    JGTJOB,JGTJOB                                                    
         MVC   JGTJOB(L'JBGTJOB),JBGTJOB                                        
         MVC   JGWC,JBGWC                                                       
         MVC   JGSTATUS,JBGSTAT                                                 
         CLI   JBGSTAT,C' '                                                     
         BNE   *+8                                                              
         MVI   JGSTATUS,C'C'                                                    
         MVC   JGLINE,MAINLINE                                                  
         L     RF,CURRLST                                                       
         XC    0(4,RF),0(RF)       ASSUME NO LIST                               
         OC    STRTLST,STRTLST                                                  
         BZ    END35                                                            
         L     R5,STRTLST          GET DISP ADDRESS                             
         S     R5,FRSTNODE                                                      
         STCM  R5,3,0(RF)          SAVE IT IN LIST                              
         MVI   2(RF),C'U'                                                       
*                                                                               
END35    DS    0H                                                               
         GOTO1 AXITSES                                                          
*                                                                               
END40    LA    R2,CONACTH          CURSOR HAS TO BE SOMEWHERE                   
         MVI   CSSPROG,0           RESET THE PF KEYS                            
         B     EXIT                                                             
         DROP  RF                                                               
         EJECT                                                                  
*                                                                               
*              VALIDATE JOB FIELD (R2=A(FLDHDR)) INPUT IS CLI/PRO/JOB           
*                                                                               
         USING ACCOMPD,RF                                                       
JOBVAL   NTR1                                                                   
         MVI   ERRNUM,EIIF                                                      
         MVI   FNDX,0                                                           
         MVC   TJOBACCT,SPACES     CLEAR SAVE AREAS                             
         MVC   TJOBNAME,SPACES                                                  
*                                                                               
         GOTO1 SCANNER,DMCB,(R2),(7,LINES),C',=/='                              
         LA    RF,COMPEL           RF=A(COMPANY ELEMENT)                        
         MVC   TNUMNTRS,4(R1)      SAVE NO OF ENTRIES                           
         MVI   FNDX,1                                                           
         LA    R3,LINES            R3=A(SCAN TABLE)                             
         LA    R4,KEY+3            R4=A(NEXT KEY FIELD)                         
         LA    R5,CLILNGTH         R5=A(ACCOUNT LENGTHS)                        
         LA    R6,CLIPROF          PROFILES REQUIRED                            
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),ACMPJOB                                                 
         SR    RF,RF               RF=L'THIS KEY ELEMENT                        
         SR    R0,R0               R0=L'KEY SO FAR                              
JOBVAL2  CLI   FNDX,4                                                           
         BE    JOBVAL10            FINISHED SCAN                                
*                                                                               
JOBVAL3  MVI   ERRNUM,EIIF                                                      
         CLI   1(R3),0             L'SECOND HALF                                
         BNE   JOBVALX                                                          
         CLI   0(R3),0             L'FIRST HALF                                 
         BE    JOBVALX                                                          
         ZIC   RF,0(R5)                                                         
         SR    RF,R0                                                            
         STC   RF,WORK                                                          
         MVI   ERRNUM,EFTL                                                      
         CLC   0(1,R3),WORK        CHECK L'THIS KEY ELEMENT                     
         BH    JOBVALX                                                          
         ZIC   R1,0(R3)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),12(R3)      MOVE ELEMENT TO KEY                          
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         BAS   RE,GETACC2          READ ACCOUNT                                 
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BE    JOBVAL3A                                                         
         B     JOBVALX                                                          
JOBVAL3A LA    R6,PSPROPPR-PSCLIPPR(R6)  NEXT PROFILE                           
*                                                                               
         CLI   FNDX,1              CLIENT LEVEL                                 
         BNE   JOBVAL4                                                          
         MVI   ERRNUM,EAIL                                                      
         TM    ACCTSTAT,X'10'      CHECK CLIENT LOCK                            
         BO    JOBVALX                                                          
         B     JOBVAL6                                                          
*                                                                               
JOBVAL4  CLI   FNDX,3              JOB LEVEL?                                   
         BNE   JOBVAL6             NO, MUST BE PRODUCT                          
         MVI   ERRNUM,EIAC         YES                                          
         TM    ACCTSTAT,X'80'      CHECK IF JOB OK TO POST TO                   
         BZ    JOBVALX                                                          
         MVI   ERRNUM,EAIL                                                      
         TM    ACCTSTAT,X'10'      CHECK JOB LOCK                               
         BO    JOBVALX                                                          
         MVI   ERRNUM,EIAC                                                      
         TM    ACCTSTAT,X'20'      CHECK JOB  OPEN                              
         BO    JOBVALX                                                          
*                                                                               
JOBVAL6  ZIC   R1,FNDX             BUMP TO NEXT LINE                            
         LA    R1,1(R1)                                                         
         STC   R1,FNDX                                                          
         LA    R3,32(R3)                                                        
         LA    R4,0(R4,RF)                                                      
         LA    R5,1(R5)                                                         
         AR    R0,RF                                                            
         B     JOBVAL2                                                          
*                                                                               
JOBVAL10 MVI   ERRNUM,EAXJ         SEE IF THIS IS AN XJOB                       
         LA    R3,KEY                                                           
         GOTO1 ASETJOB,DMCB,(R3)                                                
         TM    ACOPSTAT,ACOXJOB                                                 
         BNZ   JOBVALX             IT IS, FLAG AS ERROR                         
*                                                                               
         MVI   ERRNUM,X'FF'        SET VALID JOB EXIT                           
         MVC   TJOBACCT,ACCTNUM                                                 
         MVC   TJOBNAME,ACCTNAME                                                
         TM    COMPSTAT,X'10'                                                   
         BZ    JOBVALX             NO COSTING                                   
*                                                                               
                                                                                
         BAS   RE,PROFMERG                                                      
         LA    RE,PROFILE                                                       
         USING ACPROFD,RE                                                       
         MVC   KEY(15),ACPRCOST    SET TO READ 1C ACCOUNT                       
         BAS   RE,GETACC2                                                       
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   JOBVALX                                                          
         BAS   RE,CHKACC2                                                       
*                                                                               
JOBVALX  DS    0H                                                               
         MVC   FVXTRA,8(R2)                                                     
         B     EXIT                                                             
         EJECT                                                                  
*---------------------------------------------------------------------*         
*              EDIT THE INPUT                                         *         
*---------------------------------------------------------------------*         
*                                                                               
EDIT0010 DS    0H                                                               
         LA    R2,JBGTJOBH                                                      
         TM    JBGTJOBH+4,X'80'    NEW INPUT?                                   
         BZ    EDIT0300            NO, CONTINUE EDIT LIST OF JOBS               
         CLI   JBGTJOBH+5,0        JOB GROUP?                                   
         BNE   EDIT0100            NONE, CHECK LIST OF JOBS                     
         BAS   RE,DELLIST          DELETE LIST                                  
         B     EDIT0300                                                         
*                                                                               
EDIT0100 BAS   RE,JGRPVAL          VALIDATE JOB GROUP                           
         BNE   EDITBAD                                                          
         BAS   RE,PRSLST                                                        
         BAS   RE,SHOWJGRP         SHOW IT ON SCREEN                            
*                                                                               
         USING LINED,R3                                                         
         USING JGBLOCK,R5                                                       
EDIT0300 LA    R2,JBGWCH                                                        
         BAS   RE,WCVAL            VALIDATE WORKCODE                            
         BNE   EDITBAD                                                          
EDIT0305 LA    R2,JBGSTATH                                                      
         TM    JBGSTATH+4,X'80'    NEW STATUS?                                  
         BZ    EDIT0310                                                         
         CLI   JBGSTAT,C' '                                                     
         BNE   EDIT0307                                                         
         MVC   JBGSTAT,=C'C  '     COMMISIONABLE                                
         OI    JBGSTATH+6,X'80'    TRANSMIT                                     
         B     EDIT0310                                                         
EDIT0307 CLI   JBGSTAT,C'C'                                                     
         BE    EDIT0310                                                         
         CLC   JBGSTAT,=C'N/C'     NON-COMMISIONABLE                            
         BNE   EDITBAD                                                          
*                                                                               
EDIT0310 L     R5,CURRJGRP         CURRENT JOB GROUP                            
         LA    R6,9                9 LINES ON SCREEN                            
         LA    R3,JBGJOBH                                                       
EDIT0320 TM    LNJOBH+4,X'80'      CHANGES TO LINE?                             
         BO    EDIT0325                                                         
         TM    LNPCNTH+4,X'80'                                                  
         BO    EDIT0325                                                         
         TM    LNAMNTH+4,X'80'                                                  
         BZ    EDIT0390                                                         
*                                                                               
EDIT0325 CLI   LNJOBH+5,0          NO JOB, STOP VALIDATING LINES                
         BE    EDIT0400                                                         
         LA    R2,LNJOBH                                                        
         BAS   RE,JOBVAL                                                        
         CLI   ERRNUM,X'FF'                                                     
         BNE   EDITBAD                                                          
         MVC   LNJOBN,TJOBNAME                                                  
         OI    LNJOBNH+6,X'80'     TRANSMIT                                     
*                                                                               
         LA    R2,LNPCNTH                                                       
         MVI   FNDX,0                                                           
         MVC   FVMSGNO,=AL2(AE$AMREQ)                                           
         TM    LNAMNTH+4,X'80'     NEW AMOUNT ENTERED?                          
         BO    EDIT0330                                                         
         TM    LNPCNTH+4,X'80'     NEW PERCENTAGE?                              
         BO    EDIT0350                                                         
         CLI   LNPCNTH+5,0                                                      
         BNE   EDIT0369                                                         
         CLI   LNAMNTH+5,0                                                      
         BE    EDITBAD                                                          
         B     EDIT0369                                                         
*                                                                               
EDIT0330 LA    R2,LNAMNTH                                                       
         MVC   FVMSGNO,=AL2(AE$INAMT)                                           
         SR    R0,R0                                                            
         ICM   R0,1,LNAMNTH+5                                                   
         BZ    EDIT0390                                                         
         GOTO1 CASHVAL,DMCB,(X'82',LNAMNT),(R0)                                 
         CLI   DMCB,X'FF'                                                       
         BE    EDITBAD                                                          
         ZAP   TMPAMNT,DMCB+4(8)                                                
         EDIT  (P8,DMCB+4),(L'LNAMNT,LNAMNT),2                                  
         OI    LNAMNTH+6,X'80'                                                  
         ZAP   WORK(16),DMCB+4(8)                                               
         MP    WORK(16),=P'1000000'                                             
         DP    WORK(16),BASEAMT                                                 
         EDIT  (P8,WORK),(L'LNPCNT,LNPCNT),4                                    
         OI    LNPCNTH+6,X'80'                                                  
         AP    TOTLAMT,DMCB+4(8)                                                
         B     EDIT0360                                                         
*                                                                               
EDIT0350 LA    R2,LNPCNTH                                                       
         MVC   FVMSGNO,=AL2(AE$INAMT)                                           
         SR    R0,R0                                                            
         ICM   R0,1,LNPCNTH+5                                                   
         BZ    EDIT0390                                                         
         GOTO1 CASHVAL,DMCB,(X'84',LNPCNT),(R0)                                 
         CLI   DMCB,X'FF'                                                       
         BE    EDITBAD                                                          
         EDIT  (P8,DMCB+4),(L'LNPCNT,LNPCNT),4                                  
         OI    LNPCNTH+6,X'80'                                                  
         ZAP   WORK(16),BASEAMT                                                 
         MP    WORK(16),DMCB+4(8)                                               
         SRP   WORK(16),58,5       ROUNDING                                     
         AP    TOTLAMT,WORK(16)                                                 
         ZAP   TMPAMNT,WORK(16)                                                 
         EDIT  (P16,WORK),(L'LNAMNT,LNAMNT),2                                   
         OI    LNAMNTH+6,X'80'                                                  
*                                                                               
EDIT0360 DS    0H                                                               
         TM    LSTSTAT,LSTEOL      EOL?                                         
         BZ    *+12                                                             
         BAS   RE,INSNODE                                                       
         L     R5,FREENODE                                                      
         ZAP   JGBAMNT,TMPAMNT                                                  
EDIT0369 MVC   JGBACCT,TJOBACCT+3                                               
*                                                                               
EDIT0390 CLC   JGBFTPTR,=X'FFFF'                                                
         BE    EDIT0393                                                         
         SR    R1,R1                                                            
         ICM   R1,3,JGBFTPTR                                                    
         A     R1,FRSTNODE                                                      
         LR    R5,R1                                                            
         B     *+8                                                              
EDIT0393 OI    LSTSTAT,LSTEOL      MARK END OF LIST                             
EDIT0395 LA    R3,LINELNQ(R3)                                                   
         BCT   R6,EDIT0320                                                      
         B     EDIT0500                                                         
*                                                                               
EDIT0400 DS    0H                  HANDLE POSSIBLE LINE DELETION                
         TM    LSTSTAT,LSTEOL      END OF LIST?                                 
         BO    *+8                 YES, DON'T DELETE LAST BY ACCIDENT           
         BAS   RE,DELNODE          MOVE UP LINES                                
         LA    R3,LINELNQ(R3)      BUMP TO NEXT LINE                            
         BCT   R6,EDIT0320                                                      
*                                                                               
         USING JGROUPD,R3                                                       
EDIT0500 DS    0H                                                               
         BAS   RE,SHOWJGRP         SHOW EDITED LINES                            
         BAS   RE,SHOWTOT          SHOW TOTAL PERCENT AND AMOUNT                
         CLI   PGMODE,JGM1PASS                                                  
         BNE   EDIT0550                                                         
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     END10                                                            
EDIT0550 MVC   ERRNUM,0(RE)                                                     
         L     RF,AIOA                                                          
         ST    RF,0(RE)                                                         
         MVC   0(1,RE),ERRNUM                                                   
         TM    LSTSTAT,LSTERROR                                                 
         BZ    EDITOK                                                           
         BAS   RE,PRSLST                                                        
         BAS   RE,SHOWJGRP                                                      
         TM    LSTSTAT,LSTERROR                                                 
         BZ    EDITOK                                                           
         LA    R2,JBGJOBH                                                       
         ST    R2,FVADDR                                                        
         B     ERROR                                                            
*                                                                               
EDITOK   MVI   ERRNUM,SPECIAL                                                   
         MVC   MSG,SPACES                                                       
         MVC   MSG(L'LEAVE),LEAVE                                               
         LA    R2,JBGTJOBH         POINT BACK TO START                          
         ST    R2,FVADDR                                                        
         CLI   PFKEY,15            ONLY PF15 WILL RETURN                        
         BE    END10                                                            
         B     ERROR                                                            
*                                                                               
EDITBAD  CLI   PGMODE,JGM1PASS     LET 1 PASS THROUGH                           
         BNE   ERROR                                                            
         B     END10                                                            
         DROP  R3,R5                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
*              JOB GROUP VALIDATION                                   *         
*                  R2 -> FIELD HEADER                                 *         
*---------------------------------------------------------------------*         
         USING JGRRECD,R3                                                       
         USING JGROUPD,RF                                                       
JGRPVAL  NTR1                                                                   
         XR    R0,R0                                                            
         NI    LSTSTAT,X'FF'-LSTERROR                                           
         L     RF,AJOBGRPB                                                      
         NI    JGSTAT,X'FF'-JGERR1-JGERR2                                       
         L     R3,AIO1                                                          
         XC    0(255,R3),0(R3)                                                  
         MVI   JGRKTYP,JGRKTYPQ    X'2C'                                        
         MVI   JGRKSUB,JGRKSUBQ    X'12'                                        
         MVC   JGRKCPY,COMPANY                                                  
         MVC   JGRKUNT(2),=C'SJ'                                                
         MVC   JGRKCODE,8(R2)                                                   
         OC    JGRKCODE,SPACES                                                  
         MVI   ERRNUM,EIIF                                                      
         BAS   RE,RDKEY                                                         
         BE    JGRPV09                                                          
         L     RF,AJOBGRPB                                                      
         OI    JGSTAT,JGERR1                                                    
         B     NEQXIT                                                           
*                                                                               
         USING JGBLOCK,R5                                                       
JGRPV09  BAS   RE,DELLIST          DELETE LIST IF ANY                           
         L     R5,STRTLST                                                       
         MVI   CURRLINE,1          CURRENT LINE NUMBER                          
         ZAP   TOTPCNT,=P'0'                                                    
         ZAP   TOTLAMT,=P'0'                                                    
*                                                                               
         LA    R4,ACCORFST(R3)                                                  
JGRPV10  CLI   0(R4),0             EOR?                                         
         BE    JGRPV90                                                          
         CLI   0(R4),NAMELQ        NAME ELEMENT?                                
         BE    JGRPV30                                                          
         CLI   0(R4),JBPELQ                                                     
         BE    JGRPV50                                                          
JGRPV20  IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     JGRPV10                                                          
*                                                                               
         USING NAMELD,R4                                                        
JGRPV30  MVC   JBGTNAM,SPACES                                                   
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         AHI   R1,-3                                                            
         EX    R1,*+4                                                           
         MVC   JBGTNAM(0),NAMEREC                                               
         OI    JBGTNAMH+6,X'80'                                                 
         B     JGRPV20                                                          
*                                                                               
         USING JBPELD,R4                                                        
JGRPV50  DS    0H                                                               
         BAS   RE,INSNODE          INSERT A NODE INTO LIST                      
         L     R5,FREENODE                                                      
         LTR   R5,R5                                                            
         BZ    NEQXIT              ERROR, FILLED UP SPACE                       
*                                                                               
         LA    RF,JGBACCT                                                       
         LA    R6,CLILNGTH                                                      
         SR    R1,R1                                                            
         IC    R1,0(R6)            CLIENT LENGTH                                
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   0(0,RF),JBPCLI                                                   
         LA    R1,1(R1)                                                         
         AR    RF,R1                                                            
         LR    R0,R1                                                            
         IC    R1,1(R6)            PRODUCT LENGTH                               
         SR    R1,R0                                                            
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   0(0,RF),JBPPRO                                                   
         LA    RF,1(R1,RF)                                                      
         IC    R0,1(R6)                                                         
         IC    R1,2(R6)            JOB LENGTH                                   
         SR    R1,R0                                                            
         BCTR  R1,0                                                             
         CLC   JBPJOB,SPACES       NO JOB?                                      
         BE    JGRPV53             USE FROM JOB                                 
         EX    R1,*+4                                                           
         MVC   0(0,RF),JBPJOB                                                   
         B     JGRPV55                                                          
JGRPV53  EX    R1,*+4                                                           
         MVC   0(0,RF),FRMJOB                                                   
*                                                                               
JGRPV55  CLI   PGMODE,JGM1SPVL     SPECIAL DOESN'T NEED AMOUNTS                 
         BE    JGRPV20             BUMP TO NEXT ELEMENTS                        
         AP    TOTPCNT,JBPPERC                                                  
JGRPV60  ZAP   WORK(16),BASEAMT                                                 
         MP    WORK(16),JBPPERC    PERCENTAGE                                   
         SRP   WORK(16),58,5                                                    
         ZAP   JGBAMNT,WORK(16)                                                 
         AP    TOTLAMT,JGBAMNT     RUNNING TOTAL                                
         B     JGRPV20             NEXT ELEMENT                                 
*                                                                               
JGRPV90  CLI   PGMODE,JGM1SPVL     FORGET ABOUT AMOUNTS                         
         BE    EQXIT                                                            
         CP    TOTPCNT,=P'1000000'                                              
         BL    JGRPV99             DON'T CHECK FOR ROUNDING ERRORS              
*                                                                               
         ZAP   DIFFAMT,BASEAMT     FIND ANY ROUNDING PROBLEMS                   
         SP    DIFFAMT,TOTLAMT                                                  
         BZ    JGRPV99             NONE, LEAVE                                  
*                                                                               
TMP      USING JGBLOCK,RF                                                       
         L     R5,STRTLST          FIND LARGEST AMOUNT IN LIST                  
         LR    RF,R5                                                            
JGRPV93  CP    JGBAMNT,TMP.JGBAMNT      FOUND LARGER AMOUNT?                    
         BNH   *+6                                                              
         LR    RF,R5                    REASSIGN POINTER                        
         CLC   JGBFTPTR,=X'FFFF'        EOL?                                    
         BE    JGRPV98                                                          
         XR    R1,R1                                                            
         ICM   R1,3,JGBFTPTR                                                    
         A     R1,FRSTNODE                                                      
         LR    R5,R1                                                            
         B     JGRPV93                                                          
*                                                                               
JGRPV98  AP    TMP.JGBAMNT,DIFFAMT                                              
         AP    TOTLAMT,DIFFAMT                                                  
JGRPV99  MVI   ERRNUM,X'FF'                                                     
         ZAP   WORK(16),TOTLAMT                                                 
         MP    WORK(16),=P'1000000'                                             
         DP    WORK(16),BASEAMT                                                 
         EDIT  (P8,WORK),(L'JBGPCTS,JBGPCTS),4                                  
         OI    JBGPCTSH+6,X'80'                                                 
         EDIT  TOTLAMT,(L'JBGTOTS,JBGTOTS),2                                    
         OI    JBGTOTSH+6,X'80'                                                 
*                                                                               
         B     EQXIT                                                            
         DROP  R5,RF,TMP                                                        
         EJECT                                                                  
*--------------------------------------------------------------------*          
* PARSE THROUGH LIST LOOKING FOR ERROR                               *          
*--------------------------------------------------------------------*          
         USING ACTRECD,R3                                                       
         USING JGBLOCK,R5                                                       
         USING JGROUPD,RF                                                       
PRSLST   NTR1                                                                   
         MVI   CURRLINE,1                                                       
         NI    LSTSTAT,X'FF'-LSTERROR                                           
         L     RF,AJOBGRPB                                                      
         NI    JGSTAT,X'FF'-JGERR2                                              
         L     R5,STRTLST                                                       
         LTR   R5,R5               SEE IF THERE IS A LIST                       
         BZ    EQXIT                                                            
*                                                                               
PRSL010  DS    0H                                                               
         XC    TMPFLDH(L'TMPFLDH+L'TMPFLDH),TMPFLDH                             
         LA    R2,TMPFLDH                                                       
         BAS   RE,PJOB2FLD                                                      
         BAS   RE,JOBVAL                                                        
         CLI   ERRNUM,X'FF'                                                     
         BE    PRSL100                                                          
         OI    JGSTAT,JGERR2       GROUP CONTAINS ERRORS                        
         OI    LSTSTAT,LSTERROR                                                 
         B     NEQXIT                                                           
*                                                                               
PRSL100  CLC   JGBFTPTR,=X'FFFF'   EOL?                                         
         BE    PRSL900                                                          
         XR    R1,R1                                                            
         IC    R1,CURRLINE         BUMP LINE                                    
         LA    R1,1(R1)                                                         
         STC   R1,CURRLINE                                                      
         ICM   R1,3,JGBFTPTR       BUMP POINTER                                 
         A     R1,FRSTNODE                                                      
         LR    R5,R1                                                            
         B     PRSL010                                                          
                                                                                
PRSL900  MVI   CURRLINE,1                                                       
         B     EQXIT                                                            
         DROP  R3,R5,RF                                                         
         EJECT                                                                  
*--------------------------------------------------------------------*          
* VALIDATE WORKCODE                                                  *          
*--------------------------------------------------------------------*          
WCVAL    NTR1                                                                   
         TM    JBGWCH+4,X'80'      NEW WORKCODE ENTERED?                        
         BZ    EQXIT               NO, LEAVE                                    
         MVI   ERRNUM,19           INVALID WORK-CODE                            
         CLC   JBGWC,=C'99'                                                     
         BE    NEQXIT                                                           
         CLC   JBGWC,=C'**'                                                     
         BE    NEQXIT                                                           
         MVC   KEY,SPACES                                                       
         MVI   KEY,X'0A'           WORK-CODE RECORD                             
         MVC   KEY+1(1),COMPANY                                                 
         MVC   KEY+2(2),=C'SJ'                                                  
         MVC   KEY+4(2),JBGWC                                                   
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',KEY,KEY                      
         CLI   DMCB+8,0                                                         
         BNE   NEQXIT                                                           
         MVI   ERRNUM,X'FF'        SET WORK-CODE VALID                          
         B     EQXIT                                                            
         EJECT                                                                  
*--------------------------------------------------------------------*          
* READ FOR KEY                                                       *          
*--------------------------------------------------------------------*          
RDKEY    NTR1                                                                   
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',(R3),(R3)                    
         CLI   8(R1),0                                                          
         BE    EQXIT                                                            
NEQXIT   LTR   RB,RB                                                            
         B     EXIT                                                             
EQXIT    CR    RB,RB                                                            
         B     EXIT                                                             
         EJECT                                                                  
*--------------------------------------------------------------------*          
* PUT JOB ACCOUNT IN FIELD                                           *          
*        R2 = FIELD HEADER                                           *          
*        R5 = CURRENT JOB                                            *          
*--------------------------------------------------------------------*          
         USING JGBLOCK,R5                                                       
PJOB2FLD NTR1                                                                   
         XR    R0,R0                                                            
         LA    R3,8(R2)                                                         
         LA    RF,JGBACCT          POINT TO ACCT                                
         XR    R1,R1                                                            
         IC    R1,CLILNGTH         CLIENT LENGTH                                
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   0(0,R3),0(RF)                                                    
         LA    R3,1(R1,R3)                                                      
         LA    RF,1(R1,RF)                                                      
         MVI   0(R3),C'/'                                                       
         LA    R3,1(R3)                                                         
         IC    R0,CLILNGTH                                                      
         IC    R1,CLILNGTH+1                                                    
         SR    R1,R0               PRODUCT LENGTH                               
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   0(0,R3),0(RF)                                                    
         LA    R3,1(R1,R3)                                                      
         LA    RF,1(R1,RF)                                                      
         MVI   0(R3),C'/'                                                       
         LA    R3,1(R3)                                                         
         IC    R0,CLILNGTH+1                                                    
         IC    R1,CLILNGTH+2                                                    
         SR    R1,R0               JOB LENGTH                                   
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   0(0,R3),0(RF)                                                    
*                                                                               
PJ2FLD20 CLI   0(R3),C' '                                                       
         BNH   *+12                                                             
         LA    R3,1(R3)                                                         
         B     PJ2FLD20                                                         
         LA    RE,8(R2)                                                         
         SR    R3,RE                                                            
         STC   R3,5(R2)            SAVE LENGTH OF JOB                           
         OI    6(R2),X'80'                                                      
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
*              SHOW JOB GROUP ON SCREEN                               *         
*---------------------------------------------------------------------*         
         USING ACTRECD,R3                                                       
         USING LINED,R4                                                         
         USING JGBLOCK,R5                                                       
SHOWJGRP NTR1                                                                   
*                                  TWAXC JBGJOBH,JBGNNNNH,PROT=Y                
         SR    RE,RE                                                            
         LA    R1,JBGJOBH                                                       
         LA    RF,JBGNNNNH                                                      
SJGRP05  IC    RE,0(R1)                                                         
         SH    RE,=H'9'                                                         
         TM    1(R1),X'02'                                                      
         BZ    *+8                                                              
         SH    RE,=H'8'                                                         
         LTR   RE,RE                                                            
         BM    *+30                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    8(0,R1),8(R1)                                                    
         MVI   5(R1),0             NEED TO CLEAR THE LENGTH TOO                 
         OI    6(R1),X'80'                                                      
         IC    RE,0(R1)                                                         
         BXLE  R1,RE,SJGRP05                                                    
*                                                                               
         BAS   RE,POSNODE          FIND NODE                                    
         LA    R4,JBGJOBH                                                       
         L     R5,CURRJGRP                                                      
         LTR   R5,R5                                                            
         BZ    EXIT                                                             
         XR    R0,R0                                                            
         LA    R6,9                9 LINES ON THE SCREEN                        
         TM    LSTSTAT,LSTERROR                                                 
         BZ    SJGRP10                                                          
         OI    LNJOBH+4,X'80'                                                   
*                                                                               
SJGRP10  LA    R2,LNJOBH                                                        
         BAS   RE,PJOB2FLD         PUT JOB INTO FIELD                           
*                                                                               
         EDIT  JGBAMNT,(L'LNAMNT,LNAMNT),2                                      
         MVI   LNAMNTH+5,L'LNAMNT                                               
         OI    LNAMNTH+6,X'80'                                                  
*                                                                               
         ZAP   WORK(16),JGBAMNT                                                 
         MP    WORK(16),=P'1000000'                                             
         DP    WORK(16),BASEAMT                                                 
         EDIT  (P8,WORK),(L'LNPCNT,LNPCNT),4                                    
         MVI   LNPCNTH+5,L'LNPCNT                                               
         OI    LNPCNTH+6,X'80'                                                  
*                                                                               
         MVC   KEY,SPACES                                                       
         LA    R3,KEY                                                           
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(2),=C'SJ'                                                
         MVC   ACTKACT,JGBACCT                                                  
         BAS   RE,RDKEY                                                         
         BE    SJGRP40                                                          
         OI    LNJOBH+4,X'80'                                                   
         B     SJGRP50                                                          
*                                                                               
SJGRP40  LA    R3,IOAREA                                                        
SJGRP43  CLI   0(R3),0                                                          
         BE    SJGRP50                                                          
         CLI   0(R3),NAMELQ                                                     
         BE    SJGRP45                                                          
         SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     SJGRP43                                                          
*                                                                               
SJGRP45  MVC   LNJOBN,SPACES                                                    
         SR    R1,R1                                                            
         IC    R1,1(R3)                                                         
         AHI   R1,-3                                                            
         CHI   R1,L'LNJOBN-1                                                    
         BNH   *+8                                                              
         LA    R1,L'LNJOBN-1                                                    
         EX    R1,*+4                                                           
         MVC   LNJOBN(0),2(R3)                                                  
         OI    LNJOBNH+6,X'80'     TRANSMIT                                     
*                                                                               
SJGRP50  XR    R1,R1                                                            
         ICM   R1,3,JGBFTPTR                                                    
         C     R1,=F'65535'        EOL?                                         
         BE    EXIT                                                             
         A     R1,FRSTNODE                                                      
         LR    R5,R1                                                            
         LA    R4,LINELNQ(R4)                                                   
         BCT   R6,SJGRP10                                                       
*                                                                               
         B     EXIT                                                             
         DROP  R3,R4,R5                                                         
         EJECT                                                                  
*---------------------------------------------------------------------*         
* SHOW TOTAL PERCENT AND AMOUNT                                                 
*---------------------------------------------------------------------*         
         USING JGBLOCK,R5                                                       
SHOWTOT  NTR1                                                                   
         ZAP   TOTLAMT,=P'0'                                                    
         L     R5,STRTLST                                                       
         LTR   R5,R5                                                            
         BZ    SHOWTOT9                                                         
*                                                                               
SHOWTOT3 AP    TOTLAMT,JGBAMNT                                                  
         CLC   JGBFTPTR,=X'FFFF'   EOT?                                         
         BE    SHOWTOT9                                                         
         SR    R1,R1                                                            
         ICM   R1,3,JGBFTPTR       BUMP TO NEXT NODE                            
         A     R1,FRSTNODE                                                      
         LR    R5,R1                                                            
         B     SHOWTOT3                                                         
*                                                                               
SHOWTOT9 ZAP   WORK(16),TOTLAMT                                                 
         MP    WORK(16),=P'1000000'                                             
         DP    WORK(16),BASEAMT                                                 
         EDIT  (P8,WORK),(L'JBGPCTS,JBGPCTS),4                                  
         OI    JBGPCTSH+6,X'80'                                                 
         EDIT  TOTLAMT,(L'JBGTOTS,JBGTOTS),2                                    
         OI    JBGTOTSH+6,X'80'                                                 
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* POSITION TO NODE BASED ON CURRLINE                                            
*---------------------------------------------------------------------*         
         USING JGBLOCK,R5                                                       
POSNODE  NTR1                                                                   
         NI    LSTSTAT,X'FF'-LSTEOL                                             
         L     RF,FRSTNODE         ADDRESS OF FIRST NODE IN MEMORY              
         L     R5,STRTLST          ADDRESS OF FIRST NODE ON LIST                
         LTR   R5,R5               NOTHING IN LIST?                             
         BNZ   PNODE03             NO, CONTINUE                                 
         OI    LSTSTAT,LSTEOL                                                   
         B     PNODE09             YES, LEAVE                                   
*                                                                               
PNODE03  XR    R2,R2                                                            
         XR    R1,R1                                                            
         IC    R1,CURRLINE                                                      
         BCTR  R1,0                                                             
         LTR   R1,R1               FIRST LINE                                   
         BZ    PNODE09                                                          
PNODE05  CLC   JGBFTPTR,=X'FFFF'   NIL, LAST ONE ON LIST                        
         BE    PNODE09                                                          
         SR    R2,R2                                                            
         ICM   R2,3,JGBFTPTR       ADD DISPLACEMENT                             
         AR    R2,RF               TO FIRST ONE IN MEMORY TO                    
         LR    R5,R2               POINT TO NEXT ONE                            
         BCT   R1,PNODE05                                                       
*                                                                               
PNODE09  ST    R5,CURRJGRP                                                      
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
*              DELETE LIST                                                      
*---------------------------------------------------------------------*         
         USING JGBLOCK,R5                                                       
DELLIST  NTR1                                                                   
         L     R5,STRTLST                                                       
         LTR   R5,R5               SEE IF EMPTY?                                
         BZ    DELLSTX                                                          
DELLST5  SR    R1,R1                                                            
         ICM   R1,3,JGBFTPTR                                                    
         XC    0(JGBLN,R5),0(R5)   FREE THE NODE                                
         C     R1,=F'65535'        EOT?                                         
         BE    DELLSTX             ALL DONE                                     
         A     R1,FRSTNODE         GET ADDRESS OF NEXT ONE                      
         LR    R5,R1                                                            
         B     DELLST5             CONTINUE DELETING                            
*                                                                               
DELLSTX  XC    STRTLST,STRTLST                                                  
         L     RF,CURRLST                                                       
         XC    0(4,RF),0(RF)                                                    
         B     EXIT                                                             
         EJECT                                                                  
*---------------------------------------------------------------------*         
*              DELETE NODE POINTED TO BY R5                                     
*---------------------------------------------------------------------*         
BNODE    USING JGBLOCK,R1                                                       
FNODE    USING JGBLOCK,R2                                                       
DELNODE  NTR1                                                                   
         SR    R1,R1               POINT TO PREVIOUS NODE                       
         CLC   JGBHDPTR,=X'FFFF'                                                
         BE    DELND010                                                         
         ICM   R1,3,JGBHDPTR                                                    
         A     R1,FRSTNODE                                                      
*                                                                               
DELND010 SR    R2,R2               POINT TO NEXT NODE                           
         CLC   JGBFTPTR,=X'FFFF'                                                
         BE    DELND020                                                         
         ICM   R2,3,JGBFTPTR                                                    
         A     R2,FRSTNODE                                                      
*                                                                               
DELND020 LTR   R1,R1               DO WE HAVE A PREVIOUS POINTER?               
         BZ    DELND100                                                         
         MVC   BNODE.JGBFTPTR,JGBFTPTR                                          
         LTR   R2,R2                                                            
         BZ    DELND900                                                         
         MVC   FNODE.JGBHDPTR,JGBHDPTR                                          
         B     DELND900                                                         
*                                                                               
DELND100 ST    R2,STRTLST                                                       
         LR    RF,R2                                                            
         S     RF,FRSTNODE                                                      
         L     RE,CURRLST                                                       
         STCM  RF,3,0(RE)                                                       
         MVI   2(RE),C'U'                                                       
         LTR   R2,R2                                                            
         BZ    DELND150                                                         
         MVC   FNODE.JGBHDPTR,=X'FFFF'                                          
         B     DELND900                                                         
DELND150 XC    0(4,RE),0(RE)                                                    
*                                                                               
DELND900 XC    0(JGBLN,R5),0(R5)   WIPE OUT NODE TO USE LATER                   
         LTR   R5,R2               POINT TO NEXT NODE                           
         BZ    *+8                 NOTHING IN END                               
         B     DELND999                                                         
         OI    LSTSTAT,LSTEOL      MARK EOL                                     
         LTR   R5,R1                                                            
DELND999 XIT1  REGS=(R5)                                                        
         EJECT                                                                  
*---------------------------------------------------------------------*         
*              INSERT NODE AFTER NODE POINTED BY R5                             
*---------------------------------------------------------------------*         
INSNODE  NTR1                                                                   
         LR    R1,R5                                                            
         BAS   RE,FINDFREE                                                      
         L     R5,FREENODE                                                      
         LTR   R1,R1               SOMETHING IN LIST?                           
         BNZ   INSND100            YES, WORK IT OUT                             
         LTR   R5,R5               NO FREE NODE AVAILABLE                       
         BZ    EXIT                                                             
         ST    R5,STRTLST          SET FREE NODE AS START OF LIST               
         LR    RF,R5                                                            
         S     RF,FRSTNODE                                                      
         L     RE,CURRLST                                                       
         STCM  RF,3,0(RE)                                                       
         MVI   2(RE),C'U'                                                       
         B     EXIT                                                             
*                                                                               
INSND100 XR    R2,R2                                                            
         CLC   BNODE.JGBFTPTR,=X'FFFF'                                          
         BE    INSND110                                                         
         ICM   R2,3,BNODE.JGBFTPTR                                              
         LR    RE,R2               USED FOR LATER                               
         A     R2,FRSTNODE                                                      
INSND110 LR    RF,R1                                                            
         S     RF,FRSTNODE         GET DISP OF PREV NODE                        
         STCM  RF,3,JGBHDPTR                                                    
         LR    RF,R5                                                            
         S     RF,FRSTNODE         GET DISP OF FREE NODE                        
         STCM  RF,3,BNODE.JGBFTPTR                                              
         LTR   R2,R2                                                            
         BZ    INSND900                                                         
         STCM  RF,3,FNODE.JGBHDPTR                                              
         STCM  RE,3,JGBFTPTR                                                    
INSND900 B     EXIT                                                             
         DROP  R5,BNODE,FNODE                                                   
         EJECT                                                                  
*---------------------------------------------------------------------*         
*              FREE NODE                                                        
*---------------------------------------------------------------------*         
         USING JGBLOCK,RF                                                       
FINDFREE NTR1                                                                   
         L     RF,FRSTNODE         ADDRESS OF FIRST NODE IN MEMORY              
         LR    RE,RF                                                            
         AHI   RE,MAXSIZE          JUST UNDER 8K                                
         AHI   RE,-16                                                           
FFREE10  CR    RF,RE                                                            
         BNL   FFREEX              NO FREE NODES                                
         OC    0(JGBLN,RF),0(RF)                                                
         BZ    FFREEX9             FOUND ONE                                    
         LA    RF,JGBLN(RF)        BUMP UNTIL WE GET ONE                        
         B     FFREE10                                                          
*                                                                               
FFREEX9  MVC   JGBHDPTR,=X'FFFF'                                                
         MVC   JGBFTPTR,=X'FFFF'                                                
         ZAP   JGBAMNT,=P'0'                                                    
         B     *+6                                                              
FFREEX   XR    RF,RF                                                            
         ST    RF,FREENODE                                                      
         B     EXIT                                                             
         DROP  RF                                                               
*                                                                               
MAXSIZE  EQU   X'2000'                                                          
         EJECT                                                                  
*--------------------------------------------------------------------*          
* ANOTHER GETACC THAT EXITS TO THE PROGRAM                           *          
*--------------------------------------------------------------------*          
GETACC2  NTR1                                                                   
         MVC   BOHALF1,FVMSGNO                                                  
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         ST    R2,FVADDR                                                        
         GOTO1 AGETACC,BOPARM,KEY,(R6)                                          
         BNE   EXIT                                                             
         MVC   FVMSGNO,BOHALF1                                                  
         B     EXIT                                                             
*                                                                               
CHKACC2  MVC   FVMSGNO,=AL2(AE$INACP)                                           
         TM    ACCTSTAT,ACSBAL                                                  
         BZ    CHKACC2X                                                         
         TM    ACCTSTAT,ACSLOCK                                                 
         BO    CHKACC2X                                                         
         MVC   FVMSGNO,=AL2(FVFOK)                                              
CHKACC2X BR    RE                                                               
         EJECT                                                                  
       ++INCLUDE ACBATCODE                                                      
         EJECT                                                                  
*              CONSTANTS                                                        
*                                                                               
         EJECT                                                                  
*                                                                               
LEAVE    DC    C'INPUT COMPLETE, PLEASE RETURN TO THE INVOICE. (PF15)'          
*                                                                               
DICI     DS    0X                                                               
         DCDDL AC#PST,3                                                         
         DCDDL AC#QST,3                                                         
         DCDDL AC#ONT,3                                                         
         DCDDL AC#HST,3                                                         
         DC    AL1(EOT)                                                         
*                                                                               
       ++INCLUDE ACPRVTAB                                                       
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*              DSECT FOR LOCAL W/S                                              
*                                                                               
LWSD     DSECT                                                                  
AJOBGRPB DS    A                   A(JOB GROUP BLOCK)                           
ELCODE   DS    CL1                                                              
TMPAMNT  DS    PL6                                                              
SAVRE    DS    F                                                                
*                                                                               
OFFICE   DS    CL2                                                              
DOCNO    DS    CL6                                                              
PDATE    DS    CL3                                                              
ORDERNO  DS    CL6                                                              
*                                                                               
JOB      DS    CL15                JOB     CODE                                 
JBNME    DS    CL36                        NAME                                 
PRD      DS    CL15                PRODUCT CODE                                 
PRNME    DS    CL36                        NAME                                 
CLI      DS    CL15                CLIENT  CODE                                 
CLNME    DS    CL36                        NAME                                 
GSTVATBL DS    4CL(VTCLNQ)         GST VATICAN BLOCK                            
PSTVATBL DS    4CL(VTCLNQ)         PST VATICAN BLOCK                            
NAMTS    DS    XL1                 NUMBER OF WORKCODES                          
NOVER    DS    XL1                 NUMBER OF WORKCODES                          
TOTNET   DS    PL6                                                              
CALCTOT  DS    PL6                 CALCULATED TOTAL                             
ALARGEST DS    A                                                                
AMTBLKT  DS    CL64                                                             
OVERGPST DS    CL1                 GST/PST FLAG FOR OVERAMT PROC                
RETERR   DS    XL1                 RETURN ERROR CODE                            
LINES    DS    7CL32                                                            
TJOBACCT DS    CL15                                                             
TJOBNAME DS    CL25                                                             
TNUMNTRS DS    XL1                 NUMBER OF SCANNER ENTRIES                    
TMPFLDH  DS    XL8                 TEMPORARY FIELD AND HEADER                   
TMPFLD   DS    CL50                                                             
KEY      DS    CL49                                                             
IOAREA   DS    CL2048                                                           
*                                                                               
*                                  ERROR NUMBER EQUATES                         
EMIF     EQU   1                                                                
EIIF     EQU   2                                                                
EIPL     EQU   9                                                                
EIDF     EQU   13                                                               
EIAC     EQU   18                                                               
EAIL     EQU   18                                                               
EIAS     EQU   12                                                               
EIAM     EQU   25                                                               
EDIF     EQU   35                                                               
EFTS     EQU   36                                                               
EFTL     EQU   37                                                               
EAXJ     EQU   45                                                               
EOPT     EQU   79                                                               
*                                                                               
DICO     DS    0C                                                               
         DSDDL PRINT=YES                                                        
         DS    XL1                                                              
*                                                                               
* ACBATCTAX                                                                     
       ++INCLUDE ACBATCTAX                                                      
*                                                                               
* ACGOBLOCK                                                                     
* GOBLOCKC DS    CL(GOBLOCKX-GOBLOCK)                                           
*                                                                               
*                                                                               
LWKEY    DS    CL42                                                             
LWIO     DS    1000C                                                            
MXLNES   EQU   10                  MAXIMUM NUMBER OF LINES                      
         DS    0F                                                               
*OAREA   DS    2000C                                                            
PSTABLE  DS    (MXLNES*PSTLINE)C  TABLE OF POSTING ACCOUNTS (14 LINES)          
LWSX     DS    0C                                                               
         EJECT                                                                  
*              DSECT TO COVER INPUT LINE                                        
LINED    DSECT                                                                  
LNJOBH   DS    CL(L'JBGJOBH)                                                    
LNJOB    DS    CL(L'JBGJOB)        JOB CODE                                     
LNJOBNH  DS    CL(L'JBGJOBNH)                                                   
LNJOBN   DS    CL(L'JBGJOBN)       JOB NAME                                     
LNPCNTH  DS    CL(L'JBGPCNTH)                                                   
LNPCNT   DS    CL(L'JBGPCNT)       PERCENT                                      
LNAMNTH  DS    CL(L'JBGAMNTH)                                                   
LNAMNT   DS    CL(L'JBGAMNT)       AMOUNT                                       
LINELNQ  EQU   *-LINED             LENGTH OF INPUT LINE                         
         EJECT                                                                  
*              DSECT TO COVER LIST OF JOBS FROM A JOB GROUP                     
JGBLOCK  DSECT                                                                  
JGBHDPTR DS    XL2                 HEAD PTR                                     
JGBACCT  DS    CL12                JOB ACCOUNT                                  
JGBAMNT  DS    PL6                 AMOUNT                                       
JGBFTPTR DS    XL2                 FOOT PTR                                     
JGBLN    EQU   *-JGBHDPTR                                                       
         EJECT                                                                  
*                                                                               
*              DSECT TO COVER POSTING DATA FOR A LINE                           
*                                                                               
PSTD     DSECT                                                                  
PSTACC   DS    CL15                CREDIT ACCOUNT                               
PSTNME   DS    CL36                ACCOUNT NAME                                 
PSTLOC   DS    CL14                LOCALITY                                     
PSTLCNM  DS    CL36                LOCALITY NAME                                
PSTWKC   DS    CL2                 WORKCODE                                     
PSTEFF   DS    CL3                 EFFECTIVE DATE                               
PSTPCT   DS    PL4                 PERCENT                                      
PSTBAS   DS    PL6                 BASIS                                        
PSTAMT   DS    PL6                 POSTING AMOUNT                               
PSTLNQ   EQU   *-PSTD                                                           
PSTLINE  EQU   PSTLNQ*4            4 POSSIBLE ENTRIES PER LINE                  
         EJECT                                                                  
       ++INCLUDE ACBATDSECT                                                     
         ORG   CONTABH                                                          
       ++INCLUDE ACBATBED                                                       
*              OSVALS+550                                                       
         ORG   TWAHOLE                                                          
*                                                                               
*                                                                               
USERL    EQU   OSVALSL-550                                                      
BASEAMT  DS    PL8                                                              
TOTPCNT  DS    PL8                                                              
TOTLAMT  DS    PL8                                                              
DIFFAMT  DS    PL8                                                              
FRMJOB   DS    CL6                 JOB CODE OF FROM JOB                         
CURRJGRP DS    A                   POINTER TO CURRENT JOB GROUP                 
FRSTLIST DS    A                   FIRST LIST IN MEMORY                         
FRSTNODE DS    A                   FIRST NODE IN MEMORY                         
STRTLST  DS    A                   POINTER TO START OF LIST                     
CURRLST  DS    A                   POINTER TO CURRENT LIST                      
FREENODE DS    A                   POINTER TO A FREE NODE                       
CURRLINE DS    X                   CURRENT LINE                                 
MAINLINE DS    X                   CURRENT LINE FROM MAIN SCREEN                
PGMODE   DS    X                   TEMP MODE                                    
LSTSTAT  DS    X                   LIST STATUS                                  
LSTEOL   EQU   X'80'               END OF LIST                                  
LSTERROR EQU   X'40'               ERROR IN LIST                                
*              CL(USERL-(*-LASTBASE))   SPARE                                   
         ORG   TWASCR+10752                                                     
XTRSPACE DS    CL4000              USED FOR TYPE 34 AND THIS OVERLAY            
         EJECT                                                                  
       ++INCLUDE ACBATJGRP                                                      
         EJECT                                                                  
* ACGENBOTH                                                                     
* ACGENDAY                                                                      
* DDFLDIND                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENDAY                                                       
       ++INCLUDE DDFLDIND                                                       
       ++INCLUDE DDSCANBLKD                                                     
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006ACBAT42   05/31/01'                                      
         END                                                                    
