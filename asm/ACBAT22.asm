*          DATA SET ACBAT22    AT LEVEL 118 AS OF 10/12/17                      
*PHASE T61B22A                                                                  
*INCLUDE CADET                                                                  
         TITLE 'T61B22 - JOB-TO-JOB TRANSFER'                                   
T61B22   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 PROGDX-PROGD,*BAT22*,R8,R7,RR=R5,CLEAR=YES                       
         USING TWAD,RA                                                          
         L     R9,4(R1)                                                         
         USING GWS,R9                                                           
         USING PROGD,RC                                                         
         ST    R5,PRELOC                                                        
         LA    RE,RELOTAB                                                       
         LA    R1,ATYPES                                                        
*                                                                               
RELOOP   L     RF,0(RE)                                                         
         LA    RF,0(RF)                                                         
         A     RF,PRELOC                                                        
         ST    RF,0(R1)                                                         
         LA    RE,4(RE)                                                         
         LA    R1,4(R1)                                                         
         CLI   0(RE),X'FF'                                                      
         BNE   RELOOP                                                           
*                                                                               
         LA    RE,IOAREA                                                        
         LA    RE,L'IOAREA(RE)                                                  
         ST    RE,ASVBLOCK         RESOLVE A(SVBLOCK)                           
*                                                                               
         LA    RE,SVBLOCKQ(RE)                                                  
         ST    RE,AJGRPBLK         RESOLVE A(JGRPBLK)                           
*                                                                               
         LA    RE,L'JGRPBLK(RE)                                                 
         ST    RE,ACADETBK         RESOLVE A(CADET)                             
*                                                                               
         LA    RE,L'CADETBK(RE)                                                 
         ST    RE,ACADTBLK         RESOLVE A(CADETBLK)                          
*                                                                               
         CLI   COMPANY,X'AB'       BYPASS $CBI TEST FOR H&K                     
         BE    CLEARUP                                                          
*                                                                               
         TM    BCCPYST7,CPYSNEWB   CANT USE TYPE34 IF ON CBIL                   
         BNO   *+18                                                             
         MVC   MSG,=CL60'Must use $CBIL for transfers'                          
         LA    R2,CONACTH                                                       
         B     ALLX                                                             
*                                                                               
CLEARUP  CLI   MODE,4              CLEAR ACCTAB                                 
         BNE   CLRUP5                                                           
         BAS   RE,INFRTMPS         READ IN FROM TEMPSTR                         
         B     POST010                                                          
*                                                                               
CLRUP5   L     RE,AACCTAB          CLEAR ACCTAB                                 
         LA    RF,L'ACCTAB                                                      
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         CLI   PFKEY,9                                                          
         BNE   JGDTL005                                                         
*                                                                               
         XC    DMCB,DMCB                                                        
         MVI   BYTE,4              TYPE 34 DETAIL                               
         LA    R2,BYTE                                                          
         GOTO1 VSECRET,DMCB,('SECPOPTP',ASECBLK),(R2)                           
         CLI   DMCB,SECPYES                                                     
         BE    JGDTL003                                                         
*                                                                               
JGDTL002 L     R2,BCACUR                                                        
         MVC   FVMSGNO,=AL2(AE$PFINV)                                           
         B     ERROR                                                            
JGDTL003 CLI   MODE,1                                                           
         BE    JGDTL100                                                         
         B     JGDTL010                                                         
*                                                                               
JGDTL005 CLI   MODE,0                                                           
         BE    VALOPT                                                           
*                                                                               
JGDTL010 CLI   MODE,3              IN JOB GROUP DETAIL SCREEN                   
         BNE   VALAMT                                                           
JGDTL100 MVI   CSSPROG,2                                                        
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         GOTO1 =A(JGDETSCR),DMCB,(0,(RC)),RR=PRELOC                             
         L     R2,FVADDR                                                        
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   CURSIT                                                           
         MVC   FVMSGNO,=X'FFFF'    TELL BT61 WE HAVE TO COME BACK               
         B     CURSIT                                                           
*                                  HANDLE PROGRAM EXITS                         
ALLEXIT  XC    CONHEAD,CONHEAD                                                  
         OI    CONHEADH+6,X'80'                                                 
         OI    CONACTH+6,X'80'                                                  
         MVC   MSG,SPACES                                                       
ALLEX2   CLI   MODE,X'FF'                                                       
         BNE   ALLEX4                                                           
         XC    JOBFJOB,JOBFJOB         CLEAR & TRANSMIT FROM JOB FIELD          
         NI    JOBFJOBH+6,X'FF'-X'20'  UNPROTECT NEXT TIME IF PROTECTED         
         OI    JOBFJOBH+6,X'80'                                                 
         NI    JOBTJOBH+6,X'FF'-X'20'  UNPROTECT NEXT TIME IF PROTECTED         
         OI    JOBTJOBH+6,X'80'                                                 
         MVC   MSG(40),=C'No more data to be displayed - End Batch'             
         LA    R2,CONACTH                                                       
         B     ALLX                                                             
ALLEX4   CLI   COUNT,0                                                          
         BNE   *+12                                                             
         MVI   MODE,X'FF'                                                       
         B     ALLEX2                                                           
*                                                                               
         LA    R2,JOBIJOBH                                                      
         LR    R1,R2                                                            
         LA    R0,10               MAX # LINES                                  
         TM    1(R1),X'20'         PROTECTED FIELD?                             
         BZ    *+16                                                             
         LA    R1,LINNEXT-LINED(R1)                                             
         BCT   R0,*-12                                                          
         B     *+6                                                              
         LR    R2,R1                                                            
*                                                                               
         MVC   MSG(27),=C'Input accepted - Enter Next'                          
         CLI   MODE,1                                                           
         BE    *+10                                                             
         MVC   MSG(35),=C'Transactions displayed - Enter Data'                  
         MVI   MODE,1                                                           
ALLX     MVI   ERRNUM,X'FE'                                                     
         B     EXIT                                                             
CPYLIST  DC    X'DB70B3C3'                                                      
         DC    X'FFFFFFFF'                                                      
         EJECT                                                                  
*              VALIDATE OPTIONS FIELD                                           
*                                                                               
VALOPT   LA    R2,JOBOPTNH                                                      
         XC    OPTIONS(OPTLEN),OPTIONS                                          
         CLI   5(R2),0             CHECK FOR INPUT                              
         BE    VALOPTX                                                          
         MVI   ERRNUM,EIIF                                                      
         GOTO1 SCANNER,DMCB,(R2),(7,LINES)                                      
         CLI   4(R1),0                                                          
         BE    ERROR                                                            
         MVC   NLINES,4(R1)        SAVE NUMBER OF LINES INPUT                   
         MVI   FNDX,1                                                           
         LA    R6,LINES                                                         
VALOPT2  CLI   0(R6),0             L'FRIST HALF                                 
         BE    ERROR                                                            
         CLI   1(R6),0             L'SECOND HALF                                
         BNE   VALOPT4                                                          
         MVI   ERRNUM,EIIF         SPECIAL KEYWORD INPUT ?                      
         CLC   12(8,R6),=C'REVERSE '                                            
         BE    VALOPT4                                                          
         CLC   12(8,R6),=C'LABOR   '                                            
         BE    VALOPT4                                                          
         CLC   12(8,R6),=C'COSTS   '                                            
         BE    VALOPT4                                                          
         CLC   12(8,R6),=C'SUBREF  '                                            
         BE    VALOPT4                                                          
         CLC   12(8,R6),=C'EXCLUDE '                                            
         BNE   OPTERR                                                           
         MVI   XCLDOPT,C'Y'                                                     
         B     VALOPT10                                                         
*                                                                               
VALOPT4  ZIC   R1,0(R6)            LOOK-UP ENTRY ON OPTTAB                      
         BCTR  R1,0                                                             
         L     RE,AOPTTAB                                                       
         MVI   ERRNUM,EIIF                                                      
VALOPT6  CLI   0(RE),0                                                          
         BE    OPTERR                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R6),0(RE)                                                   
         BE    *+12                                                             
         LA    RE,L'OPTTAB(RE)                                                  
         B     VALOPT6                                                          
         MVI   ERRNUM,EDIF         CHECK FOR DUPLICATE KEYWORDS                 
         IC    R1,8(RE)                                                         
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    OPTBITS,0                                                        
         BO    OPTERR                                                           
         EX    R1,*+8                                                           
         B     *+8                                                              
         OI    OPTBITS,0           SET KEYWORD INPUT                            
         MVC   DUB+1(3),9(RE)      GET A(VALIDATION ROUTINE)                    
         L     RF,DUB                                                           
         A     RF,PRELOC                                                        
         BASR  RE,RF               AND GO TO IT                                 
*                                                                               
VALOPT10 ZIC   R1,FNDX             BUMP TO NEXT FIELD                           
         LA    R1,1(R1)                                                         
         STC   R1,FNDX                                                          
         LA    R6,32(R6)                                                        
         CLC   FNDX,NLINES                                                      
         BNH   VALOPT2                                                          
         B     VALOPTX                                                          
*                                  OUTPUT ERROR MESSAGE WITH FLD INDEX          
OPTERR   XC    CONHEAD,CONHEAD                                                  
         MVC   MSG,SPACES                                                       
         GOTO1 GETMSG,DMCB+12,(ERRNUM,MSG),(FNDX,DMCB),0                        
         OI    CONHEADH+6,X'80'                                                 
         B     EXIT                                                             
         EJECT                                                                  
*                                  VALIDATE WORK CODE                           
VALWORK  LA    R1,22(R6)                                                        
         ST    RE,FULL                                                          
         BAS   RE,WCVAL                                                         
         CLI   ERRNUM,X'FF'                                                     
         BNE   OPTERR                                                           
         MVC   WORKOPT,22(R6)                                                   
         L     RE,FULL                                                          
         BR    RE                                                               
*                                  VALIDATE REFERENCE NUMBER                    
VALREF   MVI   ERRNUM,EFTL                                                      
         CLI   1(R6),6                                                          
         BH    OPTERR                                                           
         MVC   REFOPT,22(R6)                                                    
         BR    RE                                                               
*                                  VALIDATE START REFERENCE NUMBER              
VALSREF  MVI   ERRNUM,EFTL                                                      
         CLI   1(R6),6                                                          
         BH    OPTERR                                                           
         MVC   SREFOPT,22(R6)                                                   
         BR    RE                                                               
*                                  VALIDATE REVERSE OPTION                      
VALREV   MVI   REVOPT,C'Y'                                                      
         BR    RE                                                               
*                                  VALIDATE LABOR ONLY OPTION                   
VALLAB   MVI   LABOPT,C'Y'                                                      
         BR    RE                                                               
*                                  VALIDATE DIRECT COSTS OPTION                 
VALDIR   MVI   COSTOPT,C'Y'                                                     
         BR    RE                                                               
*                                  VALIDATE EXTRA REFERENCE OPTION              
VALSUB   MVI   ERRNUM,EFTL                                                      
         CLI   1(R6),6                                                          
         BH    OPTERR                                                           
         MVI   SUBOPT,C'Y'                                                      
         CLI   1(R6),0             CAN BE 1-OR 2-SIDED ENTRY                    
         BER   RE                                                               
         MVC   SUBREF,22(R6)                                                    
         BR    RE                                                               
*                                  VALIDATE DATE/START DATE                     
VALMOS   DS    0H                                                               
         LA    R4,2                FORMAT FOR DATVAL (M/Y)                      
         B     *+6                                                              
VALDATE  DS    0H                                                               
VALSDATE DS    0H                                                               
VALEDATE DS    0H                                                               
         XR    R4,R4                                                            
         ST    RE,DUB                                                           
         MVI   ERRNUM,EIDF                                                      
         GOTO1 DATVAL,DMCB,((R4),22(R6)),WORK                                   
         OC    0(4,R1),0(R1)                                                    
         BZ    OPTERR                                                           
         LA    R3,DATOPT                                                        
         CLI   12(R6),C'S'                                                      
         BNE   *+8                                                              
         LA    R3,SDATOPT                                                       
         CLI   12(R6),C'E'                                                      
         BNE   *+8                                                              
         LA    R3,EDATOPT                                                       
         CLI   12(R6),C'M'                                                      
         BNE   *+14                                                             
         LA    R3,MOSOPT                                                        
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(1,0(R3))                                   
         GOTO1 DATECHK,DMCB,0(R3)                                               
         CLI   DMCB,X'FF'                                                       
         BE    OPTERR                                                           
         L     RE,DUB                                                           
         BR    RE                                                               
*                                  VALIDATE BILLED OPTION                       
VALBILL  MVI   ERRNUM,EFTL                                                      
         CLI   1(R6),1                                                          
         BH    OPTERR                                                           
         MVC   BILLOPT,22(R6)                                                   
         MVI   ERRNUM,EIIF                                                      
         CLI   BILLOPT,C'Y'        'Y' MEANS BILLED ONLY                        
         BER   RE                                                               
         CLI   BILLOPT,C'N'        'N' MEANS UNBILLED ONLY                      
         BER   RE                                                               
         B     OPTERR                                                           
*                                                                               
VALPCT   ST    RE,FULL                                                          
         MVI   ERRNUM,EIAM         VALIDATE PERCENT                             
         ZIC   RF,1(R6)                                                         
         GOTO1 CASHVAL,DMCB,(2,22(R6)),(RF)                                     
         CLI   DMCB,X'FF'                                                       
         BE    OPTERR                                                           
         L     RE,DMCB+4                                                        
         CVD   RE,DUB                                                           
         CP    DUB,=P'01'          FROM .01                                     
         BL    OPTERR                                                           
         CP    DUB,=P'10000'       TO 10000                                     
         BH    OPTERR                                                           
         ZAP   OPTPCT,DUB          SAVE PERCENT                                 
VALPCTX  L     RE,FULL                                                          
         BR    RE                                                               
*                                                                               
VALDOL   ST    RE,FULL                                                          
         MVI   ERRNUM,EIAM         VALIDATE AMOUNT                              
         ZIC   RF,1(R6)                                                         
         GOTO1 CASHVAL,DMCB,22(R6),(RF)                                         
         CLI   DMCB,X'FF'                                                       
         BE    OPTERR                                                           
         L     RE,DMCB+4                                                        
         CVD   RE,DUB                                                           
         ZAP   OPTAMT,DUB(8)       SAVE AMOUNT                                  
VALDOLX  L     RE,FULL                                                          
         BR    RE                                                               
*                                                                               
VALOPTX  MVI   FNDX,0                                                           
         B     VALFROM                                                          
         EJECT                                                                  
*              VALIDATE FROM JOB                                                
*                                                                               
VALFROM  LA    R2,JOBFJOBH                                                      
         MVI   ERRNUM,EMIF                                                      
         CLI   5(R2),0                                                          
         BE    ERROR                                                            
         BAS   RE,JOBVAL                                                        
         CLI   ERRNUM,X'FF'        CHECK JOB OK                                 
         BNE   ERROR                                                            
         MVI   ERRNUM,EIIF         ONLY ALLOW CLI/PRD/JOB                       
         CLI   SCANSV,3                                                         
         BH    OPTERR                                                           
         TM    4(R2),X'20'                                                      
         BO    VALFROMX                                                         
         MVC   JOBFNAM,FROMJOBN    OUTPUT NAME                                  
         OI    JOBFNAMH+6,X'80'                                                 
VALFROMX OI    4(R2),X'20'                                                      
         OI    6(R2),X'20'         PROTECT NEXT TIME                            
         OI    6(R2),X'80'         TRANSMIT THE FIELD                           
         B     VALTO                                                            
         EJECT                                                                  
*              VALIDATE TO JOB                                                  
*                                                                               
VALTO    LA    R2,JOBTJOBH                                                      
         MVI   FNDX,0                                                           
         XC    TOJOBA,TOJOBA                                                    
         L     RF,ATWAXTRA                                                      
         USING XTWAD,RF                                                         
         XC    TOSKACCT,TOSKACCT                                                
         CLI   5(R2),0                                                          
         BNE   VALTO20                                                          
         OC    JOBTNAM,JOBTNAM                                                  
         BZ    *+14                                                             
         XC    JOBTNAM,JOBTNAM                                                  
         OI    JOBTNAMH+6,X'80'                                                 
         CLI   REVOPT,C'Y'         FIELD REQUIRED IF OPT=REVERSE                
         BNE   VALTOX                                                           
         MVI   ERRNUM,EMIF                                                      
         B     ERROR                                                            
*                                                                               
VALTO20  DS    0H                                                               
         MVC   TOJGRP,SPACES                                                    
         BAS   RE,VALJGRP          VALIDATE JOB GROUP FIRST                     
         BNE   VALTO30             NOT A JOB GROUP, PROCESS AS JOB              
         CLC   FVMSGNO,=AL2(AE$GRPER)                                           
         BE    ERROR                                                            
         MVC   TOJGRPLN,JOBTJOBH+5                                              
         MVC   TOJGRP,JOBTJOB      SAVE GROUP FOR LATER                         
         OC    TOJGRP,SPACES                                                    
         MVC   JOBTJOB,JOBFJOB                                                  
         MVC   JOBTJOBH+5(1),JOBFJOBH+5                                         
*                                                                               
VALTO30  BAS   RE,JOBVAL                                                        
         CLI   ERRNUM,X'FF'        CHECK JOB OK FOR POSTING TO                  
         BNE   ERROR                                                            
         CLI   SCANSV,3                   CLI/PRD/JOB                           
         BE    VALTO50                                                          
         MVI   ERRNUM,EIIF                                                      
         B     OPTERR                                                           
*                                                                               
VALTO50  DS    0H                                                               
         TM    4(R2),X'20'                                                      
         BO    VALTOX                                                           
         CLC   TOJGRP,SPACES       STARTED WITH JOB GROUP?                      
         BNH   VALTO70                                                          
         MVC   JOBTJOB,SPACES      PUT BACK JOB GROUP                           
         MVC   JOBTJOB(L'TOJGRP),TOJGRP                                         
         MVC   JOBTJOBH+5(1),TOJGRPLN                                           
         B     VALTOX                                                           
*                                                                               
VALTO70  MVC   JOBTNAM,TOJOBN      OUTPUT NAME                                  
         OI    JOBTNAMH+6,X'80'                                                 
VALTOX   OI    4(R2),X'20'                                                      
         OI    6(R2),X'20'         PROTECT NEXT TIME                            
         OI    6(R2),X'80'         TRANSMIT THE FIELD                           
         B     VALCRAC                                                          
         EJECT                                                                  
*---------------------------------------------------------------------*         
* VALIDATE JOB GROUP                                                  *         
*        R2 --> FIELD HEADER CONTAINING JOB GROUP                     *         
*---------------------------------------------------------------------*         
         USING JGRRECD,R3                                                       
VALJGRP  NTR1                                                                   
         MVI   CSSPROG,2                                                        
         LA    R2,JOBIJOBH                                                      
         ST    R2,BCACUR                                                        
         OI    JOBIJOBH+4,X'80'                                                 
         GOTO1 =A(JGDETSCR),DMCB,(X'C0',(RC)),RR=PRELOC                         
         MVI   MODE,0                                                           
         CLC   FVMSGNO,=AL2(AE$INVCD)       INVALID CODE?                       
         BE    NEQXIT                                                           
*                                                                               
EQXIT    CR    RB,RB               CC = EQUAL                                   
         B     EXIT                                                             
NEQXIT   LTR   RB,RB               CC = NOT EQUAL                               
         B     EXIT                                                             
         EJECT                                                                  
*              VALIDATE CREDIT ACCOUNT                                          
*                                                                               
VALCRAC  LA    R2,JOBCRACH                                                      
         MVI   FNDX,0                                                           
         XC    CRACA,CRACA                                                      
         CLI   5(R2),0             OPTIONAL FIELD                               
         BNE   VALCRAC2                                                         
         OC    JOBCRNM,JOBCRNM                                                  
         BZ    *+14                                                             
         XC    JOBCRNM,JOBCRNM                                                  
         OI    JOBCRNMH+6,X'80'                                                 
         B     VALCRACX                                                         
*                                                                               
         USING ACCOMPD,R3                                                       
VALCRAC2 LA    R3,COMPEL                                                        
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),ACMPSUPP                                                
         DROP  R3                                                               
         ZIC   R3,5(R2)                                                         
         LA    R4,8(R2)                                                         
         LA    R5,KEY+3                                                         
         CLI   0(R4),C'*'          CAN BE *ULACC                                
         BNE   VALCRAC4                                                         
         MVI   ERRNUM,EFTS                                                      
         CLI   5(R2),4                                                          
         BL    ERROR                                                            
         MVI   ERRNUM,EIPL                                                      
         BCTR  R3,0                                                             
         LA    R4,1(R4)                                                         
         LA    R5,KEY+1                                                         
VALCRAC4 BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),0(R4)       MOVE FIELD TO KEY                            
         BAS   RE,GETACC                                                        
         MVC   CRACA,ACCTNUM                                                    
         MVI   ERRNUM,EIAC                                                      
         TM    ACCTSTAT,X'80'      CHECK IF A/C OK TO POST TO                   
         BZ    ERROR                                                            
         MVI   ERRNUM,EAIL                                                      
         TM    ACCTSTAT,X'10'                                                   
         BO    ERROR                                                            
         TM    4(R2),X'20'                                                      
         BO    VALCRACX                                                         
         MVC   JOBCRNM,ACCTNAME    OUTPUT NAME                                  
         OI    JOBCRNMH+6,X'80'                                                 
VALCRACX OI    4(R2),X'20'                                                      
         B     DISPLAY                                                          
         EJECT                                                                  
***********************************************************************         
* VALIDATE AMOUNT FIELD                                               *         
***********************************************************************         
         SPACE 1                                                                
VALAMT   LA    R2,CONACTH                                                       
         MVI   FNDX,0                                                           
         MVI   ERRNUM,EIAS                                                      
         CLI   MODE,X'FF'                                                       
         BNE   VALAMT1                                                          
         CLI   JOBFJOBH+5,0        ANYTHING IN FROM JOB FIELD                   
         BE    ERROR               NO - ERROR                                   
         MVI   MODE,0              YES - PROCESS NEW FROM JOB                   
         B     VALOPT                                                           
*                                                                               
         USING LINED,R3            R3=A(SCREEN LINE)                            
VALAMT1  LA    R3,JOBILINH                                                      
         USING TRNSD,R4            R4=A(KEYSAVE TABLE)                          
         LA    R4,TRNSTAB                                                       
         USING ACCTD,R5                                                         
         L     R5,AACCTAB          R5=A(CREDIT ACCOUNT TABLE)                   
*                                                                               
VALAMT2  TM    LINIJOBH+1,X'20'    PROTECTED                                    
         BNO   *+14                                                             
         CLC   LINIJOB(7),=C'PENDING'                                           
         BE    VALNEXT                                                          
*                                                                               
         GOTO1 AFVAL,LINAMNTH                                                   
         MVC   LINAMNTH+(FVILEN-FVIHDR)(L'FVILEN),FVILEN                        
         LA    R2,LINAMNTH                                                      
         MVI   ERRNUM,EIAM                                                      
         MVI   FNDX,0                                                           
         CLI   5(R2),0                                                          
         BE    ERROR                                                            
*                                                                               
         CLI   8(R2),C'H'          DID THEY INPUT HOURS                         
         BNE   VALAMT05                                                         
         CP    TRNSRTE,=P'0'                                                    
         BE    VALAMT05            NO RATE CAN'T HAVE HOURS                     
         OC    TRNSHRS,TRNSHRS                                                  
         BZ    VALAMT05            NO HOURS                                     
         ZIC   RF,5(R2)            VALIDATE NUMBER OF HOURS                     
         BCTR  RF,0                                                             
         LTR   RF,RF                                                            
         BZ    ERROR                                                            
         GOTO1 CASHVAL,DMCB,9(R2),(RF)                                          
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
         L     RE,DMCB+4                                                        
         CVD   RE,DUB                                                           
         DP    DUB,=P'25'                                                       
         CP    DUB+6(2),=P'0'                                                   
         BNE   ERROR               MUST BE IN QUARTER HOURS                     
         CVD   RE,DUB                                                           
         MP    DUB,TRNSRTE         HOURS * RATE                                 
         SRP   DUB,64-2,5                                                       
         CVB   RE,DUB                                                           
         ST    RE,DMCB+4           NOW HANDLE AS IF INPUT                       
         B     VALAMT12                                                         
*                                                                               
VALAMT05 ZIC   RF,5(R2)            CHECK FOR PERCENT INPUT                      
         BCTR  RF,0                                                             
         LTR   RF,RF                                                            
         BZ    VALAMT10                                                         
         LA    R1,8(RF,R2)         R1 TO POSSIBLE TRAILING PERCENT              
         CLI   0(R1),C'%'                                                       
         BNE   VALAMT10            LAST CHARACTER NOT A PERCENT SIGN            
*                                                                               
         CP    TRNSRTE,=P'0'                                                    
         BNE   ERROR               PERCENTAGE NOT VALID IF RATE                 
         OC    TRNSHRS,TRNSHRS                                                  
         BNZ   ERROR               OR HOURS                                     
*                                                                               
         GOTO1 CASHVAL,DMCB,(2,8(R2)),(RF)                                      
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
         L     R1,DMCB+4                                                        
         CVD   R1,DUB                                                           
         CP    DUB,=P'01'          FROM .01                                     
         BL    ERROR                                                            
         CP    DUB,=P'10000'       TO 100.00                                    
         BH    ERROR                                                            
         ZAP   PK16,DUB                                                         
         L     R1,TRNSORG          ORIGINAL AMOUNT                              
         CVD   R1,DUB                                                           
         MP    PK16,DUB+2(6)       PERCENT * ORIGINAL                           
         SRP   PK16,64-4,5         ROUNDED                                      
*                                                                               
*              DISPLAY CALCULATED AMOUNT                                        
*                                                                               
         EDIT  PK16,(11,WORK1),2,FLOAT=-                                        
         MVC   LINAMNT,WORK1                                                    
         OI    LINAMNTH+6,X'80'                                                 
*                                                                               
         LA    R0,L'LINAMNT                                                     
         LA    RF,LINAMNT+L'LINAMNT-1                                           
         CLI   0(RF),C' '                                                       
         BNE   *+10                                                             
         BCTR  RF,0                                                             
         BCT   R0,*-10                                                          
         STC   R0,5(R2)            SET LENGTH FOR CASHVAL                       
*                                                                               
VALAMT10 ZIC   RF,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,8(R2),(RF)                                          
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
*                                                                               
VALAMT12 SR    RF,RF               RF=ZERO                                      
         L     RE,DMCB+4           RE=INPUT AMOUNT                              
         MVC   DMCB,TRNSORG        DMCB=ORIGINAL AMOUNT                         
         C     RF,DMCB             BRANCH IF ORIGINAL WAS NEGATIVE              
         BH    VALAMT15                                                         
         C     RE,DMCB             INPUT MUST BE LESS THAN ORIG                 
         BH    ERROR                                                            
         CR    RE,RF               BUT NOT NEGATIVE                             
         BL    ERROR                                                            
         MVC   TRNSAMT,DMCB+4      SAVE INPUT AMOUNT                            
         B     VALINP01                                                         
*                                                                               
VALAMT15 CR    RF,RE               ORIG WAS NEG - SO MUST INPUT                 
         BNH   ERROR                                                            
         MVC   TRNSAMT,DMCB+4      SAVE INPUT AMOUNT                            
         B     VALINP01                                                         
         EJECT                                                                  
***********************************************************************         
* VALIDATE TO-JOB FIELDS                                              *         
***********************************************************************         
         SPACE 1                                                                
VALINP01 LA    R2,LINIJOBH                                                      
         MVC   TJOBGRPC,SPACES                                                  
         MVI   ERRNUM,EIIF                                                      
         XC    INSKACCT,INSKACCT   ERASE SK EACH LINE                           
         CLI   REVOPT,C'Y'                                                      
         BE    VALINP20                                                         
         GOTO1 AFVAL,(R2)                                                       
         BNE   VALINP03                                                         
         B     VALINP05                                                         
*                                                                               
VALINP03 CLI   TRNSPOST,C'Y'       NO INPUT, BUT HAVE SOMETHING                 
         BNE   VALNO                                                            
VALINP04 DS    0H                                                               
         MVC   LINIJOB(L'JOBFJOB),JOBFJOB                                       
         MVC   LINIJOBH+5(1),JOBFJOBH+5                                         
         OI    LINIJOBH+6,X'80'                                                 
         B     VALINP05                                                         
*                                                                               
VALINP05 DS    0H                                                               
         MVI   ERRNUM,EIIF                                                      
         MVI   FNDX,0                                                           
         GOTO1 SCANNER,DMCB,(R2),(7,LINES),C',=/='                              
         MVC   SCANSV,4(R1)        SAVE NO OF ENTRIES                           
*                                                                               
         CLI   SCANSV,1            JUST 1 ENTRY?                                
         BNE   VALINP10            NO, CAN'T BE JOB GROUP                       
         CLI   LINIJOBH+5,1        1 CHAR?                                      
         BNE   VALINP08            NO CHECK FOR JOB GROUP                       
         CLI   LINIJOB,C'Y'        ONLY C'Y'                                    
         BE    VALINP10                                                         
*                                                                               
VALINP08 DS    0H                                                               
         MVI   CSSPROG,2                                                        
         ST    R2,BCACUR                                                        
         GOTO1 =A(JGDETSCR),DMCB,(X'80',(RC)),RR=PRELOC                         
         CLC   FVMSGNO,=AL2(AI$PSRES)                                           
         BE    VALINP09                                                         
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BE    VALINP09                                                         
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         MVC   FVXTRA,SPACES                                                    
         B     VALINP10              NOT VALID, ASSUME OTHER                    
VALINP09 MVI   TRNSPOST,C'Y'                                                    
         CLC   TOJGRP,SPACES         TO JOB GROUP?                              
         BH    VALYES                                                           
         MVC   TJOBGRPC,LINIJOB                                                 
         B     VALINP04                                                         
*                                                                               
VALINP10 MVI   FNDX,1                                                           
         LA    R6,LINES                                                         
         CLI   0(R6),1                                                          
         BNE   VALINP19                                                         
*                                                                               
         MVI   ERRNUM,EIIF                                                      
         CLI   12(R6),C'Y'                                                      
         BNE   OPTERR                                                           
         OC    TOJOBA,TOJOBA         IF TO JOB PRESENT                          
         BZ    VALINP15                                                         
         CLC   TOJGRP,SPACES         WAS JOB GROUP IN TO JOB FIELD?             
         BNH   VALINP18              NO, EDIT WORKCODE ETC                      
         B     VALINP08              GET JOB GROUP                              
*                                                                               
VALINP15 OC    INPTJOBA,INPTJOBA     ALREADY HAVE A INPUT JOB                   
         BZ    OPTERR                IF NOT IT'S AN ERROR                       
VALINP18 ZIC   R1,SCANSV                                                        
         SH    R1,=H'1'              REDUCE FOR C'Y'                            
         STC   R1,SCANSV                                                        
         MVI   FNDX,2                                                           
         LA    R6,LINES+(L'LINES)    R6 TO WC/N OR C/SK                         
         B     VALINP30              IF OK - LOOK AT THE REST                   
*                                                                               
VALINP19 OC    TOJOBA,TOJOBA           IF TO JOB PRESENT                        
         BNZ   ERROR                   CAN'T HAVE TO JOB                        
         BAS   RE,JOBVAL               CHECK FOR VALID JOB                      
         CLI   ERRNUM,X'FF'            CHECK JOB OK                             
         BNE   ERROR                                                            
         ZIC   R1,SCANSV                                                        
         SH    R1,=H'3'                REDUCE FOR CLI/PROD/JOB                  
         STC   R1,SCANSV                                                        
         MVI   FNDX,4                                                           
         LA    R6,LINES+(L'LINES*3)    R6 TO WC/N OR C/SK                       
         B     VALINP30            IF OK - LOOK AT THE REST                     
*                                                                               
         EJECT                                                                  
*                                                                               
VALINP20 CLI   5(R2),0                REVERSE = YES                             
         BNE   VALINP25               AND NO INPUT -  POST IT                   
         CLC   TOJGRP,SPACES                                                    
         BH    VALINP08                                                         
         B     VALYES                 AND NO INPUT -  POST IT                   
*                                                                               
VALINP25 MVI   ERRNUM,EIIF                                                      
         GOTO1 SCANNER,DMCB,(R2),(4,LINES),C',=/='                              
         MVC   SCANSV,4(R1)        SAVE NO OF ENTRIES                           
         MVI   FNDX,4              JOB NOT ALLOWED - WHEN REVERSE=Y             
         CLI   SCANSV,3            SO CAN'T HAVE MORE THAN 3 FIELDS             
         BH    OPTERR                                                           
*                                                                               
         LA    R6,LINES            IF NO JOB  ON TO LINE                        
         MVI   FNDX,1                                                           
         CLI   0(R6),1                                                          
         BNE   VALINP30                                                         
         CLI   12(R6),C'N'         IF INPUT C'N' SKIP IT                        
         BE    VALNO                                                            
         EJECT                                                                  
*                                                                               
*              VALIDATE WORKCODE/SK ACCOUNT                                     
*                                                                               
VALINP30 ZIC   R1,SCANSV                                                        
         LTR   R1,R1               NUMBER OF ITEMS TO CHECK                     
         BZ    VALYES              ALL DONE                                     
         MVI   ERRNUM,EIIF                                                      
         CLI   1(R6),0                                                          
         BNE   OPTERR                                                           
         CLI   0(R6),0                                                          
         BE    VALINP40            NO WORK CODE                                 
         CLI   0(R6),2                                                          
         BH    OPTERR              MORE THAN 2 CHARACTERS                       
         LA    R1,12(R6)                                                        
         BAS   RE,WCVAL            VALIDATE THE WORKCODE                        
         CLI   ERRNUM,X'FF'                                                     
         BNE   ERROR                                                            
         MVC   TRNSWC,12(R6)       SAVE WORK CODE FOR POSTING                   
*                                                                               
*                                  VALIDATE COMMISSSIONABLE OR NON              
VALINP40 DC    0H'0'                                                            
         ZIC   R1,SCANSV                                                        
         BCTR  R1,0                                                             
         LTR   R1,R1               NUMBER OF ITEMS TO CHECK                     
         BZ    VALYES              ALL DONE                                     
         MVI   ERRNUM,EIIF                                                      
         STC   R1,SCANSV                                                        
         IC    R1,FNDX                                                          
         AH    R1,=H'1'                                                         
         STC   R1,FNDX                                                          
         LA    R6,32(R6)                                                        
         CLI   0(R6),0                                                          
         BE    VALINP50            NOT INPUT IS OK                              
         CLI   1(R6),0             CAN'T HAVE A RIGHT SIDE                      
         BNE   ERROR                                                            
         CLI   0(R6),1             ONLY ONE BYTE                                
         BNE   ERROR                                                            
         MVC   TRNSTS,12(R6)                                                    
         CLI   12(R6),C'N'          MUST BE 'N' NON-COMMISSIONABLE              
         BE    VALINP50                                                         
         CLI   12(R6),C'C'               OR 'C' COMMISSIONABLE                  
         BNE   ERROR                                                            
*                                                                               
*              VALIDATE SK ACCOUNT                                              
*                                                                               
VALINP50 ZIC   R1,SCANSV                                                        
         BCTR  R1,0                                                             
         LTR   R1,R1               NUMBER OF ITEMS TO CHECK                     
         BZ    VALYES              ALL DONE                                     
         STC   R1,SCANSV                                                        
         IC    R1,FNDX                                                          
         AH    R1,=H'1'                                                         
         STC   R1,FNDX                                                          
         LA    R6,32(R6)                                                        
         CLI   1(R6),0             CAN'T HAVE A RIGHT SIDE                      
         BNE   ERROR                                                            
         GOTOR SKVAL,DMCB,12(R6),(R2),(R3)                                      
         CLI   ERRNUM,X'FF'                                                     
         BNE   ERROR                                                            
         B     VALYES                                                           
         EJECT                                                                  
*                                                                               
VALNO    MVI   TRNSPOST,C'N'       DON'T POST                                   
         B     VALNEXT                                                          
*                                                                               
VALYES   MVC   ACCTTOJB,TOJOBA     SET ENTRY IN ACCTAB TO TO-JOB                
         MVC   ACCTTOJN,TOJOBN                                                  
         MVC   ACCTOFF,TOJOBOFF                                                 
         L     RF,ATWAXTRA                                                      
         USING XTWAD,RF                                                         
         MVC   ACCTCST,TOCST                                                    
         MVC   ACCTCSTN,TOCSTN                                                  
         MVC   ACCTSALA,TOSALA                                                  
         MVC   ACCTSALN,TOSALN                                                  
         OC    TOJOBA,TOJOBA       WAS THERE A TO-JOB                           
         BNZ   VALY2                                                            
         MVC   ACCTTOJB,INPTJOBA                                                
         MVC   ACCTTOJN,INPTJOBN                                                
         MVC   ACCTOFF,INPTJOFF                                                 
         MVC   ACCTCST,INPTCST                                                  
         MVC   ACCTCSTN,INPTCSTN                                                
         MVC   ACCTSALA,INPTSALA                                                
         MVC   ACCTSALN,INPTSALN                                                
VALY2    MVC   ACCTSK,TOSKACCT     MOVE IN SK ACCOUNT                           
         OC    TOSKACCT,TOSKACCT                                                
         BNZ   *+16                                                             
         MVC   ACCTSK,INSKACCT     MOVE IN SK ACCOUNT OR ZEROES                 
         MVC   ACCTSKNM,INSKNAME                                                
         LA    R1,LINIJOBH                                                      
         STCM  R1,15,ACCTAFLD      SAVE A(FIELD HEADER)                         
         BAS   RE,CKCONTRA         OK TO POST-FIRST CHECK IF CONTRA-            
         BNE   EXIT                ACCOUNT CAN BE POSTED TO                     
         MVI   TRNSPOST,C'Y'       DO POST                                      
*                                                                               
VALBTCH  MVI   FNDX,0                                                           
         LA    R2,TOJOBA                                                        
         OC    TOJOBA,TOJOBA                                                    
         BNZ   *+8                                                              
         LA    R2,INPTJOBA                                                      
         LA    RE,TRNSWC                                                        
         GOTO1 ASETJOB,DMCB,(X'80',(R2)),(RE)                                   
*                                                                               
         GOTO1 AOPTVAL                                                          
         BNE   CURSIT                                                           
*                                                                               
         LA    R2,TRNSWC                                                        
         BAS   RE,WCCHK                                                         
         BE    CURSIT                                                           
*                                                                               
         CLI   CHECK,C' '          CHECKING AMOUNT ?                            
         BE    VALNEXT             NO                                           
         MVI   ERRNUM,30                                                        
         MVC   THISWRKC,TRNSWC                                                  
         ICM   R2,15,TRNSAMT                                                    
         CVD   R2,DUB                                                           
         ZAP   THISAMNT,DUB                                                     
         LA    R2,THISWRKC                                                      
         GOTO1 AWRKVAL,DMCB,(R2)                                                
         BH    CURSIT                                                           
*                                                                               
VALNEXT  CLC   TJOBGRPC,SPACES     WAS THERE A JOB GROUP?                       
         BNH   VALNEXT9                                                         
         MVC   LINIJOB,TJOBGRPC                                                 
         OI    LINIJOBH+6,X'80'                                                 
*                                                                               
VALNEXT9 LA    R3,LINNEXT          BUMP TO NEXTS                                
         LA    R4,TRNSNEXT                                                      
         LA    R5,ACCTNEXT                                                      
         CLI   0(R4),X'FF'         END-OF-TABLE                                 
         BNE   VALAMT2                                                          
         B     POST                YES - POST                                   
         EJECT                                                                  
*              ROUTINE TO READ CONTRA-ACCOUNT/SK ACCOUNT                        
         SPACE 1                                                                
         USING LINED,R3                                                         
         USING ACCTD,R5                                                         
CKCONTRA NTR1                                                                   
         MVI   HALF,C'N'                                                        
         XC    LVLS1R,LVLS1R                                                    
         LA    R2,ACCTCTRN                                                      
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(14),LINSUP                                                 
CKCON2   GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',KEY,KEY                      
         CLI   8(R1),0                                                          
         BNE   CKCONNO             CAN'T FIND CONTRA-ACCOUNT                    
         CLC   KEY+1(2),=C'1R'                                                  
         BNE   CKCON3                                                           
*                                                                               
         LA    R1,IOKEY                                                         
         USING ACTRECD,R1                                                       
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(2),=C'1R'                                                
         GOTO1 AGETLDG                                                          
*                                                                               
         USING LDGTABD,R1                                                       
         ICM   R1,15,ACALDG                                                     
         MVC   LVLS1R,LDGTLVA                                                   
         DROP  R1                                                               
*                                                                               
CKCON3   LA    R1,IOAREA                                                        
         SR    RE,RE                                                            
CKCON4   CLI   0(R1),0             AND EXTRACT NAME                             
         BNE   *+6                                                              
         DC    H'0'                NO NAME ELEMENT                              
         CLI   0(R1),X'20'                                                      
         BE    *+14                                                             
         IC    RE,1(R1)                                                         
         AR    R1,RE                                                            
         B     CKCON4                                                           
         IC    RE,1(R1)                                                         
         SH    RE,=H'3'                                                         
         MVC   0(36,R2),SPACES                                                  
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),2(R1)                                                    
         CLI   HALF,C'Y'           IS THIS SK/SI ACCOUNT                        
         BE    CKCONYES            YES - I'M FINISHED                           
         CLC   KEY+1(2),=C'SK'     DO I NEED TO DO SK ACCOUNT                   
         BNE   CKCON10                                                          
         OC    ACCTSK,ACCTSK       SK ACCOUNT IN ACCTAB                         
         BZ    CKCON8                                                           
         MVC   KEY,SPACES                                                       
         MVC   KEY(L'ACCTSK),ACCTSK                                             
         LA    R2,ACCTSKNM         SK NAME                                      
         MVI   HALF,C'Y'           READING SK ACCOUNT                           
         B     CKCON2                                                           
CKCON8   MVC   ACCTSK,KEY          USE SAME AS FROM-JOB                         
         MVC   ACCTSKNM,ACCTCTRN                                                
         B     CKCONYES                                                         
         SPACE 1                                                                
CKCON10  CLC   KEY+1(2),=C'SI'     DO I NEED TO DO SI ACCOUNT                   
         BNE   CKCONYES            NO, DONE                                     
         OC    ACCTSI,ACCTSI       SI ACCOUNT IN ACCTAB                         
         BZ    CKCON12                                                          
         MVC   KEY,SPACES                                                       
         MVC   KEY(L'ACCTSI),ACCTSI                                             
         LA    R2,ACCTSINM         SI NAME                                      
         MVI   HALF,C'Y'           READING SK ACCOUNT                           
         B     CKCON2                                                           
         SPACE 1                                                                
CKCON12  MVC   ACCTSI,KEY          USE SAME AS FROM-JOB                         
         MVC   ACCTSINM,ACCTCTRN                                                
         LA    R2,KEY                                                           
         GOTO1 SIVAL,DMCB,(R2),(R5)                                             
         SPACE 1                                                                
CKCONYES CR    RB,RB               RETURN CC EQUAL                              
         B     CURSIT                                                           
         SPACE 1                                                                
CKCONNO  DS    0H                  PUT OUT ERROR MESSAGE                        
         MVI   ERRNUM,X'FE'                                                     
         MVC   MSG,SPACES                                                       
         MVC   MSG(L'CNTRAMSG),CNTRAMSG                                         
         OI    CONHEADH+6,X'80'    AND TRANSMIT IT                              
         LA    R2,LINIJOBH         CURSOR TO INPUT FIELD                        
         LTR   RB,RB               RETURN CC NOT EQUAL                          
         B     CURSIT                                                           
         EJECT                                                                  
*              PROCESS TRNSTAB/ACCTAB TO GENERATE POSTINGS                      
*                                                                               
POST     DS    0H                                                               
         MVI   GBCNTR,0            RESET GOBACK COUNTER                         
POST010  LA    R4,TRNSTAB          R4=A(TRNSTAB)                                
         L     R5,AACCTAB          R5=A(ACCTAB)                                 
         USING ACCTD,R5                                                         
         LA    R6,IOAREA                                                        
         USING TRANSD,R6           R6=A(TRANSACTION ELEMENT)                    
         LA    R3,JOBILINH                                                      
         ST    R3,ACURLIN                                                       
         LR    R2,R3                                                            
         L     R3,ATWA0            R3=A(JOB GROUP POSTINGS)                     
         AHI   R3,X'2A00'                                                       
         MVI   JGRPUSD,C'N'                                                     
         CLC   =C'T61B42',0(R3)                                                 
         BNE   *+8                                                              
         MVI   JGRPUSD,C'Y'                                                     
         LA    R3,6(R3)                                                         
         ST    R3,AJOBGRP                                                       
         LA    R3,40(R3)                                                        
         ST    R3,AFRSTNOD                                                      
         MVC   CREDITA,CRACA                                                    
         XC    THATSTA2,THATSTA2                                                
         XC    THATSTA3,THATSTA3                                                
         XC    THATDATE,THATDATE                                                
         CLI   MODE,4                                                           
         BNE   POST075                                                          
         BAS   RE,ALGNPTRS         ALIGN THE POINTERS BASED ON LINENO           
         B     POST100                                                          
*                                                                               
POST075  MVI   MODE,4                                                           
         MVI   LINENO,1                                                         
*                                                                               
POST100  CLI   TRNSPOST,C'Y'       POST THIS LINE ?                             
         BNE   POST500                                                          
         NI    JGRPLST1,X'FF'-JGRPFULL                                          
         CLC   TRNSORG,TRNSAMT                                                  
         BNE   *+8                                                              
         OI    JGRPLST1,JGRPFULL   FULL AMOUNT ALLOCATED                        
         L     R3,AJOBGRP                                                       
         MVI   JGRPUSD,C'N'                                                     
         XC    CURRJGRP,CURRJGRP                                                
         CLI   2(R3),C'U'          USED?                                        
         BNE   POST220                                                          
         MVI   JGRPUSD,C'Y'                                                     
         SR    RF,RF                                                            
         ICM   RF,3,0(R3)                                                       
         A     RF,AFRSTNOD                                                      
         ST    RF,CURRJGRP                                                      
         BAS   RE,OVER250          DID WE GO OVER 250?                          
         BE    POST200             NO, CONTINUE                                 
         MVC   FVMSGNO,=AL2(AI$BITXD)                                           
         MVI   FVOMTYP,C'I'                                                     
         L     R2,ACURLIN                                                       
         ST    R2,FVADDR                                                        
         B     ERROR                                                            
*                                                                               
POST200  L     R3,ACURLIN                                                       
         MVC   TJOBGRPC,LINIJOB                                                 
         MVC   TAMOUNT,LINAMNT                                                  
         MVC   TWC,LINWORK                                                      
         MVC   TSTATUS,LINSTAT                                                  
         ZAP   TMPCDAMT,=P'0'                                                   
*                                                                               
POST220  DS    0H                                                               
         CLI   JGRPUSD,C'N'                                                     
         BE    POST230                                                          
         L     R3,CURRJGRP         POINT TO CURRENT JOB                         
         LTR   R3,R3                                                            
         BZ    POST230                                                          
         BAS   RE,PREPJGRP                                                      
         MVC   ACCTCST,ACCTNUM                                                  
         MVC   ACCTCSTN,ACCTNAME                                                
*                                                                               
         USING ACPROFD,RE                                                       
         LA    RE,PROFILE                                                       
         MVC   ACCTOFF,ACPROFFC    REAL OFFICE                                  
*                                                                               
POST230  MVC   KEY(15),FROMJOBA    YES - READ TRANSACTION                       
         MVC   KEY+15(27),TRNSKEY                                               
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',KEY,KEY                      
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVSUBSEQ,TRNSKEY+26     SAVE SUBREF #                            
         MVC   SV2NDSEQ,TRNSKEY+26     SAVE SUBREF # FOR 2NDARY                 
         MVC   SVBATREF,TRNSKEY+20     BATCH REFERENCE                          
         ZAP   SVORGAMT,=P'0'                                                   
*                                                                               
         USING FFTELD,RF                                                        
         SR    R0,R0                                                            
         LA    RF,IOAREA           SEE IF THERE IS ANOTHER SUBREF #             
POST235  CLI   FFTEL,0             EOR?                                         
         BE    POST240             USE ONE FROM KEY                             
         CLI   FFTEL,TRSELQ                                                     
         BE    POST239                                                          
         CLI   FFTEL,FFTELQ                                                     
         BNE   POST236                                                          
         CLI   FFTTYPE,FFTTTOSD    TRANSFER ORIGINAL SEQUENCE DATA              
         BE    POST238                                                          
POST236  IC    R0,1(RF)                                                         
         AR    RF,R0                                                            
         B     POST235                                                          
*                                                                               
POST238  SR    R1,R1               USE ORIGINAL DATA                            
         IC    R1,FFTDLEN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SVSUBSEQ(0),FFTSUBSQ                                             
         B     POST236                                                          
*                                                                               
         USING TRSELD,RF                                                        
POST239  MVC   SVACTDAT,TRSDATE    SAVE ACTIVITY DATE                           
         MVC   THISSEQ,TRSBSEQ                                                  
         B     POST236                                                          
*                                                                               
POST240  ICM   RF,15,TRNSAMT                                                    
         CVD   RF,DUB                                                           
         CLI   JGRPUSD,C'Y'                                                     
         BE    *+10                                                             
         ZAP   THISAMNT,DUB        POSTING AMOUNT                               
         ICM   RF,15,TRNSORG                                                    
         CVD   RF,DUB                                                           
         ZAP   THISORIG,DUB        ORIGINAL AMOUNT                              
         CP    SVORGAMT,=P'0'                                                   
         BNE   *+10                                                             
         ZAP   SVORGAMT,DUB        SAVE FOR FFTEL                               
         GOTO1 VCONVMOS,DMCB,(R6),THISMOS                                       
*                                                                               
         LA    RF,KEY                                                           
         USING ACKEYD,RF                                                        
         MVC   THISANAL,ACKEYWRK   GET WORK CODE FROM KEY                       
         DROP  RF                                                               
*                                                                               
         MVC   THISSTAT,TRNSSTAT                                                
         NI    THISSTAT,X'89'                                                   
         MVC   THISINWC,TRNSWC                                                  
         MVC   THISREF,TRNSREF                                                  
         MVC   THISDATE,TRNSDATE                                                
         MVC   THISBTCH,TRNSBTCH                                                
         MVC   NEWSTAT,TRNSTS                                                   
         ZIC   R1,TRNSLEN                                                       
         SH    R1,=Y(TRNSNARR-TRANSD)                                           
         BP    POST250                                                          
         LA    R1,1                                                             
         MVI   THISNARR,C' '                                                    
         B     POST280                                                          
*                                                                               
POST250  BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   THISNARR(0),TRNSNARR                                             
         LA    R1,1(R1)                                                         
*                                                                               
POST280  STC   R1,THISNRLN                                                      
         IC    R1,TRNSLEN                                                       
         LA    RE,TRANSD(R1)                                                    
         XC    THISCASH,THISCASH   CLEAR ELEMENT AREAS                          
         XC    THISOTH,THISOTH                                                  
         XC    THISOTH2,THISOTH2                                                
         XC    THISHOUR,THISHOUR                                                
         XC    THISUNPR,THISUNPR                                                
         XC    THISTRSD,THISTRSD                                                
         XC    THISORGJ,THISORGJ                                                
         XC    THISORGW,THISORGW                                                
         XC    THISORGD,THISORGD                                                
         XC    THISORGB,THISORGB                                                
         XC    THISORGS,THISORGS                                                
         XC    THISCST,THISCST                                                  
         XC    THISOFF,THISOFF                                                  
         XC    THISPAKL,THISPAKL                                                
         XC    THISPID,THISPID                                                  
         XC    THISASK,THISASK                                   NMAL           
         XC    THISCRDE,THISCRDE                                 NMAL           
****     XC    ASKTHERE,ASKTHERE                                 NMAL           
*                                                                               
POST300  CLI   0(RE),0             GET AND SAVE INFO FROM THESE ELEMS           
         BE    POST440                                                          
         CLI   0(RE),SCIELQ        X'50' - SUBSIDIARY CASH INFO ELEM            
         BE    POST310                                                          
         CLI   0(RE),OTHELQ        X'23' - AND OTHERS (SUBREF) EL               
         BE    POST320                                                          
         CLI   0(RE),FFNELQ        X'25' - AND OTHERS (ORDER) EL                
         BE    POST330                                                          
         CLI   0(RE),SPAELQ        X'2C' - SPECIAL POSTING A/C ELEMENT          
         BE    POST340                                                          
         CLI   0(RE),PRTELQ        X'40' - PERSONNEL RATE ELM (HOURS)           
         BE    POST350                                                          
         CLI   0(RE),UNPELQ        X'7C' - UNIT PRICING ELEMENT                 
         BE    POST360                                                          
         CLI   0(RE),SPDELQ        X'4C' - SUBSIDIARY POSTING ELEMENT           
         BE    POST370                                                          
         CLI   0(RE),TRSELQ        X'60' - TRANSACTION STATUS ELEMENT           
         BE    POST380                                                          
         CLI   0(RE),ANOELQ        X'65' - ANALYZED OFFICE ELEMENT              
         BE    POST390                                                          
         CLI   0(RE),PXDELQ        X'4E' - XFER DETAIL ELEMENT                  
         BE    POST400                                                          
         CLI   0(RE),PAKELQ        X'D4' - PAYABEL ACCOUNT ELEMENT              
         BE    POST410                                                          
         CLI   0(RE),PIDELQ        X'D8' - PERSON ID ELEMENT                    
         BE    POST420                                                          
         CLI   0(RE),ASKELQ        X'E2' - ACCOUNT KEY ELEMENT    NMAL          
         BE    POST430                                            NMAL          
*                                                                               
POST305  SR    R1,R1                                                            
         IC    R1,1(RE)                                                         
         AR    RE,R1                                                            
         B     POST300                                                          
*                                                                               
         USING TRCASHD,RE                                                       
POST310  CLI   TRCSTYPE,C'D'                                                    
*NMAL    BNE   POST305                                                          
         BNE   POST312                     NMAL                                 
         MVC   THISCASH,TRCSEL     SAVE ELEMENT                                 
         ZAP   PK16,THISAMNT       POSTING AMOUNT                               
         MP    PK16,TRCSAMNT       * TOTAL CD                                   
         MP    PK16,=P'100'                                                     
         DP    PK16,THISORIG       DIVIDED BY ORIGINAL AMOUNT                   
         SRP   PK16(10),64-2,5     ROUNDED                                      
         DROP  RE                                                               
         LA    R3,THISCASH                                                      
         USING TRCASHD,R3                                                       
         ZAP   TRCSAMNT,PK16(10)                                                
         CLI   JGRPUSD,C'Y'                                                     
         BNE   POST305                                                          
         TM    JGRPLST1,JGRPELST    LAST ONE IN LIST?                           
         BZ    POST315                                                          
         TM    JGRPLST1,JGRPFULL    FULL AMOUNT ALLOCATED                       
         BZ    POST315                                                          
         ZAP   TRCSAMNT,TRCSAMNT-TRCASHD(L'TRCSAMNT,RE)                         
         SP    TRCSAMNT,TMPCDAMT                                                
         B     POST305                                                          
*                                                                               
         USING SCIELD,RE                                  NMAL                  
POST312  CLI   SCITYPE,SCITCPAD   CHECK JOB TRX CREDITOR DETAIL                 
         BNE   POST305                                                          
         MVC   THISCRDE,SCIELD     SAVE ELEMENT                                 
         B     POST305                                                          
*                                                                               
POST315  AP    TMPCDAMT,TRCSAMNT                                                
         B     POST305                                                          
*                                                                               
         USING ACOTHERD,RE                                                      
POST320  MVC   THISOTH,ACOTNUM                                                  
         B     POST305                                                          
         DROP  RE                                                               
*                                                                               
         USING ACNOD,RE                                                         
POST330  ZIC   R3,ACNOLEN                                                       
         SH    R3,=H'3'                                                         
         STC   R3,THISOTH2+L'THISOTH2-1                                         
         EX    R3,*+8                                                           
         B     POST305                                                          
         MVC   THISOTH2(0),ACNO                                                 
         DROP  RE                                                               
*                                                                               
         USING SPAELD,RE                                                        
POST340  CLI   SPATYPE,SPATCCST    IS IT A CLIENT COSTING ACCOUNT?              
         BNE   POST305             NO - GET NEXT ELEM                           
         MVC   THISCST(1),COMPANY                                               
         MVC   THISCST+1(14),SPAAULA                                            
         B     POST305                                                          
         DROP  RE                                                               
*                                                                               
         USING ACPERSD,RE                                                       
POST350  SR    R1,R1                                                            
         IC    R1,ACPSLEN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   THISHOUR(0),ACPERSD                                              
         ZAP   PK16,THISAMNT       AMOUNT ALLOCATED                             
         MP    PK16,=P'1000'                                                    
         DP    PK16,TRNSRTE        AMOUNT/RATE                                  
         ZAP   DUB,PK16(12)        GIVES HOURS                                  
         DP    DUB,=P'25'          GET TO NEAREST QTR HOUR                      
         SRP   DUB(6),64-1,5                                                    
         MP    DUB(6),=P'25'                                                    
         ZAP   PK16,DUB(6)                                                      
         ZAP   DUB,PK16                                                         
         CVB   R1,DUB                                                           
         ST    R1,THISTIME         SAVE NEW HOURS                               
         ST    R1,FULL                                                          
         MVC   TRNSHRS,FULL                                                     
         MP    PK16,TRNSRTE        RATE * HOURS                                 
         SRP   PK16,64-2,5                                                      
         ZAP   DUB,PK16                                                         
         CVB   R1,DUB                                                           
         ST    R1,TRNSAMT          NEW MARKED AMOUNT                            
         CVD   R1,DUB                                                           
         ZAP   THISAMNT,DUB                                                     
         DROP  RE                                                               
         LA    R3,THISHOUR                                                      
         USING ACPERSD,R3                                                       
         L     R1,THISTIME                                                      
         CVD   R1,DUB                                                           
         ZAP   ACPSHOUR,DUB                                                     
         B     POST305                                                          
*                                                                               
         USING UNPELD,RE                                                        
POST360  SR    R1,R1                                                            
         IC    R1,UNPLN                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   THISUNPR(0),UNPELD  SAVE ORIGINAL ELEMENT                        
         B     POST305                                                          
*                                                                               
         USING TRSDESCD,RE                                                      
POST370  ZIC   R1,TRSDLEN          SAVE SUBSIDIARY POSTINGS                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   THISTRSD(0),TRSDEL                                               
         CLC   TRSDACCS(2),=C'SK'    MEMO SK                                    
         BNE   POST378                                                          
         CLC   KEY+18(2),=C'1R'      ONLY IF CONTRA IS 1R                       
         BNE   POST305                                                          
         LR    R3,RE                                                            
         GOTO1 =A(MEMOSK),DMCB,(0,(RC)),RR=PRELOC                               
         CLI   ERRNUM,X'FF'                                                     
         BNE   ERRXIT                                                           
         LR    RE,R3                                                            
         B     POST305                                                          
*                                                                               
POST378  CLC   TRSDACCS(2),=C'SI'    MEMO SI                                    
         BNE   POST305                                                          
         CLC   KEY+18(2),=C'1R'      ONLY IF CONTRA IS 1R                       
         BNE   POST305                                                          
         LR    R1,RE                                                            
         BAS   RE,MEMOSI                                                        
         CLI   ERRNUM,X'FF'                                                     
         BNE   ERRXIT                                                           
         LR    RE,R1                                                            
         B     POST305                                                          
*                                                                               
         USING TRSELD,RE                                                        
POST380  MVC   THATSTA2,TRSSTAT2                                                
         NI    THATSTA2,X'0D'        ONLY WANT TIMESHEET INFO                   
         MVC   THATSTA3,TRSSTAT3                                                
         NI    THATSTA3,X'FF'-TRSSNBIL                                          
         MVC   THATDATE,TRSDATE                                                 
         B     POST305                                                          
*                                                                               
         USING ANOELD,RE                                                        
POST390  CLI   ANOTYPE,ANOTCLI     CLIENT OFFICE?                               
         BNE   POST305             NO - GET NEXT ELEM                           
         MVC   THISOFF,ANOOFFC     SAVE OFFICE CODE                             
         B     POST305                                                          
*                                                                               
         USING PXDELD,RE                                                        
POST400  CLI   PXDTYPE,PXDTORG     IS THIS AN ORIGINAL JOB ELEMENT?             
         BNE   POST305             NO, GET NEXT                                 
         MVC   THISORGJ,PXDFRTO    YES, SAVE THE ACCOUNT                        
         MVC   THISORGW,PXDFRTOW       THE WORKCODE                             
         MVC   THISORGD,PXDDATE        AND THE DATE                             
         CLI   PXDLN,PXDLN2Q           IF LONG ENOUGH                           
         BL    POST305                                                          
         MVC   THISORGB,PXDOBAT        SAVE THE BATCH                           
         MVC   THISORGS,PXDOSEQ        AND SEQUENCE ALSO                        
         B     POST305                                                          
*                                                                               
         USING PAKELD,RE                                                        
POST410  SR    R1,R1                                                            
         IC    R1,PAKLN                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   THISPAKL(0),PAKELD  SAVE PAYABLE ELEMENT                         
         B     POST305                                                          
*                                                                               
         USING PIDELD,RE                                                        
POST420  MVC   THISPID,PIDNO                                                    
         B     POST305                                                          
*                                                              NMAL             
         USING ASKELD,RE                                       NMAL             
POST430  MVC   THISASK,ASKKEY                                  NMAL             
         B     POST305                                         NMAL             
*                                                                               
POST440  DS    0H                                                               
         OC    THISASK,THISASK     DO WE HAVE ASKEL PRESENT    NMAL             
         BNZ   POST450                                         NMAL             
         CLI   TRNSTYPE,X'22'                                  NMAL             
         BE    POST450                                         NMAL             
         MVC   THISASK,KEY                                     NMAL             
POST450  DS    0H                                                               
         OC    THISORGJ,THISORGJ   DO WE HAVE AN ORIGINAL ELEMENT?              
         BNZ   POST460             YES                                          
         CLI   TRNSTYPE,X'22'      IS THIS A TYPE 34?                           
         BE    POST460             YES, DON'T CREATE THIS ELEMENT               
         MVC   THISORGJ,FROMJOBA   NO, SAVE THE FROM JOB                        
         MVC   THISORGW,THISANAL       THE WORKCODE                             
         MVC   THISORGD,TODAYP         TODAY'S DATE                             
         MVC   THISORGB,THISBTCH       THE BATCH REFERENCE                      
         MVC   THISORGS,THISSEQ        AND SEQUENCE NUMBER                      
*                                                                               
POST460  MVC   SVKEY,KEY           SAVE KEY                                     
         OC    THISCST,THISCST                                                  
         BZ    POST480                                                          
         MVC   KEY,SPACES          READ FOR COSTING ACCOUNT NAME                
         MVC   KEY(15),THISCST                                                  
         L     R2,ACURLIN          R2=I/P FIELD IN CASE OF ERROR                
         BAS   RE,GETACC                                                        
         BAS   RE,CHECKACC                                                      
         MVC   THISCSTN,ACCTNAME   SAVE NAME                                    
         MVC   KEY,SVKEY           RESET KEY                                    
*                                                                               
POST480  MVC   CREDITA,KEY+17                                                   
*        L     R2,ACURLIN          R2=FIRST I/P FIELD IN CASE OF ERROR          
         GOTO1 =A(TESTMS),DMCB,(RC),RR=PRELOC                                   
*                                                                               
         MVC   KEY,SVKEY           RESET KEY AGAIN                              
*                                                                               
         L     RF,=A(BLDTRNS)                                                   
         A     RF,PRELOC                                                        
         GOTO1 (RF),DMCB,(RC)      BUILD & POST TRANSACTIONS                    
*                                                                               
         L     R2,ACURLIN          R2=FIRST I/P FIELD IN CASE OF ERROR          
         BAS   RE,PUTDAY           ADD TO ACCDAY                                
         XC    WORK,WORK                                                        
         MVC   WORK(6),THISREF                                                  
         L     R1,DMCB+8                                                        
         MVC   WORK+10(4),0(R1)                                                 
         ZAP   TRANSAMT,THISAMNT                                                
         BAS   RE,ADTWA1           AND TWA1                                     
         AP    TOTAMNT,THISAMNT                                                 
*                                                                               
         USING JGBLOCK,R3                                                       
         CLI   JGRPUSD,C'Y'                                                     
         BNE   POST500                                                          
         L     R3,CURRJGRP         LOOP HERE FOR THE OTHER JOBS                 
         CLC   JGBFTPTR,=X'FFFF'                                                
         BE    POST490                                                          
         SR    R1,R1                                                            
         ICM   R1,3,JGBFTPTR                                                    
         A     R1,AFRSTNOD                                                      
         LR    R3,R1                                                            
         ST    R3,CURRJGRP                                                      
         B     POST220                                                          
         DROP  R3                                                               
*                                                                               
         USING LINED,R3                                                         
POST490  BAS   RE,RESTLINE                                                      
*                                                                               
POST500  LA    R4,TRNSNEXT         BUMP TO NEXT LINE                            
         LA    R5,ACCTNEXT                                                      
         L     R3,ACURLIN                                                       
         LA    R3,LINNEXT                                                       
         ST    R3,ACURLIN                                                       
         SR    R3,R3                                                            
         IC    R3,LINENO                                                        
         LA    R3,1(R3)                                                         
         STC   R3,LINENO           SAVE LINENO FOR MORE POSTINGS                
*                                                                               
         USING SYSFACD,RF                                                       
         L     RF,ASYSFAC                                                       
         L     RF,VSSB                                                          
         USING SSBD,RF                                                          
         L     RF,SSBTKADR                                                      
         USING TCBD,RF                                                          
         SR    RE,RE                                                            
         ICM   RE,7,TCBIOCNT                                                    
         L     RF,=A(MNIOCNT)      SEE IF IO BREAKS OUR LIMIT                   
         CR    RE,RF                                                            
         BL    POST550                                                          
         CLI   0(R4),X'FF'         END OF TABLE                                 
         BE    POST550                                                          
         CLI   LINENO,10           OR FINISHED LAST LINE                        
         BH    POST550                                                          
         BAS   RE,OUT2TMPS         WRITE OUT TO TEMPSTR                         
         LA    R2,CONACTH                                                       
*                                                                               
         CLI   GBCNTR,2            DID GOBACK TWICE ALREADY                     
         BNL   POST530                                                          
         SR    RE,RE                                                            
         IC    RE,GBCNTR           BUMP UP GOBACK COUNTER                       
         LA    RE,1(RE)                                                         
         STC   RE,GBCNTR                                                        
         MVC   BASSRV,SPACES                                                    
         MVC   BASSRV(7),=C'=GOBACK'                                            
         OI    BASSRVH+6,X'80'     TRANSMIT                                     
*                                                                               
POST530  MVC   FVMSGNO,=AL2(AI$HENXT)                                           
         MVI   FVOMTYP,C'I'                                                     
         MVI   FNDX,0                                                           
         B     EXIT                                                             
         DROP  RF                                                               
*                                                                               
POST550  LR    R2,R3                                                            
         DROP  R3                                                               
         L     R3,AJOBGRP                                                       
         LA    R3,4(R3)                                                         
         ST    R3,AJOBGRP                                                       
*                                                                               
         CLI   0(R4),X'FF'         END-OF-TABLE                                 
         BNE   POST100                                                          
*                                                                               
         LR    RE,RA               CLEAR 8K OF SPACES                           
         AHI   RE,X'2A00'                                                       
         LHI   RF,MAXSIZE                                                       
         XCEF                                                                   
*                                                                               
         XC    JOBITOT,JOBITOT     YES - DISPLAY TOTAL POSTED SO FAR            
         MVC   JOBITOT(12),=C'Total so far'                                     
         LA    R2,JOBITOT+16                                                    
         EDIT  (P6,TOTAMNT),(11,0(R2)),2,MINUS=YES                              
         OI    JOBITOTH+6,X'80'                                                 
         CLI   LKEY,X'FF'          E-O-F                                        
         BNE   DISPLAY             NO - DISPLAY NEXT                            
         MVI   MODE,X'FF'          YES - DONE                                   
         NI    JOBTJOBH+1,X'FF'-X'20'                                           
         OI    JOBTJOBH+6,X'80'                                                 
         NI    JOBFJOBH+1,X'FF'-X'20'                                           
         OI    JOBFJOBH+6,X'80'                                                 
         B     ALLEXIT                                                          
*                                                                               
MNIOCNT  EQU   80000               MINIMUM I/O BEFORE MULTIPLE                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
* READ IN ACCTAB FROM TEMPSTR                                        *          
*--------------------------------------------------------------------*          
INFRTMPS NTR1                                                                   
         L     R5,AACCTAB                                                       
         ST    R5,ATIA                                                          
         MVI   DMCB+8,1            PAGE 1 OF TEMPSTR                            
         MVI   DMCB+9,0                                                         
         MVC   DMCB+10(2),TWATRM                                                
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),=Y(10*ACCTLNQ)                                        
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'TEMPSTR',,ATIA,0                      
         CLI   DMCB+8,0                                                         
         BE    EXIT                                                             
         DC    H'0'                READ ERROR                                   
*--------------------------------------------------------------------*          
* WRITE OUT ACCTAB TO TEMPSTR                                        *          
*--------------------------------------------------------------------*          
OUT2TMPS NTR1                                                                   
         L     R5,AACCTAB                                                       
         ST    R5,ATIA                                                          
         MVI   DMCB+8,1            PAGE 1 OF TEMPSTR                            
         MVI   DMCB+9,0                                                         
         MVC   DMCB+10(2),TWATRM                                                
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),=Y(10*ACCTLNQ)                                        
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'TEMPSTR',,ATIA,0                       
         CLI   DMCB+8,0                                                         
         BE    EXIT                                                             
         DC    H'0'                WRITE ERROR                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
* ALIGN POINTERS BASED ON LINE NUMBER                                *          
*--------------------------------------------------------------------*          
         USING LINED,R6                                                         
ALGNPTRS NTR1                                                                   
         SR    R1,R1                                                            
         IC    R1,LINENO                                                        
         BCTR  R1,0                                                             
         LTR   R1,R1                                                            
         BZ    EXIT                STARTING AT LINE 1                           
         L     R3,AJOBGRP                                                       
         L     R6,ACURLIN                                                       
*                                                                               
ALPTR10  LA    R3,4(R3)            BUMP TO NEXT LINE                            
         LA    R4,TRNSNEXT                                                      
         LA    R5,ACCTNEXT                                                      
         LA    R6,LINNEXT                                                       
         BCT   R1,ALPTR10                                                       
*                                                                               
         ST    R3,AJOBGRP                                                       
         ST    R6,ACURLIN                                                       
         XIT1  REGS=(R4,R5)                                                     
         DROP  R4,R6                                                            
         EJECT                                                                  
*--------------------------------------------------------------------*          
* RESTORE LINE AS IT WAS                                             *          
*--------------------------------------------------------------------*          
         USING LINED,R3                                                         
RESTLINE DS    0H                                                               
         L     R3,ACURLIN                                                       
         MVC   LINIJOB,TJOBGRPC                                                 
         MVC   LINAMNT,TAMOUNT                                                  
         MVC   LINWORK,TWC                                                      
         MVC   LINSTAT(3),TSTATUS                                               
         OI    LINIJOBH+6,X'80'                                                 
         OI    LINAMNTH+6,X'80'                                                 
         OI    LINDISPH+6,X'80'                                                 
         BR    RE                                                               
         EJECT                                                                  
*--------------------------------------------------------------------*          
* WILL WE GO OVER 250 ITEMS WITH THIS GROUP?                         *          
*        CC = EQUAL IF <= 250 ITEMS AFTER THIS GROUP                 *          
*        CC = NOT EQUAL IF > 250 ITEMS AFTER THIS GROUP              *          
*--------------------------------------------------------------------*          
         USING LSTTABD,R2                                                       
         USING JGBLOCK,R3                                                       
OVER250  NTR1                                                                   
         LA    R2,CSLSTCUR                                                      
         SR    R0,R0                                                            
         ICM   R0,3,LSTBITMA       ITEMS ENTERED                                
         MVC   BOHALF1,LSTBDELI                                                 
         SH    R0,BOHALF1          MINUS ONES DELETED                           
*                                                                               
         L     R3,CURRJGRP                                                      
         LA    R4,1                                                             
O250_10  CLC   JGBFTPTR,=X'FFFF'   END OF LIST?                                 
         BE    O250_50                                                          
         SR    R1,R1                                                            
         ICM   R1,3,JGBFTPTR       BUMP TO NEXT ONE                             
         A     R1,AFRSTNOD                                                      
         LR    R3,R1                                                            
         LA    R4,1(R4)                                                         
         B     O250_10                                                          
*                                                                               
O250_50  AR    R0,R4                                                            
         CLM   R0,3,CSBMAXIT       HAVE WE EXCEEDED MAX?                        
         BNH   EQXIT                                                            
         B     NEQXIT                                                           
         DROP  R2                                                               
         EJECT                                                                  
*--------------------------------------------------------------------*          
* PREPARE JOB GROUP AS USER INPUT                                    *          
*--------------------------------------------------------------------*          
         USING JGBLOCK,R3                                                       
         USING TRNSD,R4                                                         
PREPJGRP NTR1                                                                   
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),=C'SJ'                                                  
         MVC   KEY+3(L'JGBACCT),JGBACCT                                         
         BAS   RE,GETACC                                                        
         MVC   ACCTTOJB,ACCTNUM                                                 
         MVC   ACCTTOJN,ACCTNAME                                                
*                                                                               
         ZAP   THISAMNT,JGBAMNT                                                 
         L     R2,ACURLIN                                                       
         LA    R5,JGBACCT          POINT TO CLIENT                              
         LA    R6,3                3 ITEMS, CLI / PROD / JOB                    
         USING LINED,R2                                                         
         MVC   LINIJOB,SPACES                                                   
         LA    RF,LINIJOB          PUT IT ON SCREEN FIELD                       
         LA    RE,CLILNGTH                                                      
         SR    R1,R1                                                            
         IC    R1,0(RE)                                                         
PRJG100  BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),0(R5)                                                    
         LA    R1,1(R1)                                                         
         LA    RF,0(R1,RF)         BUMP POSITION                                
         LA    R5,0(R1,R5)                                                      
         MVI   0(RF),C'/'          ADD SEPERATOR                                
         LA    RF,1(RF)                                                         
         SR    R0,R0                                                            
         IC    R0,0(RE)                                                         
         LA    RE,1(RE)                                                         
         IC    R1,0(RE)                                                         
         SR    R1,R0                                                            
         BCT   R6,PRJG100                                                       
         BCTR  RF,0                                                             
         MVI   0(RF),C' '          REMOVE LAST '/'                              
         LA    R0,LINIJOB                                                       
         SR    RF,R0                                                            
         STC   RF,LINIJOBH+5                                                    
         OI    LINIJOBH+6,X'80'    TRANSMIT                                     
         EDIT  JGBAMNT,(11,LINAMNT),2,FLOAT=-                                   
         OI    LINAMNTH+6,X'80'                                                 
         MVC   LINWORK,TRNSWC                                                   
         MVC   LINSTAT(3),=C'   '                                               
         CLI   TRNSTS,C'N'                                                      
         BNE   *+10                                                             
         MVC   LINSTAT(3),=C'N/C'                                               
         OI    LINDISPH+6,X'80'                                                 
*                                                                               
         LA    R2,LINIJOBH                                                      
         BAS   RE,JOBVAL                                                        
*                                                                               
         NI    JGRPLST1,X'FF'-JGRPELST                                          
         CLC   JGBFTPTR,=X'FFFF'                                                
         BNE   EXIT                                                             
         OI    JGRPLST1,JGRPELST                                                
*                                                                               
         B     EXIT                                                             
         DROP  R2,R4                                                            
         EJECT                                                                  
*              ROUTINE TO READ MEMO SI ACCOUNT                                  
         SPACE 1                                                                
         USING LINED,R3                                                         
         USING ACCTD,R5                                                         
         USING TRSDESCD,RE                                                      
MEMOSI   NTR1                                                                   
         OC    ACCTSI,ACCTSI                                                    
         BNZ   MEMOSIOK            ALREADY HAVE SI                              
         LA    RE,THISTRSD                                                      
         MVC   SKKEY,SPACES                                                     
         MVC   SKKEY(1),COMPANY                                                 
         ZIC   R1,TRSDLEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SKKEY+1(0),TRSDACCS                                              
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',SKKEY,SKKEY                  
         CLI   8(R1),0                                                          
         BNE   MEMOSINO            CAN'T FIND                                   
         LA    R1,SKIOAREA                                                      
         SR    RE,RE                                                            
*                                                                               
MEMOSI04 CLI   0(R1),0             AND EXTRACT NAME                             
         BNE   *+6                                                              
         DC    H'0'                NO NAME ELEMENT                              
         CLI   0(R1),X'20'                                                      
         BE    MEMOSI05                                                         
         IC    RE,1(R1)                                                         
         AR    R1,RE                                                            
         B     MEMOSI04                                                         
*                                                                               
MEMOSI05 MVC   ACCTSINM,SPACES                                                  
         IC    RE,1(R1)                                                         
         SH    RE,=H'3'                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   ACCTSINM(0),2(R1)   CONTRA NAME                                  
         MVC   ACCTSI,SKKEY        AND ACCOUNT                                  
         LA    R3,SKKEY                                                         
         GOTO1 SIVAL,DMCB,(R3),(R5)                                             
*                                                                               
MEMOSIOK MVI   ERRNUM,X'FF'        SET RETURN OK                                
         B     CURSIT                                                           
*                                                                               
MEMOSINO DS    0H                  PUT OUT ERROR MESSAGE                        
         MVI   ERRNUM,X'FE'                                                     
         MVC   MSG,SPACES                                                       
         MVC   MSG(L'CNTRAMSG),CNTRAMSG                                         
         OI    CONHEADH+6,X'80'    AND TRANSMIT IT                              
         LA    R2,LINIJOBH         CURSOR TO INPUT FIELD                        
         LTR   RB,RB               RETURN CC NOT EQUAL                          
         B     CURSIT                                                           
         DROP  R3,R5,RE                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY UP TO 10 TRANSACTIONS ON SCREEN                             *         
***********************************************************************         
         SPACE 1                                                                
DISPLAY  CLI   MODE,0              FIRST TIME - CLEAR ALL FIELDS                
         BNE   DISP50                                                           
         XC    LKEY,LKEY                                                        
         TWAXC JOBILINH,PROT=Y                                                  
         OI    JOBFJOBH+(FVATRB-FVIHDR),FVAMODF                                 
         ZAP   TOTAMNT,=P'0'                                                    
*                                                                               
         LA    RF,JOBILINH                                                      
         LA    RE,JOBITOTH-9                                                    
DISP10   OI    6(RF),X'80'                                                      
         NI    1(RF),X'FF'-X'20'                                                
         SR    R1,R1                                                            
         ICM   R1,1,0(RF)                                                       
         BZ    DISP100                                                          
         AR    RF,R1                                                            
         CR    RF,RE                                                            
         BH    DISP100                                                          
         B     DISP10                                                           
*                                                                               
DISP50   LA    RF,JOBITOTH-9                                                    
         TWAXC JOBILINH,(RF),PROT=Y                                             
*                                                                               
DISP100  LA    R2,TRNSTAB                                                       
         USING TRNSD,R2            R2=A(TRANSACTION SAVE KEY TABLE)             
         LA    R3,JOBILINH                                                      
         USING LINED,R3            R3=A(TWA LINE)                               
         LA    R4,KEY                                                           
         USING ACKEYD,R4           R4=A(ACCOUNT KEY)                            
         LA    R5,ACRECORD                                                      
         USING TRANSD,R5           R5=A(TRANSACTION ELEMENT)                    
         MVI   COUNT,0                                                          
         MVC   KEY,LKEY                                                         
         CLI   KEY,0                                                            
         BE    *+12                                                             
         BAS   RE,MYHIGH                                                        
         B     DISP120                                                          
         MVI   WORK,X'F0'          BUILD FIRST KEY                              
         BAS   RE,KEYBUILD                                                      
*                                                                               
DISP110  BAS   RE,MYHIGH                                                        
         B     *+8                                                              
*                                                                               
DISP120  BAS   RE,MYSEQ                                                         
*                                                                               
* CLEAR ALL PROTECTED FIELD/MARKED LINES DUE TO PENDING STATUS IF SET           
*                                                                               
         NI    LINAMNTH+1,X'FF'-X'20'    UNPROTECT AMOUNT FIELD                 
         OI    LINAMNTH+6,X'80'          TRANSMIT                               
         MVC   LINIJOB,SPACES                                                   
         NI    LINIJOBH+1,X'FF'-X'20'    PROTECT JOB FIELD                      
         OI    LINIJOBH+6,X'80'          TRANSMIT                               
*                                                                               
         XC    THISAMNT,THISAMNT                                                
         XC    THISTIME,THISTIME                                                
         XC    THISUNPR,THISUNPR                                                
         ZAP   TRNSRTE,=P'0'                                                    
         XC    TRNSHRS,TRNSHRS                                                  
         ZAP   THISRATE,=P'0'                                                   
         XC    TRNSTS,TRNSTS                                                    
*                                                                               
         CLI   TRNSEL,TRNSELQ                                                   
         BNE   DISP120                                                          
         TM    TRNSSTAT,X'20'      IGNORE OFFSETS                               
         BO    DISP120                                                          
         CLI   TRNSTYPE,47         TEST FOR BATCH TYPE 47                       
         BE    DISP120             YES-DO NOT ALLOW TRANSFER                    
         CLI   TRNSTYPE,27         TEST FOR BATCH TYPE 27                       
         BE    DISP120             YES-DO NOT ALLOW TRANSFER                    
         CLI   TRNSTYPE,41         TEST FOR BATCH TYPE 41                       
         BE    DISP120             YES-DO NOT ALLOW TRANSFER                    
         TM    ACSTATUS,X'40'      IGNORE DRAFT                                 
         BO    DISP120                                                          
         OC    ACDTPEEL,ACDTPEEL   IGNORE PEELED ITEMS                          
         BNZ   DISP120                                                          
         CLC   ACKEYWRK,=C'**'     IGNORE ORDERS                                
         BE    DISP120                                                          
         BAS   RE,STRAMTO          STRIP AMOUNT OF PARTIALS OFF                 
         BNE   DISP120             ZERO AMOUNT, SKIP                            
         CLI   TRNSTYPE,34         DO NOT INCLUDE TYPE 34 TRANSFERS             
         BNE   *+12                                                             
         BAS   RE,XCLTRNS          EXCLUDE "TRANSFER TO"                        
         BNE   DISP120                                                          
*                                                                               
         CLC   ACKEYACC,FROMJOBA   STILL AT SAME JOB?                           
         BNE   DISP155             NO                                           
         MVC   SVKEY,KEY                                                        
*                                                                               
DISP130  BAS   RE,MYSEQ                                                         
         CLC   SVKEY(41),KEY                                                    
         BNE   DISP140                                                          
         LA    R4,KEY                                                           
         USING ACKEYD,R4           R4=A(ACCOUNT KEY)                            
         LA    R5,ACRECORD                                                      
         USING TRANSD,R5           R5=A(TRANSACTION ELEMENT)                    
         CLI   TRNSTYPE,34                                                      
         BNE   DISP130                                                          
         TM    ACSTATUS,X'40'      IGNORE ITEMS W/PENDING DRAFT 34'S            
         BNO   DISP130                                                          
*                                                                               
         LA    R1,CSLSTCUR                                                      
*        CLC   TRNSBTCH,LSTBREFN-LSTTABD(R1)                                    
*        BE    DISP130             IGNORE IF SAME BATCH                         
*                                                                               
         BAS   RE,CHKMATSQ         CHECK FOR MATCHING SEQUENCE NUMBERS          
         BH    DISP130                                                          
*                                                                               
         MVC   WORK,SPACES                                                      
         MVC   WORK(24),=CL24'PENDING(BBBBBB-MMMDD/YY)'                         
         MVC   WORK+8(6),TRNSBTCH                                               
         LR    RF,R5                                                            
         SR    R1,R1                                                            
         IC    R1,1(RF)                                                         
         AR    RF,R1                                                            
         CLI   0(RF),TRSELQ        X'60' ELEMENT                                
         BNE   *-10                                                             
         USING TRSELD,RF                                                        
         GOTO1 DATCON,DMCB,(2,TRSEFDT),(17,WORK+15)                             
         DROP  RF                                                               
         OI    LINAMNTH+1,X'20'    PROTECT AMOUNT FIELD                         
         OI    LINAMNTH+6,X'80'                                                 
         MVC   LINIJOB,WORK                                                     
         OI    LINIJOBH+1,X'20'    PROTECT JOB FIELD                            
         OI    LINIJOBH+6,X'80'                                                 
*                                                                               
DISP140  MVC   KEY,SVKEY                                                        
         BAS   RE,MYHIGH                                                        
         BAS   RE,STRAMTO          STRIP AMOUNT OF PARTIALS OFF                 
         BNE   DISP120                                                          
*                                                                               
DISP150  MVC   LKEY,KEY            SAVE LATEST KEY READ                         
*                                  CHECK IF RECORD OBEYS FILTERS                
         CLC   ACKEYACC,FROMJOBA   SAME FROM-JOB                                
         BE    DISP160                                                          
*                                                                               
DISP155  MVI   LKEY,X'FF'          NO - DONE                                    
         B     ALLEXIT                                                          
*                                                                               
DISP160  OC    WORKOPT,WORKOPT      WORK CODE FILTER                            
         BZ    DISP170                                                          
         CLC   ACKEYWRK,WORKOPT                                                 
         BE    DISP170                                                          
         MVI   LKEY,X'FF'                                                       
         B     ALLEXIT                                                          
*                                                                               
DISP170  CLC   ACKEYWRK,=C'99'                                                  
         BL    DISP180                                                          
         MVI   LKEY,X'FF'                                                       
         B     ALLEXIT                                                          
*                                                                               
DISP180  OC    CRACA,CRACA         CREDIT A/C FILTER                            
         BZ    DISP190                                                          
         CLC   ACKEYCON,CRACA                                                   
         BE    DISP190                                                          
         BL    *+16                                                             
         MVI   WORK,X'78'                                                       
         BAS   RE,KEYBUILD                                                      
         B     DISP110                                                          
         MVI   WORK,X'70'                                                       
         BAS   RE,KEYBUILD                                                      
         B     DISP110                                                          
*                                                                               
DISP190  OC    SDATOPT,SDATOPT     START DATE OPTION                            
         BZ    DISP200                                                          
         CLC   ACKEYDTE,SDATOPT                                                 
         BNL   DISP200                                                          
         BL    *+16                                                             
         MVI   WORK,X'34'                                                       
         BAS   RE,KEYBUILD                                                      
         B     DISP110                                                          
         MVI   WORK,X'30'                                                       
         BAS   RE,KEYBUILD                                                      
         B     DISP110                                                          
*                                                                               
DISP200  OC    DATOPT,DATOPT       DATE FILTER                                  
         BZ    DISP210                                                          
         CLC   ACKEYDTE,DATOPT                                                  
         BE    DISP210                                                          
         BL    *+16                                                             
         MVI   WORK,X'34'                                                       
         BAS   RE,KEYBUILD                                                      
         B     DISP110                                                          
         MVI   WORK,X'30'                                                       
         BAS   RE,KEYBUILD                                                      
         B     DISP110                                                          
*                                                                               
DISP210  OC    EDATOPT,EDATOPT     END DATE OPTION                              
         BZ    DISP220                                                          
         CLC   ACKEYDTE,EDATOPT                                                 
         BNH   DISP220                                                          
         MVI   WORK,X'34'                                                       
         BAS   RE,KEYBUILD                                                      
         B     DISP110                                                          
*                                                                               
DISP220  OC    SREFOPT,SREFOPT     START REF OPTION                             
         BZ    DISP230                                                          
         CLC   ACKEYREF,SREFOPT                                                 
         BNL   DISP230                                                          
         BL    *+16                                                             
         MVI   WORK,X'12'                                                       
         BAS   RE,KEYBUILD                                                      
         B     DISP110                                                          
         MVI   WORK,X'10'                                                       
         BAS   RE,KEYBUILD                                                      
         B     DISP110                                                          
*                                                                               
DISP230  OC    REFOPT,REFOPT       REF FILTER                                   
         BZ    DISP240                                                          
         CLC   REFOPT,=CL6'Y'      IS THIS REF=Y ?                              
         BE    DISP240             YES                                          
         CLC   ACKEYREF,REFOPT                                                  
         BE    DISP240                                                          
         BL    *+16                                                             
         MVI   WORK,X'12'                                                       
         BAS   RE,KEYBUILD                                                      
         B     DISP110                                                          
         MVI   WORK,X'10'                                                       
         BAS   RE,KEYBUILD                                                      
         B     DISP110                                                          
*                                                                               
DISP240  MVC   THISSUB,ACKEYREF    DEFAULT                                      
         LA    RF,ACRECORD                                                      
*                                                                               
DISP250  CLI   0(RF),0                                                          
         BE    DISP300                                                          
         CLI   0(RF),X'23'                                                      
         BE    DISP270                                                          
         CLI   0(RF),X'40'                                                      
         BE    DISP280                                                          
         CLI   0(RF),UNPELQ        UNIT/PRICE ELEMENT                           
         BE    DISP290                                                          
*                                                                               
DISP260  SR    RE,RE                                                            
         IC    RE,1(RF)                                                         
         AR    RF,RE                                                            
         B     DISP250                                                          
*                                                                               
         USING ACOTHERD,RF                                                      
DISP270  MVC   THISSUB,ACOTNUM                                                  
         B     DISP260                                                          
*                                                                               
         USING ACPERSD,RF                                                       
DISP280  CLI   COSTOPT,C'Y'         IF DIRECT COSTS ONLY OPTION                 
         BE    DISP120             SKIP THOSE WITH LABOR                        
         ZAP   DUB,ACPSHOUR                                                     
         CVB   R1,DUB                                                           
         ST    R1,THISTIME        SAVE THE TIME                                 
         ZAP   THISRATE,ACPSRATE   AND RATE                                     
         B     DISP260                                                          
*                                                                               
DISP290  MVI   THISUNPR,X'FF'      FLAG TRAN AS HAVING A UNIT/PRICE             
         OC    OPTPCT,OPTPCT       TRANSFER A PERCENT                           
         BNZ   DISP120             SKIP THOSE WITH UNITS                        
         B     DISP260                                                          
*                                                                               
DISP300  CLI   SUBOPT,C'Y'         EXTRA REFERENCE OPTION                       
         BNE   DISP310                                                          
         OC    SUBREF,SUBREF                                                    
         BZ    DISP310                                                          
         CLC   SUBREF,THISSUB                                                   
         BNE   DISP120                                                          
*                                                                               
DISP310  CLI   ACRECORD,X'44'      MUST BE A TRANSACTION                        
         BNE   DISP120                                                          
         ZAP   DUB,TRNSAMNT                                                     
         CVB   R1,DUB                                                           
         STCM  R1,15,THISAMNT      SAVE THE TOTAL AMOUNT                        
         TM    TRNSSTAT,X'80'      AND NOT A CREDIT                             
         BZ    DISP120                                                          
         MVC   THISSTAT,TRNSSTAT   SAVE STATUS                                  
*                                                                               
         OC    MOSOPT,MOSOPT       MOS FILTER                                   
         BZ    DISP320                                                          
         GOTO1 VCONVMOS,DMCB,(R5),THISMOS                                       
         CLC   THISMOS,MOSOPT                                                   
         BNE   DISP120                                                          
*                                                                               
DISP320  CLI   SAVETYPE,C'C'       LOOK FOR CLIENT BILLING                      
         BE    DISP340                                                          
         OC    THISAMNT,THISAMNT   ANYTHING TO WORK WITH?                       
         BZ    DISP120             NO, SKIP IT                                  
         CLI   BILLOPT,0           ANY BILLING FILTER?                          
         BE    DISP330             NO, SHOW IT                                  
         CLI   BILLOPT,C'Y'        SHOW BILLED ONLY?                            
         BE    DISP330             YES, TEST IT LATER                           
         OC    ACDTUSED,ACDTUSED   NO, MUST WANT UNBILLED                       
         BNZ   DISP120             BILLED, SKIP IT                              
         B     DISP340             LOOK FURTHER                                 
*                                                                               
DISP330  OC    ACDTUSED,ACDTUSED   NON-CLIENT FULLY BILLED?                     
         BNZ   DISP420             YES, NO NEED TO LOOK AT DETAILS              
*                                                                               
DISP340  SR    RE,RE               SUBTRACT PREVIOUSLY BILLED                   
         LA    RF,ACRECORD         OR ALLOCATED                                 
*                                                                               
DISP350  CLI   0(RF),0                                                          
         BE    DISP400                                                          
         CLI   0(RF),X'4B'                                                      
         BE    DISP370                                                          
         CLI   0(RF),X'77'                                                      
         BE    DISP380                                                          
*                                                                               
DISP360  IC    RE,1(RF)                                                         
         AR    RF,RE                                                            
         B     DISP350                                                          
*                                                                               
         USING TRBDETD,RF                                                       
DISP370  OC    TRBDNO,TRBDNO       IS THIS ALLOCATED/BILLED?                    
         BZ    DISP360             NO, IGNORE                                   
         ICM   R1,15,TRBDAMNT                                                   
         ICM   R6,15,THISAMNT                                                   
         SR    R6,R1               REDUCE CASH BY AMOUNTS BILLED                
         STCM  R6,15,THISAMNT                                                   
         CLI   TRBDLEN,X'20'                                                    
         BL    DISP360                                                          
         ICM   R1,15,TRBDHRS                                                    
         L     R6,THISTIME                                                      
         SR    R6,R1               REDUCE HOURS BY AMOUNTS BILLED               
         ST    R6,THISTIME                                                      
         B     DISP360                                                          
*                                                                               
         USING PTAELD,RF                                                        
DISP380  CLI   PTATYPE,0           IS THIS UNUSED?                              
         BE    DISP360             YES                                          
*                                                                               
DISP390  ZAP   DUB,PTANET          REDUCE CASH BY AMOUNTS                       
         CVB   R1,DUB                                                           
         ICM   R6,15,THISAMNT                                                   
         SR    R6,R1                                                            
         STCM  R6,15,THISAMNT                                                   
*                                                                               
         LH    R1,PTAHOURS                                                      
         L     R6,THISTIME                                                      
         SR    R6,R1               REDUCE HOURS BY AMOUNTS BILLED               
         ST    R6,THISTIME                                                      
         B     DISP360                                                          
*                                  FORMAT THIS TRANSACTION                      
DISP400  OC    THISAMNT,THISAMNT                                                
         BZ    DISP120             NO CASH TO TRANSFER                          
         CLI   BILLOPT,0           ANY FILTER?                                  
         BE    DISP420             NO                                           
         ZAP   DUB,TRNSAMNT                                                     
         CVB   R1,DUB                                                           
         CLI   BILLOPT,C'N'        YES, SHOW UNBILLED ONLY?                     
         BNE   *+12                                                             
         CLM   R1,15,THISAMNT      SKIP BILLED                                  
         BNE   DISP120                                                          
         CLI   BILLOPT,C'Y'        SHOW BILLED ONLY?                            
         BNE   DISP420             NO                                           
         CLM   R1,15,THISAMNT      SKIP IF NOT BILLED                           
         BE    DISP120                                                          
*                                                                               
DISP420  CLI   LABOPT,C'Y'         LABOR ONLY OPTION                            
         BNE   *+14                                                             
         OC    THISTIME,THISTIME                                                
         BZ    DISP120             SKIP IF NO TIME                              
*                                                                               
         OC    OPTAMT,OPTAMT       OPTIONAL AMOUNT                              
         BZ    DISP425                                                          
         CP    DUB,OPTAMT                                                       
         BNE   DISP120                                                          
*                                                                               
DISP425  MVC   LINWORK,ACKEYWRK                                                 
         MVC   LINSUP,ACKEYCON+1                                                
         MVC   LINREF,ACKEYREF                                                  
         CLI   SUBOPT,C'Y'                                                      
         BNE   *+10                                                             
         MVC   LINREF,THISSUB      DISPLAY EXTRA REFERENCE                      
         OC    THISTIME,THISTIME   DISPLAY HOURS IF PRESENT                     
         BZ    DISP450                                                          
         CLC   REFOPT,=CL6'Y'      SHOW REFERENCE NUMBER ?                      
         BE    DISP450             YES                                          
         L     R1,THISTIME                                                      
         EDIT  (R1),(6,LINREF),2,FLOAT=-                                        
*                                                                               
DISP450  GOTO1 DATCON,DMCB,(1,ACKEYDTE),(8,LINDATE)                             
         ICM   R1,15,THISAMNT                                                   
*                                                                               
         OC    OPTPCT,OPTPCT                                                    
         BZ    DISP500             OPTIONAL PERCENT                             
         CVD   R1,DUB                                                           
         ZAP   PK16,DUB            * ORIGINAL AMOUNT                            
         MP    PK16,OPTPCT                                                      
         SRP   PK16,64-4,5         ROUNDED                                      
         ZAP   DUB,PK16                                                         
         CVB   R1,DUB                                                           
*                                                                               
DISP500  EDIT  (R1),(11,LINAMNT),2,FLOAT=-                                      
         OI    LINAMNTH+6,X'80'    TRANSMIT                                     
         CLC   LINIJOB(7),=C'PENDING'                                           
         BE    *+8                                                              
         NI    LINAMNTH+1,X'FF'-X'20'                                           
         OC    THISUNPR,THISUNPR   ARE THERE UNITS ON THIS TRAN                 
         BZ    *+8                 NO                                           
         OI    LINAMNTH+1,X'20'    PROTECT AMOUNT FOR TYPE 62'S                 
*                                                                               
         LA    R1,LINSTAT                                                       
         OC    ACDTUSED,ACDTUSED                                                
         BZ    *+14                                                             
         MVC   0(2,R1),=C'B,'                                                   
         LA    R1,2(R1)                                                         
         TM    TRNSSTAT,X'01'                                                   
         BZ    *+14                                                             
         MVC   0(3,R1),=C'N/C'                                                  
         LA    R1,3(R1)                                                         
         BCTR  R1,0                                                             
         CLI   0(R1),C','                                                       
         BNE   *+8                                                              
         MVI   0(R1),C' '                                                       
*                                                                               
*              ADD THIS TRANSACTION TO TABLE                                    
*                                                                               
         MVC   TRNSKEY,ACKEYWRK    SAVE W/C,CONTRA,DATE,REF,SUBREF              
         MVC   TRNSWC,TRNSKEY                                                   
         MVI   TRNSPOST,0                                                       
         MVC   TRNSORG,THISAMNT                                                 
         MVC   TRNSHRS,THISTIME                                                 
         ZAP   TRNSRTE,THISRATE                                                 
         LA    R2,TRNSNEXT                                                      
         MVI   0(R2),X'FF'         SET END OF TABLE                             
         LA    R3,LINNEXT          BUMP TO NEXT LINE                            
         ZIC   R1,COUNT                                                         
         LA    R1,1(R1)                                                         
         STC   R1,COUNT                                                         
         CLI   COUNT,MAXCOUNT      SCREEN EXHAUSTED                             
         BNE   DISP120                                                          
         B     ALLEXIT             YES - EXIT                                   
         DROP  R2,R3,R4,R5,RF                                                   
         EJECT                                                                  
*---------------------------------------------------------------------*         
* CHECK IF SEQUENCE NUMBER MATCH                                      *         
*---------------------------------------------------------------------*         
         USING FFTEL,R5                                                         
CHKMATSQ NTR1                                                                   
         SR    R1,R1                                                            
CHKMSQ10 CLI   FFTEL,0             EOR?                                         
         BE    LOWXIT                                                           
         CLI   FFTEL,FFTELQ                                                     
         BNE   CHKMSQ20                                                         
         CLI   FFTTYPE,FFTTTOSD    TRANSFER ORIGINAL SEQUENCE DATA              
         BE    CHKMSQ40                                                         
CHKMSQ20 IC    R1,FFTLN                                                         
         AR    R5,R1                                                            
         B     CHKMSQ10                                                         
*                                                                               
CHKMSQ40 CLI   FFTLN,FFTLN1Q+FFTSQLNQ+1                                         
         BL    LOWXIT                                                           
         CLI   XCLDOPT,C'Y'                                                     
         BE    CHKMSQ50                                                         
         CLC   FFT2NDSQ,SVKEY+41   SEQUENCE NUMBER THE SAME?                    
         B     *+10                                                             
CHKMSQ50 CLC   FFTSUBSQ,SVKEY+41                                                
         BE    EQXIT                                                            
         B     HIGHXIT                                                          
*                                                                               
LOWXIT   LA    RF,0                                                             
         B     *+8                                                              
HIGHXIT  LA    RF,2                                                             
         LA    RE,1                                                             
         CR    RF,RE                                                            
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* EXCLUDE TRANSFER TO TYPE 34 TRANSACTIONS                            *         
*---------------------------------------------------------------------*         
         USING PXDELD,R5                                                        
XCLTRNS  NTR1                                                                   
         CLI   XCLDOPT,C'Y'        NEED OPTION ON                               
         BNE   EQXIT                                                            
         SR    R1,R1                                                            
XCLT10   CLI   PXDEL,0             EOR?                                         
         BE    EQXIT                                                            
         CLI   PXDEL,PXDELQ        LOOK FOR POSTING XFER DETAIL ELEMENT         
         BE    XCLT15                                                           
         IC    R1,PXDLN                                                         
         AR    R5,R1                                                            
         B     XCLT10                                                           
*                                                                               
XCLT15   CLI   PXDTYPE,PXDTTO      ONLY EXCLUDE TRANSFER TO'S                   
         BNE   EQXIT                                                            
         B     NEQXIT                                                           
         EJECT                                                                  
         DROP  R5                                                               
*---------------------------------------------------------------------*         
* STRIP AMOUNT IN SCIEL FROM ORIGINAL AMOUNT                          *         
*---------------------------------------------------------------------*         
         USING TRANSD,R5                                                        
         USING SCIELD,R6                                                        
STRAMTO  NTR1                                                                   
         CLI   XCLDOPT,C'Y'        NEED OPTION ON                               
         BNE   EQXIT                                                            
         SR    R1,R1                                                            
         LR    R6,R5               COPY ADDRESS OF TRNSEL                       
SAO10    CLI   SCIEL,0                                                          
         BE    EQXIT                                                            
         CLI   SCIEL,SCIELQ        FIND PARTIAL AMOUNTS                         
         BNE   SAO20                                                            
         CLI   SCITYPE,SCITPART                                                 
         BE    SAO30                                                            
SAO20    IC    R1,SCILN                                                         
         AR    R6,R1                                                            
         B     SAO10                                                            
*                                                                               
SAO30    ZAP   DUB,TRNSAMNT        ADD ORIGINAL AMOUNT                          
         AP    DUB,SCIAMNT         TO PARTIALS                                  
         ZAP   TRNSAMNT,DUB        MAKES NEW ORIGINAL AMOUNT                    
         CP    TRNSAMNT,=P'0'                                                   
         BE    NEQXIT                                                           
         B     EQXIT                                                            
         EJECT                                                                  
         DROP  R5,R6                                                            
***********************************************************************         
* BUILD A KEY FOR TRANSACTION READING                                 *         
***********************************************************************         
         SPACE 1                                                                
KEYBUILD NTR1                                                                   
         LA    R4,KEY                                                           
         USING ACKEYD,R4                                                        
         MVC   ACKEYACC,FROMJOBA                                                
         TM    WORK,X'80'          X'80' - SET W/C                              
         BZ    KEYB2                                                            
         MVC   ACKEYWRK,SPACES                                                  
         MVI   ACKEYWRK,X'41'                                                   
         OC    WORKOPT,WORKOPT                                                  
         BZ    *+10                                                             
         MVC   ACKEYWRK,WORKOPT                                                 
KEYB2    TM    WORK,X'40'          X'40' - SET CONTRA A/C                       
         BZ    KEYB4                                                            
         MVC   ACKEYCON,SPACES                                                  
         MVI   ACKEYCON,X'41'                                                   
         OC    CRACA,CRACA                                                      
         BZ    *+10                                                             
         MVC   ACKEYCON,CRACA                                                   
KEYB4    TM    WORK,X'20'          X'20' - SET SDATE/DATE                       
         BZ    KEYB6                                                            
         MVC   ACKEYDTE(3),=X'700101'                                           
         OC    SDATOPT,SDATOPT                                                  
         BZ    *+10                                                             
         MVC   ACKEYDTE,SDATOPT                                                 
         OC    DATOPT,DATOPT                                                    
         BZ    *+10                                                             
         MVC   ACKEYDTE,DATOPT                                                  
KEYB6    TM    WORK,X'10'          X'10' - SET SREF/REF                         
         BZ    KEYB8                                                            
         MVC   ACKEYREF,SPACES                                                  
         MVI   ACKEYREF,X'41'                                                   
         OC    SREFOPT,SREFOPT                                                  
         BZ    *+10                                                             
         MVC   ACKEYREF,SREFOPT                                                 
         OC    REFOPT,REFOPT                                                    
         BZ    KEYB8                                                            
         CLC   REFOPT,=CL6'Y'      IS THIS REF=Y OPTION ?                       
         BE    KEYB8               YES                                          
         MVC   ACKEYREF,REFOPT                                                  
KEYB8    SR    RE,RE                                                            
         IC    RE,ACKEYWRK+1                                                    
         LA    RE,1(RE)                                                         
         TM    WORK,X'08'          X'08' - BUMP W/C                             
         BZ    *+8                                                              
         STC   RE,ACKEYWRK+1                                                    
         IC    RE,ACKEYCON+14                                                   
         LA    RE,1(RE)                                                         
         TM    WORK,X'04'          X'04' - BUMP CONTRA A/C                      
         BZ    *+8                                                              
         STC   RE,ACKEYCON+14                                                   
         IC    RE,ACKEYDTE+2                                                    
         LA    RE,1(RE)                                                         
         TM    WORK,X'02'          X'02' - BUMP DATE                            
         BZ    *+8                                                              
         STC   RE,ACKEYDTE+2                                                    
         IC    RE,ACKEYREF+5                                                    
         LA    RE,1(RE)                                                         
         TM    WORK,X'01'          X'01' - BUMP REF                             
         BZ    *+8                                                              
         STC   RE,ACKEYREF+5                                                    
         MVI   ACKEYSBR,0          SET SUBREF                                   
         B     CURSIT                                                           
         EJECT                                                                  
*              VALIDATE JOB FIELD (R2=A(FLDHDR)) INPUT IS CLI/PRO/JOB           
*                                                                               
*                                                                               
         USING ACCOMPD,RF                                                       
JOBVAL   NTR1                                                                   
         XC    LVLSSJ,LVLSSJ                                                    
         MVI   ERRNUM,EIIF                                                      
         MVI   FNDX,0                                                           
         MVC   SALEACCT,SPACES     CLEAR SAVE AREAS                             
         MVC   SALENAME,SPACES                                                  
*                                                                               
         GOTO1 SCANNER,DMCB,(R2),(7,LINES),C',=/='                              
         LA    RF,COMPEL           RF=A(COMPANY ELEMENT)                        
         MVC   SCANSV,4(R1)        SAVE NO OF ENTRIES                           
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
         BAS   RE,GETACC           READ ACCOUNT                                 
         MVC   LVLSSJ,BCCLILEN     SAVE HIERARCHY                               
         LA    R6,PSPROPPR-PSCLIPPR(R6)  NEXT PROFILE                           
         MVI   ERRNUM,EAIL                                                      
         TM    ACCTSTAT,X'10'      CHECK CLIENT/product locked                  
         BO    JOBVALX                                                          
*                                                                               
         CLI   FNDX,1              CLIENT LEVEL                                 
         BE    JOBVAL6                                                          
*                                                                               
JOBVAL4  CLI   FNDX,3              JOB LEVEL?                                   
         BNE   JOBVAL7             NO, MUST BE PRODUCT                          
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
JOBVAL7  L     R1,AIO1             WE ARE AT PRODUCT LEVEL                      
         AH    R1,DATADISP                                                      
         SR    RE,RE                                                            
*                                                                               
JOBVAL8  CLI   0(R1),0                                                          
         BE    JOBVAL6                                                          
         CLI   0(R1),SANELQ        LOOK FOR SALES ELEMENT                       
         BNE   JOBVAL9                                                          
*                                                                               
         USING SANELD,R1                                                        
         MVC   SALEACCT,SANCODE    SAVE ACCOUNT AND NAME                        
         MVC   SALENAME,SANNAME                                                 
         B     JOBVAL6                                                          
*                                                                               
JOBVAL9  SR    RE,RE                                                            
         IC    RE,1(R1)                                                         
         AR    R1,RE                                                            
         B     JOBVAL8                                                          
*                                                                               
JOBVAL10 MVI   ERRNUM,EAXJ         SEE IF THIS IS AN XJOB                       
         LA    R3,KEY                                                           
         GOTO1 ASETJOB,DMCB,(R3)                                                
         TM    ACOPSTAT,ACOXJOB                                                 
         BNZ   JOBVALX             IT IS, FLAG AS ERROR                         
*                                                                               
         MVI   ERRNUM,X'FF'        SET VALID JOB EXIT                           
         BAS   RE,PROFMERG                                                      
         LA    RE,PROFILE                                                       
         USING ACPROFD,RE                                                       
         LA    RF,JOBTJOBH                                                      
         CR    RF,R2               IS THIS TO-JOB                               
         BNE   JOBVAL13                                                         
         MVC   TOJOBA,ACCTNUM                                                   
         MVC   TOJOBN,ACCTNAME                                                  
         MVC   TOJOBOFF,ACPROFFC                                                
         L     RF,ATWAXTRA                                                      
         USING XTWAD,RF                                                         
         MVC   TOSALA,SALEACCT     MOVE TO APPRORIATE PRINT FIELD               
         MVC   TOSALN,SALENAME                                                  
         TM    COMPSTAT,X'10'                                                   
         BZ    JOBVALX               NO COSTING                                 
         MVC   KEY(15),ACPRCOST    SET TO READ 1C ACCOUNT                       
         BAS   RE,GETACC                                                        
         BAS   RE,CHECKACC                                                      
         L     RF,ATWAXTRA                                                      
         MVC   TOCST,ACCTNUM       SAVE COST ACCOUNT CODE                       
         MVC   TOCSTN,ACCTNAME     AND NAME                                     
         MVI   ERRNUM,X'FF'                                                     
         B     JOBVALX                                                          
*                                                                               
JOBVAL13 LA    RF,JOBFJOBH                                                      
         CR    RF,R2               IS THIS FROM-JOB                             
         BNE   JOBVAL15                                                         
*                                                                               
         USING GOBLOCKD,RF         SAVE THE BILLTYPE                            
         L     RF,AGOBLOCK                                                      
         MVC   SAVETYPE,GOBILTYP                                                
         CLI   SAVETYPE,C'C'       CLIENT BILL TYPE?                            
         BNE   JOBVAL14            NO                                           
         CLI   BILLOPT,0           YES, BILLOPT ENTERED?                        
         BE    JOBVAL14            NO                                           
         LA    R2,JOBOPTNH         YES, ERROR                                   
         MVI   FNDX,0                                                           
         MVI   ERRNUM,EOPT                                                      
         XIT1  REGS=(R2)                                                        
         DROP  RF                                                               
*                                                                               
JOBVAL14 MVC   FROMJOBA,ACCTNUM                                                 
         MVC   FROMJOBN,ACCTNAME                                                
         MVC   FROMJOFF,ACPROFFC   SAVE OFFICE                                  
         L     RF,ATWAXTRA                                                      
         USING XTWAD,RF                                                         
         MVC   FROMSALA,SALEACCT   MOVE TO APPROPRIATE FIELDS                   
         MVC   FROMSALN,SALENAME                                                
         TM    COMPSTAT,X'10'                                                   
         BZ    JOBVALX               NO COSTING                                 
         MVC   KEY(15),ACPRCOST      SET TO READ 1C ACCOUNT                     
         BAS   RE,GETACC                                                        
         BAS   RE,CHECKACC                                                      
         L     RF,ATWAXTRA                                                      
         MVC   FROMCST,ACCTNUM       SAVE COST ACCOUNT CODE                     
         MVC   FROMCSTN,ACCTNAME     AND NAME                                   
         MVI   ERRNUM,X'FF'                                                     
         B     JOBVALX                                                          
*                                                                               
JOBVAL15 MVC   INPTJOBA,ACCTNUM      MUST BE BOTTOM HALF TO-JOB                 
         MVC   INPTJOBN,ACCTNAME                                                
         MVC   INPTJOFF,ACPROFFC                                                
         MVC   INPTSALA,SALEACCT     MOVE TO APPROPRIATE FIELDS                 
         MVC   INPTSALN,SALENAME                                                
         TM    COMPSTAT,X'10'                                                   
         BZ    JOBVALX               NO COSTING                                 
         MVC   KEY(15),ACPRCOST      SET TO READ 1C ACCOUNT                     
         BAS   RE,GETACC                                                        
         BAS   RE,CHECKACC                                                      
         MVC   INPTCST,ACCTNUM       SAVE COST ACCOUNT CODE                     
         MVC   INPTCSTN,ACCTNAME     AND NAME                                   
         MVI   ERRNUM,X'FF'                                                     
JOBVALX  B     CURSIT                                                           
***********************************************************************         
* GET ACCOUNT DATA FROM SI ACCOUNT                                    *         
*     P1=A(AIOAREA)                                                   *         
*     P2=A(ACCTAB)                                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING ACCTD,R3                                                         
SIVAL    NTR1                                                                   
         TM    COMPSTAT,X'10'                                                   
         BZ    SIVAL12             NO COSTING                                   
         LM    R2,R3,0(R1)                                                      
         AH    R2,DATADISP                                                      
         SR    RE,RE                                                            
         MVC   SKKEY,KEY           SAVE THE KEY                                 
*                                                                               
SIVAL02  CLI   0(R2),0             END OF RECORD ?                              
         BE    SIVAL12             YES                                          
         CLI   0(R2),ACSTELQ       DID WE FIND STATUS ELEMENT ?                 
         BE    SIVAL06             YES                                          
         CLI   0(R2),ACSPELQ       DID WE FIND POSTING ELEMENT ?                
         BE    SIVAL08             YES                                          
*                                                                               
SIVAL04  IC    RE,1(R2)            KEEP LOOKING                                 
         AR    R2,RE                                                            
         B     SIVAL02                                                          
*                                                                               
         USING ACSTATD,R2                                                       
SIVAL06  MVC   ACCT12+3(1),ACSTCOST                                             
         OC    ACCT12,SPACES                                                    
         B     SIVAL10                                                          
*                                                                               
         USING ACSPECD,R2                                                       
SIVAL08  CLI   ACSPTYP,ACSPOAN     ANALYSIS OVERRIDE ?                          
         BNE   SIVAL04             NOPE                                         
         MVC   ACCT12+3(12),ACSPACCT                                            
*                                                                               
SIVAL10  MVC   ACCT12(1),COMPANY                                                
         MVC   ACCT12+1(2),=C'12'                                               
         MVC   KEY,SPACES                                                       
         MVC   KEY(L'ACCT12),ACCT12                                             
         BAS   RE,GETACC                                                        
         BAS   RE,CHECKACC                                                      
         MVC   ACCT12NM,ACCTNAME                                                
         MVI   ERRNUM,X'FF'        SET ACCOUNT VALID                            
         MVC   KEY,SKKEY           RESTORE KEY                                  
*                                                                               
SIVAL12  B     CURSIT                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE WORKCODE                                                   *         
***********************************************************************         
         SPACE 1                                                                
WCVAL    NTR1                                                                   
         XC    THISWRKC,THISWRKC                                                
         MVI   ERRNUM,19           INVALID WORK-CODE                            
         CLC   0(2,R1),=C'99'                                                   
         BE    CURSIT                                                           
         CLC   0(2,R1),=C'**'                                                   
         BE    CURSIT                                                           
         MVC   KEY,SPACES                                                       
         MVI   KEY,X'0A'           WORK-CODE RECORD                             
         MVC   KEY+1(1),COMPANY                                                 
         MVC   KEY+2(2),=C'SJ'                                                  
         MVC   KEY+4(2),0(R1)     R0=A(WORK-CODE)                               
         MVC   THISWRKC,0(R1)                                                   
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',KEY,KEY                      
         CLI   DMCB+8,0                                                         
         BNE   *+8                                                              
         MVI   ERRNUM,X'FF'        SET WORK-CODE VALID                          
         B     CURSIT                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE WORKCODE AGAINST NON-BILLABLE LIST R2=A(WORKCODE)          *         
***********************************************************************         
         SPACE 1                                                                
         USING GOBLOCKD,R1                                                      
WCCHK    NTR1                                                                   
         LA    R0,6                MAXIMUM NUMBER OF ENTRIES                    
         L     R1,AGOBLOCK                                                      
         LA    R1,GOUWLIST                                                      
*                                                                               
WCCHK02  CLC   0(2,R2),0(R1)       IS ENTRY IN TABLE ?                          
         BE    WCCHKY              YES                                          
         LA    R1,2(R1)            NO, TRY NEXT                                 
         BCT   R0,WCCHK02                                                       
         LTR   R1,R1               SET CONDITION CODE TO NE                     
         B     WCCHKX                                                           
*                                                                               
WCCHKY   CR    R1,R1               SET CONDITION CODE TO EQ                     
         MVI   ERRNUM,19                                                        
*                                                                               
WCCHKX   B     CURSIT                                                           
         EJECT                                                                  
* ACBATCODE                                                                     
       ++INCLUDE ACBATCODE                                                      
*                                                                               
MYHIGH   NTR1  ,                   READ HIGH FOR A RECORD                       
         MVC   IOKEY,KEY                                                        
         GOTO1 ARDHI,AIO1                                                       
         CLC   IOKEYSAV,KEY                                                     
         BE    MOVER                                                            
         MVC   FVMSGNO,=AL2(AE$INACC)                                           
         B     ERRXIT                                                           
*                                                                               
MYSEQ    NTR1  ,                   READ SEQUENTIAL                              
         GOTO1 ASEQ,AIO1                                                        
         B     MOVER                                                            
         EJECT                                                                  
CNTRAMSG DC    C'*ERROR* CONTRA-ACCOUNT DOES NOT EXIST-CANNOT TRANSFER'         
*                                                                               
         LTORG                                                                  
*                                                                               
RELOTAB  DS    0A                                                               
         DC    A(ACCTAB)                                                        
         DC    A(OPTTAB)                                                        
         DC    V(TMSXFR)                                                        
         DC    V(CADET)                                                         
         DC    X'FF'                                                            
         LTORG                                                                  
         EJECT                                                                  
*              VALIDATE AN SK ACCOUNT                                           
         SPACE 1                                                                
*                       PARA 1 = A(SK ACCOUNT) TO VALIDATE                      
*                       PARA 2 = A(FIELD HEADER)                                
*                       PARA 3 = A(BEG OF LINE) IF AT BOTTOM HALF               
SKVAL    NTR1  BASE=*,LABEL=*                                                   
         LM    R3,R5,0(R1)                                                      
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),=C'SK'                                                  
         MVC   KEY+3(12),0(R3)     R3=A(ACCOUNT)                                
         OC    KEY+1(14),SPACES                                                 
         BAS   RE,GETACC                                                        
         MVI   ERRNUM,EIAC                                                      
         TM    ACCTSTAT,X'80'      IS IT OK TO POST TO                          
         JZ    CURSIT                                                           
         L     RF,ATWAXTRA                                                      
         USING XTWAD,RF                                                         
         LA    R1,JOBTJOBH         IS THIS TO-JOB                               
         CR    R1,R4                                                            
         JE    SKVAL2                                                           
         MVI   ERRNUM,EIIF                                                      
         LA    R1,LINSUP-LINED(R5)                                              
         CLC   0(2,R1),=C'SK'      IS CONTRA AN SK ACCOUNT                      
         JNE   CURSIT                                                           
         OC    TOSKACCT,TOSKACCT   WAS THERE AN SK ACCOUNT ALREADY              
         JNZ   CURSIT                                                           
         MVC   INSKACCT,ACCTNUM                                                 
         MVC   INSKNAME,ACCTNAME                                                
         J     SKVAL4                                                           
SKVAL2   MVC   TOSKACCT,ACCTNUM                                                 
SKVAL4   MVI   ERRNUM,X'FF'        SET ACCOUNT VALID                            
         J     CURSIT                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO READ MEMO SK ACCOUNT                                     *         
***********************************************************************         
         USING LINED,R3                                                         
         USING ACCTD,R5                                                         
         USING TRSDESCD,RE                                                      
MEMOSK   NMOD1 0,*MEMOSK*                                                       
         L     RC,0(R1)                                                         
         OC    ACCTSK,ACCTSK                                                    
         BNZ   MEMOSKOK            ALREADY HAVE SK                              
         LA    RE,THISTRSD                                                      
         MVC   SKKEY,SPACES                                                     
         MVC   SKKEY(1),COMPANY                                                 
         ZIC   R1,TRSDLEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SKKEY+1(0),TRSDACCS                                              
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',SKKEY,SKKEY                  
         CLI   8(R1),0                                                          
         BNE   MEMOSKNO            CAN'T FIND CONTRA-ACCOUNT                    
         LA    R1,SKIOAREA                                                      
         SR    RE,RE                                                            
*                                                                               
MEMOSK04 CLI   0(R1),0             AND EXTRACT NAME                             
         BNE   *+6                                                              
         DC    H'0'                NO NAME ELEMENT                              
         CLI   0(R1),X'20'                                                      
         BE    MEMOSK05                                                         
         IC    RE,1(R1)                                                         
         AR    R1,RE                                                            
         B     MEMOSK04                                                         
*                                                                               
MEMOSK05 MVC   ACCTSKNM,SPACES                                                  
         IC    RE,1(R1)                                                         
         SH    RE,=H'3'                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   ACCTSKNM(0),2(R1)   CONTRA NAME                                  
         MVC   ACCTSK,SKKEY        AND ACCOUNT                                  
*                                                                               
MEMOSKOK MVI   ERRNUM,X'FF'        SET ERROR OK                                 
         B     CURSIT                                                           
*                                                                               
MEMOSKNO DS    0H                  PUT OUT ERROR MESSAGE                        
         MVI   ERRNUM,X'FE'                                                     
         MVC   MSG,SPACES                                                       
         MVC   MSG(L'CNTRAMSG),CNTRAMSG                                         
         OI    CONHEADH+6,X'80'    AND TRANSMIT IT                              
         LA    R2,LINIJOBH         CURSOR TO INPUT FIELD                        
         B     EXIT                                                             
         DROP  R3,R5,RE                                                         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* JOB GROUP DETAIL SCREEN                                             *         
***********************************************************************         
         USING TRNSD,R4                                                         
JGDETSCR NMOD1 0,*JGDS*                                                         
         L     RC,0(R1)                                                         
         MVC   BYTE,0(R1)                                                       
*                                                                               
         TM    BYTE,X'80'          ONE-PASS?                                    
         BO    JGD050                                                           
         CLI   MODE,3                                                           
         BE    JGD200                                                           
         CLI   PFKEY,9                                                          
         BNE   JGD200                                                           
JGD050   LR    RE,RA                                                            
         AHI   RE,X'2A00'                                                       
         CLC   =C'T61B42',0(RE)    SEE IF WE WERE HERE BEFORE                   
         BE    JGD070                                                           
         LHI   RF,MAXSIZE          CLEAR 8K OF SPACES                           
         XCEF                                                                   
*                                                                               
         LR    RE,RA                                                            
         AHI   RE,X'2A00'                                                       
         MVC   0(6,RE),=C'T61B42'                                               
*                                                                               
JGD070   L     R0,BCACUR           ADDRESS OF CURSOR AT TIME OF INPUT           
         LA    R1,1                MAIN LINE NUMBER                             
         LA    RF,JOBILINH         FIND WHICH LINE THE CURSOR IS AT             
         LA    R4,TRNSTAB                                                       
JGD100   LR    RE,RF                                                            
         LA    RF,JOBLINQ(RE)      NEXT LINE                                    
         CR    R0,RF                                                            
         BL    JGD150              FOUND IT.                                    
         LA    R1,1(R1)            BUMP LINE NUMBER                             
         LA    R4,TRNSNEXT                                                      
         B     JGD100                                                           
*                                                                               
         USING LINED,RE                                                         
         USING JGROUPD,RF                                                       
JGD150   L     RF,AJGRPBLK                                                      
         MVC   JGTJOB,SPACES                                                    
         MVC   JGTJOB,LINIJOB      TO JOB                                       
         CLC   TOJGRP,SPACES       USE TO JOB GROUP IF IT EXISTS                
         BNH   *+10                                                             
         MVC   JGTJOB(L'TOJGRP),TOJGRP                                          
         MVI   JGMODE,JGMINIT      INITIALIZE                                   
         TM    BYTE,X'80'                                                       
         BZ    JGD170                                                           
         MVI   JGMODE,JGM1PASS     1 PASS                                       
         CLC   TOJGRP,SPACES                                                    
         BNH   *+8                                                              
         MVI   JGMODE,JGM1SPCL                                                  
         TM    BYTE,X'40'                                                       
         BZ    JGD170                                                           
         MVI   JGMODE,JGM1SPVL     1 PASS, SPECIAL                              
         MVC   JGTJOB,JOBTJOB                                                   
JGD170   MVC   JGFJOB,JOBFJOB      FROM JOB                                     
*                                                                               
         MVC   JGWC,TRNSWC         WORK CODE                                    
         MVC   JGSUPPLR(L'LINSUP),LINSUP               SUPPLIER                 
         MVC   JGREFN,LINREF       REFERENCE NUMBER                             
         MVC   JGDATE,LINDATE      DATE                                         
         MVC   JGSTATUS(1),TRNSTS  STATUS                                       
         CLI   JGSTATUS,0                                                       
         BNE   *+10                                                             
         MVC   JGSTATUS(1),LINSTAT                                              
         MVC   JGTOTAMT,LINAMNT    AMOUNT                                       
         STC   R1,JGLINE           LINE NUMBER                                  
         NI    JGSTAT,X'FF'-JGNEW                                               
         CLC   TOJGRP,SPACES                                                    
         BH    JGD180                                                           
         TM    LINIJOBH+4,X'80'    INPUT THIS TIME?                             
         BZ    *+8                                                              
JGD180   OI    JGSTAT,JGNEW                                                     
         DROP  RE                                                               
         DROP  RF                                                               
*                                                                               
JGD200   LA    R3,X'42'            JOB GROUP SCREEN                             
         GOTO1 CALLOV,DMCB,((R3),0),(0,0)                                       
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVI   CSSPROG,2                                                        
         L     RE,AJGRPBLK                                                      
         L     RF,0(R1)                                                         
         GOTO1 (RF),DMCB,(RE),(R9)    CALL IT                                   
         L     R2,FVADDR                                                        
         MVI   MODE,3              USING JOB GROUP SCREEN                       
*                                                                               
         USING JGROUPD,RF                                                       
         USING LINED,R5                                                         
         L     RF,AJGRPBLK                                                      
         LA    R5,JOBILINH                                                      
         CLI   JGMODE,JGM1PASS                                                  
         BE    JGD300                                                           
         CLI   JGMODE,JGM1SPVL                                                  
         BE    JGD300                                                           
         CLI   JGMODE,JGM1SPCL                                                  
         BE    JGD300                                                           
         CLI   JGMODE,JGMEXIT                                                   
         BNE   EXIT                                                             
JGD300   MVI   MODE,1                                                           
         TM    JGSTAT,JGERR1                                                    
         BZ    JGD310                                                           
         MVI   FVOMTYP,C'E'                                                     
         MVC   FVMSGNO,=AL2(AE$INVCD)        INVALID CODE                       
         B     EXIT                                                             
JGD310   TM    JGSTAT,JGERR2                                                    
         BZ    JGD340                                                           
         MVI   FVOMTYP,C'E'                                                     
         MVC   FVMSGNO,=AL2(AE$GRPER)        GROUP HAS ERRORS                   
         B     EXIT                                                             
JGD340   MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
JGD350   CLI   JGMODE,JGM1SPVL                                                  
         BE    EXIT                                                             
         SR    R1,R1                                                            
         IC    R1,JGLINE           FIND LINE NUMBER                             
         BCTR  R1,0                                                             
         LR    RE,RA                                                            
         AHI   RE,X'2A00'                                                       
         LA    RE,6(RE)                                                         
         LA    R4,TRNSTAB                                                       
         LTR   R1,R1                                                            
         BZ    JGD550                                                           
JGD500   LA    RE,4(RE)                                                         
         LA    R4,TRNSNEXT                                                      
         LA    R5,JOBLINQ(R5)                                                   
         BCT   R1,JGD500                                                        
*                                                                               
JGD550   LA    R2,LINIJOBH                                                      
         ST    R2,FVADDR                                                        
         CLI   2(RE),C'U'          USED JOB GROUP?                              
         BNE   EXIT                                                             
         CLC   TOJGRP,SPACES                                                    
         BH    JGD600                                                           
         MVC   LINIJOB(L'JGTJOB),JGTJOB                                         
         CLC   JGTJOB,SPACES                                                    
         BH    JGD650                                                           
         MVC   LINIJOB(16),=CL16'** MULTIPLE JOBS'                              
         B     JGD650                                                           
JGD600   MVC   LINIJOB(L'LINIJOB),SPACES                                        
         MVI   LINIJOB,C'Y'                                                     
*                                                                               
JGD650   OI    LINIJOBH+6,X'80'                                                 
         MVI   TRNSPOST,C'Y'                                                    
         MVC   TRNSWC,JGWC                                                      
         MVC   TRNSTS,JGSTATUS                                                  
JGD700   CLI   BYTE,0                                                           
         BNE   EXIT                                                             
         MVC   FVMSGNO,=AL2(AI$PSRES)                                           
         B     EXIT                                                             
         DROP  RF,R4,R5                                                         
*                                                                               
JOBLINQ  EQU   L'JOBILINH+L'JOBILIN+L'JOBILINX+L'JOBIAMTH+L'JOBIAMT+L'JX        
               OBIAMTX+L'JOBIJOBH+L'JOBIJOB+L'JOBIJOBX                          
MAXSIZE  EQU   X'2000'                                                          
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BUILD A TRANSACTION RECORD AND POST TO ACCDAY/TWA1                  *         
***********************************************************************         
         SPACE 1                                                                
BLDTRNS  DS    0D                                                               
         NMOD1 0,*BLDT*                                                         
         L     RC,0(R1)                                                         
*                                                                               
         LA    RE,IOAREA           CLEAR IOAREA                                 
         LA    RF,L'IOAREA                                                      
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         LR    R4,R5               R4=ACCTAB ENTRY                              
         USING ACCTD,R4                                                         
         LA    R5,IOAREA+2         R5=A(ELEMENT)                                
         USING DLDESCD,R5                                                       
         XC    DLDSEL(250),DLDSEL                                               
         MVI   DLDSEL,X'64'        BUILD DESCRIPTION ELEMENT                    
         MVC   DLDSDATE,THISDATE                                                
         MVC   DLDSREF,THISREF                                                  
         TM    THISSTAT,X'40'                                                   
         BZ    *+8                                                              
         OI    DLDSSTAT,X'40'      URGENT                                       
         TM    THISSTAT,X'01'                                                   
         BZ    *+8                                                              
         OI    DLDSSTAT,X'01'      NON-COMMISIONABLE                            
         SR    R1,R1                                                            
         IC    R1,THISNRLN         USE NARRATIVE OF ORIGINAL DEBIT              
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DLDSNARR(0),THISNARR                                             
*                                                                               
         LA    RF,DLDSNARR                                                      
         SR    RF,R5               GET LENGTH UP TO NARRATIVE                   
         AR    RF,R1               ADD LENGTH OF NARRATIVE                      
         STC   RF,DLDSLEN          RESULT IS LENGTH OF RECORD                   
*                                                                               
         LA    R5,0(R5,RF)         BUMP TO NEXT ELEMENT                         
         OC    THISHOUR,THISHOUR                                                
         BZ    BLDT10                                                           
         SR    R1,R1                                                            
         IC    R1,THISHOUR+1       GET LENGTH FOR MOVE                          
         SH    R1,=H'1'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),THISHOUR                                                 
         USING ACPERSD,R5                                                       
         ZAP   DUB,ACPSHOUR                                                     
         MP    DUB,=P'-1'                                                       
         ZAP   ACPSHOUR,DUB                                                     
         IC    R1,1(R5)                                                         
         AR    R5,R1                                                            
*                                                                               
BLDT10   OC    THISUNPR,THISUNPR   WAS A UNIT/PRICE ELEMENT BUILT               
         BZ    BLDT20                                                           
         SR    R1,R1                                                            
         IC    R1,THISUNPR+1       GET LENGTH FOR MOVE                          
         SH    R1,=H'1'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),THISUNPR    MOVE ELEMENT                                 
         USING UNPELD,R5                                                        
         ZAP   DUB,UNPUNIT         REVERSE                                      
         MP    DUB,=P'-1'                                                       
         ZAP   UNPUNIT,DUB                                                      
         IC    R1,1(R5)                                                         
         AR    R5,R1                                                            
*                                                                               
BLDT20   OC    THISTRSD,THISTRSD                                                
         BZ    BLDT30                                                           
         SR    R1,R1                                                            
         IC    R1,THISTRSD+1                                                    
         SH    R1,=H'1'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),THISTRSD                                                 
         IC    R1,1(R5)                                                         
         AR    R5,R1                                                            
*                                                                               
BLDT30   OC    THISCASH,THISCASH                                                
         BZ    BLDT40                                                           
         MVC   0(L'THISCASH,R5),THISCASH                                        
         USING TRCASHD,R5                                                       
         MP    TRCSAMNT,=P'-1'                                                  
         IC    R1,1(R5)                                                         
         AR    R5,R1                                                            
*                                                                               
BLDT40   OC    THISOTH,THISOTH                                                  
         BZ    BLDT50                                                           
         USING ACOTHERD,R5                                                      
         MVC   ACOTEL(15),SPACES                                                
         MVC   ACOTEL(2),=X'230F'                                               
         MVC   ACOTNUM(6),THISOTH                                               
         IC    R1,ACOTLEN                                                       
         AR    R5,R1                                                            
*                                                                               
         USING ACNOD,R5                                                         
BLDT50   OC    THISOTH2,THISOTH2                                                
         BZ    BLDT60                                                           
         ZIC   R1,THISOTH2+L'THISOTH2-1                                         
         MVI   ACNOEL,X'25'                                                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ACNO(0),THISOTH2                                                 
         LA    R1,3(R1)                                                         
         STC   R1,ACNOLEN                                                       
         AR    R5,R1                                                            
*                                                                               
         USING SPAELD,R5                                                        
BLDT60   OC    THISCST,THISCST                                                  
         BZ    BLDT70                                                           
         MVI   SPAEL,SPAELQ        X'2C' - SPECIAL POSTING A/C ELEM             
         MVI   SPALN,SPALNQ                                                     
         MVI   SPATYPE,SPATCCST    CLIENT COSTING ACCOUNT                       
         MVC   SPAAULA,THISCST+1   COSTING U/L/A                                
         SR    R1,R1                                                            
         IC    R1,SPALN                                                         
         AR    R5,R1                                                            
*                                                                               
         USING PXDELD,R5           BUILD POSTING TRANSFER ELEMENT               
BLDT70   XC    PXDEL(PXDLNQ),PXDEL                                              
         MVI   PXDEL,PXDELQ                                                     
         MVI   PXDLN,PXDLNQ                                                     
         MVI   PXDTYPE,C'T'                                                     
         MVC   PXDDATE,TODAYP                                                   
         MVC   PXDFRTO,ACCTTOJB                                                 
         MVC   PXDFRTOW,THISINWC                                                
         OC    PXDFRTO,PXDFRTO                                                  
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    R5,AD4EL            SAVE A(POSTING TRANSFER ELEMENT)             
         IC    R1,1(R5)                                                         
         AR    R5,R1                                                            
*                                                                               
         USING TRSELD,R5           BUILD SKELETON X'60' ELEMENT                 
         OC    THATSTA2,THATSTA2                                                
         BZ    BLDT80                                                           
         XC    0(TRSLNQ,R5),0(R5)                                               
         MVI   TRSEL,TRSELQ                                                     
         MVI   TRSLN,TRSLNQ                                                     
         MVC   TRSSTAT2,THATSTA2                                                
         MVC   TRSSTAT3,THATSTA3                                                
         IC    R1,1(R5)                                                         
         AR    R5,R1                                                            
*                                                                               
         USING ANOELD,R5                                                        
BLDT80   OC    THISOFF,THISOFF                                                  
         BZ    BLDT85                                                           
         MVI   ANOEL,ANOELQ        X'65' - ANALYZED OFFICE ELEMENT              
         MVI   ANOLN,ANOLNQ                                                     
         MVI   ANOTYPE,ANOTCLI     CLIENT OFFICE                                
         MVC   ANOOFFC,THISOFF     COSTING U/L/A                                
         SR    R1,R1                                                            
         IC    R1,ANOLN                                                         
         AR    R5,R1                                                            
*                                                                               
         USING PXDELD,R5                                                        
BLDT85   OC    THISORGJ,THISORGJ                                                
*NMAL    BZ    BLDT90                                                           
         BZ    BLDT86                                       NMAL                
         MVI   PXDEL,PXDELQ                                                     
         MVI   PXDLN,PXDLN2Q                                                    
         MVI   PXDTYPE,PXDTORG                                                  
         MVC   PXDDATE,THISORGD                                                 
         MVC   PXDFRTO,THISORGJ                                                 
         MVC   PXDFRTOW,THISORGW                                                
         MVC   PXDOBAT,THISORGB                                                 
         MVC   PXDOSEQ,THISORGS                                                 
         IC    R1,1(R5)                                                         
         AR    R5,R1                                                            
*                                                                               
         USING ASKELD,R5                                    NMAL                
BLDT86   OC    THISASK,THISASK                              NMAL                
         BZ    BLDT87                                       NMAL                
         MVI   ASKEL,ASKELQ                                 NMAL                
         MVI   ASKLN,ASKLNQ                                 NMAL                
         MVC   ASKKEY,THISASK                               NMAL                
         IC    R1,1(R5)                                     NMAL                
         AR    R5,R1                                        NMAL                
*                                                           NMAL                
         USING SCIELD,R5                                    NMAL                
BLDT87   OC    THISCRDE,THISCRDE                            NMAL                
         BZ    BLDT90                                       NMAL                
         MVC   SCIELD(SCITCPLQ),THISCRDE                    NMAL                
         MVC   SCIAMNT,THISAMNT                  NMALIK     NMAL                
         MP    SCIAMNT,=P'-1'                               NMAL                
         IC    R1,1(R5)                                     NMAL                
         AR    R5,R1                                        NMAL                
*                                                           NMAL                
         USING PAKELD,R5                                                        
BLDT90   OC    THISPAKL,THISPAKL                                                
         BZ    BLDT95                                                           
         SR    R1,R1                                                            
         IC    R1,THISPAKL+1                                                    
         SHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),THISPAKL                                                 
         IC    R1,1(R5)                                                         
         AR    R5,R1                                                            
*                                                                               
         USING FFTELD,R5                                                        
BLDT95   DS    0H                                                               
         MVI   FFTEL,FFTELQ        X'DB' - FREEFORM TEXT ELEMENT                
         LA    R1,FFTSQLNQ                                                      
         STC   R1,FFTDLEN          DATA LENGTH                                  
         LA    R1,FFTLN1Q+1(R1)                                                 
         STC   R1,FFTLN                                                         
         MVI   FFTTYPE,FFTTTOSD                                                 
         MVC   FFTSUBSQ,SVSUBSEQ   SUB SEQUENCE NUMBER                          
         MVC   FFTBTREF,SVBATREF   BATCH REFERENCE                              
         MVC   FFTACTDT,SVACTDAT   ACTIVITY DATE                                
         ZAP   FFTORGAM,SVORGAMT   ORIGINAL AMOUNT                              
         MVC   FFT2NDSQ,SV2NDSEQ   2NDARY SEQUENCE NUMBER                       
         SR    R1,R1                                                            
         IC    R1,FFTLN                                                         
         AR    R5,R1                                                            
*                                                                               
         USING DLPOSTD,R5                                                       
         XC    DLPSEL(113),DLPSEL                                               
         MVI   DLPSEL,X'69'        BUILD SINGLE DEBIT ELEMENT                   
         MVI   DLPSLEN,113                                                      
         MVC   DLPSDBAC,FROMJOBA   FROM JOB IS DEBIT A/C                        
         MVC   DLPSDBNM,FROMJOBN                                                
         MVC   DLPSCRAC,CREDITA                                                 
         MVC   DLPSCRNM,ACCTCTRN                                                
         ZAP   DUB,THISAMNT                                                     
         MP    DUB,=P'-1'          REVERSE SIGN OF ORIGINAL POSTING             
         ZAP   DLPSAMNT,DUB                                                     
         MVC   DLPSANAL,THISANAL                                                
         TM    THISSTAT,X'01'                                                   
         BZ    *+8                                                              
         OI    DLPSTYPE,X'40'                                                   
         ST    R5,AD69L            SAVE A(POSTING ELEMENT)                      
*                                                                               
         IC    R1,1(R5)                                                         
         AR    R5,R1                                                            
         CLC   CREDITA+1(2),=C'SK' IF CONTRA IS AN SK ACCOUNT                   
         BNE   BLDT100                                                          
         L     RE,AD69L            COPY 69 EL TO CREATE 6A                      
         MVC   DLPSEL(113),0(RE)                                                
         MVI   DLPSEL,X'6A'         BUILD SINGLE CREDIT ELEMENT                 
         MVC   DLPSANAL,FROMJOFF    USE OFFICE CODE                             
         OC    THISOFF,THISOFF     DO WE HAVE AN OFFICE SAVED?                  
         BZ    *+10                                                             
         MVC   DLPSANAL,THISOFF    OVERWRITE WITH OFFICE FROM 65 ELEM           
         IC    R1,1(R5)                                                         
         AR    R5,R1                                                            
         B     BLDT110                                                          
*                                                                               
BLDT100  CLC   ACCTSK+1(2),=C'SK'   MEMO CONTRA IS AN SK ACCOUNT                
         BNE   BLDT110                                                          
         L     RE,AD69L            COPY 69 EL TO CREATE 6A                      
         MVC   DLPSEL(113),0(RE)                                                
         MVI   DLPSEL,X'6A'        BUILD SINGLE CREDIT ELEMENT                  
         MVC   DLPSCRAC,ACCTSK     TAKE CONTRA FROM ACCTAB                      
         MVC   DLPSCRNM,ACCTSKNM                                                
         MVC   DLPSANAL,FROMJOFF    USE OFFICE CODE                             
         OC    THISOFF,THISOFF     DO WE HAVE AN OFFICE SAVED?                  
         BZ    *+10                                                             
         MVC   DLPSANAL,THISOFF    OVERWRITE WITH OFFICE FROM 65 ELEM           
         IC    R1,1(R5)                                                         
         AR    R5,R1                                                            
*                                                                               
BLDT110  OC    ACCTSI,ACCTSI       CONTRA ACCOUNT SI ?                          
         BZ    BLDT120             NO                                           
*                                                                               
         USING ACMTD,R5                                                         
         XC    0(ACMTLNQ,R5),0(R5)                                              
         MVI   ACMTEL,ACMTELQ      ADD MEDIA TRANSFER FOR TO SI POSTING         
         MVI   ACMTLEN,ACMTLNQ                                                  
         MVI   ACMTSYS,C'J'                                                     
         MVC   ACMTMED,FROMJOBA+9     MEDIA CODE                                
         MVC   ACMTCLI(12),FROMJOBA+3 CLI/PRD/JOB                               
         MVC   ACMTMOS,THISMOS                                                  
         MVC   ACMTDSCP,FROMJOBN                                                
         ZAP   DUB,THISAMNT                                                     
         MP    DUB,=P'-1'                                                       
         CVB   R0,DUB                                                           
         STCM  R0,15,ACMTCOM         INCOME (COMMISSION)                        
         STCM  R0,15,ACMTINTL        INCOME (INTERNAL)                          
         IC    R1,ACMTLEN                                                       
         AR    R5,R1                                                            
*                                                                               
         USING DLPOSTD,R5                                                       
         XC    DLPSEL(113),DLPSEL                                               
         MVI   DLPSEL,X'6A'        BUILD SINGLE CREDIT ELEMENT                  
         MVI   DLPSLEN,113                                                      
         MVC   DLPSCRAC,ACCTSI     MINUS CREDIT TO SI                           
         MVC   DLPSCRNM,ACCTSINM                                                
         MVC   DLPSDBAC,FROMJOBA   CONTRA OF JOB                                
         MVC   DLPSDBNM,FROMJOBN                                                
         USING XTWAD,RF                                                         
         L     RF,ATWAXTRA                                                      
         CLC   FROMSALA,SPACES     IS THERE A SALES ACCOUNT?                    
         BE    *+16                NO                                           
         MVC   DLPSDBAC,FROMSALA   YES, USE THAT AS THE CONTRA                  
         MVC   DLPSDBNM,FROMSALN                                                
         ZAP   DUB,THISAMNT        MINUS AMOUNT                                 
         MP    DUB,=P'-1'                                                       
         ZAP   DLPSAMNT,DUB        MINUS AMOUNT                                 
         MVC   DLPSANAL,FROMJOFF                                                
         OC    THISOFF,THISOFF     DO WE HAVE AN OFFICE SAVED?                  
         BZ    *+10                                                             
         MVC   DLPSANAL,THISOFF    OVERWRITE WITH OFFICE FROM 65 ELEM           
         OI    DLPSTYPE,X'00'                                                   
         IC    R1,DLPSLEN                                                       
         AR    R5,R1                                                            
         DROP  R5                                                               
*                                                                               
BLDT120  TM    COMPSTAT,X'10'      COST POSTINGS FOR FROM JOB                   
         BZ    BLDT170             NOT ON COSTING                               
         CLC   CREDITA+1(2),=C'1R'                                              
         BNE   BLDT160             CONTRA IS NOT U/L 1R                         
         OC    THISHOUR,THISHOUR                                                
         BZ    BLDT160             NO HOURS                                     
*                                                                               
         CLI   DOTMS,C'Y'          MAKE TMS POSTINGS?                           
         BNE   BLDT140             NO                                           
*                                                                               
         L     RE,ASVBLOCK         CLEAR BUILD AREA                             
         LA    RF,SVBLOCKQ                                                      
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         USING SVBLOCKD,R2                                                      
         L     R2,ASVBLOCK                                                      
         MVC   SVSJCPJ,FROMJOBA+1  SJ CLIENT/PROD/JOB                           
         MVC   SVTASK,THISANAL     WORKCODE                                     
         MVC   SVCLIOFF,FROMJOFF   CLIENT OFFICE                                
         OC    THISOFF,THISOFF     DO WE HAVE A TMS OFFICE?                     
         BZ    *+10                NO                                           
         MVC   SVCLIOFF,THISOFF    YES, USE IT                                  
*                                                                               
         MVC   SVMOA,THISMOS       MONTH OF ACTIVITY                            
         MVC   SVTODAY3,TODAYP     ACTIVITY DATE                                
         MVC   SVSJDATE,THISDATE   TRANSACTION DATE                             
         ZAP   SVAMOUNT,THISAMNT   MINUS AMOUNT                                 
         MP    SVAMOUNT,=P'-1'                                                  
         MVI   SVIND,TIMIADJ       ITEM WAS ADJUSTED                            
         MVC   SVULA,ACCTSI+1      SAVE INCOME ACCOUNT                          
         OC    ACCTSI,ACCTSI       WAS IT INCOME OR SUSPENSE?                   
         BNZ   *+10                                                             
         MVC   SVULA,ACCTSK+1      SAVE SUSPENSE ACCOUNT                        
*                                                                               
         MVC   SVPIDNO,THISPID     SAVE PID FOR CADET                           
         MVC   SV1RCODE,CREDITA+3  SAVE 1R ACCOUNT                              
*                                                                               
         USING PRTELD,RF                                                        
         LA    RF,THISHOUR                                                      
         MVI   SVTYPE,TIMTCB       CLIENT BILLABLE                              
         TM    PRTSTAT,PRTSRTEQ                                                 
         BNO   *+8                                                              
         MVI   SVTYPE,TIMTCR       CLIENT REALIZATION                           
         TM    PRTSTAT,PRTSNOTQ                                                 
         BNO   *+8                                                              
         MVI   SVTYPE,TIMTCN       CLIENT NON BILLABLE                          
         ZAP   SVHOURS,PRTHOUR     HOURS                                        
         MP    SVHOURS,=P'-1'                                                   
         ZAP   SVRATE,PRTRATE      HOURLY RATE                                  
         MVC   SVRATEFF,PRTSTRT    EFFECTIVE DATE                               
         DROP  RF                                                               
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,1,THISNRLN        COPY NARRATIVE                              
         CH    R1,=Y(L'SVNARR)                                                  
         BNH   *+8                                                              
         LA    R1,L'SVNARR                                                      
         SH    R1,=H'1'                                                         
         BNP   BLDT130                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SVNARR(0),THISNARR                                               
         LA    R1,1(R1)                                                         
         STC   R1,SVNARRLN                                                      
BLDT130  BAS   RE,TIMEBLD                                                       
         SR    R1,R1               ATTATCH CLUSTER TO NEW RECORD                
         ICM   R1,3,CLUSTLNQ       R1 = LENGTH OF CLUSTER                       
         BZ    BLDT140                                                          
         LA    R0,CLUSTER          R0 = AREA TO MOVE FROM                       
         LR    RF,R1               RF = LENGTH OF CLUSTER                       
         LR    RE,R5               RE = AREA TO MOVE TO                         
         AR    R5,R1               BUMP R5 PAST CLUSTER                         
         MVCL  RE,R0                                                            
*                                                                               
BLDT140  SR    R1,R1                                                            
         IC    R1,THISHOUR+1       GET LENGTH FOR MOVE                          
         SH    R1,=H'1'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),THISHOUR                                                 
*                                                                               
         USING ACPERSD,R5                                                       
         ZAP   DUB,ACPSHOUR                                                     
         MP    DUB,=P'-1'                                                       
         ZAP   ACPSHOUR,DUB                                                     
         IC    R1,1(R5)                                                         
         AR    R5,R1                                                            
*                                                                               
         USING TRCASHD,R5                                                       
         MVC   TRCSEL(2),=X'5009'                                               
         MVI   TRCSTYPE,C'H'                                                    
         ZAP   TRCSAMNT,DUB                                                     
         IC    R1,1(R5)                                                         
         AR    R5,R1                                                            
*                                                                               
         USING ACPCD,R5                                                         
         MVC   ACPCEL(2),=X'5122'                                               
         MVC   ACPCCLI,FROMJOBA                                                 
         MVC   ACPCPRJT,FROMJOBA                                                
         MVC   ACPCTSK,THISANAL                                                 
         IC    R1,1(R5)                                                         
         AR    R5,R1                                                            
*                                                                               
         USING TRSELD,R5           BUILD SKELETON X'60' ELEMENT                 
         OC    THATSTA2,THATSTA2                                                
         BZ    BLDT150                                                          
         XC    0(TRSLNQ,R5),0(R5)                                               
         MVI   TRSEL,TRSELQ                                                     
         MVI   TRSLN,TRSLNQ                                                     
         MVC   TRSSTAT2,THATSTA2                                                
         MVC   TRSSTAT3,THATSTA3                                                
         IC    R1,1(R5)                                                         
         AR    R5,R1                                                            
*                                                                               
         USING DLPOSTD,R5                                                       
BLDT150  XC    DLPSEL(113),DLPSEL                                               
         MVI   DLPSEL,X'69'        BUILD SINGLE DEBIT ELEMENT                   
         MVI   DLPSLEN,113                                                      
         MVC   DLPSDBAC,CREDITA    MINUS DEBIT TO 1R                            
         MVC   DLPSDBNM,ACCTCTRN                                                
         L     RF,ATWAXTRA                                                      
         USING XTWAD,RF                                                         
         MVC   DLPSCRAC,FROMCST    CONTRA IS COSTING                            
         MVC   DLPSCRNM,FROMCSTN                                                
         OC    THISCST,THISCST     DO WE HAVE A COSTING ACCT?                   
         BZ    *+16                                                             
         MVC   DLPSCRAC,THISCST    OVERWRITE WITH COSTING FROM 2C ELEM          
         MVC   DLPSCRNM,THISCSTN                                                
*                                                                               
         MVC   DLPSANAL,FROMJOFF                                                
         OC    THISOFF,THISOFF     DO WE HAVE AN OFFICE SAVED?                  
         BZ    *+10                                                             
         MVC   DLPSANAL,THISOFF    OVERWRITE WITH OFFICE FROM 65 ELEM           
*                                                                               
         ZAP   DLPSAMNT,=P'0'      ZERO AMOUNT(JUST WANT THE HOURS)             
         TM    THISSTAT,X'01'                                                   
         BZ    *+8                                                              
         OI    DLPSTYPE,X'40'                                                   
         IC    R1,1(R5)                                                         
         AR    R5,R1                                                            
*                                                                               
         USING XTWAD,RF                                                         
BLDT160  L     RF,ATWAXTRA                                                      
         OC    THISCST,THISCST     ANY COST ACCOUNT FROM 2C ELEMENT?            
         BZ    BLDT165             NO, USE FROM ACCOUNT                         
         CLC   THISCST,ACCTCST                                                  
         BE    BLDT170             DON'T POST IF FROM/TO THE SAME               
         B     *+14                CHECK SI NOW                                 
*                                                                               
BLDT165  CLC   FROMCST,ACCTCST     DON'T POST IF FROM AND TO ACCOUNTS           
         BE    BLDT170             ARE THE SAME                                 
*                                                                               
         OC    ACCTSI,ACCTSI       WAS THERE AN SI POSTING ?                    
         BZ    BLDT170             NO, SKIP THIS THEN                           
*                                                                               
         USING DLPOSTD,R5                                                       
         XC    DLPSEL(113),DLPSEL                                               
         MVI   DLPSEL,X'68'        DOUBLE POSTINGS                              
         MVI   DLPSLEN,113                                                      
         MVC   DLPSDBAC,FROMCST    MINUS DEBIT TO 1C                            
         MVC   DLPSDBNM,FROMCSTN                                                
         OC    THISCST,THISCST     DO WE HAVE A COSTING ACCT?                   
         BZ    *+16                                                             
         MVC   DLPSDBAC,THISCST    OVERWRITE WITH COSTING FROM 2C ELEM          
         MVC   DLPSDBNM,THISCSTN                                                
         MVC   DLPSCRAC,ACCT12     MINUS CREDIT TO 12                           
         MVC   DLPSCRNM,ACCT12NM                                                
         ZAP   DLPSAMNT,THISAMNT   MINUS AMOUNT                                 
         MP    DLPSAMNT,=P'-1'                                                  
         MVC   DLPSANAL,FROMJOFF                                                
         OC    THISOFF,THISOFF     DO WE HAVE AN OFFICE SAVED?                  
         BZ    *+10                                                             
         MVC   DLPSANAL,THISOFF    OVERWRITE WITH OFFICE FROM 65 ELEM           
         OI    DLPSTYPE,X'80'      SUBSIDIARY POSTING                           
         IC    R1,DLPSLEN                                                       
         AR    R5,R1                                                            
         DROP  RF                                                               
         EJECT                                                                  
*                                                                               
* BUILD POSTINGS FOR TO JOB                                                     
*                                                                               
         USING ACOTHERD,R5                                                      
BLDT170  OC    THISOTH,THISOTH     X'23' - OTHERS ELEMENT                       
         BZ    BLDT180                                                          
         MVC   ACOTEL(15),SPACES                                                
         MVC   ACOTEL(2),=X'230F'                                               
         MVC   ACOTNUM(6),THISOTH                                               
         IC    R1,ACOTLEN                                                       
         AR    R5,R1                                                            
*                                                                               
         USING ACNOD,R5                                                         
BLDT180  OC    THISOTH2,THISOTH2   X'25' - FREE FORM NUMBER ELEMENT             
         BZ    BLDT190                                                          
         ZIC   R1,THISOTH2+L'THISOTH2-1                                         
         MVI   ACNOEL,X'25'                                                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ACNO(0),THISOTH2                                                 
         LA    R1,3(R1)                                                         
         STC   R1,ACNOLEN                                                       
         AR    R5,R1                                                            
*                                                                               
         USING SPAELD,R5                                                        
BLDT190  OC    THISCST,THISCST     X'2C' - SPECIAL POSTING A/C ELEM             
         BZ    BLDT200                                                          
         MVI   SPAEL,SPAELQ                                                     
         MVI   SPALN,SPALNQ                                                     
         MVI   SPATYPE,SPATCCST    CLIENT COSTING ACCOUNT                       
         MVC   SPAAULA,ACCTCST+1   COSTING U/L/A                                
         SR    R1,R1                                                            
         IC    R1,SPALN                                                         
         AR    R5,R1                                                            
*                                                                               
BLDT200  OC    THISHOUR,THISHOUR   X'40' - PERSONNEL RATE ELEMENT               
         BZ    BLDT210                                                          
         SR    R1,R1                                                            
         IC    R1,THISHOUR+1       GET LENGTH FOR MOVE                          
         SH    R1,=H'1'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),THISHOUR                                                 
         IC    R1,1(R5)                                                         
         AR    R5,R1                                                            
*                                                                               
BLDT210  OC    THISTRSD,THISTRSD   X'4C' - SUBSIDIARY POSTING ELEMENT           
         BZ    BLDT220                                                          
         SR    R1,R1                                                            
         IC    R1,THISTRSD+1                                                    
         SH    R1,=H'1'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),THISTRSD                                                 
         IC    R1,1(R5)                                                         
         AR    R5,R1                                                            
*                                                                               
         USING PXDELD,R5                                                        
BLDT220  L     RE,AD4EL            X'4E' - POSTING XFER DETAIL ELEMENT          
         MVC   0(30,R5),0(RE)      DUPLICATE PREVIOUS ONE                       
         MVI   PXDTYPE,C'F'                                                     
         MVC   PXDFRTO,FROMJOBA                                                 
         MVC   PXDFRTOW,THISANAL                                                
         OC    PXDFRTO,PXDFRTO                                                  
         BNZ   *+6                                                              
         DC    H'0'                                                             
         IC    R1,1(R5)                                                         
         AR    R5,R1                                                            
*                                                                               
         OC    THISCASH,THISCASH   X'50' - SUBSIDIARY CASH                      
         BZ    BLDT230                                                          
         MVC   0(L'THISCASH,R5),THISCASH                                        
         IC    R1,1(R5)                                                         
         AR    R5,R1                                                            
*                                                                               
         USING TRSELD,R5                                                        
BLDT230  OC    THATSTA2,THATSTA2   TRANSACTION STATUS ELEMENT                   
         BZ    BLDT240                                                          
         XC    0(TRSLNQ,R5),0(R5)                                               
         MVI   TRSEL,TRSELQ                                                     
         MVI   TRSLN,TRSLNQ                                                     
         MVC   TRSSTAT2,THATSTA2                                                
         MVC   TRSSTAT3,THATSTA3                                                
         IC    R1,1(R5)                                                         
         AR    R5,R1                                                            
*                                                                               
         USING ANOELD,R5                                                        
BLDT240  OC    THISOFF,THISOFF     X'65' - ANALYZED OFFICE ELEMENT              
         BZ    BLDT250                                                          
         MVI   ANOEL,ANOELQ                                                     
         MVI   ANOLN,ANOLNQ                                                     
         MVI   ANOTYPE,ANOTCLI     CLIENT OFFICE                                
         MVC   ANOOFFC,ACCTOFF     COSTING U/L/A                                
         SR    R1,R1                                                            
         IC    R1,ANOLN                                                         
         AR    R5,R1                                                            
*                                                                               
BLDT250  OC    THISUNPR,THISUNPR   X'7C' - UNIT PRICING ELEMENT                 
         BZ    BLDT255                                                          
         SR    R1,R1                                                            
         IC    R1,THISUNPR+1       GET LENGTH FOR MOVE                          
         SH    R1,=H'1'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),THISUNPR                                                 
         IC    R1,1(R5)                                                         
         AR    R5,R1                                                            
*                                                                               
         USING PXDELD,R5                                                        
BLDT255  OC    THISORGJ,THISORGJ                                                
*NMAL    BZ    BLDT260                                                          
         BZ    BLDT256                                      NMAL                
         MVI   PXDEL,PXDELQ                                                     
         MVI   PXDLN,PXDLN2Q                                                    
         MVI   PXDTYPE,PXDTORG                                                  
         MVC   PXDDATE,THISORGD                                                 
         MVC   PXDFRTO,THISORGJ                                                 
         MVC   PXDFRTOW,THISORGW                                                
         MVC   PXDOBAT,THISORGB                                                 
         MVC   PXDOSEQ,THISORGS                                                 
         IC    R1,1(R5)                                                         
         AR    R5,R1                                                            
*                                                                               
         USING ASKELD,R5                                    NMAL                
BLDT256  OC    THISASK,THISASK                              NMAL                
         BZ    BLDT257                                      NMAL                
         MVI   ASKEL,ASKELQ                                 NMAL                
         MVI   ASKLN,ASKLNQ                                 NMAL                
         MVC   ASKKEY,THISASK                               NMAL                
         IC    R1,1(R5)                                     NMAL                
         AR    R5,R1                                        NMAL                
*                                                           NMAL                
         USING SCIELD,R5                                    NMAL                
BLDT257  OC    THISCRDE,THISCRDE                            NMAL                
         BZ    BLDT260                                      NMAL                
         MVC   SCIELD(SCITCPLQ),THISCRDE                    NMAL                
         MVC   SCIAMNT,THISAMNT                  NMALIK     nmal                
         IC    R1,1(R5)                                     NMAL                
         AR    R5,R1                                        NMAL                
*                                                           NMAL                
         USING PAKELD,R5                                                        
BLDT260  OC    THISPAKL,THISPAKL   X'D4' - PAYABLE ACCOUNT ELEMENT              
         BZ    BLDT265                                                          
         SR    R1,R1                                                            
         IC    R1,THISPAKL+1                                                    
         SHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),THISPAKL                                                 
         IC    R1,1(R5)                                                         
         AR    R5,R1                                                            
*                                                                               
         USING DLPOSTD,R5                                                       
BLDT265  L     RE,AD69L                                                         
         MVC   0(113,R5),0(RE)     DUPLICATE X'69' ELEMENT                      
         CLC   CREDITA+1(2),=C'SK' IF CONTRA IS AN SK ACCOUNT                   
         BNE   BLDT270                                                          
         MVC   DLPSCRAC,ACCTSK     TAKE CONTRA FROM ACCTAB                      
         MVC   DLPSCRNM,ACCTSKNM                                                
*                                                                               
BLDT270  MVC   DLPSDBAC,ACCTTOJB   TO-JOB IS DEBIT ACCOUNT                      
         MVC   DLPSDBNM,ACCTTOJN                                                
         ZAP   DLPSAMNT,THISAMNT                                                
         MVC   DLPSANAL,THISINWC   USE INPUT W-CODE IF PRESENT                  
         CLI   NEWSTAT,C'C'        IS NEW STATUS  COMMISSIONBALE                
         BNE   *+8                                                              
         NI    DLPSTYPE,X'FF'-X'40' IF SO - TURN OFF(POSSIBLE) NON-COMM         
         CLI   NEWSTAT,C'N'         OR IF NON IS INPUT                          
         BNE   *+8                                                              
         OI    DLPSTYPE,X'40'       TURN ON NON BIT                             
         ST    R5,AD69L                                                         
*                                                                               
         IC    R1,1(R5)                                                         
         AR    R5,R1                                                            
         CLC   CREDITA+1(2),=C'SK' IF CONTRA IS AN SK ACCOUNT                   
         BNE   BLDT280                                                          
         L     RE,AD69L            COPY 69 EL TO CREATE 6A                      
         MVC   DLPSEL(113),0(RE)                                                
         MVI   DLPSEL,X'6A'         BUILD SINGLE CREDIT ELEMENT                 
         MVC   DLPSANAL,ACCTOFF     USE OFFICE CODE                             
         ZIC   R1,1(R5)                                                         
         AR    R5,R1                                                            
         B     BLDT290                                                          
*                                                                               
BLDT280  CLC   ACCTSK+1(2),=C'SK'   MEMO IS AN SK ACCOUNT                       
         BNE   BLDT290                                                          
         L     RE,AD69L            COPY 69 EL TO CREATE 6A                      
         MVC   DLPSEL(113),0(RE)                                                
         MVI   DLPSEL,X'6A'        BUILD SINGLE CREDIT ELEMENT                  
         MVC   DLPSCRAC,ACCTSK     TAKE CONTRA FROM ACCTAB                      
         MVC   DLPSCRNM,ACCTSKNM                                                
         MVC   DLPSANAL,ACCTOFF     USE OFFICE CODE                             
         ZIC   R1,1(R5)                                                         
         AR    R5,R1                                                            
*                                                                               
BLDT290  OC    ACCTSI,ACCTSI       CONTRA ACCOUNT SI ?                          
         BZ    BLDT300             NO                                           
*                                                                               
         USING ACMTD,R5                                                         
         XC    0(ACMTLNQ,R5),0(R5)                                              
         MVI   ACMTEL,ACMTELQ      ADD MEDIA TRANSFER FOR TO SI POSTING         
         MVI   ACMTLEN,ACMTLNQ                                                  
         MVI   ACMTSYS,C'J'                                                     
         MVC   ACMTMED,ACCTTOJB+9    MEDIA CODE                                 
         MVC   ACMTCLI(12),ACCTTOJB+3  CLI/PRD/JOB                              
         MVC   ACMTMOS,THISMOS                                                  
         MVC   ACMTDSCP,ACCTTOJN                                                
         ZAP   DUB,THISAMNT                                                     
         CVB   R0,DUB                                                           
         STCM  R0,15,ACMTCOM         INCOME (COMMISSION)                        
         STCM  R0,15,ACMTINTL        INCOME (INTERNAL)                          
         IC    R1,ACMTLEN                                                       
         AR    R5,R1                                                            
*                                                                               
         USING DLPOSTD,R5                                                       
         XC    DLPSEL(113),DLPSEL                                               
         MVI   DLPSEL,X'6A'        BUILD SINGLE CREDIT ELEMENT                  
         MVI   DLPSLEN,113                                                      
         MVC   DLPSCRAC,ACCTSI     CREDIT TO SI                                 
         MVC   DLPSCRNM,ACCTSINM                                                
         MVC   DLPSDBAC,ACCTTOJB   CONTRA OF JOB                                
         MVC   DLPSDBNM,ACCTTOJN                                                
         CLC   ACCTSALA,SPACES     IS THERE A SALES ACCOUNT?                    
         BE    *+16                NO                                           
         MVC   DLPSDBAC,ACCTSALA   YES, USE IT AS CONTRA                        
         MVC   DLPSDBNM,ACCTSALN                                                
         ZAP   DLPSAMNT,THISAMNT                                                
         MVC   DLPSANAL,ACCTOFF                                                 
         OI    DLPSTYPE,X'00'                                                   
         IC    R1,DLPSLEN                                                       
         AR    R5,R1                                                            
*                                                                               
* DO COST POSTINGS FOR TO JOB                                                   
*                                                                               
BLDT300  TM    COMPSTAT,X'10'                                                   
         BZ    BLDT350             NOT ON COSTING                               
         CLC   CREDITA+1(2),=C'1R'                                              
         BNE   BLDT340             CONTRA IS NOT U/L 1R                         
         OC    THISHOUR,THISHOUR                                                
         BZ    BLDT340             NO HOURS                                     
         CLI   DOTMS,C'Y'          MAKING TMS POSTINGS?                         
         BNE   BLDT320             NO                                           
*                                                                               
BLDT304  L     RE,ASVBLOCK         CLEAR BUILD AREA                             
         LA    RF,SVBLOCKQ                                                      
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         USING SVBLOCKD,R2                                                      
         L     R2,ASVBLOCK                                                      
         MVC   SVSJCPJ,ACCTTOJB+1  SJ CLIENT/PROD/JOB                           
         MVC   SVTASK,THISINWC     WORKCODE                                     
         MVC   SVCLIOFF,ACCTOFF    CLIENT OFFICE                                
         MVC   SVMOA,THISMOS       MONTH OF ACTIVITY                            
         MVC   SVTODAY3,TODAYP     ACTIVITY DATE                                
         MVC   SVSJDATE,THISDATE   TRANSACTION DATE                             
         ZAP   SVAMOUNT,THISAMNT   AMOUNT                                       
         MVI   SVIND,TIMIADJ       ITEM WAS ADJUSTED                            
         MVC   SVULA,ACCTSI+1      SAVE INCOME ACCOUNT                          
         OC    ACCTSI,ACCTSI       WAS IT INCOME OR SUSPENSE?                   
         BNZ   *+10                                                             
         MVC   SVULA,ACCTSK+1      SAVE SUSPENSE ACCOUNT                        
*                                                                               
         MVC   SVPIDNO,THISPID     SAVE PID FOR CADET                           
         MVC   SV1RCODE,CREDITA+3  SAVE 1R ACCOUNT                              
*                                                                               
         USING PRTELD,RF                                                        
         LA    RF,THISHOUR                                                      
         MVI   SVTYPE,TIMTCB       CLIENT BILLABLE                              
         TM    PRTSTAT,PRTSRTEQ                                                 
         BNO   *+8                                                              
         MVI   SVTYPE,TIMTCR       CLIENT REALIZATION                           
         TM    PRTSTAT,PRTSNOTQ                                                 
         BNO   *+8                                                              
         MVI   SVTYPE,TIMTCN       CLIENT NON BILLABLE                          
         ZAP   SVHOURS,PRTHOUR     HOURS                                        
         ZAP   SVRATE,PRTRATE      HOURLY RATE                                  
         MVC   SVRATEFF,PRTSTRT    EFFECTIVE DATE                               
         DROP  RF                                                               
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,1,THISNRLN        COPY NARRATIVE                              
         CH    R1,=Y(L'SVNARR)                                                  
         BNH   *+8                                                              
         LA    R1,L'SVNARR                                                      
         SH    R1,=H'1'                                                         
         BNP   BLDT310                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SVNARR(0),THISNARR                                               
         LA    R1,1(R1)                                                         
         STC   R1,SVNARRLN                                                      
BLDT310  BAS   RE,TIMEBLD                                                       
         SR    R1,R1               ATTATCH CLUSTER TO NEW RECORD                
         ICM   R1,3,CLUSTLNQ       R1 = LENGTH OF CLUSTER                       
         BZ    BLDT320                                                          
         LA    R0,CLUSTER          R0 = AREA TO MOVE FROM                       
         LR    RF,R1               RF = LENGTH OF CLUSTER                       
         LR    RE,R5               RE = AREA TO MOVE TO                         
         AR    R5,R1               BUMP R5 PAST CLUSTER                         
         MVCL  RE,R0                                                            
*                                                                               
BLDT320  SR    R1,R1                                                            
         IC    R1,THISHOUR+1       GET LENGTH FOR MOVE                          
         SH    R1,=H'1'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),THISHOUR                                                 
         IC    R1,1(R5)                                                         
         AR    R5,R1                                                            
*                                                                               
         LA    RF,THISHOUR                                                      
         USING ACPERSD,RF                                                       
         ZAP   DUB,ACPSHOUR                                                     
         USING TRCASHD,R5                                                       
         MVC   TRCSEL(2),=X'5009'                                               
         MVI   TRCSTYPE,C'H'                                                    
         ZAP   TRCSAMNT,DUB                                                     
         IC    R1,1(R5)                                                         
         AR    R5,R1                                                            
*                                                                               
         USING ACPCD,R5                                                         
         MVC   ACPCEL(2),=X'5122'                                               
         MVC   ACPCCLI,ACCTTOJB                                                 
         MVC   ACPCPRJT,ACCTTOJB                                                
         MVC   ACPCTSK,THISINWC                                                 
         IC    R1,1(R5)                                                         
         AR    R5,R1                                                            
*                                                                               
         USING TRSELD,R5           BUILD SKELETON X'60' ELEMENT                 
         OC    THATSTA2,THATSTA2                                                
         BZ    BLDT330                                                          
         XC    0(TRSLNQ,R5),0(R5)                                               
         MVI   TRSEL,TRSELQ                                                     
         MVI   TRSLN,TRSLNQ                                                     
         MVC   TRSSTAT2,THATSTA2                                                
         MVC   TRSSTAT3,THATSTA3                                                
         IC    R1,1(R5)                                                         
         AR    R5,R1                                                            
*                                                                               
         USING DLPOSTD,R5                                                       
BLDT330  XC    DLPSEL(113),DLPSEL                                               
         MVI   DLPSEL,X'69'        BUILD SINGLE DEBIT ELEMENT                   
         MVI   DLPSLEN,113                                                      
         MVC   DLPSDBAC,CREDITA    DEBIT TO IR                                  
         MVC   DLPSDBNM,ACCTCTRN                                                
         MVC   DLPSCRAC,ACCTCST    CONTRA IS COSTING                            
         MVC   DLPSCRNM,ACCTCSTN                                                
         ZAP   DLPSAMNT,=P'0'      ZERO AMOUNT(JUST WANT THE HOURS)             
         MVC   DLPSANAL,ACCTOFF                                                 
         CLI   NEWSTAT,C'C'        IS NEW STATUS  COMMISSIONBALE                
         BNE   *+8                                                              
         NI    DLPSTYPE,X'FF'-X'40' IF SO - TURN OFF(POSSIBLE) NON-COMM         
         CLI   NEWSTAT,C'N'         OR IF NON IS INPUT                          
         BNE   *+8                                                              
         OI    DLPSTYPE,X'40'       TURN ON NON BIT                             
         IC    R1,1(R5)                                                         
         AR    R5,R1                                                            
*                                                                               
         USING XTWAD,RF                                                         
BLDT340  L     RF,ATWAXTRA                                                      
         OC    THISCST,THISCST     ANY COST ACCOUNT FROM 2C ELEMENT?            
         BZ    BLDT345             NO, USE FROM ACCOUNT                         
         CLC   THISCST,ACCTCST                                                  
         BE    BLDT350             DON'T POST IF FROM/TO THE SAME               
         B     *+14                CHECK SI NOW                                 
*                                                                               
BLDT345  CLC   FROMCST,ACCTCST     DON'T POST IF FROM AND TO ACCOUNTS           
         BE    BLDT350             ARE THE SAME                                 
*                                                                               
         OC    ACCTSI,ACCTSI       WAS THERE AN SI POSTING ?                    
         BZ    BLDT350             NO, SKIP THIS THEN                           
*                                                                               
         USING DLPOSTD,R5                                                       
         XC    DLPSEL(113),DLPSEL                                               
         MVI   DLPSEL,X'68'        DOUBLE POSTINGS                              
         MVI   DLPSLEN,113                                                      
         MVC   DLPSDBAC,ACCTCST    DEBIT TO 1C                                  
         MVC   DLPSDBNM,ACCTCSTN                                                
         MVC   DLPSCRAC,ACCT12     CREDIT TO 12                                 
         MVC   DLPSCRNM,ACCT12NM                                                
         ZAP   DLPSAMNT,THISAMNT   MINUS AMOUNT                                 
         MVC   DLPSANAL,ACCTOFF                                                 
         OI    DLPSTYPE,X'80'      SUBSIDIARY POSTING                           
         IC    R1,DLPSLEN                                                       
         AR    R5,R1                                                            
         DROP  RF                                                               
*                                                                               
BLDT350  MVI   0(R5),0             SET E-O-R                                    
         LA    R1,IOAREA-1         SET RECORD LENGTH                            
         SR    R5,R1                                                            
         STH   R5,DUB                                                           
         MVC   IOAREA(2),DUB                                                    
         XMOD1 1                                                                
         DROP  R5                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BUILD 8B TIMEL TO BE HOOKED ONTO 1R POSTINGS                        *         
***********************************************************************         
         SPACE 1                                                                
*TIMEBLD  NTR1                                             NMAL                 
TIMEBLD  NTR1  BASE=*,LABEL=*                              NMAL                 
         XC    CLUSTLNQ,CLUSTLNQ   CLEAR CLUSTER LENGTH                         
*                                                                               
         LA    RE,CLUSTER          CLEAR CLUSTER BUILD AREA                     
         LA    RF,L'CLUSTER                                                     
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         LA    R3,CLUSTER          *** 8B INPUT DETAIL ELEMENT ***              
         USING TIMELD,R3                                                        
         MVI   TIMEL,TIMELQ                                                     
         MVI   TIMETYP,TIMEINP                                                  
         MVC   TIMACC,SVSJCPJ      SJ ACCOUNT                                   
         MVC   TIMOFF,SVCLIOFF     CLIENT OFFICE                                
         MVC   TIMTSK,SVTASK       TASK CODE                                    
         MVC   TIMTTYP,SVTYPE      TYPE OF TIME B/N/R                           
         MVC   TIMIND,SVIND                                                     
         MVC   TIMMOA,SVMOA        MONTH OF ACTIVITY                            
         OI    TIMSTAT,TIMNOTAX                                                 
         MVC   TIMADAT,SVTODAY3    ACTIVITY DATE                                
         ZAP   TIMHRS,SVHOURS      HOURS                                        
         LA    R1,TIMILN1Q         ADD TO RECORD LENGTH                         
*                                                                               
         CLI   SVTYPE,TIMTCB       B AND R TIME HAVE RATE                       
         BE    *+12                                                             
         CLI   SVTYPE,TIMTCR                                                    
         BNE   TIME100                                                          
         MVC   TIMRATE,SVRATE      RATE                                         
         MVI   TIMRBSTA,TIMRORAT   RATE INDICATOR                               
         MVC   TIMREFF,SVRATEFF    RATE EFFECTIVE DATE                          
         CLC   SVULA,SPACES                                                     
         BNH   *+16                                                             
         MVC   TIMINC,SVULA        INCOME ACCOUNT                               
         OC    TIMINC,SPACES                                                    
         ZAP   TIMAMNT,SVAMOUNT    AMOUNT                                       
         LA    R1,TIMILN2Q         ADD TO RECORD LENGTH                         
*                                                                               
TIME100  STC   R1,TIMLN                                                         
         SR    R0,R0                                                            
         ICM   R0,3,CLUSTLNQ                                                    
         AR    R0,R1                                                            
         STCM  R0,3,CLUSTLNQ                                                    
*                                                                               
         CLI   SVNARRLN,0          *** BUILD 8B NARRATIVE ELEMENT ***           
         BE    TIME200                                                          
*                                                                               
         SR    R1,R1                                                            
         IC    R1,1(R3)                                                         
         AR    R3,R1                                                            
*                                                                               
         MVI   TIMEL,TIMELQ                                                     
         MVI   TIMETYP,TIMENAR                                                  
         SR    R1,R1                                                            
         IC    R1,SVNARRLN                                                      
         SH    R1,=H'1'                                                         
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TIMNARR(0),SVNARR                                                
*                                                                               
         LA    R0,TIMHLNQ          HEADER+1 FOR EX                              
         SR    R1,R1                                                            
         IC    R1,SVNARRLN                                                      
         AR    R1,R0                                                            
         STC   R1,TIMLN                                                         
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,CLUSTLNQ                                                    
         AR    R0,R1                                                            
         STCM  R0,3,CLUSTLNQ                                                    
*                                                                               
TIME200  CLI   THATSTA3,TRSSMCS    IS THIS BRANDO TIME?                         
         BNE   TIMEX               NO, DONE                                     
*                                                                               
         SR    R1,R1                                                            
         IC    R1,1(R3)                                                         
         AR    R3,R1                                                            
*                                                                               
*        XC    CADETBLK,CADETBLK   BUILD CADET BLOCK                            
         L     RE,ACADTBLK         CLEAR CADETBLK                               
         LA    RF,L'CADETBLK                                                    
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         USING CADETD,R6                                                        
         L     R6,ACADTBLK                                                      
         MVC   CCOMFACS,ACOMFACS                                                
         MVC   CADASEC,ASECBLK                                                  
         MVC   CADAIOA,ACADETBK    A(IO AREA TO USE)                            
         ST    R3,CADAELM                                                       
         OC    LVLS1R,LVLS1R       ANY LEVELS?                                  
         BZ    TIMEX               No - Skip                                    
         MVC   CAD1RLVS,LVLS1R     1R CUMULATIVE LEVEL LENGTHS                  
         MVC   CADSJLVS,LVLSSJ     SJ CUMULATIVE LEVEL LENGTHS                  
         MVC   CAD1RACT,SV1RCODE   1R ACCOUNT CODE                              
         MVC   CADTMULA,SVSJCPJ    SJ U/L/ACCOUNT CODE                          
         MVC   CADCPYCD,COMPANY                                                 
         MVC   CADPIDN,SVPIDNO     PID                                          
         ZAP   CADTSHRS,SVHOURS    HOURS                                        
         MVC   CADTSPED,THISDATE   PERIOD END DATE                              
         MVC   CADTODAY,TODAYP                                                  
         MVC   CADCUID,TWAUSRID    CONNECTED USER-ID                            
         MVI   CADXCALL,CADXFXRR                                                
         GOTO1 ACADET,CADETD                                                    
*                                                                               
         MVI   TIMEL,TIMELQ        BUILD ETIME STATUS ELEMENT                   
         MVI   TIMLN,TIMETLNQ                                                   
         MVI   TIMETYP,TIMETIME                                                 
         MVC   TIMEIDNO,CADHIROW                                                
         OI    TIMEPST1,TIMESAPR   MARK APPROVED                                
         MVC   TIMEPIDC,SVPIDNO                                                 
         MVC   TIMETPDT,THISDATE   ACTUAL PERIOD END DATE                       
*                                                                               
         GOTO1 VDATCON,BOPARM,(1,THISDATE),(0,WORK)                             
         GOTO1 VADDAY,BOPARM,(C'D',WORK),WORK+6,F'-6'                           
         GOTO1 VDATCON,BOPARM,(0,WORK+6),(1,TIMETDT1)                           
         MVC   TIMEHRS1,SVHOURS                                                 
*                                                                               
         LA    R0,6                                                             
         LA    R6,TIMETDT2                                                      
*                                                                               
TIME300  MVC   WORK(6),WORK+6                                                   
         GOTO1 VADDAY,BOPARM,(C'D',WORK),WORK+6,F'1'                            
         GOTO1 VDATCON,BOPARM,(0,WORK+6),(1,0(R6))                              
         ZAP   3(L'TIMEHRS1,R6),=P'0'                                           
         AHI   R6,L'TIMEHRS1+L'TIMETDT1                                         
         BCT   R0,TIME300                                                       
*                                                                               
         SR    R1,R1                                                            
         IC    R1,TIMLN                                                         
         SR    R0,R0                                                            
         ICM   R0,3,CLUSTLNQ                                                    
         AR    R0,R1                                                            
         STCM  R0,3,CLUSTLNQ                                                    
*                                                                               
TIMEX    DS    0H                                                               
         XIT1                                                                   
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        TEST IF TMS POSTINGS NEEDED                                            
*        SET DOTMS TO Y IF: 1) ON TMS                                           
*                           2) TMS COSTING ACCOUNT                              
*                           3) TMS OFFICE CODE (IF ON 2-BYTE OFFICES)           
*                           4) TMS RECORD EXISTS                                
***********************************************************************         
         USING CPYELD,R3                                                        
TESTMS   DS    0D                                                               
         NMOD1 0,*TEST*                                                         
         L     RC,0(R1)                                                         
         L     R2,ASVBLOCK                                                      
*                                                                               
         MVI   DOTMS,C'N'             ASSUME NO TMS                             
         XC    TMSDATE,TMSDATE                                                  
*                                                                               
         LA    R3,BCCPYEL                                                       
         TM    CPYSTAT7,CPYSTMSY      ARE THEY ON TMS?                          
         BNO   TESTMSX                NO, INDICATE NO                           
         CLI   CPYLN,CPYLN3Q          YES, DO WE NEW ELEMENT?                   
         BL    TESTMS2                NO                                        
         OC    CPYTMSSD,CPYTMSSD      DO WE HAVE A START DATE?                  
         BZ    TESTMS2                NO                                        
         MVC   TMSDATE,CPYTMSSD       YES, SAVE IT                              
         GOTO1 VDATCON,DMCB,(1,THISDATE),(2,WORK)                               
         CLC   WORK(2),CPYTMSSD       YES,COMPARE TO TRANSACTION DATE           
         BL    TESTMSX                TOO LOW, SKIP IT                          
*                                                                               
TESTMS2  OC    THISCST,THISCST        OK, DO WE HAVE THE 1C ACCOUNT?            
         BZ    TESTMSX                NO                                        
*                                                                               
         MVC   SVCLIOFF,THISOFF                                                 
         TM    BCCPYST4,CPYSOFF2      ON NEW OFFICES?                           
         BO    TESTMS4                YES, CHECK THISOFF                        
*                                                                               
         CLC   THATDATE,TMSDATE       COMPARE ADD DATE TO TMS DATE              
         BL    TESTMSX                TOO LOW, SKIP IT                          
         MVC   SVCLIOFF,SPACES        OK, USE OFFICE OF BLANKS                  
         B     TESTMS6                BRANCH AROUND                             
*                                                                               
TESTMS4  OC    THISOFF,THISOFF        DO WE HAVE THE TMS OFFICE?                
         BZ    TESTMSX                NO, MUST HAVE FOR 2-BYTES                 
*                                                                               
TESTMS6  XC    KEY,KEY                                                          
         MVC   KEY(15),CREDITA        READ RECORD TO SEE IF TMS                 
         MVC   KEY+(ACKEYWRK-ACKEYD)(L'SVCLIOFF),SVCLIOFF                       
         MVC   KEY+(ACKEYCON-ACKEYD)(L'THISCST),THISCST                         
         MVC   KEY+(ACKEYDTE-ACKEYD)(L'THISDATE),THISDATE                       
         MVC   KEY+(ACKEYREF-ACKEYD)(L'ACKEYREF),=C'*TIME*'                     
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',KEY,KEY                      
         CLI   8(R1),0                                                          
         BNE   TESTMSX                                                          
         MVI   DOTMS,C'Y'          INDICATE MAKE TMS POSTINGS                   
*                                                                               
TESTMSX  XMOD1                                                                  
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LITERAL POOL                                                        *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ACCOUNT TABLE                                                       *         
***********************************************************************         
         SPACE 1                                                                
ACCTAB   DS    CL(10*ACCTLNQ)                                                   
         SPACE 3                                                                
***********************************************************************         
* INPUT OPTIONS TABLE                                                 *         
***********************************************************************         
         SPACE 1                                                                
OPTTAB   DS    0CL12                                                            
         DC    CL8'WC      ',X'80',AL3(VALWORK)                                 
         DC    CL8'REF#    ',X'40',AL3(VALREF)                                  
         DC    CL8'SREF#   ',X'40',AL3(VALSREF)                                 
         DC    CL8'REVERSE ',X'20',AL3(VALREV)                                  
         DC    CL8'DATE    ',X'18',AL3(VALDATE)                                 
         DC    CL8'MOS     ',X'18',AL3(VALMOS)                                  
         DC    CL8'SDATE   ',X'10',AL3(VALSDATE)                                
         DC    CL8'EDATE   ',X'08',AL3(VALEDATE)                                
         DC    CL8'BILLED  ',X'04',AL3(VALBILL)                                 
         DC    CL8'SUBREF  ',X'02',AL3(VALSUB)                                  
         DC    CL8'LABOR   ',X'00',AL3(VALLAB)                                  
         DC    CL8'COSTS   ',X'00',AL3(VALDIR)                                  
         DC    CL8'PCT     ',X'01',AL3(VALPCT)                                  
         DC    CL8'AMOUNT  ',X'01',AL3(VALDOL)                                  
         DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
* PROGRAM DSECT                                                       *         
***********************************************************************         
         SPACE 1                                                                
PROGD    DSECT                                                                  
PRELOC   DS    F                                                                
ASVBLOCK DS    A                   A(8B SAVE BLOCK)                             
AJGRPBLK DS    A                   A(JOB GROUP BLOCK)                           
ACADETBK DS    A                   A(CADET IO AREA)                             
ACADTBLK DS    A                   A(CADETBLK AREA)                             
NLINES   DS    CL1                                                              
COUNT    DS    CL1                                                              
DOTMS    DS    C                                                                
INPTJOBA DS    CL15                                                             
INPTJOBN DS    CL36                                                             
INPTJOFF DS    CL2                                                              
INPTCST  DS    CL15                                                             
INPTCSTN DS    CL36                                                             
INPTSALA DS    CL15                SALES ACCOUNT FROM PRODUCT                   
INPTSALN DS    CL36                SALES ACCOUNT NAME FROM PRODUCT              
INSKACCT DS    CL15                                                             
INSKNAME DS    CL36                                                             
CREDITA  DS    CL15                                                             
NEWSTAT  DS    CL1                                                              
THISWRKC DS    CL2                                                              
THISAMNT DS    PL6                 POSTING AMOUNT                               
THISORIG DS    PL6                 ORIGINAL AMOUNT                              
THISSTAT DS    CL1                                                              
THISANAL DS    CL2                                                              
THISREF  DS    CL6                                                              
THISDATE DS    CL3                                                              
THISBTCH DS    CL6                                                              
THISNARR DS    CL200                                                            
THISSEQ  DS    XL2                                                              
THISNRLN DS    CL1                                                              
THISHOUR DS    CL(ACPSLNQ)                                                      
THISTRSD DS    CL110                                                            
THISCST  DS    CL15                COSTING U/L/ACCT                             
THISCSTN DS    CL36                COSTING ACCOUNT NAME                         
THISCASH DS    CL10                                                             
THISSUB  DS    CL6                                                              
THISINWC DS    CL2                                                              
THISOTH  DS    CL6                                                              
THISOTH2 DS    CL11                LAST BYTE IS L'ACNO                          
THISMOS  DS    CL2                                                              
THISOFF  DS    CL2                 OFFICE CODE                                  
THISTIME DS    F                                                                
THISRATE DS    PL4                                                              
THISPAKL DS    CL(PAKLNQ)                                                       
THISPID  DS    XL2                 PID                                          
THISASK  DS    XL42                ACCOUNT SYSTEM KEY            NMAL           
THISCRDE DS    XL(SCITCPLQ)        TO HOLD CASH ELEMENT          NMAL           
**                                                                              
*                                                                               
SALEACCT DS    CL15                HOLD AREA FOR SALES ACCOUNT                  
SALENAME DS    CL36                HOLD AREA FOR SALES ACCOUNT NAME             
*                                                                               
THISUNPR DS    CL(UNPLNQ)          UNIT PRICE ELEMENT                           
*                                                                               
THISORGJ DS    CL(L'FROMJOBA)      ORIGINAL JOB ELEMENT                         
THISORGW DS    CL2                 ORIGINAL WORKCODE                            
THISORGD DS    PL3                 ORIGINAL DATE                                
THISORGB DS    CL6                 ORIGINAL BATCH                               
THISORGS DS    XL2                 ORIGINAL SEQUENCE                            
*                                                                               
THATSTA2 DS    X                   TRSSTAT2 FROM X'60' ELEMENT                  
THATSTA3 DS    X                   TRSSTAT3 FROM X'60' ELEMENT                  
THATDATE DS    XL2                 TRSDATE FROM X'60' ELEMENT                   
TMSDATE  DS    XL2                 TMS START DATE FROM COMPANY ELEMENT          
BOSDATE  DS    CL1                 GDATYPE                                      
SCANSV   DS    CL1                                                              
PK16     DS    PL16                                                             
TMPCDAMT DS    PL6                                                              
LINES    DS    7CL32                                                            
AD4EL    DS    F                                                                
AD69L    DS    F                                                                
*                                                                               
ATYPES   DS    0A                                                               
AACCTAB  DS    A                                                                
AOPTTAB  DS    A                                                                
ATMSXFR  DS    A                   TMS TRANSFER MODULE                          
ACADET   DS    A                   CADET                                        
AJOBGRP  DS    A                   ADDRESS OF LIST OF JOBS                      
CURRJGRP DS    A                   ADDRESS OF CURRENT JOB GROUP                 
AFRSTNOD DS    A                   ADDRESS OF FIRST NODE                        
ACURLIN  DS    A                   ADDRESS OF CURRENT LINE                      
*                                                                               
JGRPUSD  DS    C                                                                
JGRPLST1 DS    X                                                                
JGRPELST EQU   X'80'               LAST ONE ON LIST                             
JGRPFULL EQU   X'40'               FULL USE OF AMOUNT                           
*                                                                               
SVSUBSEQ DS    X                   SUB SEQUENCE NUMBER FOR FFTEL                
SVBATREF DS    CL6                 BATCH REF FOR FFTEL                          
SVACTDAT DS    XL2                 ACTIVITY DATE FOR FFTEL                      
SVORGAMT DS    PL6                 ORIGINAL AMOUNT FOR FFTEL                    
SV2NDSEQ DS    X                   SECONDARY SEQUENCE NUMBER FOR FFTEL          
*                                                                               
BYTE     DS    C                                                                
TJOBGRPC DS    CL(L'LINIJOB)                                                    
TAMOUNT  DS    CL(L'LINAMNT)                                                    
TWC      DS    CL2                                                              
TSTATUS  DS    CL3                                                              
CLUSTLNQ DS    XL2                                                              
CLUSTER  DS    CL500                                                            
*                                                                               
SKKEY    DS    CL49                                                             
SKIOAREA DS    2000C                                                            
*                                                                               
SVKEY    DS    CL42                                                             
KEY      DS    CL49                                                             
IOAREA   DS    CL2048                                                           
*                                                                               
SVBLOCK  DS    CL(SVBLOCKQ)                                                     
JGRPBLK  DS    CL1000              JOB GROUP BLOCK                              
CADETBK  DS    CL2000              CADET BLOCK                                  
*                                                                               
CADETBLK DS    CL(CADETDQ)         CADET BLOCK                                  
*                                                                               
PROGDX   DS    0C                                                               
         EJECT                                                                  
***********************************************************************         
* EQUATES                                                             *         
***********************************************************************         
         SPACE 1                                                                
MAXCOUNT EQU   10                                                               
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
         EJECT                                                                  
       ++INCLUDE ACBATJGRP                                                      
***********************************************************************         
* SAVE BLOCK DSECT                                                    *         
***********************************************************************         
         SPACE 1                                                                
SVBLOCKD DSECT                     *** SAVE BLOCK ***                           
SVSJCPJ  DS    0CL14               SJ UNIT/LEDGER/ACCOUNT                       
SVSJUL   DS    CL2                 SJ UNIT/LEDGER                               
SVSJCODE DS    CL12                SJ ACCOUNT CODE                              
SVTASK   DS    CL2                 TASK CODE                                    
SVCLIOFF DS    CL2                 CLIENT OFFICE                                
SVTYPE   DS    CL1                 TYPE OF TIME (BIT EQUIVALENT)                
SVIND    DS    XL1                 TYPE OF TIME INDICATOR BYTE                  
SVMOA    DS    PL2                 MOA                                          
SVTODAY3 DS    PL3                 ACTIVITY DATE                                
SVHOURS  DS    PL4                 HOURS                                        
SVRATE   DS    PL4                 RATE                                         
SVRATIND DS    XL1                 RATE BITS AS IN RECORD                       
SVRATEFF DS    PL3                 RATE EFFECTIVE DATE                          
SVULA    DS    CL14                SI/SK UNIT/LEDGER/ACCOUNT                    
SVAMOUNT DS    PL8                 AMOUNT                                       
SVNARRLN DS    XL1                 LENGTH OF NARRARIVE                          
SVNARR   DS    CL60                NARRATIVE                                    
SVPIDNO  DS    XL2                 PERSON PID                                   
SV1RCODE DS    CL12                1R ACCOUNT                                   
SVSJDATE DS    CL3                 SJ DATE                                      
SVBLOCKQ EQU   *-SVBLOCKD                                                       
         EJECT                                                                  
***********************************************************************         
* POSTING INFORMATION JOB TO JOB TABLE DSECT                          *         
***********************************************************************         
         SPACE 1                                                                
ACCTD    DSECT                                                                  
ACCTTOJB DS    CL15                TO-JOB ACCOUNT CODE                          
ACCTTOJN DS    CL36                TO-JOB ACCOUNT NAME                          
ACCTCTRN DS    CL36                CONTRA ACCOUNT NAME                          
ACCTOFF  DS    CL2                 OFFICE CODE                                  
ACCTSK   DS    CL15                SK ACCOUNT OR BINARY ZEROS                   
ACCTSKNM DS    CL36                SK ACCOUNT NAME                              
ACCTSI   DS    CL15                SI ACCOUNT OR BINARY ZEROS                   
ACCTSINM DS    CL36                SI ACCOUNT NAME                              
ACCT12   DS    CL15                12 ACCOUNT OR BINARY ZEROS                   
ACCT12NM DS    CL36                12 ACCOUNT NAME                              
ACCTCST  DS    CL15                COSTING ACCOUNT                              
ACCTCSTN DS    CL36                COSTING ACCOUNT NAME                         
ACCTSALA DS    CL15                SALES ACCOUNT                                
ACCTSALN DS    CL36                SALES ACCOUNT NAME                           
ACCTAFLD DS    XL4                 A(SCREEN INPUT FIELD)                        
ACCTNEXT EQU   *                                                                
ACCTLNQ  EQU   *-ACCTD                                                          
         EJECT                                                                  
***********************************************************************         
* TRNSTAB DSECT IN TWA 0                                              *         
***********************************************************************         
         SPACE 1                                                                
TRNSD    DSECT                                                                  
TRNSPOST DS    CL1                 POST Y OR N                                  
TRNSKEY  DS    CL27                W-C/CONTRA/DATE/REF/SUB                      
TRNSORG  DS    CL4                 ORIGINAL AMOUNT                              
TRNSAMT  DS    CL4                 POSTING AMOUNT                               
TRNSWC   DS    CL2                 NEW WORK CODE                                
TRNSHRS  DS    CL4                 HOURS                                        
TRNSRTE  DS    PL4                 HOURLY RATE                                  
TRNSTS   DS    CL1                 'C' COMMISSIONABLE  'N' NON                  
TRNSNEXT EQU   *                                                                
         EJECT                                                                  
***********************************************************************         
* DISPLAY LINE DSECT                                                  *         
***********************************************************************         
         SPACE 1                                                                
LINED    DSECT                                                                  
LINDISPH DS    CL8                                                              
LINWORK  DS    CL2                                                              
         DS    CL2                                                              
LINSUP   DS    CL14                                                             
         DS    CL1                                                              
LINREF   DS    CL6                                                              
         DS    CL1                                                              
LINDATE  DS    CL8                                                              
         DS    CL1                                                              
LINSTAT  DS    CL5                                                              
         DS    CL1                                                              
LINDISP  DS    CL8                                                              
LINAMNTH DS    CL8                                                              
LINAMNT  DS    CL11                                                             
LINAMNTX DS    CL8                                                              
LINIJOBH DS    CL8                                                              
LINIJOB  DS    CL24                                                             
LINIJOBX DS    CL8                                                              
LINNEXT  EQU   *                                                                
         EJECT                                                                  
*              DSECT TO COVER LIST OF JOBS FROM A JOB GROUP                     
JGBLOCK  DSECT                                                                  
JGBHDPTR DS    XL2                 HEAD PTR                                     
JGBACCT  DS    CL12                JOB ACCOUNT                                  
JGBAMNT  DS    PL6                 AMOUNT                                       
JGBFTPTR DS    XL2                 FOOT PTR                                     
JGBLN    EQU   *-JGBHDPTR                                                       
         EJECT                                                                  
***********************************************************************         
* DSECT FOR TWAXTRA                                                   *         
***********************************************************************         
         SPACE 1                                                                
XTWAD    DSECT                                                                  
FROMCST  DS    CL15                FROM JOB COST ACCOUNT                        
FROMCSTN DS    CL36                FROM JOB COST ACCOUNT NAME                   
FROMSALA DS    CL15                FROM JOB SALES ACCOUNT                       
FROMSALN DS    CL36                FROM JOB SALES ACCOUNT NAME                  
*                                                                               
TOSALA   DS    CL15                TO JOB SALES ACOUNT                          
TOSALN   DS    CL36                TO JOB SALES ACCOUNT NAME                    
TOCST    DS    CL15                TO JOB COST ACCOUNT                          
TOCSTN   DS    CL36                TO JOB COST ACCOUNT NAME                     
TOSKACCT DS    CL15                TO JOB SK ACCOUNT                            
         ORG                                                                    
         EJECT                                                                  
* ACBATDSECT                                                                    
       ++INCLUDE ACBATDSECT                                                     
* ACBATDBD                                                                      
       ++INCLUDE ACBATDBD                                                       
         ORG   TWAHOLE                                                          
LKEY     DS    CL49                                                             
TOTAMNT  DS    PL6                                                              
LINENO   DS    XL1                                                              
GBCNTR   DS    XL1                 GOBACK COUNTER                               
*                                                                               
OPTIONS  DS    0C                                                               
OPTBITS  DS    CL1                                                              
WORKOPT  DS    CL2                                                              
REFOPT   DS    CL6                                                              
SREFOPT  DS    CL6                                                              
DATOPT   DS    CL3                                                              
SDATOPT  DS    CL3                                                              
EDATOPT  DS    CL3                                                              
MOSOPT   DS    CL3                                                              
REVOPT   DS    CL1                                                              
BILLOPT  DS    CL1                                                              
XCLDOPT  DS    CL1                                                              
SUBOPT   DS    CL1                                                              
SUBREF   DS    CL6                                                              
LABOPT   DS    CL1                                                              
COSTOPT  DS    CL1                                                              
OPTPCT   DS    PL3                                                              
OPTAMT   DS    PL6                                                              
OPTLEN   EQU   *-OPTIONS                                                        
FROMJOBA DS    CL15                                                             
FROMJOBN DS    CL36                                                             
FROMJOFF DS    CL2                                                              
TOJOBA   DS    CL15                                                             
TOJOBN   DS    CL36                                                             
TOJOBOFF DS    CL2                                                              
CRACA    DS    CL15                                                             
TRNSTAB  DS    10CL(TRNSNEXT-TRNSD)                                             
SAVETYPE DS    C                                                                
TOJGRPLN DS    X                                                                
TOJGRP   DS    CL8                                                              
LVLSSJ   DS    XL4                                                              
LVLS1R   DS    XL4                                                              
*                                                                               
TWAEND   DS    0C                                                               
         EJECT                                                                  
***********************************************************************         
* ++INCLUDES                                                          *         
***********************************************************************         
         SPACE 1                                                                
* ACGENBOTH                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
* ACGENDAY                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENDAY                                                       
         PRINT ON                                                               
* DDFLDIND                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDFLDIND                                                       
         PRINT ON                                                               
* ACTMSXFRD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACTMSXFRD                                                      
         PRINT ON                                                               
* FASYSFAC                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASYSFAC                                                       
         PRINT ON                                                               
* FASSB                                                                         
         PRINT OFF                                                              
       ++INCLUDE FASSB                                                          
         PRINT ON                                                               
* FATCB                                                                         
         PRINT OFF                                                              
       ++INCLUDE FATCB                                                          
         PRINT ON                                                               
* FASECRETD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FASECRETD                                                      
         PRINT ON                                                               
* ACCADETD                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACCADETD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'118ACBAT22   10/12/17'                                      
         END                                                                    
