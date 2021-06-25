*          DATA SET ACREPP802  AT LEVEL 030 AS OF 05/01/02                      
*PHASE ACP802A                                                                  
*INCLUDE SQUASHER                                                               
*INCLUDE ACLIST                                                                 
*INCLUDE UNDERLIN                                                               
         TITLE 'WORK-CODE/BILLING SUMMARY'                                      
ACP802   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACP8**,RR=R5                                                 
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING ACP802+4096,R9     R9 AS SECOND BASE                             
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACP8D,RC                                                         
         ST    R5,RELO                                                          
         EJECT                                                                  
*                                                                               
**********************************************************************          
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BNE   ACP5                                                             
*&&US                                                                           
         L     RF,=A(SAVERC)                                                    
         A     RF,RELO                                                          
         ST    RC,0(RF)            SAVE REG C                                   
         L     RF,VEXTRAS                                                       
         USING RUNXTRAD,RF                                                      
         L     RF,ADMASTD                                                       
         USING MASTD,RF                                                         
         L     RF,MCBXAREA                                                      
         ST    RF,ADBOX            STORE ADDR OF BOX ROUTINE                    
         L     RF,=A(HOOK)                                                      
         A     RF,RELO                                                          
         ST    RF,HEADHOOK                                                      
*&&                                                                             
         L     RF,=V(ACLIST)                                                    
         A     RF,RELO                                                          
         ST    RF,VACLIST                                                       
         L     RF,=A(MYIO)                                                      
         A     RF,RELO                                                          
         ST    RF,AMYIO                                                         
         L     RF,=A(BILLTAB)                                                   
         A     RF,RELO                                                          
         ST    RF,ABILLTAB                                                      
         LA    R2,JTOTS            CLEAR ACCUMS                                 
         LA    RE,4                                                             
ACP2     MVC   0(36,R2),=6PL6'0'                                                
         LA    R2,36(R2)                                                        
         BCT   RE,ACP2                                                          
         ZAP   BCOUNT,=P'0'                                                     
         ZAP   JCOUNT,=P'0'                                                     
         ZAP   RCOUNT,=P'0'                                                     
         B     EXIT                                                             
*                                                                               
EXIT     XMOD1 1                                                                
         EJECT                                                                  
*                                                                               
**********************************************************************          
*                                                                               
ACP5     CLI   MODE,REQFRST                                                     
         BNE   ACP10                                                            
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         MVI   SKIPSW,C'N'                                                      
         MVC   MYHEAD8,SPACES                                                   
         MVC   MYHEAD9,SPACES                                                   
         MVC   MYJOB,SPACES                                                     
         XC    WCS,WCS                                                          
         MVI   WCODES,0                                                         
         MVI   WCOUNT,0                                                         
         MVC   QSTART+4(2),=C'01'                                               
         GOTO1 DATCON,DMCB,(0,QSTART),(9,PERIOD)                                
         GOTO1 (RF),(R1),,(1,START)                                             
         MVI   PERIOD+6,C'-'                                                    
         MVC   QEND+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,QEND),(9,PERIOD+7)                                
         GOTO1 (RF),(R1),,(1,END)                                               
         L     R4,VEXTRAS                                                       
         USING RUNXTRAD,R4                                                      
         OC    VLISTREC,VLISTREC   GET LIST                                     
         BNZ   *+6                                                              
         DC    H'0'                NO LIST RECORD                               
         L     R3,VLISTREC                                                      
         MVI   ELCODE,X'1E'                                                     
         USING ACLISTD,R3                                                       
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                NO TYPE ELEMENT                              
         CLI   ACLIUSE,C'I'                                                     
         BNE   EXIT                DID NOT INCLUDE LIST RECORD!                 
         MVI   ELCODE,X'1F'                                                     
         USING ACLDATAD,R3                                                      
         BAS   RE,FIRSTEL          GET WORK-CODES                               
         BE    *+6                                                              
         DC    H'0'                NO DATA ELEMENT                              
         LH    R2,=H'6'            R2=SPROG                                     
         LA    R3,ACLDACCS         R3=WORK-CODES                                
         LH    R4,=H'74'           R4=STARTING COLUMN                           
         LA    R5,WCS              R5=STORED WORK-CODES                         
         LH    R7,=H'18'           R7=ACTUAL POS. OF FIRST ACCUM.               
ACP7     CLI   0(R3),X'20'         NAME ELEMENT MUST FOLLOW                     
         BE    ACP9                                                             
         CLC   0(2,R3),=C'99'      DON'T ALLOW W/C 99                           
         BE    ACP8                                                             
         MVC   0(2,R5),0(R3)                                                    
         AI    WCODES,1            W/C COUNT                                    
         SH    R2,=H'2'                                                         
         SH    R4,=H'12'                                                        
         SH    R7,=H'6'                                                         
         CLI   WCODES,3            TAKE FIRST THREE                             
         BE    ACP9                                                             
         LA    R5,2(R5)                                                         
ACP8     LA    R3,2(R3)                                                         
         B     ACP7                                                             
*                                                                               
ACP9     STC   R2,RCSUBPRG                                                      
         STC   R4,COLPOS           STARTING COL. POSITIONS                      
         SH    R4,=H'16'                                                        
         STC   R4,BILLPOS                                                       
         SH    R4,=H'23'                                                        
         CLI   WCODES,2                                                         
         BE    AC9B                                                             
         BL    *+12                                                             
         AH    R4,=H'12'           MOVE OVER WHEN 3 W/C'S                       
         B     AC9B                                                             
         SH    R4,=H'12'           MOVE MORE WHEN 1 W/C                         
AC9B     STC   R4,TOTPOS                                                        
         STC   R7,AMTPOS                                                        
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
**********************************************************************          
*                                                                               
ACP10    CLI   MODE,LEVAFRST                                                    
         BNE   ACP11                                                            
         MVI   CLIACTV,C'N'        SET CLIENT NOT ACTIVE                        
         B     EXIT                                                             
*                                                                               
**********************************************************************          
*                                                                               
ACP11    CLI   MODE,PROCACC                                                     
         BNE   ACP30                                                            
         BAS   RE,BLDBILLS         BUILD BILLING TABLE                          
         MVI   JOBSW,0             SET JOB PRINT PENDING                        
         B     EXIT                                                             
*                                                                               
**********************************************************************          
*                                                                               
ACP30    CLI   MODE,ANALFRST                                                    
         BNE   TRANREC                                                          
         MVI   SVANAL,0                                                         
         L     R2,ADTRANS                                                       
         USING TRANSD,R2                                                        
         LA    R3,WCS              IS THIS A W/C WE WANT                        
         LA    R4,3                                                             
ACP32    CLC   TRNSANAL,0(R3)                                                   
         BE    ACP34               YES                                          
         LA    R3,2(R3)                                                         
         BCT   R4,ACP32                                                         
         B     EXIT                NOT FOUND                                    
*                                                                               
ACP34    MVC   SVANAL,TRNSANAL                                                  
         CLI   6(R3),0             DOES IT NEED TO BE POSITIONED                
         BE    ACP35                                                            
         MVC   POSITION,6(R3)      NO, MOVE IT IN                               
         B     EXIT                                                             
ACP35    BAS   RE,POSIT                                                         
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
**********************************************************************          
*                                                                               
TRANREC  CLI   MODE,PROCTRNS                                                    
         BNE   ACP80                                                            
         L     R2,ADTRANS                                                       
         USING TRANSD,R2                                                        
         CLI   0(R2),X'44'                                                      
         BNE   TRANX                                                            
         CLC   TRNSWRKC,=C'99'     ALREADY LOOKED AT BILLING                    
         BE    TRANX                                                            
         CLI   SVANAL,0            DO WE WANT THIS W/C                          
         BE    TRANX               NO                                           
*                                                                               
         USING ACMD,R3                                                          
         L     R3,AMONACC                                                       
         L     R3,ACMAPRO2                                                      
         CLI   0(R3),PTAELQ                                                     
         B     TRAN02+8                                                         
*                                                                               
TRAN02   MVI   ELCODE,PTAELQ                                                    
         BAS   RE,NEXTEL                                                        
         BNE   TRANX                                                            
*                                                                               
         USING PTAELD,R3                                                        
         TM    PTASTAT1,PTASPEND   SKIP PENDING                                 
         BO    TRAN02                                                           
         CLI   PTATYPE,PTATRAL     ONLY WANT BILLING                            
         BNE   TRAN02                                                           
         MVC   THISBDNO,PTARBLNO                                                
         MVC   THISBDTE,PTARBLDT                                                
         ZAP   AMOUNT,PTANET                                                    
         CLI   QOPT2,C'G'                                                       
         BNE   TRAN04                                                           
         TM    TRNSSTAT,X'01'                                                   
         BO    TRAN04              NON-COMMISSIONABLE ITEM                      
         AP    AMOUNT,PTARCOM                                                   
*                                                                               
TRAN04   GOTO1 DATCON,DMCB,(2,THISBDTE),(1,WORK)                                
         CLC   WORK(2),START       BILLED WITHIN REQUEST PERIOD                 
         BL    TRAN02                                                           
         CLC   WORK(2),END                                                      
         BH    TRAN02                                                           
*                                                                               
         CLI   JOBSW,0             GET JOB NUMBER AND NAME                      
         BNE   TRAN08                                                           
*                                                                               
         MVI   JOBSW,1                                                          
         MVC   MYJOB,SPACES                                                     
         MVC   MYPRT1,SPACES                                                    
         MVC   MYPRT2,SPACES                                                    
         L     R6,ADACC                                                         
         MVC   MYJOB(9),6(R6)      PRODUCT AND JOB CODE                         
         L     R6,ADACCNAM                                                      
         USING ACNAMED,R6                                                       
         ZIC   RF,ACNMLEN                                                       
         SH    RF,=H'3'                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   MYJOB+10(0),ACNMNAME                                             
         GOTO1 =V(SQUASHER),DMCB,MYJOB,46,RR=RELO                               
         CLI   WCODES,3                                                         
         BNE   TRAN06                                                           
         GOTO1 CHOPPER,DMCB,(46,MYJOB),(36,MYPRT1+1),(C'P',2)                   
         CLC   MYPRT2,SPACES       SLIDE OVER JOB NAME                          
         BE    TRAN08                                                           
         MVC   WORK(26),MYPRT2+1                                                
         MVC   MYPRT2+11(26),WORK                                               
         MVC   MYPRT2+1(10),SPACES                                              
         B     TRAN08                                                           
*                                                                               
TRAN06   MVC   MYPRT1+1(L'MYJOB),MYJOB                                          
*                                                                               
TRAN08   L     R4,ABILLTAB         FIND ENTRY IN TABLE                          
*                                                                               
         USING BILLD,R4                                                         
TRAN10   CLI   0(R4),X'FF'         MATCH IN TABLE?                              
         BE    TRAN02              NO, GET THE NEXT                             
         CLC   BILLNUM,THISBDNO    BILL NUMBERS MUST MATCH                      
         BE    *+12                                                             
         LA    R4,BILLNEXT         GO TO NEXT                                   
         B     TRAN10                                                           
*                                                                               
         LA    R5,BILLWC3                                                       
         CLI   POSITION,3                                                       
         BE    TRAN12                                                           
         LA    R5,BILLWC2                                                       
         CLI   POSITION,2                                                       
         BE    TRAN12                                                           
         LA    R5,BILLWC1                                                       
*                                                                               
TRAN12   AP    0(6,R5),AMOUNT      FOUND - ADD IN W/C AMOUNT                    
         B     TRAN02                                                           
*                                                                               
TRANX    B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
**********************************************************************          
*                                                                               
ACP80    CLI   MODE,ACCLAST                                                     
         BNE   ACP95                                                            
         BAS   RE,PRTBILLS                                                      
         MVI   FORMBILL,C'N'                                                    
         CP    BCOUNT,=P'1'                                                     
         BE    ACP82                                                            
         BL    ACP84                                                            
         LA    R4,JTOTS                                                         
         BAS   RE,FORMAT                                                        
         ZIC   R3,TOTPOS                                                        
         LA    R3,P(R3)                                                         
         MVC   0(10,R3),TOTFOR                                                  
         MVC   11(3,R3),=C'JOB'                                                 
         MVI   SPACING,2                                                        
         GOTO1 MYREPORT                                                         
         MVI   SKIPSW,C'N'                                                      
ACP82    AP    JCOUNT,=P'1'                                                     
ACP84    MVC   JTOTS(36),=6PL6'0'                                               
         ZAP   BCOUNT,=P'0'                                                     
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
**********************************************************************          
*                                                                               
ACP95    CLI   MODE,LEVBLAST                                                    
         BNE   ACP100                                                           
         CP    JCOUNT,=P'2'                                                     
         BL    ACP97                                                            
         CLC   PTOTS(36),=6PL6'0'                                               
         BE    ACP97                                                            
         GOTO1 MYREPORT                                                         
         LA    R4,PTOTS                                                         
         BAS   RE,FORMAT                                                        
         ZIC   R3,TOTPOS                                                        
         LA    R3,P(R3)                                                         
         MVC   0(10,R3),TOTFOR                                                  
         MVC   11(7,R3),=C'PRODUCT'                                             
         L     R2,ADACC                                                         
         MVC   19(3,R3),6(R2)                                                   
         MVI   SPACING,2                                                        
         GOTO1 MYREPORT                                                         
         MVI   SKIPSW,C'N'                                                      
ACP97    MVC   PTOTS(36),=6PL6'0'                                               
         ZAP   JCOUNT,=P'0'                                                     
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
**********************************************************************          
*                                                                               
ACP100   CLI   MODE,LEVALAST                                                    
         BNE   ACP120                                                           
         CLI   CLIACTV,C'Y'                                                     
         BNE   EXIT                                                             
         GOTO1 MYREPORT                                                         
         LA    R4,CTOTS                                                         
         BAS   RE,FORMAT                                                        
         ZIC   R3,TOTPOS                                                        
         LA    R3,P(R3)                                                         
         MVC   0(10,R3),TOTFOR                                                  
         MVC   11(6,R3),=C'CLIENT'                                              
         GOTO1 MYREPORT                                                         
         MVC   CTOTS(36),=6PL6'0'                                               
         AP    RCOUNT,=P'1'                                                     
         MVI   FORCEHED,C'Y'                                                    
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
**********************************************************************          
*                                                                               
ACP120   CLI   MODE,REQLAST                                                     
         BNE   EXIT                                                             
         CP    RCOUNT,=P'2'                                                     
         BL    ACP125                                                           
         AI    RCSUBPRG,1                                                       
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 MYREPORT                                                         
         LA    R4,RTOTS                                                         
         BAS   RE,FORMAT                                                        
         ZIC   R3,TOTPOS                                                        
         LA    R3,P(R3)                                                         
         MVC   0(10,R3),TOTFOR                                                  
         MVC   11(7,R3),=C'REQUEST'                                             
         GOTO1 MYREPORT                                                         
ACP125   MVC   RTOTS(36),=6PL6'0'                                               
         ZAP   RCOUNT,=P'0'                                                     
         MVI   FORCEHED,C'Y'                                                    
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
**********************************************************************          
*        ROUTINE TO BUILD BILLING TABLE                                         
*                                                                               
BLDBILLS NTR1                                                                   
         L     R2,ADACC                                                         
         L     R3,AMYIO                                                         
         USING ACKEYD,R3                                                        
         L     R4,ABILLTAB                                                      
         USING BILLD,R4                                                         
         MVI   ELCODE,X'44'                                                     
         MVC   ACKEYACC(42),SPACES                                              
         MVC   ACKEYACC(15),0(R2)                                               
         MVC   ACKEYWRK,=C'99'                                                  
         GOTO1 DATAMGR,DMCB,DMRDHI,=C'ACCOUNT',(R3),(R3)                        
*                                                                               
BLD2     CLC   ACKEYACC(15),0(R2)                                               
         BNE   BLDX                NO MORE FOR ACCOUNT                          
         CLC   ACKEYWRK,=C'99'                                                  
         BNE   BLDX                                                             
         BAS   RE,GETEL                                                         
         BNE   BLD6                NO TRANSACTIONS FOR RECORD                   
*                                                                               
         USING TRANSD,R3                                                        
         MVC   BILLNUM,TRNSREF                                                  
         MVC   BILLDATE,TRNSDATE                                                
         ZAP   BILLNET,TRNSAMNT         NET                                     
         ZAP   BILLCOMM,TRNSNARR+15(6)  COMM                                    
         ZAP   BILLGRS,BILLNET                                                  
         AP    BILLGRS,BILLCOMM         GROSS                                   
         MVC   BILLWC1(18),=3PL6'0'     W/C'S - FILLED IN LATER                 
         LA    R4,BILLNEXT                                                      
         CLI   0(R4),X'FE'                                                      
         BNE   *+6                                                              
         DC    H'0'                TABLE FULL                                   
*                                                                               
BLD6     L     R3,AMYIO                                                         
         GOTO1 DATAMGR,DMCB,DMRSEQ,=C'ACCOUNT',(R3),(R3)                        
         B     BLD2                                                             
*                                                                               
BLDX     MVI   0(R4),X'FF'                                                      
         USING ACKEYD,R3                                                        
         MVC   ACKEYACC(42),SPACES                                              
         MVC   ACKEYACC(15),0(R2)                                               
         GOTO1 DATAMGR,DMCB,DMREAD,=C'ACCOUNT',(R3),(R3)                        
         CLI   DMCB+8,0                                                         
         BE    EXIT                                                             
         DC    H'0'                ACCOUNT NOT FOUND                            
         EJECT                                                                  
*                                                                               
**********************************************************************          
*        ROUTINE TO POSITION WORK-CODES                                         
*                                                                               
*                                                                               
POSIT    ST    RE,SAVERE                                                        
         AI    WCOUNT,1                                                         
         MVI   POSITION,3                                                       
         MVI   6(R3),3             SET AS 'POSITIONED'                          
         LA    R2,MYHEAD8+62       START OF COL. HEADING                        
         CLC   WCOUNT,WCODES       IS IT LAST?                                  
         BE    POS3                THEN IT MUST BE IN THIRD POSITION            
         MVI   POSITION,2                                                       
         MVI   6(R3),2                                                          
         LA    R2,MYHEAD8+50                                                    
         CLI   WCOUNT,1            FIRST?                                       
         BNE   POS3                NO, THEN MUST BE IN 2ND POSITION             
         CLI   WCODES,2            ARE THERE TWO?                               
         BE    POS3                YES-SO MUST BE IN SECOND POSITION            
         MVI   POSITION,1          NO - IT'S IN FIRST POSITION                  
         MVI   6(R3),1                                                          
         LA    R2,MYHEAD8+38                                                    
POS3     MVC   MYANAL,SPACES                                                    
         MVC   MYANAL+16(2),SVANAL                                              
         L     R4,ADLEDGER         FIND WORK-CODE NAME                          
         AH    R4,DATADISP                                                      
         SR    R3,R3                                                            
POS7     CLI   0(R4),0                                                          
         BE    POSX                                                             
         CLI   0(R4),X'12'                                                      
         BNE   POS9                                                             
         USING ACANALD,R4                                                       
         CLC   SVANAL,ACANCODE                                                  
         BNE   POS9                                                             
         MVC   MYANAL(15),ACANDESC                                              
         B     POSX                                                             
*                                                                               
POS9     IC    R3,1(R4)                                                         
         AR    R4,R3                                                            
         B     POS7                                                             
*                                                                               
POSX     DS    0H                                                               
         GOTO1 =V(SQUASHER),DMCB,MYANAL,18,RR=RELO                              
         GOTO1 CHOPPER,DMCB,(18,MYANAL),(11,0(R2)),(C'P',2)                     
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*        ROUTINE TO PRINT BILLING TABLE                                         
*                                                                               
PRTBILLS NTR1                                                                   
         MVI   FORMBILL,C'Y'       FORMATTING BILLTAB                           
         L     R4,ABILLTAB         PRINT TABLE                                  
PRTB1    CLI   0(R4),X'FF'                                                      
         BE    EXIT                                                             
         BAS   RE,FORMAT           COMES BACK WITH R4=NEXT ITEM                 
         CLI   PRINTED,C'Y'        IF NO W/C AMOUNTS                            
         BE    *+14                                                             
         MVC   P,SPACES            THEN SKIP IT                                 
         B     PRTB1                                                            
*                                                                               
         CLI   JOBSW,2             SOMETHING TO PRINT                           
         BE    PRTB3               SO HAVE I PRINTED JOB INFO YET               
         MVI   JOBSW,2                                                          
         MVC   PSAVE,P             NO - SAVE DETAIL LINE                        
         MVC   P,SPACES                                                         
         CLI   SKIPSW,C'Y'         DO I NEED TO SKIP A LINE                     
         BNE   *+8                                                              
         BAS   RE,MYREPORT                                                      
         MVC   P,MYPRT1            MOVE IN MY PRINT LINES                       
         MVC   PSECOND,MYPRT2                                                   
         MVI   SPACING,2                                                        
         BAS   RE,MYREPORT         AND PRINT THEM                               
         MVC   P,PSAVE                                                          
         MVI   CLIACTV,C'Y'                                                     
PRTB3    BAS   RE,MYREPORT                                                      
         MVI   SKIPSW,C'Y'                                                      
         AP    BCOUNT,=P'1'                                                     
         LA    RF,4                AND ADD TO COUNTERS                          
         LA    RE,JTOTS                                                         
PRTB5    LA    R1,6                                                             
         LR    R5,R4                                                            
         SH    R5,=H'36'           RETURN TO BEGINNING OF COUNTERS              
PRTB7    AP    0(6,RE),0(6,R5)                                                  
         LA    RE,6(RE)                                                         
         LA    R5,6(R5)                                                         
         BCT   R1,PRTB7                                                         
         BCT   RF,PRTB5                                                         
         B     PRTB1                                                            
         EJECT                                                                  
FORMAT   DC    0H'0'                                                            
         ST    RE,SAVERE                                                        
         MVI   PRINTED,C'N'                                                     
         CLI   FORMBILL,C'Y'       IS R4=BILLTAB                                
         BNE   FORMATB                                                          
         ZIC   R5,BILLPOS          R5=POSITION OF BILL NO./DATE                 
         LA    R5,P(R5)                                                         
         MVC   0(6,R5),0(R4)                 BILL NUMBER                        
         GOTO1 DATCON,DMCB,(1,6(R4)),(8,7(R5))    AND DATE                      
         LA    R4,9(R4)            R4=START OF ACCUMULATORS                     
FORMATB  ZIC   R5,COLPOS           R5=POSITION OF FIRST AMOUNT COL.             
         LA    R5,P(R5)                                                         
         ZIC   R3,WCODES           R3=NUMBER OF AMOUNT COLUMNS                  
         LA    R3,3(R3)                                                         
         ZIC   RF,AMTPOS                                                        
         LA    R4,0(R4,RF)         R4=ACTUAL FIRST ACCUMULATOR                  
FORMATC  CLI   FORMBILL,C'Y'       DETAIL LINE                                  
         BNE   FORMATD                                                          
         CH    R3,=H'4'            IF WORK-CODE AMOUNT                          
         BL    FORMATD                                                          
         CP    0(6,R4),=P'0'       AND NOT ZERO                                 
         BE    FORMATE                                                          
         MVI   PRINTED,C'Y'        THEN TURN ON PRINT SWITCH                    
FORMATD  EDIT  (P6,0(R4)),(11,0(R5)),2,MINUS=YES                                
FORMATE  LA    R4,6(R4)                                                         
         LA    R5,12(R5)                                                        
         BCT   R3,FORMATC                                                       
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
MYREPORT NTR1                                                                   
         MVC   HEAD5+89(L'PERIOD),PERIOD                                        
         MVC   HEAD6+79(18),=C'WORK-CODE(S)=NET  '                              
         CLI   QOPT2,C'G'                                                       
         BNE   *+10                                                             
         MVC   HEAD6+92(5),=C'GROSS'                                            
         L     R2,ADHEIRA                                                       
         MVC   HEAD5+8(3),3(R2)                                                 
         L     R3,ADLVANAM                                                      
         USING ACNAMED,R3                                                       
         ZIC   R4,ACNMLEN                                                       
         SH    R4,=H'3'                                                         
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   HEAD5+12(0),ACNMNAME                                             
         MVC   HEAD8,MYHEAD8       WORK-CODE HEADINGS                           
         MVC   HEAD9,MYHEAD9                                                    
         GOTO1 ACREPORT                                                         
         B     EXIT                                                             
         EJECT                                                                  
TOTFOR   DC    C'TOTALS FOR'                                                    
*                                                                               
         GETEL R3,DATADISP,ELCODE                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              BOX ROUTINES (HOOK)                                              
*                                                                               
*&&US                                                                           
         ENTRY HOOK                                                             
HOOK     NMOD1 0,*HOOK*                                                         
         L     RC,SAVERC           RESTORE REG C                                
         L     R7,ADBOX                                                         
         USING BOXD,R7                                                          
         MVC   MYCOL,SPACES                                                     
         MVC   MYROW,SPACES                                                     
         MVI   MYROW+6,C'T'        SET ROWS                                     
         MVI   MYROW+9,C'M'                                                     
         MVI   MYROW+56,C'B'                                                    
         MVI   MYCOL,C'L'          SET MARGINS                                  
         MVI   MYCOL+109,C'R'                                                   
*                                                                               
*                                  FIND NO. OF WORK-CODES                       
         CLI   WCODES,1                                                         
         BE    HOOK1                                                            
         BL    HOOKX                                                            
         CLI   WCODES,3                                                         
         BL    HOOK2                                                            
         BE    HOOK3                                                            
         B     HOOKX                                                            
*                                                                               
HOOK3    MVI   MYCOL+37,C'C'       3 W/C'S                                      
HOOK2    MVI   MYCOL+49,C'C'       2 W/C'S                                      
HOOK1    MVI   MYCOL+61,C'C'       1 W/C                                        
         MVI   MYCOL+73,C'C'                                                    
         MVI   MYCOL+85,C'C'                                                    
         MVI   MYCOL+97,C'C'                                                    
         B     HOOKX                                                            
*                                                                               
HOOKX    MVC   BOXROWS,MYROW                                                    
         MVC   BOXCOLS,MYCOL                                                    
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         XMOD1 1                                                                
*                                                                               
SAVERC   DC    A(0)                                                             
         LTORG                                                                  
*&&                                                                             
         EJECT                                                                  
         ENTRY MYIO                                                             
MYIO     DS    0D                                                               
         DS    2050C                                                            
*                                                                               
         ENTRY BILLTAB                                                          
BILLTAB  DS    0D                                                               
         DS    300CL45                                                          
         DC    X'FE'                                                            
         EJECT                                                                  
BILLD    DSECT                     DSECT FOR BILLTAB                            
BILLNUM  DS    CL6                                                              
BILLDATE DS    PL3                                                              
BILLWC1  DS    PL6                                                              
BILLWC2  DS    PL6                                                              
BILLWC3  DS    PL6                                                              
BILLNET  DS    PL6                                                              
BILLCOMM DS    PL6                                                              
BILLGRS  DS    PL6                                                              
BILLNEXT EQU   *                                                                
         EJECT                                                                  
ACP8D    DSECT                                                                  
RELO     DS    F                                                                
ELCODE   DS    CL1                                                              
WCS      DS    CL12                2ND CL6 IS TO MARK AS 'POSITIONED'           
WCODES   DS    CL1                 NO. OF W/C'S REQUESTED                       
POSITION DS    CL1                 WHICH COLUMN OF W/C'S IT IS IN               
FORMBILL DS    CL1                                                              
PRINTED  DS    CL1                                                              
SAVERE   DS    F                                                                
MYJOB    DS    CL46                                                             
MYANAL   DS    CL18                                                             
PL13     DS    PL13                                                             
TOTPOS   DS    CL1                 COL. 'TOTALS FOR' STARTS IN                  
BILLPOS  DS    CL1                 COL. BILL NO./AMT STARTS IN                  
COLPOS   DS    CL1                 STARTING EDIT COL.                           
AMTPOS   DS    CL1                 STARTING ACCUM. COL.                         
AMOUNT   DS    PL6                 W/C                                          
JTOTS    DS    6PL6                JOB                                          
PTOTS    DS    6PL6                PRODUCT                                      
CTOTS    DS    6PL6                CLIENT                                       
RTOTS    DS    6PL6                REQUEST                                      
PERIOD   DS    CL13                                                             
WCOUNT   DS    CL1                 NO. OF W/C'S POSITIONED SO FAR               
BCOUNT   DS    PL3                                                              
JCOUNT   DS    PL3                                                              
RCOUNT   DS    PL3                                                              
SVANAL   DS    CL2                                                              
JOBSW    DS    CL1         0=SETUP PENDING, 1=PRINT PENDING, 2=PRINTED          
START    DS    PL3                                                              
END      DS    PL3                                                              
THISBDNO DS    PL6                                                              
THISBDTE DS    CL2                                                              
MYHEAD8  DS    CL132                                                            
MYHEAD9  DS    CL132                                                            
VACLIST  DS    F                                                                
AMYIO    DS    F                                                                
ABILLTAB DS    F                                                                
ADBOX    DS    F                                                                
MYROW    DS    CL100                                                            
MYCOL    DS    CL132                                                            
SKIPSW   DS    CL1                                                              
CLIACTV  DS    CL1                                                              
PSAVE    DS    CL132                                                            
MYPRT1   DS    CL132                                                            
MYPRT2   DS    CL132                                                            
         EJECT                                                                  
GOBLOCKD DSECT                                                                  
       ++INCLUDE ACGOBLOCK                                                      
         EJECT                                                                  
*ACGENBOTH                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
*ACGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
*ACREPWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
*ACGENMODES                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
*DDBIGBOX                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
*DDREPXTRAD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDREPXTRAD                                                     
         PRINT ON                                                               
*DDREPMASTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDREPMASTD                                                     
         PRINT ON                                                               
*ACMASTD                                                                        
         PRINT OFF                                                              
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'030ACREPP802 05/01/02'                                      
         END                                                                    
