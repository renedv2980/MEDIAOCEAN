*          DATA SET SPREPPF02  AT LEVEL 013 AS OF 03/30/05                      
*PHASE SPPF02A                                                                  
*INCLUDE DLFLD                                                                  
         SPACE 1                                                                
*=================================================================*             
* THREE RUN TYPES                                                               
* P PURCHASED                                                                   
* I AFFID LOOKUP ON SPECIFIED BOOK                                              
* A AFFID RERATE ON ACTUAL BOOK                                                 
*                                                                               
* QOPT1 =Y FOR TEST RUN, WHICH JUST CHECKS FOR MISSING AFFIDS                   
*=================================================================*             
         SPACE 1                                                                
*=================================================================*             
* CREATE DOWNLOAD FILE OF SPOTS AND RATINGS                       *             
* COMMA SEPARATED FILE  X'0D0A' TERMINATORS                                     
*=================================================================*             
         TITLE 'SPPF02 - PARAMOUNT ACT FILE DOWNLOAD'                           
SPPF02   CSECT                                                                  
         DS    4096C                                                            
         ORG   *-4096                                                           
         PRINT NOGEN                                                            
         NMOD1 0,SPPF02                                                         
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         USING SPPF02+4096,RC                                                   
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,PROCBUY                                                     
         BE    PROCB                                                            
*                                                                               
         CLI   MODE,MKTFRST                                                     
         BE    MKTF                                                             
*                                                                               
         CLI   MODE,ESTFRST                                                     
         BE    ESTF                                                             
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
*                                                                               
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
         B     EXIT                                                             
*                                                                               
EQXIT    CR    RB,RB                                                            
         B     EXIT                                                             
*                                                                               
NEQXIT   LTR   RB,RB                                                            
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*============================================================*                  
* OPEN A REPORT ON THE PRTQUE AND INITALIZE FOR DOWNLOADING  *                  
*============================================================*                  
         SPACE 1                                                                
RUNF     DS    0H                                                               
         XC    DLCB,DLCB                                                        
D        USING DLCBD,DLCB                                                       
*                                                                               
         MVI   D.DLCBACT,C'I'      START AND INITIALIZE REPORT                  
         MVC   D.DLCBAPR,=A(BLPRINT) PRINT ROUTINE ADDRESS                      
         LA    R0,P                                                             
         ST    R0,D.DLCBAPL        PRINT LINE ADDRESS                           
         OI    D.DLCBFLG1,DLCBFXTN                                              
         MVC   D.DLCXTND(7),MAXLINE                                             
         MVI   FORCEHED,C'Y'       POUR GASTON                                  
*                                                                               
         GOTO1 =V(DLFLD),DLCB                                                   
         B     EXIT                                                             
                                                                                
         EJECT                                                                  
*=================================================================              
* AT RUNLAST, PUT OUT A RECORD WHICH WILL BECOME A SECOND FILE                  
* WHEN RECEVED BY PIANO. THE 'X' RECORD HAS THE FILENAME, AND                   
* TOTAL RECORDS/SPOTS/DOLLARS                                                   
*=================================================================              
                                                                                
*                                                                               
RUNL     LA    R4,RUNTOTS                                                       
         LA    R5,PSLIST                BUILD 3 12 BYTE FIELDS                  
         XC    0(36,R5),0(R5)                                                   
*                                                                               
         EDIT  (P8,(R4)),(12,(R5))      RECORD COUNT                            
*                                                                               
         EDIT  (P8,8(R4)),(12,12(R5))   SPOT COUNT                              
*                                                                               
         EDIT  (P8,16(R4)),(12,24(R5)),2  TOTAL DOLLARS                         
         BAS   RE,BUYPUT                                                        
                                                                                
         LA    R1,DLCB                                                          
         USING DLCBD,R1                                                         
         MVI   D.DLCBACT,C'R'      SET E-O-R                                    
         GOTO1 =V(DLFLD),(R1)                                                   
         DROP  R1                                                               
         B     EXIT                                                             
         EJECT                                                                  
*=====================================================================*         
* REQUEST FIRST PROCESSING                                            *         
*=====================================================================*         
                                                                                
REQF     MVC   MYFLPRD,QPRD        MOVE REQUEST DATA TO FILENAME                
         MVC   MYFLEST,QEST                                                     
         GOTO1 DATCON,DMCB,(5,0),(20,MYFLDATE) TODAY'S DATE YYYYMMDD            
*                                                                               
         L     RE,MEDBUFF                                                       
         USING MEDBLOCK,RE                                                      
         LHI   R0,1                                                             
         STCM  R0,15,MEDNUMPE                                                   
         LHI   R0,256                                                           
         STCM  R0,15,MEDLCHNK                                                   
         XC    MEDNUMMO,MEDNUMMO                                                
         XC    MEDNUMQT,MEDNUMQT                                                
         XC    MEDNUMWK,MEDNUMWK                                                
         MVI   MEDEXTDM,8          ALWAYS 8 DEMOS                               
         MVC   MEDEXTAX,SPOTPROF+12   SET TAX EXTRACT OPTION                    
         MVI   RQPRDEST,C'Y'                                                    
         MVI   RQDAYPT,C'Y'                                                     
         MVI   RQEQUIV,C'Y'                                                     
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         MVI   RUNTYPE,C'P'        SET TYPE=PURCHASED                           
         CLI   QBOOK1,C' '                                                      
         BE    REQF2                                                            
* AFFID LOOKUP NAMES BOOK  AFFID RERATE SAYS ACT                                
         MVI   RUNTYPE,C'I'        SET AFFID LOOKUP                             
         CLC   =C'ACT',QBOOK1                                                   
         BNE   REQF2                                                            
         MVI   RUNTYPE,C'A'        SET AFFID RERATE                             
*                                                                               
REQF2    LA    RE,MYHDHK                                                        
         ST    RE,HEADHOOK                                                      
         STM   R9,RC,HDHKR9                                                     
         LA    RE,BUYHOOK                                                       
         ST    RE,SPOTHOOK                                                      
         STM   R9,RC,BUYHKR9                                                    
*                                                                               
REQFX    B     EXIT                                                             
         DROP  RE                                                               
         EJECT                                                                  
*=====================================================================*         
* ESTIMATE FIRST PROCESSING                                           *         
*=====================================================================*         
                                                                                
ESTF     L     R6,ADEST            GET EST DATES AS MM/DD/YY                    
         USING ESTHDRD,R6                                                       
         GOTO1 DATCON,DMCB,ESTART,(3,DUB)                                       
         GOTO1 (RF),(R1),(3,DUB),(10,MYESTST)                                   
         GOTO1 DATCON,DMCB,EEND,(3,DUB)                                         
         GOTO1 (RF),(R1),(3,DUB),(10,MYESTND)                                   
         DROP  R6                                                               
                                                                                
         GOTOR MEDPRDRD,DMCB,SPWORKD                                            
*                                                                               
         SR    R2,R2                                                            
         IC    R2,BPRD             PRODUCT NUMBER                               
         BCTR  R2,0                                                             
         MH    R2,PRDBUFLN                                                      
         A     R2,PRDBUFF          POINT TO PRDBUFF ENTRY                       
         USING PTBUFFD,R2                                                       
         ST    R2,SVPRDBUF                                                      
*                                                                               
* GET AND SAVE DEMO NAMES                                                       
*                                                                               
         MVC   SVDEMNMS,SPACES                                                  
         L     RE,ADBLOCK                                                       
         USING DBLOCKD,RE                                                       
         MVC   DBSELMED,QMED       SET MEDIA CODE IN DBLOCK                     
         DROP  RE                                                               
* GET 6 CHARACTER DEMO NAMES                                                    
         L     R4,ADEST                                                         
         USING ESTHDRD,R4                                                       
*                                                                               
         MVC   SVDEMNMS,SPACES                                                  
         LHI   R0,8                                                             
*                                                                               
         GOTO1 DEMOCON,DMCB,((R0),PTDEMLST),(6,SVDEMNMS),              X        
               (C'S',ADBLOCK),(SPOTPROF+9,EUSRNMS)                              
         DROP  R2,R4                                                            
         EJECT                                                                  
         GOTO1 MEDDATE,DMCB,SPWORKD                                             
*                                                                               
         LHI   R0,2                SET EST ADJ                                  
         CLI   QRERATE,C' '                                                     
         BE    ESTF20                                                           
*                                                                               
         LHI   R0,3                SET FOR PURCHASED RERATED                    
         CLC   =C'NO',QHUT1                                                     
         BE    *+8                                                              
         AHI   R0,1                SET FOR ADJUSTMENT                           
         CLI   QRERATE,C'I'        RERATE BASED ON INVOICE                      
         BNE   *+8                                                              
         AHI   R0,3                                                             
                                                                                
ESTF20   ST    R0,SVRERATE                                                      
         B     EXIT                                                             
         EJECT                                                                  
*=====================================================================*         
* MARKET FIRST PROCESSING                                             *         
*=====================================================================*         
                                                                                
MKTF     LA    R0,L'MKTNM                                                       
         LA    R1,MKTNM                                                         
*                                                                               
MKTF2    CLI   0(R1),C','                                                       
         BNE   *+8                                                              
         MVI   0(R1),C' '                                                       
         LA    R1,1(R1)                                                         
         BCT   R0,MKTF2                                                         
         B     EXIT                                                             
         EJECT                                                                  
*=====================================================================*         
* BUY RECORD PROCESSING                                               *         
*=====================================================================*         
                                                                                
PROCB    L     R6,ADBUY                                                         
         USING BUYRECD,R6                                                       
*                                                                               
         LA    R0,17               REMOVE COMMAS FROM PROGRAM NAME              
         LA    R1,BDPROGRM                                                      
*                                                                               
PROCB02  CLI   0(R1),C','                                                       
         BNE   *+8                                                              
         MVI   0(R1),C' '                                                       
         LA    R1,1(R1)                                                         
         BCT   R0,PROCB02                                                       
         OC    BDPROGRM(17),SPACES                                              
         DROP  R6                                                               
*                                                                               
         XC    PSLIST,PSLIST                                                    
         GOTOR MEDPSL,DMCB,SPWORKD,PSLIST                                       
*                                                                               
         LA    R2,PSLIST-2         USE R2 AS PSLIST POINTER                     
*                                                                               
         CLI   RUNTYPE,C'P'        TEST PURCHASED RUN                           
         BNE   PROCB30             NO - PROCESS AFFIDS                          
*                                                                               
PROCB10  LA    R2,2(R2)                                                         
         CLI   0(R2),0                                                          
         BE    EXIT                                                             
         CLC   0(1,R2),BPRD                                                     
         BNE   PROCB10                                                          
                                                                                
         L     R3,MEDBUFF          SET PRODUCT AND SPOT LENGTH                  
         USING MEDBLOCK,R3                                                      
         MVC   MEDBRAND,0(R2)                                                   
         MVC   MEDSPTLN,1(R2)                                                   
         SPACE 1                                                                
*================================================================               
* TO DO PURCHASED, NEED ALL SPOTS ON A SINGLE DATE                              
* SO MAKE A FIRST PASS TO BUILD A LIST OF DATES, THEN FILTER ON                 
* THEM ONE AT A TIME                                                            
*================================================================               
         SPACE 1                                                                
         XC    DATELIST,DATELIST                                                
         XC    ADATELST,ADATELST                                                
         XC    CURDATE,CURDATE     SET 'BUILD DATE LIST' FLAG                   
*                                                                               
         GOTOR MEDGETBY,DMCB,SPWORKD,=F'0'                                      
*                                                                               
         LA    R4,DATELIST                                                      
*                                                                               
PROCB20  MVC   CURDATE,0(R4)                                                    
         GOTOR MEDGETBY,DMCB,SPWORKD,SVRERATE                                   
*                                                                               
         BAS   RE,BUYBLD                                                        
         BZ    *+8                                                              
         BAS   RE,BUYPUT                                                        
*                                                                               
         LA    R4,2(R4)            NEXT DATE                                    
         OC    0(2,R4),0(R4)                                                    
         BNZ   PROCB20                                                          
*                                                                               
         B     PROCB10                                                          
         EJECT                                                                  
*================================================================               
* TO DO AFFID ON ACTUAL, WE NEED TO DO ONE SPOT AT A TIME                       
* SPOTHOOK WILL MATCH MGBYSPOT TO CURSPOT AND SET SPOTYORN TO Y                 
* FOR ONE AND ONLY ONE SPOT ON EACH MEDGETBY CALL                               
* IF ACURSPOT=0 ON RETURN, THIS BUY IS FINISHED                                 
*================================================================               
                                                                                
PROCB30  LA    R2,2(R2)                                                         
         CLI   0(R2),0                                                          
         BE    EXIT                                                             
         CLC   0(1,R2),BPRD                                                     
         BNE   PROCB30                                                          
                                                                                
         L     R3,MEDBUFF          SET PRODUCT AND SPOT LENGTH                  
         USING MEDBLOCK,R3                                                      
         MVC   MEDBRAND,0(R2)                                                   
         MVC   MEDSPTLN,1(R2)                                                   
*                                                                               
         XC    MGBYSPOT,MGBYSPOT                                                
*                                                                               
PROCB34  LH    R0,MGBYSPOT                                                      
         AHI   R0,1                                                             
         STH   R0,MGBYSPOT         BUMP SEARCH SPOT NUMBER                      
*                                                                               
         XC    CURSPOT,CURSPOT     CLEAR HOOK COUNT                             
         XC    ACURSPOT,ACURSPOT   AND ADDRESS OF SPOT                          
         GOTOR MEDGETBY,DMCB,SPWORKD,SVRERATE                                   
*                                                                               
         OC    ACURSPOT,ACURSPOT   TEST SPOT FOUND                              
         BZ    PROCB30             IF NOT FOUND, NEXT LIST ENTRY                
*                                                                               
         BAS   RE,BUYBLD                                                        
         BZ    *+8                                                              
         BAS   RE,BUYPUT                                                        
         B     PROCB34                                                          
                                                                                
*=====================================================================*         
* CREATE ACT FILE RECORD                                              *         
*=====================================================================*         
                                                                                
BUYBLD   NTR1                                                                   
         LA    R0,THISREC                                                       
         LA    R1,THISRECL                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R5,MEDPERD                                                       
         L     R4,4(R5)            POINT TO PERIOD TOTALS                       
         USING MEDDATA,R4                                                       
         OC    MEDBYD(12),MEDBYD   TEST SPOTS OR DOLLARS                        
         BZ    EQXIT               NONE - EXIT WITH CC EQ                       
*                                                                               
         MVC   THISRTYP,RUNTYPE                                                 
         MVC   THISPRD,PRD                                                      
         MVC   THISPRNM,PRDNM                                                   
         OC    THISPRNM,SPACES                                                  
         MVI   THISEST,C'0'                                                     
         MVC   THISEST+1(3),EST                                                 
         MVC   THISESNM,ESTNM                                                   
         OC    THISESNM,SPACES                                                  
         MVC   THISMKNM,MKTNM                                                   
         OC    THISMKNM,SPACES                                                  
         MVC   THISSTA,STA                                                      
         OC    THISSTA,SPACES                                                   
*                                                                               
         L     R6,ADSTAT                                                        
         USING STARECD,R6                                                       
         MVC   THISAFF,SNETWRK                                                  
         CLI   THISAFF,C' '                                                     
         BH    *+10                                                             
         MVC   THISAFF,=C'IND'                                                  
         DROP  R6                                                               
*                                                                               
         L     R6,ADBUY                                                         
         USING BUYRECD,R6                                                       
         MVC   THISDPT,BDDAYPT                                                  
         MVC   THISDPNM,MEDDPART                                                
*                                                                               
         SR    R0,R0                                                            
         IC    R0,1(R2)            CURRENT SLN                                  
         EDIT  (R0),(3,THISSLN),ALIGN=LEFT                                      
*                                                                               
         MVC   THISESST,MYESTST      MM/DD/YY                                   
         MVC   THISESND,MYESTND      MM/DD/YY                                   
*                                                                               
         LA    R1,BDTIMST                                                       
         BAS   RE,CVTIME                                                        
         MVC   THISSTTM,TIME                                                    
*                                                                               
         LA    R1,BDTIMEND                                                      
         BAS   RE,CVTIME                                                        
         MVC   THISNDTM,TIME                                                    
*                                                                               
         MVC   THISPROG,BDPROGRM                                                
         OC    THISPROG,SPACES                                                  
         DROP  R6                                                               
*                                                                               
         ICM   R0,15,MEDBYSPT                                                   
         BZ    BUYBLDX                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  THISSPTS,DUB                                                     
         AP    RUNSPOTS,DUB        ADD TO TOTAL SPOT COUNT                      
*                                                                               
         L     R0,MEDBYD                                                        
         CVD   R0,DUB                                                           
         AP    RUNDOLS,DUB                                                      
* NEED TO DIVIDE DOLS BY SPOTS TO GET RATE PER SPOT                             
         L     R1,MEDBYD                                                        
         M     R0,=F'2'                                                         
         D     R0,MEDBYSPT                                                      
         AHI   R0,1                                                             
         SRL   R1,1                                                             
         EDIT  (R1),(10,THISCOST),2,ZERO=NOBLANK                                
*                                                                               
BUYBLD2  LA    RE,THISDEM1                                                      
         LHI   RF,8                                                             
         LA    R6,MEDBY1                                                        
         LA    R7,SVDEMNMS                                                      
*                                                                               
BUYBLD4  MVC   0(6,RE),0(R7)       DEMO NAME                                    
         L     R1,0(R6)            DEMO VALUE                                   
*                                                                               
         CLC   MEDBYSPT,=F'1'                                                   
         BE    BUYBLD6                                                          
* NEED TO DIVIDE DEMO VALUE BY NUMBER OF SPOTS                                  
         M     R0,=F'2'            X 2                                          
         D     R0,MEDBYSPT                                                      
         AHI   R1,1                                                             
         SRL   R1,1                                                             
*                                                                               
BUYBLD6  TM    RQOPTS,RQOPTS_2DEC  TEST 2-DECIMAL RATINGS ACTIVE                
         BZ    BUYBLD8             NO                                           
         CLI   0(R7),C'R'                                                       
         BE    *+12                                                             
         CLI   0(R7),C'E'                                                       
         BNE   BUYBLD6                                                          
         M     R0,=F'2'                                                         
         D     R0,=F'10'           ROUND TO 1 DECIMAL                           
         AHI   R1,1                                                             
         SRL   R1,1                                                             
*                                                                               
BUYBLD8  EDIT  (R1),(6,6(RE)),1,ZERO=NOBLANK                                    
         LA    RE,12(RE)                                                        
         LA    R6,8(R6)                                                         
         LA    R7,6(R7)                                                         
         BCT   RF,BUYBLD4                                                       
*                                                                               
         CLI   RUNTYPE,C'P'        TEST PURCHASED RUN                           
         BNE   BUYBLD10                                                         
         GOTO1 DATCON,DMCB,(2,CURDATE),(10,THISDATE)                            
         MVC   THISTIME,THISSTTM   SET AIRTIME=START TIME                       
         B     BUYBLDX                                                          
*                                                                               
BUYBLD10 L     R6,ACURSPOT                                                      
         TM    6(R6),X'C0'         TEST MINUS OR MINUSSED                       
         BNZ   NEQXIT                                                           
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0               POINT TO AFFID                               
         CLI   0(R6),X'10'                                                      
         BNE   BUYBLD20                                                         
         GOTO1 DATCON,DMCB,(2,2(R6)),(10,THISDATE)                              
*                                                                               
         XC    FULL,FULL                                                        
         MVC   FULL(2),4(R6)                                                    
         NI    FULL,X'0F'                                                       
         LA    R1,FULL                                                          
         BAS   RE,CVTIME                                                        
         MVC   THISTIME,TIME                                                    
*                                                                               
BUYBLDX  DS    0H                                                               
         B     NEQXIT                                                           
*                                                                               
BUYBLD20 CLI   QOPT1,C'Y'          TEST  RUN ?                                  
         BE    *+6                                                              
         DC    H'0'                IF NOT, DIE                                  
         MVC   P(19),=C'** MISSING AFFID **'                                    
         MVC   P+20(1),QMED                                                     
         MVC   P+22(3),CLT                                                      
         MVC   P+26(3),PRD                                                      
         MVC   P+30(4),MKT                                                      
         MVC   P+35(5),BIGSTA                                                   
         MVC   P+41(3),EST                                                      
         MVI   P+44,C'-'                                                        
         L     RE,ADBUY                                                         
         SR    R0,R0                                                            
         IC    R0,10(RE)           LINE NUMBER                                  
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+45(3),DUB                                                      
         GOTO1 DATCON,DMCB,(2,2(R6)),(8,P+50)                                   
         GOTO1 REPORT                                                           
         B     NEQXIT                                                           
         DROP  R3                                                               
         EJECT                                                                  
CVTIME   NTR1                                                                   
         XC    TIME,TIME                                                        
         SR    RE,RE                                                            
         ICM   RF,3,0(R1)                                                       
         D     RE,=F'100'          HOURS IN RF, MINS IN RE                      
         MVI   TIME+6,C'A'                                                      
         CHI   RF,12                                                            
         BNH   *+12                                                             
         AHI   RF,-12                                                           
         MVI   TIME+6,C'P'                                                      
         LTR   RF,RF                                                            
         BNZ   *+8                                                              
         AHI   RF,12                                                            
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  TIME(2),DUB                                                      
         MVI   TIME+2,C':'                                                      
*                                                                               
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  TIME+3(2),DUB                                                    
         MVI   TIME+5,C':'                                                      
         MVC   TIME+6(2),=C'00'                                                 
         B     EXIT                                                             
         EJECT                                                                  
*=============================================================*                 
* OUTPUT  DATA TO PRTQUE REPORT                                                 
*=============================================================*                 
         SPACE 1                                                                
*                                                                               
BUYPUT   NTR1                                                                   
         CLI   QOPT1,C'Y'          TEST RUN ?                                   
         BE    EXIT                                                             
*                                                                               
         CLI   MODE,RUNLAST                                                     
         BNE   BUYPUT2                                                          
         LA    R4,TOTTAB                                                        
         B     BUYPUT4                                                          
*                                                                               
BUYPUT2  AP    RUNRECS,=P'1'                                                    
         LA    R4,THISBTAB                                                      
*                                                                               
BUYPUT4  L     RE,0(R4)            GET DATA ADDR                                
         ZIC   RF,4(R4)            GET DATA LEN                                 
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   D.DLCBFLD(0),0(RE)                                               
*                                                                               
         MVC   D.DLCBTYP(1),5(R4)                                               
         MVI   D.DLCBACT,DLCBPUT                                                
*                                                                               
         L     RF,=V(DLFLD)                                                     
         GOTO1 (RF),DLCB                                                        
*                                                                               
         LA    R4,L'THISBTAB(R4)                                                
         CLI   0(R4),X'FF'                                                      
         BNE   BUYPUT4                                                          
*                                                                               
         MVI   D.DLCBACT,DLCBEOL                                                
         L     RF,=V(DLFLD)                                                     
         GOTO1 (RF),DLCB                                                        
*                                                                               
         MVI   D.DLCBACT,DLCBEOL                                                
         L     RF,=V(DLFLD)                                                     
         GOTO1 (RF),DLCB                                                        
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*==============================================================*                
* USER PRINT ROUTINE EXIT CALLED BY DLFLD                      *                
* ALL DATA PRINTED HERE GOES ON PAGE 2                         *                
*==============================================================*                
BLPRINT  NTR1                                                                   
         MVI   LINE,0              FORCE NO PAGE BREAK                          
         GOTO1 REPORT                                                           
         MVI   FORCEHED,C'N'                                                    
         B     EXIT                                                             
*                                                                               
* THESE FIELDS USED TO GET WIDE PRINT LINE OVERRIDES                            
* THEY GET MOVED INTO THE DLCB TO OVERRIDE MAXLINE                              
*                                                                               
MAXLINE  DC    H'132'              MAX LINE WIDTH                               
DELIM    DC    C' '                FIELD DELIMITER CHR                          
EOTCHR   DC    C'"'                END OF TEXT FIELD DELIMITER                  
EOTALT   DC    C''''               END OF TEXT CHR ALTERNATE                    
EOLCHR   DC    X'5E'               END OF LINE CHAR - SEMICOLON                 
EORCHR   DC    C':'                END OF REPORT CONTROL CHR                    
*                                                                               
         DS    0D                                                               
         DC    CL8'*THISREC'                                                    
THISREC  DS    0D                                                               
THISRTYP DS    CL1     01          P=PUR A=ACT I=INVOICED                       
THISPRD  DS    CL3     02          PRODUCT CODE                                 
THISPRNM DS    CL24    03          PRODUCT CODE                                 
THISEST  DS    CL4     04          EST NUM                                      
THISESNM DS    CL20    05          EST NAME                                     
THISMKNM DS    CL24    06          MARKET NAME                                  
THISSTA  DS    CL5     07          STATION CALL LETTERS                         
THISAFF  DS    CL4     08          STATION AFFILIATE                            
THISDPT  DS    CL1     09          DAYPART CODE                                 
THISDPNM DS    CL3     10          DAYPART NAME (SAME AS DPT CODE)              
THISSLN  DS    CL3     11          SPOTLEN                                      
THISESST DS    CL8     12          MM/DD/YY                                     
THISESND DS    CL8     13          MM/DD/YY                                     
THISSTTM DS    CL8     14          START TIME HH:MM:SS                          
THISNDTM DS    CL8     15          END TIME   HH:MM:SS                          
THISPROG DS    CL16    16          PROGRAM NAME                                 
THISSPTS DS    CL3     17          =1 FOR A/I  TOTAL FOR P                      
THISCOST DS    CL10    18          COST/SPOT 9999999.99                         
*                                                                               
THISDEM1 DS    CL6     19          DEMO CODE 1                                  
THISVAL1 DS    CL6     20          DEMO VALUE 1 (9999.9)                        
THISDEM2 DS    CL6     21                                                       
THISVAL2 DS    CL6     22                                                       
THISDEM3 DS    CL6     23                                                       
THISVAL3 DS    CL6     24                                                       
THISDEM4 DS    CL6     25                                                       
THISVAL4 DS    CL6     26                                                       
THISDEM5 DS    CL6     27                                                       
THISVAL5 DS    CL6     28                                                       
THISDEM6 DS    CL6     29                                                       
THISVAL6 DS    CL6     30                                                       
THISDEM7 DS    CL6     31                                                       
THISVAL7 DS    CL6     32                                                       
THISDEM8 DS    CL6     33                                                       
THISVAL8 DS    CL6     34                                                       
*                                                                               
THISDATE DS    CL8     35          AIR DATE MM/DD/YY                            
THISTIME DS    CL8     36          AIR TIME HH:MM:SS                            
THISRECX EQU   *                                                                
THISRECL EQU   *-THISREC                                                        
*                                                                               
         DS    0D                                                               
THISBTAB DS    0XL6                                                             
         DC   AL4(THISRTYP),AL1(L'THISRTYP),C'T'                                
         DC   AL4(THISPRD),AL1(L'THISPRD),C'T'                                  
         DC   AL4(THISPRNM),AL1(L'THISPRNM),C'T'                                
         DC   AL4(THISEST),AL1(L'THISEST),C'T'                                  
         DC   AL4(THISESNM),AL1(L'THISESNM),C'T'                                
         DC   AL4(THISMKNM),AL1(L'THISMKNM),C'T'                                
         DC   AL4(THISSTA),AL1(L'THISSTA),C'T'                                  
         DC   AL4(THISAFF),AL1(L'THISAFF),C'T'                                  
         DC   AL4(THISDPT),AL1(L'THISDPT),C'T'                                  
         DC   AL4(THISDPNM),AL1(L'THISDPNM),C'T'                                
         DC   AL4(THISSLN),AL1(L'THISSLN),C'T'                                  
         DC   AL4(THISESST),AL1(L'THISESST),C'T'                                
         DC   AL4(THISESND),AL1(L'THISESND),C'T'                                
         DC   AL4(THISSTTM),AL1(L'THISSTTM),C'T'                                
         DC   AL4(THISNDTM),AL1(L'THISNDTM),C'T'                                
         DC   AL4(THISPROG),AL1(L'THISPROG),C'T'                                
         DC   AL4(THISSPTS),AL1(L'THISSPTS),C'T'                                
         DC   AL4(THISCOST),AL1(L'THISCOST),C'T'                                
         DC   AL4(THISDEM1),AL1(L'THISDEM1),C'T'                                
         DC   AL4(THISVAL1),AL1(L'THISVAL1),C'T'                                
         DC   AL4(THISDEM2),AL1(L'THISDEM2),C'T'                                
         DC   AL4(THISVAL2),AL1(L'THISVAL2),C'T'                                
         DC   AL4(THISDEM3),AL1(L'THISDEM3),C'T'                                
         DC   AL4(THISVAL3),AL1(L'THISVAL3),C'T'                                
         DC   AL4(THISDEM4),AL1(L'THISDEM4),C'T'                                
         DC   AL4(THISVAL4),AL1(L'THISVAL4),C'T'                                
         DC   AL4(THISDEM5),AL1(L'THISDEM5),C'T'                                
         DC   AL4(THISVAL5),AL1(L'THISVAL5),C'T'                                
         DC   AL4(THISDEM6),AL1(L'THISDEM6),C'T'                                
         DC   AL4(THISVAL6),AL1(L'THISVAL6),C'T'                                
         DC   AL4(THISDEM7),AL1(L'THISDEM7),C'T'                                
         DC   AL4(THISVAL7),AL1(L'THISVAL7),C'T'                                
         DC   AL4(THISDEM8),AL1(L'THISDEM8),C'T'                                
         DC   AL4(THISVAL8),AL1(L'THISVAL8),C'T'                                
         DC   AL4(THISDATE),AL1(L'THISDATE),C'T'                                
         DC   AL4(THISTIME),AL1(L'THISTIME),C'T'                                
         DC   X'FF'                                                             
THISBEAB DC   C'B'                                                              
THISBEAX DC   C'X'                                                              
*                                                                               
* FIELDS BELOW ARE CONSTRUCTED ON THE FLY AT RUNLAST                            
*                                                                               
TOTTAB   DC   AL4(THISBEAX),AL1(1),C'T'    C'X'   DRAPEAU POUR GASTON           
         DC   AL4(MYFLNAME),AL1(L'MYFLNAME),C'T'  CLT_EST_DATE                  
         DC   AL4(PSLIST+00),AL1(12),C'T'  RECORDS                              
         DC   AL4(PSLIST+12),AL1(12),C'T'  SPOTS                                
         DC   AL4(PSLIST+24),AL1(12),C'T'  DOLLARS                              
         DC   X'FF'                                                             
*                                                                               
         DS    0D                                                               
PSLIST   DS    XL64                                                             
SVPRDBUF DS    A                   PRDBUFF ENTRY FOR THIS PRD                   
SVRERATE DS    A                                                                
SVDEMNMS DS    8CL6                DEMO NAMES                                   
SVNUMDEM DS    H                                                                
MGBYSPOT DS    H                   REQUESTED SPOT NUMBER                        
CURSPOT  DS    H                   CURRENT SPOT NUM COUNTED BY HOOK             
ACURSPOT DS    A                   POINTER TO CURRENT SPOT                      
TIME     DS    D                                                                
*                                                                               
MYFLNAME DS    0CL16                                                            
MYFLPRD  DS    CL3                                                              
         DC    C'_'                                                             
MYFLEST  DS    CL3                                                              
         DC    C'_'                                                             
MYFLDATE DS    CL8                                                              
*                                                                               
RUNTYPE  DS    C                                                                
MYESTST  DS    CL8                                                              
MYESTND  DS    CL8                                                              
CURDATE  DS    H                   CURRENT SPOT NUM COUNTED BY HOOK             
ADATELST DS    A                   ADDR OF MOST RECENT LIST ENTRY               
DATELIST DC    0XL128                                                           
         DS    64XL2                                                            
*                                                                               
         DS    0D                                                               
         DC    CL8'RUNTOTS'                                                     
RUNTOTS  DS    0D                                                               
RUNRECS  DC    PL8'0'                                                           
RUNSPOTS DC    PL8'0'                                                           
RUNDOLS  DC    PL8'0'                                                           
*                                                                               
         DS    0D                                                               
DLCB     DS    XL256                                                            
         LTORG                                                                  
         EJECT                                                                  
         DROP  R9,RA,RB,RC                                                      
         DS    0D                                                               
         USING *,RF                                                             
MYHDHK   NTR1                                                                   
         LM    R9,RC,HDHKR9                                                     
         DROP  RF                                                               
         USING SPPF02,RB,RC                                                     
         USING SPWORKD,RA,R9                                                    
         XIT1                                                                   
*                                                                               
HDHKR9   DS    A                                                                
HDHKRA   DS    A                                                                
HDHKRB   DS    A                                                                
HDHKRC   DS    A                                                                
         EJECT                                                                  
*=============================================================                  
* THIS HOOK FOR AFFID RERATES SELECTS ONE SPOT                                  
*=============================================================                  
         DROP  R9,RA,RB,RC                                                      
         DS    0D                                                               
         USING *,RF                                                             
BUYHOOK  NTR1                      FILTER FOR AFFID RERATES                     
         LM    R9,RC,BUYHKR9                                                    
         DROP  RF                                                               
         USING SPPF02,RB,RC                                                     
         USING SPWORKD,RA,R9                                                    
*                                                                               
         MVI   SPOTYORN,C'N'       PRESET TO REJECT                             
*                                                                               
         CLI   RUNTYPE,C'P'        TEST PURCHASED RUN                           
         BNE   BUYHK20                                                          
* FOR PURCHASED, MATCH SPOT DATE TO CURDATE                                     
         OC    CURDATE,CURDATE     TEST BUILD DATES                             
         BNZ   BUYHK10             YES                                          
*                                                                               
         L     R6,SPOTADDR                                                      
         ICM   R4,15,ADATELST      GET ADDR OF LAST DATE ENTRY                  
         BNZ   BUYHK2                                                           
         LA    R4,DATELIST         IF NONE, MAKE THIS THE FIRST                 
         B     BUYHK4                                                           
*                                                                               
BUYHK2   CLC   0(2,R4),2(R6)       TEST SAME DATE                               
         BE    EXIT                YES - DONE                                   
         LA    R4,2(R4)            POINT TO NEXT ENTRY                          
*                                                                               
BUYHK4   MVC   0(2,R4),2(R6)                                                    
         ST    R4,ADATELST                                                      
         B     BUYHKX              ALL SPOTS EXIT WITH DO NOT PROCESS           
*                                                                               
BUYHK10  L     R6,SPOTADDR                                                      
         CLC   2(2,R6),CURDATE                                                  
         BNE   BUYHKX                                                           
         MVI   SPOTYORN,C'Y'                                                    
         B     BUYHKX                                                           
*                                                                               
BUYHK20  LH    R0,CURSPOT          BUMP SPOT COUNT                              
         AHI   R0,1                                                             
         STH   R0,CURSPOT                                                       
         CLC   CURSPOT,MGBYSPOT                                                 
         BNE   BUYHKX                                                           
         MVC   ACURSPOT,SPOTADDR   SAVE SPOT ADDRESS                            
         MVI   SPOTYORN,C'Y'       AND SET TO PROCESS IT                        
*                                                                               
BUYHKX   XIT1                                                                   
*                                                                               
BUYHKR9  DS    A                                                                
BUYHKRA  DS    A                                                                
BUYHKRB  DS    A                                                                
BUYHKRC  DS    A                                                                
         LTORG                                                                  
         EJECT                                                                  
         PRINT OFF                                                              
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
*                                                                               
       ++INCLUDE DDDLCB                                                         
         EJECT                                                                  
* DSECT FOR PRINT LINE                                                          
PLINED   DSECT                                                                  
*                                                                               
PMED     DS    CL1                                                              
         DS    CL1                                                              
PCLT     DS    CL3                                                              
         DS    CL1                                                              
PPRD     DS    CL3                                                              
         DS    CL1                                                              
PMKT     DS    CL4                                                              
         DS    CL1                                                              
PSTA     DS    CL5                                                              
         DS    CL1                                                              
PEST     DS    CL3                                                              
         DS    CL1                                                              
PLIN     DS    CL3                                                              
         DS    CL1                                                              
PCOST    DS    CL8                                                              
         DS    CL1                                                              
PDATE    DS    CL6                                                              
         DS    CL1                                                              
PTIME    DS    CL6                                                              
         DS    CL1                                                              
         ORG   PDATE                                                            
PSPOTS   DS    CL3                                                              
         ORG                                                                    
                                                                                
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPMEDBLOCK                                                     
       ++INCLUDE SPREPPTBUF                                                     
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013SPREPPF02 03/30/05'                                      
         END                                                                    
